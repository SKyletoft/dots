-- maki/fm — the formal-methods workflow plugin
--
-- A deterministic coordinator for the autonomous Rust/Agda loop described in
-- the project's AGENTS.md.  Lua owns everything mechanical (task selection,
-- worktrees, subagent spawning, nix builds, fmt/clippy/test/agda gates, git
-- commit/merge and bookkeeping).  A cheap "decision" model is asked only for
-- small, enumerated verdicts (accept/revise/escalate).  The chat model is
-- woken exactly once per block, with a short, actionable report.
--
-- Installed as ~/.config/maki/lua/fm (a plugin folder: init.lua +
-- plugin.toml) and loaded from ~/.config/maki/init.lua with:
--
--     require("fm").setup({ --[[ optional overrides ]] })
--
-- The same plugin.toml is installed at ~/.config/maki/plugin.toml, because
-- maki grants permissions for its config Lua from the config-dir manifest.
-- It needs:
--
--     [permissions]
--     fs_read = true
--     fs_write = true
--     run = true
--     agent = true
--     session_write = true
--
-- Commands:
--     /fm start [task-id]   start the loop (whole backlog, or one task)
--     /fm stop              stop it and close the running subagent
--     /fm status            state, attempts, last checks (float window)
--     /fm resume [note]     continue after a block, with optional guidance
--
-- The coordinating model gets one tool, `fm_loop`, so it can answer a wake
-- with { action = "resume", note = "..." } without any further ceremony.

local M = {}

local DEFAULTS = {
	-- Where worktrees live. Defaults to the common git dir's parent, which is
	-- the repository root (AGENTS.md: worktrees go directly in there).
	worktree_prefix = "wt-",
	branch_prefix = "task/",

	-- Execute everything project-related inside the devShell.
	nix = "nix develop --command",
	fmt_check_cmd = "cargo fmt -- --check",
	fmt_write_cmd = "cargo fmt",
	clippy_cmd = "cargo clippy --all-targets -- -D warnings",
	test_cmd = "cargo test && cargo test --release",
	agda_cmd = "agda -i . -i ../spec ImplProofs.agda",
	agda_cwd = "backend",
	proofs_path = "backend/ImplProofs.agda",
	tests_dir = "backend/tests/",

	-- Files the coordinator consults.
	instructions_file = "AGENTS.md",
	todo_file = "todo.md",
	spec_files = { "spec/Twitter.agda", "spec/Interface.agda" },

	-- Tracked paths whose edits must not block a merge (session/tool config).
	dirty_allow_prefixes = { ".maki/" },

	-- Decision model: a no-tools session on the cheapest tier. Set
	-- use_decision_model = false to rely only on the deterministic gates and
	-- the subagents' own FM_VERDICT line.
	use_decision_model = true,
	decision_tier = "weak",
	decision_model = nil,

	-- Optional model spec for the review subagent (nil inherits the chat model).
	review_model = nil,

	-- Bounded repair loops. Exceeding one of these blocks and wakes the chat.
	max_plan_revisions = 2,
	max_tests_revisions = 2,
	max_impl_revisions = 2,
	max_verify_rounds = 3,
	max_review_revisions = 1,

	command_timeout_secs = 1800,
	agda_timeout_secs = 1800,

	-- Resume a persisted running state when its session regains focus
	-- (e.g. after /reload). Blocks never auto-resume.
	auto_resume = true,

	coauthor = "maki@deepseek-v4.1-flash <maki@deepseek-v4.1-flash>",
}

local cfg = {}
for k, v in pairs(DEFAULTS) do
	cfg[k] = v
end

-- ---------------------------------------------------------------------------
-- Small utilities
-- ---------------------------------------------------------------------------

local function copy_list(t)
	local out = {}
	for i, v in ipairs(t or {}) do
		out[i] = v
	end
	return out
end

local function trim(s)
	s = tostring(s or "")
	return (s:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function clip(s, n)
	s = tostring(s or "")
	if #s <= n then
		return s
	end
	return s:sub(1, n) .. "\n[... " .. (#s - n) .. " bytes omitted]"
end

local function slug(s)
	s = tostring(s or "")
	s = s:gsub("[^%w%._%-]+", "_")
	if s == "" then
		s = "x"
	end
	return s
end

local function join(...)
	return maki.fs.joinpath(...)
end

local function read_file(path)
	if not path then
		return nil
	end
	local s, err = maki.fs.read(path)
	if not s then
		return nil, err
	end
	return s
end

local function now()
	return os.time()
end

local function iso(ts)
	return os.date("!%Y-%m-%dT%H:%M:%SZ", ts or now())
end

local function plain(s, n)
	s = tostring(s or "")
	if n and #s > n then
		s = s:sub(1, n)
	end
	return s
end

local function lines_of(s)
	local out = {}
	for line in tostring(s or ""):gmatch("[^\r\n]+") do
		out[#out + 1] = line
	end
	return out
end

-- Keep the lines a human (or a cheap model) needs from a build log, never the
-- whole thing.
local function digest(text, max_lines)
	max_lines = max_lines or 30
	local keep, last = {}, {}
	for line in tostring(text or ""):gmatch("[^\r\n]+") do
		local l = line:gsub("\27%[[%d;]*%a", "")
		if #l > 240 then
			l = l:sub(1, 240) .. "..."
		end
		last[#last + 1] = l
		if #last > 8 then
			table.remove(last, 1)
		end
		local low = l:lower()
		if
			low:find("error", 1, true)
			or low:find("warning:", 1, true)
			or low:find("failed", 1, true)
			or low:find("panicked", 1, true)
			or low:find("test result", 1, true)
			or low:find("unsolved", 1, true)
			or low:find("postulate", 1, true)
			or low:find("cannot", 1, true)
		then
			if #keep < max_lines then
				keep[#keep + 1] = l
			end
		end
	end
	if #keep == 0 then
		return table.concat(last, "\n")
	end
	local out = table.concat(keep, "\n")
	if #keep >= max_lines then
		out = out .. "\n[... more lines omitted]"
	end
	return out
end

-- ---------------------------------------------------------------------------
-- Processes and git
-- ---------------------------------------------------------------------------

local function command_label(cmd)
	if type(cmd) == "table" then
		return table.concat(cmd, " ")
	end
	return tostring(cmd)
end

-- Run a command and keep only capped tails of its output.  Result:
--   { ok, code, stdout, stderr, timed_out, start_error, secs, command }
local function run_cmd(cmd, opts)
	opts = opts or {}
	local cap = opts.capture_lines or 4000
	-- Ring buffers: keep the tail, where build errors actually are.
	local ring_out, ring_err = {}, {}
	local n_out, n_err = 0, 0
	local done, code = false, nil

	local function push(ring, n, line)
		ring[((n - 1) % cap) + 1] = line
	end

	local id, err = maki.fn.jobstart(cmd, {
		cwd = opts.cwd,
		env = opts.env,
		tail = 100,
		on_stdout = function(_, line)
			n_out = n_out + 1
			push(ring_out, n_out, line)
		end,
		on_stderr = function(_, line)
			n_err = n_err + 1
			push(ring_err, n_err, line)
		end,
		on_exit = function(_, c)
			code = c
			done = true
		end,
	})

	if not id then
		return {
			ok = false,
			start_error = err or "jobstart failed",
			command = command_label(cmd),
			stdout = "",
			stderr = "",
			secs = 0,
		}
	end

	local t0 = now()
	local timed_out = false
	local timeout = opts.timeout or cfg.command_timeout_secs
	while not done do
		if now() - t0 >= timeout then
			timed_out = true
			maki.fn.jobstop(id)
			for _ = 1, 10 do
				maki.async.sleep(20)
			end
			break
		end
		maki.async.sleep(100)
	end

	local function finish(ring, n)
		local kept = math.min(n, cap)
		local out = {}
		local start = 1
		if n > cap then
			start = (n % cap) + 1
		end
		for i = 0, kept - 1 do
			local idx = ((start - 1 + i) % cap) + 1
			out[#out + 1] = ring[idx] or ""
		end
		local t = table.concat(out, "\n")
		if n > kept then
			t = "[" .. (n - kept) .. " earlier lines omitted]\n" .. t
		end
		return t
	end

	return {
		ok = (not timed_out) and code == 0,
		code = code,
		stdout = finish(ring_out, n_out),
		stderr = finish(ring_err, n_err),
		timed_out = timed_out,
		secs = now() - t0,
		command = command_label(cmd),
	}
end

local function git(cwd, args, timeout)
	local argv = { "git", "-C", cwd }
	for _, a in ipairs(args) do
		argv[#argv + 1] = a
	end
	return run_cmd(argv, { timeout = timeout or 180 })
end

local function nix_cmd(cmd)
	if not cfg.nix or cfg.nix == "" then
		return cmd
	end
	return cfg.nix .. " " .. cmd
end

-- Strip Agda comments before looking for postulates, so prose does not count.
local function strip_agda_comments(src)
	src = src:gsub("{%-.-%-}", "")
	src = src:gsub("%-%-%[%[.-%]%]", "")
	src = src:gsub("%-%-[^\n]*", "")
	return src
end

-- ---------------------------------------------------------------------------
-- State, reports, plans
-- ---------------------------------------------------------------------------

local function data_dir(sid, root)
	local d = maki.session.data_dir({ session = sid })
	if d then
		return join(d, "loop")
	end
	local base = maki.env.state_dir() or "/tmp"
	return join(base, "formal-methods", slug(root or "root"))
end

local function state_path(data)
	return join(data, "state.json")
end

local function load_state(path)
	local text = read_file(path)
	if not text then
		return nil
	end
	local ok, st = pcall(maki.json.decode, text)
	if not ok or type(st) ~= "table" then
		return nil
	end
	return st
end

local function save_state(s)
	if not s.state.updated_at then
		s.state.updated_at = now()
	end
	s.state.updated_at = now()
	maki.fs.mkdir(s.data, { parents = true })
	maki.fs.atomic_write(state_path(s.data), maki.json.encode(s.state))
end

local function mkdirs(s)
	maki.fs.mkdir(s.data, { parents = true })
	maki.fs.mkdir(join(s.data, "plans"), { parents = true })
	maki.fs.mkdir(join(s.data, "reports"), { parents = true })
end

local function plan_path(s)
	return join(s.data, "plans", slug(s.state.task and s.state.task.id or "task") .. ".md")
end

local function report_path(s, stage)
	return join(
		s.data,
		"reports",
		slug(s.state.task and s.state.task.id or "task") .. "-" .. slug(stage) .. "-" .. tostring(os.time()) .. ".md"
	)
end

local function save_text(path, text)
	maki.fs.mkdir(maki.fs.dirname(path), { parents = true })
	local ok, err = maki.fs.write(path, text or "")
	return ok and path or nil, err
end

local function attempt(st, name)
	return (st.attempts and st.attempts[name]) or 0
end

local function bump(st, name)
	st.attempts = st.attempts or {}
	st.attempts[name] = attempt(st, name) + 1
	return st.attempts[name]
end

local function history(s, stage, outcome, note)
	s.state.history = s.state.history or {}
	local h = s.state.history
	h[#h + 1] = {
		at = iso(),
		stage = tostring(stage),
		outcome = tostring(outcome),
		note = plain(note, 400),
	}
	if #h > 200 then
		table.remove(h, 1)
	end
end

local function fresh_state(sid, root)
	return {
		version = 1,
		session = sid,
		root = root,
		running = false,
		stage = "select",
		attempts = { plan = 0, tests = 0, impl = 0, verify = 0, review = 0 },
		history = {},
		completed = {},
	}
end

-- ---------------------------------------------------------------------------
-- Decision model: one JSON object, no tools, cheapest tier
-- ---------------------------------------------------------------------------

local DECISION_SCHEMA = [[
Reply with exactly one JSON object and no other text:
{"decision": "accept" | "revise" | "escalate",
 "reason": "<one sentence>",
 "instructions": "<concrete guidance for the next attempt; empty on accept>"}
Use "revise" for fixable work, "escalate" only for a spec ambiguity/contradiction,
a task that must be split, or a required proof that cannot be constructed.
]]

local DECISION_SYSTEM = {
	task_size = [[You are the decision model of a deterministic task loop. Judge only whether the given backlog task is a reasonable single unit of work for one implementation subagent. Accept if it can be planned, tested and implemented in one focused change. Choose revise only to demand a smaller scope you can spell out; escalate if it must be split into separate tasks or needs a spec decision.
]] .. DECISION_SCHEMA,
	plan_review = [[You are the decision model of a deterministic task loop. Judge a plan against the spec obligations, the project rules and the task. The plan must be concrete Rust work, name property tests and matching Agda proof obligations in backend/ImplProofs.agda, avoid spec edits, and stay in scope. Reject vague or oversized plans with specific instructions. Escalate if the plan reveals a spec ambiguity.
]] .. DECISION_SCHEMA,
	tests_review = [[You are the decision model of a deterministic task loop. Judge whether the tests and Agda proofs are aligned: every quickcheck property needs a matching Agda proof that talks about the spec transition, not a tautology about the Rust code. Confirm the proof is in backend/ImplProofs.agda and that the spec was not edited. Accept, revise with specific missing/incorrect properties, or escalate a proof that cannot be constructed.
]] .. DECISION_SCHEMA,
	impl_review = [[You are the decision model of a deterministic task loop. Deterministic checks (fmt, clippy, tests, Agda) are given and may already have passed. Judge the independent review report: is the implementation correct against the spec, are tests and proofs still aligned, was performance considered, and is scope respected? Accept only when no blocking issue remains. Revise with concrete instructions, or escalate for a spec problem.
]] .. DECISION_SCHEMA,
	todo_cleanup = [[You are the decision model of a deterministic task loop. Decide whether todo.md needs a cleanup pass by a scribe subagent: stale or duplicated entries, a completed task still in the backlog, or an unclear next task. Reply with exactly one JSON object:
{"needs_cleanup": true | false, "reason": "<one sentence>"}
]],
	generic = [[You are the decision model of a deterministic task loop. Reply with one short JSON verdict.
]] .. DECISION_SCHEMA,
}

local function parse_json_object(text)
	text = tostring(text or "")
	text = text:gsub("```json", ""):gsub("```", "")
	local start = text:find("{", 1, true)
	if not start then
		return nil
	end
	local depth, finish = 0, nil
	local in_str, esc = false, false
	for i = start, #text do
		local ch = text:sub(i, i)
		if in_str then
			if esc then
				esc = false
			elseif ch == "\\" then
				esc = true
			elseif ch == '"' then
				in_str = false
			end
		elseif ch == '"' then
			in_str = true
		elseif ch == "{" then
			depth = depth + 1
		elseif ch == "}" then
			depth = depth - 1
			if depth == 0 then
				finish = i
				break
			end
		end
	end
	if not finish then
		return nil
	end
	local ok, value = pcall(maki.json.decode, text:sub(start, finish))
	if not ok or type(value) ~= "table" then
		return nil
	end
	return value
end

local function normalize_decision(value, kind)
	if type(value) ~= "table" then
		return nil
	end
	if kind == "todo_cleanup" then
		if type(value.needs_cleanup) ~= "boolean" then
			return nil
		end
		return { needs_cleanup = value.needs_cleanup, reason = plain(value.reason, 500) }
	end
	local d = tostring(value.decision or ""):lower()
	if d == "reject" or d == "rejected" or d == "deny" then
		d = "revise"
	elseif d == "split" or d == "needs_human" or d == "blocked" then
		d = "escalate"
	end
	if d ~= "accept" and d ~= "revise" and d ~= "escalate" then
		return nil
	end
	return {
		decision = d,
		reason = plain(value.reason, 500),
		instructions = plain(value.instructions, 2000),
	}
end

local function decision_model_spec(s)
	if cfg.decision_model and cfg.decision_model ~= "" then
		return cfg.decision_model
	end
	if s.decision_spec then
		return s.decision_spec
	end
	local provider = nil
	local info = maki.session.read({ session = s.session })
	local spec = info and info.model
	if spec then
		local mi = maki.model.info(spec)
		provider = mi and mi.provider or nil
	end
	local model = maki.model.tier(cfg.decision_tier, provider)
	s.decision_spec = model and model.spec or nil
	return s.decision_spec
end

-- Returns a normalized verdict, or nil when no decision model is available
-- or it produced garbage (the caller then falls back to reporter verdicts and
-- deterministic gates).
local function decide(s, kind, prompt)
	if not cfg.use_decision_model then
		return nil, "decision model disabled"
	end
	local spec = decision_model_spec(s)
	if not spec then
		return nil, "no model for tier '" .. cfg.decision_tier .. "'"
	end
	local sess, err = maki.agent.session({
		session = s.session,
		name = "fm-decide/" .. kind,
		system = DECISION_SYSTEM[kind] or DECISION_SYSTEM.generic,
		tools = {},
		model_spec = spec,
	})
	if not sess then
		return nil, err or "could not open decision session"
	end

	if s.entry then
		s.entry.session = sess
	end
	local result, perr = sess:prompt(prompt)
	local value = nil
	if result then
		value = normalize_decision(parse_json_object(result.text), kind)
		if not value then
			local retry, rerr = sess:prompt(
				"That was not a valid JSON object. Reply with exactly one JSON object matching the schema, nothing else."
			)
			if retry then
				value = normalize_decision(parse_json_object(retry.text), kind)
			end
			if not value and rerr then
				perr = rerr
			end
		end
	end
	sess:close()
	if s.entry then
		s.entry.session = nil
	end
	if not result then
		return nil, perr or "decision prompt failed"
	end
	if not value then
		return nil, "decision model returned invalid JSON"
	end
	return value, nil
end

-- ---------------------------------------------------------------------------
-- Subagents
-- ---------------------------------------------------------------------------

local COMMON_SYSTEM_PREFIX = [[
You are a subagent inside a deterministic coordinator loop for a Rust implementation of an Agda specification (diesel, rocket, sqlite).

The loop's Lua plugin owns task selection, git worktrees, builds, tests, commits and merges. Do only the stage below. Never create worktrees, never switch tasks, never commit or merge, never edit spec/. The coordinator will do those.

Working copy for this task: ]]

local COMMON_SYSTEM_SUFFIX = [[

Every bash call MUST pass this worktree as the tool's `workdir` parameter. The chat's default cwd is the coordinator checkout; building or editing there is a task failure. Use absolute paths under the worktree with read/edit/write. Run cargo/agda only inside the nix devShell (`nix develop --command ...`).

You may read only ]]

local COMMON_PROTOCOL = [[ in spec/. The spec is the source of truth.

End your final message with exactly these two protocol lines:
FM_VERDICT: ACCEPT
FM_REASON: <one line, at most 200 characters>
Use REVISE when the stage's own output is not usable yet, and ESCALATE only for a spec ambiguity/contradiction or a required proof that cannot be constructed. A human-readable call to the user does not replace these lines.
]]

local STAGE_SYSTEM = {
	research = [[
Your stage: research and plan. Make no code changes.
Read AGENTS.md, todo.md, the current backend/frontend and the allowed spec files. Produce one implementation plan for the task. It must: restate the spec obligations as concrete function/postcondition targets; list the Rust modules and functions to add or change; list the quickcheck property tests needed and the matching Agda proof obligations in backend/ImplProofs.agda; note performance constraints; state assumptions and any spec question. Write the plan to the plan path given in the prompt with the write tool, then repeat it in your final message.
]],
	tests = [[
Your stage: write tests and Agda proofs. Implementation comes later; the coordinator runs cargo only after implementation.
Write quickcheck property tests (plus a few unit tests where properties do not fit) under backend/tests/, and a matching Agda proof in backend/ImplProofs.agda for every property. Proofs must talk about the spec transitions in spec/Twitter.agda and spec/Interface.agda, not restate Rust code. No postulates. Do not modify spec/ and do not touch unrelated files. In your final message list each pair:
FM_PROPERTY: <rust test name>
FM_PROOF: <agda theorem name>
Do not implement the feature in this stage.
]],
	implement = [[
Your stage: implement the feature in Rust so the reviewed property tests and proofs hold. Follow the plan and the test/proof report. Do not weaken or delete existing tests; if a test or proof is wrong, say so with REVISE/ESCALATE instead of editing it away. Do not modify spec/. Keep performance in mind (allocation, query count, indexes). Run targeted `nix develop --command cargo test ...` while working. End with the protocol lines.
]],
	review = [[
Your stage: review. Do not modify files.
Review the complete diff from the task base commit to the working tree (including untracked files), the plan, the tests and the proofs. Check correctness against the spec, that property tests and Agda proofs prove the same statements about the spec, no postulates, performance (allocations, N+1 queries, missing indexes, pathological loops), error handling, and that nothing outside the task changed. Do not fix anything. Report prioritized issues, then the protocol lines.
]],
	scribe = [[
Your stage: maintain todo.md. Edit only todo.md. Do not change code, spec/ or other files.
Clean up the backlog the way the coordinator asked: mark finished work done, remove duplicates, reorder or reword unclear entries, and record assumptions under the assumptions heading. Commit nothing; the coordinator commits. End with the protocol lines.
]],
}

local function system_for(s, stage)
	local wt = s.state.task and s.state.task.worktree or s.root
	local spec = table.concat(copy_list(cfg.spec_files), ", ")
	return COMMON_SYSTEM_PREFIX
		.. wt
		.. "\nBranch: "
		.. tostring(s.state.task and s.state.task.branch or "?")
		.. "\nBase commit: "
		.. tostring(s.state.task and s.state.task.base_rev or "?")
		.. "\n\n--- Project AGENTS.md (verbatim; the coordinator-only parts are not yours) ---\n"
		.. (s.agents or "")
		.. "\n--- End of AGENTS.md ---\n"
		.. COMMON_SYSTEM_SUFFIX
		.. spec
		.. COMMON_PROTOCOL
		.. "\n"
		.. (STAGE_SYSTEM[stage] or "")
end

local function task_header(s)
	local t = s.state.task or {}
	return "Task: " .. tostring(t.id or "?") .. " -- " .. tostring(t.title or "?") .. "\n"
		.. "Worktree: " .. tostring(t.worktree or "?") .. "\n"
		.. "Branch: " .. tostring(t.branch or "?") .. "\n"
		.. "Base commit: " .. tostring(t.base_rev or "?") .. "\n"
end

local function feedback_block(feedback)
	if not feedback or trim(feedback) == "" then
		return ""
	end
	return "\nCoordinator feedback from the previous attempt (fix these):\n" .. clip(feedback, 6000) .. "\n"
end

-- Run one subagent to completion. Returns { text=... } or
-- { err=... } or { stop=true, err=... } when the user/session stopped it.
local function run_subagent(s, name, tools, system, prompt, model_spec)
	local defs, terr = maki.agent.tools({
		session = s.session,
		audience = "general_sub",
		only = tools,
	})
	if not defs then
		return { err = "could not resolve tools: " .. tostring(terr) }
	end

	local sess, serr = maki.agent.session({
		session = s.session,
		name = name,
		system = system,
		tools = defs,
		model_spec = model_spec,
	})
	if not sess then
		return { err = serr or "could not open subagent session" }
	end

	if s.entry then
		s.entry.session = sess
	end
	local result, perr = sess:prompt(prompt)
	sess:close()
	if s.entry then
		s.entry.session = nil
	end

	local low = tostring(perr or ""):lower()
	if low:find("stopped by user", 1, true) or low:find("cancelled", 1, true) or low:find("session closed", 1, true) then
		return { stop = true, err = perr, text = result and result.text or "" }
	end
	if not result then
		return { err = perr or "subagent prompt failed" }
	end
	return { text = result.text or "", err = perr }
end

local function reporter_verdict(text)
	local v = tostring(text or ""):match("FM_VERDICT:%s*([A-Za-z]+)")
	local r = tostring(text or ""):match("FM_REASON:%s*([^\r\n]+)")
	v = v and v:upper() or nil
	if v ~= "ACCEPT" and v ~= "REVISE" and v ~= "ESCALATE" then
		v = nil
	end
	return v, r and trim(r) or ""
end

-- ---------------------------------------------------------------------------
-- Deterministic checks (always run, never shown to the model unless it asks)
-- ---------------------------------------------------------------------------

local CHECK_ORDER = { "fmt", "clippy", "test", "agda", "agda_source" }

local function run_checks(s, base)
	local out = {}

	local function one(name, cmd, cwd, timeout)
		local r = run_cmd(cmd, { cwd = cwd, timeout = timeout or cfg.command_timeout_secs })
		local digest_text = digest((r.stdout or "") .. "\n" .. (r.stderr or ""))
		-- Keep the captured tail on disk; it never reaches a model unless asked.
		save_text(
			join(s.data, "logs", name .. "-" .. tostring(os.time()) .. ".log"),
			r.command .. "\n\n-- stdout --\n" .. tostring(r.stdout) .. "\n-- stderr --\n" .. tostring(r.stderr)
		)
		out[name] = {
			ok = r.ok,
			code = r.code,
			secs = r.secs,
			command = r.command,
			digest = digest_text,
			timed_out = r.timed_out or false,
			start_error = r.start_error,
		}
		return out[name]
	end

	one("fmt", nix_cmd(cfg.fmt_check_cmd), base)
	one("clippy", nix_cmd(cfg.clippy_cmd), base)
	one("test", nix_cmd(cfg.test_cmd), base)

	local proof = join(base, cfg.proofs_path)
	local src = read_file(proof)
	if src then
		local clean = strip_agda_comments(src)
		local found = clean:match("%f[%a]postulate%f[%A]")
		out.agda_source = {
			ok = not found,
			code = found and 1 or 0,
			secs = 0,
			command = "postulate scan: " .. cfg.proofs_path,
			digest = found and ("postulate found in " .. cfg.proofs_path) or "no postulate",
		}
	else
		out.agda_source = {
			ok = false,
			code = 1,
			secs = 0,
			command = "postulate scan: " .. cfg.proofs_path,
			digest = "missing file: " .. cfg.proofs_path,
		}
	end

	one("agda", nix_cmd(cfg.agda_cmd), join(base, cfg.agda_cwd), cfg.agda_timeout_secs)
	return out
end

local function checks_ok(checks)
	for _, name in ipairs(CHECK_ORDER) do
		local c = checks and checks[name]
		if c and not c.ok then
			return false
		end
	end
	return true
end

local function checks_brief(checks)
	if not checks then
		return "checks: none yet"
	end
	local parts = {}
	for _, name in ipairs(CHECK_ORDER) do
		local c = checks[name]
		if c then
			parts[#parts + 1] = name .. (c.ok and ":ok" or ":FAIL")
		end
	end
	return "checks: " .. table.concat(parts, " ")
end

local function checks_detail(checks)
	local parts = {}
	for _, name in ipairs(CHECK_ORDER) do
		local c = checks and checks[name]
		if c then
			parts[#parts + 1] = "- " .. name .. " " .. (c.ok and "ok" or "FAIL (exit " .. tostring(c.code) .. ")")
				.. " " .. tostring(c.secs or 0) .. "s"
		end
	end
	return table.concat(parts, "\n")
end

local function check_failures(checks)
	local parts = {}
	for _, name in ipairs(CHECK_ORDER) do
		local c = checks and checks[name]
		if c and not c.ok then
			parts[#parts + 1] = "## " .. name .. " failed (exit " .. tostring(c.code) .. ")\n"
				.. tostring(c.digest or "")
		end
	end
	return clip(table.concat(parts, "\n\n"), 7000)
end

-- ---------------------------------------------------------------------------
-- Worktrees, diffs, commits
-- ---------------------------------------------------------------------------

local function changed_paths(s)
	local t = s.state.task
	local seen, list = {}, {}
	local function add(p)
		p = trim(p)
		if p ~= "" and not seen[p] then
			seen[p] = true
			list[#list + 1] = p
		end
	end

	if t.base_rev then
		local r = git(t.worktree, { "diff", "--name-only", t.base_rev })
		for _, p in ipairs(lines_of(r.stdout)) do
			add(p)
		end
	end
	local st = git(t.worktree, { "status", "--porcelain" })
	for _, line in ipairs(lines_of(st.stdout)) do
		if #line > 3 then
			local p = line:sub(4)
			local arrow = p:find(" -> ", 1, true)
			if arrow then
				p = p:sub(arrow + 4)
			end
			p = p:gsub('^"', ""):gsub('"$', "")
			add(p)
		end
	end
	return list
end

local function path_matches_any(path, prefixes)
	for _, p in ipairs(prefixes) do
		if path == p or path:sub(1, #p) == p then
			return true
		end
	end
	return false
end

-- AGENTS.md forbids editing the spec; enforce it outside the prompt too.
local function spec_edits(s)
	local bad = {}
	for _, p in ipairs(changed_paths(s)) do
		if p:sub(1, 5) == "spec/" then
			bad[#bad + 1] = p
		end
	end
	return bad
end

local function ensure_worktree(s)
	local t = s.state.task
	local name = slug(t.id):lower()
	t.branch = t.branch or (cfg.branch_prefix .. name)
	t.worktree = t.worktree or join(s.repo, cfg.worktree_prefix .. name)

	local meta = maki.fs.metadata(t.worktree)
	if not meta then
		local exists = git(s.repo, { "rev-parse", "--verify", "--quiet", "refs/heads/" .. t.branch })
		local args
		if exists.code == 0 then
			args = { "worktree", "add", t.worktree, t.branch }
		else
			args = { "worktree", "add", "-b", t.branch, t.worktree, s.base_rev }
		end
		local r = git(s.repo, args)
		if not r.ok then
			return nil, "git worktree add failed: " .. digest((r.stdout or "") .. "\n" .. (r.stderr or ""))
		end
	end

	if not t.base_rev then
		local head = git(s.root, { "rev-parse", "HEAD" })
		local rev = head.ok and trim(head.stdout) or ""
		t.base_rev = rev ~= "" and rev or s.base_rev
	end
	return true
end

local function commit_tree(_s, cwd, message)
	local add = git(cwd, { "add", "-A" })
	if not add.ok then
		return nil, "git add failed: " .. digest(add.stderr .. "\n" .. add.stdout)
	end
	local staged = git(cwd, { "status", "--porcelain", "--cached" })
	if trim(staged.stdout) == "" then
		return nil, "nothing to commit"
	end
	local c = git(cwd, { "commit", "-m", message })
	if not c.ok then
		return nil, "git commit failed: " .. digest(c.stdout .. "\n" .. c.stderr)
	end
	local rev = git(cwd, { "rev-parse", "HEAD" })
	return trim(rev.stdout)
end

local function commit_message(id, title, body)
	return tostring(id) .. ": " .. tostring(title) .. "\n\n" .. tostring(body or "")
		.. "\n\nCo-authored-by: " .. cfg.coauthor
end

local function pick_task(todo, completed)
	local in_backlog = false
	local fallback = nil
	for _, line in ipairs(lines_of(todo)) do
		local level = #(line:match("^#+") or "")
		if level > 0 and level <= 2 then
			in_backlog = line:lower():find("backlog", 1, true) ~= nil
		end
		local id, title = line:match("^%s*%- %[ %]%s*([%w%._%-]+)%s*:?%s*(.-)%s*$")
		if id and not (completed and completed[id]) then
			local entry = { id = id, title = title ~= "" and title or id }
			if in_backlog then
				return entry
			end
			fallback = fallback or entry
		end
	end
	return fallback
end

-- ---------------------------------------------------------------------------
-- Blocking and finishing
-- ---------------------------------------------------------------------------

local function escalate(s, stage, reason)
	local st = s.state
	st.running = false
	st.blocked_stage = stage
	st.blocked_reason = plain(reason, 4000)
	st.stage = "blocked"
	history(s, stage, "escalate", reason)
	save_state(s)
	maki.log.warn("[formal-methods] blocked at " .. tostring(stage) .. ": " .. plain(reason, 1000))

	local msg = "[formal-methods] blocked: task " .. tostring(st.task and st.task.id or "?")
		.. " at " .. tostring(stage) .. ".\n"
		.. "Reason: " .. clip(reason, 1800) .. "\n"
		.. "Worktree: " .. tostring(st.task and st.task.worktree or "-") .. "\n"
		.. "State: " .. state_path(s.data) .. "\n"
		.. "Resolve this, then call fm_loop {action=\"resume\", note=\"<what to change>\"} (or /fm resume <note>)."
	local ok, err = maki.session.notify(msg, {
		session = s.session,
		wake = true,
		display = "formal-methods: blocked at " .. tostring(stage),
	})
	if not ok then
		maki.log.warn("[formal-methods] notify failed: " .. tostring(err))
		maki.ui.flash("[fm] blocked at " .. tostring(stage) .. " (notify failed: " .. tostring(err) .. ")")
	else
		maki.ui.flash("[fm] blocked at " .. tostring(stage) .. ", chat woken")
	end
end

local function finish_task(s)
	local st = s.state
	local t = st.task or {}
	st.completed = st.completed or {}
	if t.id then
		st.completed[t.id] = true
	end
	history(s, "done", "task-complete", tostring(t.id))
	local summary = "[fm] " .. tostring(t.id) .. " merged ("
		.. tostring(t.merged_sha or "?") .. "), checks green"
	maki.ui.flash(summary)
	maki.log.info(summary)

	st.task = nil
	st.feedback = nil
	st.attempts = { plan = 0, tests = 0, impl = 0, verify = 0, review = 0 }
	st.last_checks = nil

	if st.stop_after_task then
		st.running = false
		st.stage = "idle"
		st.stop_after_task = false
		save_state(s)
		return "halt"
	end
	st.stage = "select"
	save_state(s)
	return "continue"
end

local function finish_empty(s)
	s.state.running = false
	s.state.stage = "idle"
	history(s, "select", "backlog-empty", "")
	save_state(s)
	local msg = "[formal-methods] backlog empty: todo.md has no unchecked task to pick up. "
		.. "Create new tasks (benchmarks, spec review), then call fm_loop {action=\"start\"}."
	maki.session.notify(msg, {
		session = s.session,
		wake = true,
		display = "formal-methods: backlog empty",
	})
	maki.ui.flash("[fm] backlog empty")
	return "halt"
end

local function pattern_escape(s)
	return (tostring(s):gsub("[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0"))
end

-- Mark the task's own unchecked bullet done, line by line, so a fresh
-- session (a new data dir) will not re-run shipped work.
local function mark_task_done(todo, id)
	local lines = {}
	for line in (todo .. "\n"):gmatch("(.-)\n") do
		lines[#lines + 1] = line
	end
	local want = "^%s*%- %[ %]%s*" .. pattern_escape(id) .. "%s*:?"
	local marked = false
	for i, line in ipairs(lines) do
		if not marked and line:match(want) then
			lines[i] = line:gsub("%[ %]", "[x]", 1)
			marked = true
		end
	end
	return table.concat(lines, "\n"), marked
end

local function append_loop_log(s, summary)
	local path = join(s.root, cfg.todo_file)
	local todo = read_file(path)
	if not todo then
		return
	end
	local id = tostring(s.state.task and s.state.task.id or "")
	if id ~= "" then
		todo = mark_task_done(todo, id)
	end
	if not todo:find("## Loop log", 1, true) then
		todo = todo:gsub("%s*$", "") .. "\n\n## Loop log\n"
	end
	todo = todo:gsub("%s*$", "") .. "\n- " .. iso() .. " " .. summary .. "\n"
	save_text(path, todo)
	local add = git(s.root, { "add", cfg.todo_file })
	if not add.ok then
		return
	end
	local staged = git(s.root, { "status", "--porcelain", "--cached" })
	if trim(staged.stdout) ~= "" then
		git(s.root, {
			"commit",
			"-m",
			"docs(todo): record " .. tostring(s.state.task and s.state.task.id or "loop") .. "\n\nCo-authored-by: " .. cfg.coauthor,
		})
	end
end

-- ---------------------------------------------------------------------------
-- Stages
-- ---------------------------------------------------------------------------

local function task_size_prompt(s, todo)
	return "Decide whether this backlog task is a reasonable single unit of work.\n\n"
		.. "Task: " .. tostring(s.state.task.id) .. " -- " .. tostring(s.state.task.title) .. "\n\n"
		.. "todo.md:\n" .. clip(todo, 6000) .. "\n\n"
		.. "Accept only if it can be planned, tested and implemented in one focused change."
end

local function stage_select(s)
	local st = s.state
	local todo = read_file(join(s.root, cfg.todo_file)) or ""

	if st.seed_task and st.seed_task ~= "" then
		st.task = { id = st.seed_task, title = st.seed_task, source = "argument" }
		st.seed_task = nil
	elseif not st.task then
		local picked = pick_task(todo, st.completed)
		if not picked then
			return finish_empty(s)
		end
		st.task = { id = picked.id, title = picked.title, source = "todo" }
	end

	-- Judge size once per task, not again on every resume.
	if not st.task.base_rev then
		local verdict, derr = decide(s, "task_size", task_size_prompt(s, todo))
		if verdict then
			if verdict.decision == "escalate" then
				escalate(
					s,
					"task_size",
					"Task " .. tostring(st.task.id) .. " needs splitting or a spec decision: "
						.. (verdict.reason or "")
						.. "\n"
						.. (verdict.instructions or "")
				)
				return "halt"
			elseif verdict.decision == "revise" then
				escalate(
					s,
					"task_size",
					"Task " .. tostring(st.task.id) .. " is too large as written: "
						.. (verdict.reason or "")
						.. "\n"
						.. (verdict.instructions or "")
						.. "\nSplit it in todo.md, then /fm resume."
				)
				return "halt"
			end
		end
		if not verdict and derr then
			maki.log.info("[formal-methods] decision model unavailable at task_size: " .. tostring(derr))
		end
	end

	local ok, err = ensure_worktree(s)
	if not ok then
		escalate(s, "select", err)
		return "halt"
	end
	history(s, "select", "picked", st.task.id .. ": " .. tostring(st.task.title))
	st.stage = "research"
	save_state(s)
	return "continue"
end

local function stage_research(s)
	local st = s.state
	local feedback = st.feedback
	st.feedback = nil
	local target = plan_path(s)
	local prompt = task_header(s)
		.. "\nPlan path (write the plan there with the write tool): " .. target .. "\n"
		.. feedback_block(feedback)
		.. "\nProduce the plan for this task. Stay in scope. If the spec makes the task impossible or ambiguous, ESCALATE instead of guessing."

	local out = run_subagent(s, "fm/research/" .. tostring(st.task.id), {
		"read", "grep", "glob", "list", "bash",
	}, system_for(s, "research"), prompt)
	if out.stop then
		return "stop"
	end
	if out.err and not out.text then
		escalate(s, "research", "research subagent failed: " .. tostring(out.err))
		return "halt"
	end

	local text = out.text or ""
	local verdict, reason = reporter_verdict(text)
	if verdict == "ESCALATE" then
		escalate(s, "research", "research subagent asks to escalate: " .. (reason ~= "" and reason or "see plan"))
		return "halt"
	end
	if verdict == "REVISE" then
		local n = bump(st, "plan")
		if n > cfg.max_plan_revisions then
			escalate(s, "research", "research/plan still not usable: " .. (reason ~= "" and reason or "see report"))
			return "halt"
		end
		st.feedback = "Research/plan needs revision: " .. (reason ~= "" and reason or "see report")
		st.stage = "research"
		save_state(s)
		return "continue"
	end
	if text == "" then
		local disk = read_file(target)
		text = disk or ""
	end
	if #text < 200 then
		escalate(s, "research", "research subagent produced no usable plan: " .. tostring(out.err or "empty reply"))
		return "halt"
	end
	save_text(target, text)
	st.plan_chars = #text
	history(s, "research", "plan-written", target)
	st.stage = "plan_review"
	save_state(s)
	return "continue"
end

local function stage_plan_review(s)
	local st = s.state
	local text = read_file(plan_path(s)) or ""
	local self_verdict, reason = reporter_verdict(text)

	if #text < 200 then
		local n = bump(st, "plan")
		if n > cfg.max_plan_revisions then
			escalate(s, "plan_review", "plan is still empty after " .. tostring(n) .. " attempts")
			return "halt"
		end
		st.feedback = "The plan was empty or too short. Produce a concrete plan."
		st.stage = "research"
		save_state(s)
		return "continue"
	end

	local prompt = task_header(s)
		.. "\nPlan length: " .. tostring(#text) .. " characters.\n\nPlan:\n"
		.. clip(text, 16000) .. "\n\n"
		.. "Accept if this is a concrete, task-sized plan with named Rust work, property tests and matching Agda proof obligations. "
		.. "Revise otherwise with specific instructions. Escalate on a spec ambiguity."

	local verdict, derr = decide(s, "plan_review", prompt)
	local decision, why, instructions
	if verdict then
		decision, why, instructions = verdict.decision, verdict.reason, verdict.instructions
	else
		decision = (self_verdict == "ESCALATE" and "escalate")
			or (self_verdict == "REVISE" and "revise")
			or "accept"
		why = reason
		maki.log.info("[formal-methods] plan_review fell back to reporter verdict: " .. tostring(derr))
	end

	if decision == "escalate" then
		escalate(s, "plan_review", why ~= "" and why or "plan rejected as needing a spec decision")
		return "halt"
	elseif decision == "revise" then
		local n = bump(st, "plan")
		if n > cfg.max_plan_revisions then
			escalate(s, "plan_review", "plan rejected " .. tostring(n) .. " times: " .. tostring(why or "") .. "\n" .. tostring(instructions or ""))
			return "halt"
		end
		st.feedback = "Plan rejected: " .. tostring(why or "") .. "\n" .. tostring(instructions or "")
		history(s, "plan_review", "revise", why)
		st.stage = "research"
		save_state(s)
		return "continue"
	end

	history(s, "plan_review", "accept", why)
	st.stage = "tests"
	save_state(s)
	return "continue"
end

local function stage_tests(s)
	local st = s.state
	local feedback = st.feedback
	st.feedback = nil
	local plan = clip(read_file(plan_path(s)) or "", 9000)
	local prompt = task_header(s)
		.. "\nAccepted plan:\n" .. plan .. "\n"
		.. feedback_block(feedback)
		.. "\nWrite the property tests and matching Agda proofs now. Do not implement the feature."

	local out = run_subagent(s, "fm/tests/" .. tostring(st.task.id), {
		"read", "grep", "glob", "list", "write", "edit", "multiedit", "bash",
	}, system_for(s, "tests"), prompt)
	if out.stop then
		return "stop"
	end
	if out.err and not out.text then
		escalate(s, "tests", "tests subagent failed: " .. tostring(out.err))
		return "halt"
	end

	local text = out.text or ""
	local verdict, reason = reporter_verdict(text)
	if verdict == "ESCALATE" then
		escalate(s, "tests", "tests subagent asks to escalate: " .. (reason ~= "" and reason or "see report"))
		return "halt"
	end
	local rpath = report_path(s, "tests")
	save_text(rpath, text)
	st.tests_report = rpath

	if verdict == "REVISE" then
		local n = bump(st, "tests")
		if n > cfg.max_tests_revisions then
			escalate(s, "tests", "tests stage still needs revision: " .. (reason ~= "" and reason or "see report"))
			return "halt"
		end
		st.feedback = "Tests/proofs stage needs revision: " .. (reason ~= "" and reason or "see report")
		st.stage = "tests"
		save_state(s)
		return "continue"
	end

	-- Deterministic gates: the proof file and a test file must have changed,
	-- and the proof file may not introduce postulates.
	local paths = changed_paths(s)
	local proofs_changed = false
	local tests_changed = false
	for _, p in ipairs(paths) do
		if p == cfg.proofs_path then
			proofs_changed = true
		end
		if path_matches_any(p, { cfg.tests_dir }) then
			tests_changed = true
		end
	end
	local proof_src = read_file(join(st.task.worktree, cfg.proofs_path))
	local postulate = proof_src and strip_agda_comments(proof_src):match("%f[%a]postulate%f[%A]")
	local bad_spec = spec_edits(s)

	local gate_err
	if #bad_spec > 0 then
		gate_err = "spec/ must never be edited (changed: " .. table.concat(bad_spec, ", ")
			.. "). Revert those files and prove the obligation instead."
	elseif not tests_changed then
		gate_err = "No test files changed under " .. cfg.tests_dir .. ". Add quickcheck property tests there."
	elseif not proofs_changed then
		gate_err = "backend/ImplProofs.agda was not changed. Every property needs a matching Agda proof."
	elseif postulate then
		gate_err = "backend/ImplProofs.agda contains a postulate. Prove the obligation or escalate the spec problem."
	end

	-- The proofs should typecheck on their own, before any Rust work starts.
	if not gate_err then
		local agda = run_cmd(nix_cmd(cfg.agda_cmd), {
			cwd = join(st.task.worktree, cfg.agda_cwd),
			timeout = cfg.agda_timeout_secs,
		})
		if not agda.ok then
			gate_err = "Agda does not typecheck yet:\n"
				.. digest((agda.stdout or "") .. "\n" .. (agda.stderr or ""))
		end
	end

	if gate_err then
		local n = bump(st, "tests")
		if n > cfg.max_tests_revisions then
			escalate(s, "tests", gate_err .. " (after " .. tostring(n) .. " attempts)")
			return "halt"
		end
		st.feedback = gate_err
		history(s, "tests", "gate-fail", gate_err)
		st.stage = "tests"
		save_state(s)
		return "continue"
	end

	-- Commit the reviewed pair so the implementation diff is separable.
	local sha, cerr = commit_tree(
		s,
		st.task.worktree,
		commit_message(st.task.id, "tests and proofs", tostring(st.task.title))
	)
	if not sha and tostring(cerr):find("nothing to commit", 1, true) then
		local rev = git(st.task.worktree, { "rev-parse", "HEAD" })
		local head = trim(rev.stdout)
		if head ~= "" and head ~= st.task.base_rev then
			sha, cerr = head, nil
		end
	end
	if not sha then
		local n = bump(st, "tests")
		if n > cfg.max_tests_revisions then
			escalate(s, "tests", "could not commit tests: " .. tostring(cerr))
			return "halt"
		end
		st.feedback = "The tests stage produced nothing commit-able: " .. tostring(cerr)
		st.stage = "tests"
		save_state(s)
		return "continue"
	end
	st.task.tests_sha = sha
	history(s, "tests", "committed", sha)
	st.stage = "tests_review"
	save_state(s)
	return "continue"
end

local function stage_tests_review(s)
	local st = s.state
	local report = read_file(st.tests_report) or ""
	local props, proofs, no_props = {}, {}, nil
	for _, line in ipairs(lines_of(report)) do
		local p = line:match("FM_PROPERTY:%s*(%S+)")
		if p then
			props[#props + 1] = p
		end
		local pf = line:match("FM_PROOF:%s*(%S+)")
		if pf then
			proofs[#proofs + 1] = pf
		end
		local np = line:match("FM_NO_PROPERTIES:%s*(.+)")
		if np then
			no_props = trim(np)
		end
	end

	if #props == 0 and #proofs == 0 and not no_props then
		local n = bump(st, "tests")
		if n > cfg.max_tests_revisions then
			escalate(s, "tests_review", "tests stage listed no FM_PROPERTY/FM_PROOF pairs and no FM_NO_PROPERTIES reason")
			return "halt"
		end
		st.feedback = "List every property/proof pair as FM_PROPERTY/FM_PROOF. If this task genuinely has no "
			.. "property, add a line `FM_NO_PROPERTIES: <reason>`."
		st.stage = "tests"
		save_state(s)
		return "continue"
	end

	local prompt = task_header(s)
		.. "\nProperties reported by the tests stage:\n- " .. (table.concat(props, "\n- "))
		.. "\n\nProofs reported:\n- " .. (table.concat(proofs, "\n- "))
		.. "\n\nNo-properties reason: " .. tostring(no_props or "none")
		.. "\n\nAccepted plan:\n" .. clip(read_file(plan_path(s)) or "", 7000)
		.. "\n\nTests/proofs report:\n" .. clip(report, 6000)
		.. "\n\nAccept only if every property has an Agda proof that talks about the spec transition. Revise otherwise."

	local verdict, derr = decide(s, "tests_review", prompt)
	local decision, why, instructions
	if verdict then
		decision, why, instructions = verdict.decision, verdict.reason, verdict.instructions
	else
		decision = "accept"
		why = "no decision model; structural gates passed"
		maki.log.info("[formal-methods] tests_review fell back: " .. tostring(derr))
	end

	if decision == "escalate" then
		escalate(s, "tests_review", (why ~= "" and why or "tests/proofs need a spec decision")
			.. "\n" .. tostring(instructions or ""))
		return "halt"
	elseif decision == "revise" then
		local n = bump(st, "tests")
		if n > cfg.max_tests_revisions then
			escalate(s, "tests_review", "tests/proofs rejected " .. tostring(n) .. " times: " .. tostring(why or "")
				.. "\n" .. tostring(instructions or ""))
			return "halt"
		end
		st.feedback = "Tests/proofs rejected: " .. tostring(why or "") .. "\n" .. tostring(instructions or "")
		history(s, "tests_review", "revise", why)
		st.stage = "tests"
		save_state(s)
		return "continue"
	end

	history(s, "tests_review", "accept", why)
	st.stage = "implement"
	save_state(s)
	return "continue"
end

local function stage_implement(s)
	local st = s.state
	local feedback = st.feedback
	st.feedback = nil
	local plan = clip(read_file(plan_path(s)) or "", 7000)
	local tests_report = clip(read_file(st.tests_report) or "", 4000)
	local prompt = task_header(s)
		.. "\nPlan:\n" .. plan
		.. "\n\nTest/proof report:\n" .. tests_report
		.. feedback_block(feedback)
		.. "\n\nImplement the feature. Keep the reviewed tests and proofs intact."

	local out = run_subagent(s, "fm/implement/" .. tostring(st.task.id), {
		"read", "grep", "glob", "list", "write", "edit", "multiedit", "edit_lines", "insert_lines", "bash",
	}, system_for(s, "implement"), prompt)
	if out.stop then
		return "stop"
	end
	if out.err and not out.text then
		escalate(s, "implement", "implementation subagent failed: " .. tostring(out.err))
		return "halt"
	end

	local text = out.text or ""
	local verdict, reason = reporter_verdict(text)
	if verdict == "ESCALATE" then
		escalate(s, "implement", "implementation subagent asks to escalate: " .. (reason ~= "" and reason or "see report"))
		return "halt"
	end
	save_text(report_path(s, "implement"), text)

	if verdict == "REVISE" then
		local n = bump(st, "impl")
		if n > cfg.max_impl_revisions then
			escalate(s, "implement", "implementation stage still needs revision: " .. (reason ~= "" and reason or "see report"))
			return "halt"
		end
		st.feedback = "Implementation needs revision: " .. (reason ~= "" and reason or "see report")
		st.stage = "implement"
		save_state(s)
		return "continue"
	end

	local paths = changed_paths(s)
	local src_changed = false
	for _, p in ipairs(paths) do
		if path_matches_any(p, { "backend/src/", "frontend/" }) then
			src_changed = true
			break
		end
	end
	if not src_changed then
		local n = bump(st, "impl")
		if n > cfg.max_impl_revisions then
			escalate(s, "implement", "implementation produced no changes under backend/src or frontend/ after "
				.. tostring(n) .. " attempts")
			return "halt"
		end
		st.feedback = "No implementation changes were found under backend/src or frontend/. Implement the plan."
		st.stage = "implement"
		save_state(s)
		return "continue"
	end

	history(s, "implement", "implemented", tostring(#paths) .. " changed files")
	st.stage = "verify"
	save_state(s)
	return "continue"
end

local function stage_verify(s)
	local st = s.state
	local bad_spec = spec_edits(s)
	if #bad_spec > 0 then
		local msg = "spec/ was modified (" .. table.concat(bad_spec, ", ")
			.. "). Revert those files; the spec is read-only."
		local n = bump(st, "verify")
		if n > cfg.max_verify_rounds then
			escalate(s, "verify", msg)
			return "halt"
		end
		st.feedback = msg
		st.stage = "implement"
		save_state(s)
		return "continue"
	end
	local checks = run_checks(s, st.task.worktree)
	st.last_checks = checks
	maki.log.info("[formal-methods] verify " .. tostring(st.task.id) .. ": " .. checks_brief(checks))

	if checks_ok(checks) then
		history(s, "verify", "pass", checks_brief(checks))
		st.feedback = nil
		st.stage = "review"
		save_state(s)
		return "continue"
	end

	local detail = check_failures(checks)
	local n = bump(st, "verify")
	if n > cfg.max_verify_rounds then
		escalate(s, "verify", "deterministic checks still fail after " .. tostring(n) .. " rounds:\n" .. detail)
		return "halt"
	end
	st.feedback = "The coordinator ran fmt/clippy/test/agda and they fail. Fix exactly these:\n" .. detail
	history(s, "verify", "fail", checks_brief(checks))
	st.stage = "implement"
	save_state(s)
	return "continue"
end

local function stage_review(s)
	local st = s.state
	local diffstat = git(st.task.worktree, { "diff", "--stat", tostring(st.task.base_rev) })
	local untracked = git(st.task.worktree, { "status", "--porcelain" })
	local prompt = task_header(s)
		.. "\nPlan:\n" .. clip(read_file(plan_path(s)) or "", 6000)
		.. "\n\nDiffstat vs base:\n" .. clip(diffstat.stdout, 3000)
		.. "\n\nWorking tree status:\n" .. clip(untracked.stdout, 2000)
		.. "\n\nDeterministic checks (run by the coordinator in the worktree):\n" .. checks_detail(st.last_checks)
		.. "\n\nReview the full diff (" .. tostring(st.task.base_rev) .. "..working tree) and the tests/proofs. "
		.. "Do not modify anything."

	local out = run_subagent(s, "fm/review/" .. tostring(st.task.id), {
		"read", "grep", "glob", "list", "bash",
	}, system_for(s, "review"), prompt, cfg.review_model)
	if out.stop then
		return "stop"
	end
	if out.err and not out.text then
		escalate(s, "review", "review subagent failed: " .. tostring(out.err))
		return "halt"
	end

	local text = out.text or ""
	local self_verdict, self_reason = reporter_verdict(text)
	local rpath = report_path(s, "review")
	save_text(rpath, text)
	st.review_report = rpath

	local prompt2 = task_header(s)
		.. "\nDeterministic checks: " .. checks_brief(st.last_checks) .. "\n"
		.. checks_detail(st.last_checks) .. "\n\n"
		.. "Independent review report:\n" .. clip(text, 7000) .. "\n\n"
		.. "Accept only when no blocking issue remains and performance was considered."

	local verdict, derr = decide(s, "impl_review", prompt2)
	local decision, why, instructions
	if verdict then
		decision, why, instructions = verdict.decision, verdict.reason, verdict.instructions
	else
		decision = (self_verdict == "ESCALATE" and "escalate")
			or (self_verdict == "REVISE" and "revise")
			or "accept"
		why = self_reason
		maki.log.info("[formal-methods] impl_review fell back to reviewer verdict: " .. tostring(derr))
	end

	if decision == "escalate" then
		escalate(s, "review", (why ~= "" and why or "review asks for a spec decision")
			.. "\n" .. tostring(instructions or ""))
		return "halt"
	elseif decision == "revise" then
		local n = bump(st, "review")
		if n > cfg.max_review_revisions then
			escalate(s, "review", "review rejected " .. tostring(n) .. " times: " .. tostring(why or "")
				.. "\n" .. tostring(instructions or ""))
			return "halt"
		end
		st.feedback = "Review rejected the change: " .. tostring(why or "") .. "\n" .. tostring(instructions or "")
		history(s, "review", "revise", why)
		st.stage = "implement"
		save_state(s)
		return "continue"
	end

	history(s, "review", "accept", why)
	st.stage = "finalize"
	save_state(s)
	return "continue"
end

local function stage_finalize(s)
	local st = s.state
	local wt = st.task.worktree

	local bad_spec = spec_edits(s)
	if #bad_spec > 0 then
		escalate(s, "finalize", "spec/ was modified (" .. table.concat(bad_spec, ", ")
			.. "); refusing to commit or merge. Revert it deliberately and /fm resume.")
		return "halt"
	end

	local formatted = run_cmd(nix_cmd(cfg.fmt_write_cmd), { cwd = wt, timeout = cfg.command_timeout_secs })
	if not formatted.ok then
		local n = bump(st, "verify")
		if n > cfg.max_verify_rounds then
			escalate(s, "finalize", "cargo fmt failed: " .. digest(formatted.stderr .. "\n" .. formatted.stdout))
			return "halt"
		end
		st.feedback = "cargo fmt failed:\n" .. digest(formatted.stderr .. "\n" .. formatted.stdout)
		st.stage = "implement"
		save_state(s)
		return "continue"
	end

	local checks = run_checks(s, wt)
	st.last_checks = checks
	if not checks_ok(checks) then
		local n = bump(st, "verify")
		if n > cfg.max_verify_rounds then
			escalate(s, "finalize", "checks fail after rustfmt:\n" .. check_failures(checks))
			return "halt"
		end
		st.feedback = "Checks fail after rustfmt:\n" .. check_failures(checks)
		st.stage = "implement"
		save_state(s)
		return "continue"
	end

	local sha, cerr = commit_tree(
		s,
		wt,
		commit_message(st.task.id, "implementation", tostring(st.task.title))
	)
	if not sha and tostring(cerr):find("nothing to commit", 1, true) then
		-- Resuming after a crash: the implementation commit already exists.
		local rev = git(wt, { "rev-parse", "HEAD" })
		local head = trim(rev.stdout)
		if head ~= "" and head ~= tostring(st.task.base_rev) and head ~= tostring(st.task.tests_sha) then
			sha, cerr = head, nil
		end
	end
	if not sha then
		escalate(s, "finalize", "could not commit implementation: " .. tostring(cerr))
		return "halt"
	end
	st.task.impl_sha = sha
	history(s, "finalize", "committed", sha)

	-- Never merge over uncommitted work in the coordinator checkout. todo.md
	-- edits are the coordinator's own bookkeeping and are tolerated.
	local dirty = git(s.root, { "status", "--porcelain" })
	if not dirty.ok then
		escalate(s, "merge", "git status failed in the coordinator checkout: " .. digest(dirty.stderr))
		return "halt"
	end
	local blockers = {}
	local wprefix = cfg.worktree_prefix
	for _, line in ipairs(lines_of(dirty.stdout)) do
		local path = line:sub(4)
		-- Worktrees live inside the repository root; from the main checkout
		-- they are untracked siblings, not user work.
		local is_worktree = line:sub(1, 2) == "??"
			and (path:sub(1, #wprefix) == wprefix or path:find("/" .. wprefix, 1, true) ~= nil)
		local tolerated = line:find(cfg.todo_file, 1, true) ~= nil
		for _, prefix in ipairs(cfg.dirty_allow_prefixes or {}) do
			if path:sub(1, #prefix) == prefix then
				tolerated = true
			end
		end
		if not tolerated and not is_worktree then
			blockers[#blockers + 1] = line
		end
	end
	if #blockers > 0 then
		escalate(s, "merge", "the coordinator checkout is dirty; refusing to merge:\n" .. clip(table.concat(blockers, "\n"), 3000))
		return "halt"
	end

	local merge_msg = "Merge " .. tostring(st.task.branch) .. ": " .. tostring(st.task.title)
		.. "\n\nCo-authored-by: " .. cfg.coauthor
	local merge = git(s.root, { "merge", "--no-ff", "-m", merge_msg, tostring(st.task.branch) }, 600)
	if not merge.ok then
		git(s.root, { "merge", "--abort" })
		escalate(s, "merge", "merge of " .. tostring(st.task.branch) .. " failed:\n"
			.. digest(merge.stdout .. "\n" .. merge.stderr))
		return "halt"
	end
	local rev = git(s.root, { "rev-parse", "HEAD" })
	st.task.merged_sha = trim(rev.stdout)
	history(s, "merge", "merged", st.task.merged_sha)

	local merged_checks = run_checks(s, s.root)
	st.merged_checks = merged_checks
	if not checks_ok(merged_checks) then
		escalate(s, "merge", "merged " .. tostring(st.task.merged_sha)
			.. ", but the merged branch fails checks:\n" .. check_failures(merged_checks)
			.. "\nThe merge was kept so you can inspect it; fix and run /fm resume or revert deliberately.")
		return "halt"
	end

	append_loop_log(s, tostring(st.task.id) .. ": merged " .. tostring(st.task.merged_sha)
		.. " (branch " .. tostring(st.task.branch) .. ")")
	st.stage = "todo"
	save_state(s)
	return "continue"
end

local function stage_todo(s)
	local st = s.state
	local todo = read_file(join(s.root, cfg.todo_file)) or ""
	local prompt = "Task shipped: " .. tostring(st.task.id) .. " -- " .. tostring(st.task.title)
		.. "\nMerge commit: " .. tostring(st.task.merged_sha)
		.. "\n\nCurrent todo.md:\n" .. clip(todo, 12000)
		.. "\n\nDecide whether todo.md needs a cleanup pass now."

	local verdict = decide(s, "todo_cleanup", prompt)
	if verdict and verdict.needs_cleanup then
		local out = run_subagent(s, "fm/scribe", {
			"read", "grep", "glob", "list", "edit", "multiedit", "write", "bash",
		}, system_for(s, "scribe"), "Clean up the coordinator's todo.md now. Edit ONLY "
			.. join(s.root, cfg.todo_file)
			.. " (absolute path, not the worktree copy, not code, not spec/). Reason: "
			.. tostring(verdict.reason or "") .. "\n\n" .. clip(todo, 12000))
		if not out.stop and not out.err then
			local st2 = git(s.root, { "status", "--porcelain" })
			local only_todo = trim(st2.stdout) ~= ""
			for _, line in ipairs(lines_of(st2.stdout)) do
				if not line:find(cfg.todo_file, 1, true) then
					only_todo = false
				end
			end
			if only_todo then
				git(s.root, { "add", cfg.todo_file })
				git(s.root, { "commit", "-m", "docs(todo): cleanup\n\nCo-authored-by: " .. cfg.coauthor })
				history(s, "todo", "cleanup", verdict.reason)
			else
				maki.log.warn("[formal-methods] scribe touched files other than todo.md; cleanup left uncommitted")
			end
		end
	elseif verdict then
		history(s, "todo", "clean", verdict.reason)
	end
	return finish_task(s)
end

local STAGES = {
	select = stage_select,
	research = stage_research,
	plan_review = stage_plan_review,
	tests = stage_tests,
	tests_review = stage_tests_review,
	implement = stage_implement,
	verify = stage_verify,
	review = stage_review,
	finalize = stage_finalize,
	todo = stage_todo,
}

-- ---------------------------------------------------------------------------
-- The loop
-- ---------------------------------------------------------------------------

local loops = {} -- data dir -> { task = maki.async.Task, session = Session?, cancelled = bool }

local function prepare(s)
	mkdirs(s)
	if not s.repo then
		local common = git(s.root, { "rev-parse", "--git-common-dir" })
		if not common.ok then
			return nil, "not a git repository: " .. s.root .. " (" .. digest(common.stderr) .. ")"
		end
		local common_dir = trim(common.stdout)
		if common_dir:sub(1, 1) ~= "/" then
			common_dir = maki.fs.normalize(join(s.root, common_dir))
		end
		local repo = common_dir
		if repo:sub(-5) == "/.git" then
			repo = repo:sub(1, -6)
		end
		s.repo = repo

		local head = git(s.root, { "rev-parse", "HEAD" })
		if not head.ok then
			return nil, "git rev-parse HEAD failed"
		end
		s.base_rev = trim(head.stdout)
		local branch = git(s.root, { "rev-parse", "--abbrev-ref", "HEAD" })
		s.base = trim(branch.stdout)
		if s.base == "" or s.base == "HEAD" then
			return nil, "coordinator checkout is not on a branch"
		end
	end

	if not s.agents then
		local text, err = read_file(join(s.root, cfg.instructions_file))
		if not text then
			return nil, "cannot read " .. cfg.instructions_file .. ": " .. tostring(err)
		end
		s.agents = text
	end
	return true
end

local function loop_main(key, s)
	local ok, err = prepare(s)
	if not ok then
		escalate(s, "prepare", tostring(err))
		return
	end

	local guard = 0
	while true do
		guard = guard + 1
		if guard > 500 then
			escalate(s, s.state.stage, "transition guard exceeded (possible loop)")
			return
		end
		local entry = loops[key]
		if not entry or entry.cancelled then
			return
		end
		if not s.state.running then
			return
		end

		local stage = s.state.stage or "select"
		local runner = STAGES[stage]
		if not runner then
			escalate(s, stage, "unknown stage '" .. tostring(stage) .. "'")
			return
		end

		local verdict = runner(s)
		if verdict == "stop" then
			s.state.running = false
			if s.state.stage ~= "blocked" then
				s.state.stage = "idle"
			end
			save_state(s)
			return
		end
		if verdict == "halt" then
			return
		end
		maki.async.sleep(0)
	end
end

local function spawn_loop(key, s)
	loops[key] = loops[key] or {}
	local entry = loops[key]
	entry.cancelled = false
	s.entry = entry
	entry.task = maki.async.spawn(function()
		loop_main(key, s)
		if loops[key] == entry then
			loops[key] = nil
		end
	end)
end

-- ---------------------------------------------------------------------------
-- Control surface
-- ---------------------------------------------------------------------------

local function session_cwd(sid)
	local info = maki.session.read({ session = sid })
	if info and info.cwd and info.cwd ~= "" then
		return info.cwd
	end
	return maki.uv.cwd()
end

local function root_for(cwd)
	if not cwd then
		return nil
	end
	local root = maki.fs.root(cwd, ".git")
	return root or cwd
end

local function existing_state(sid)
	local d = maki.session.data_dir({ session = sid })
	local dir
	if d then
		dir = join(d, "loop")
	else
		local cwd = session_cwd(sid)
		local root = root_for(cwd)
		dir = join(maki.env.state_dir() or "/tmp", "formal-methods", slug(root or "root"))
	end
	return load_state(state_path(dir)), dir
end

local function make_context(sid, st, data)
	local root = (st and st.root) or root_for(session_cwd(sid))
	return {
		session = sid,
		root = root,
		data = data or data_dir(sid, root),
		state = st,
	}
end

local function start_loop(sid, opts)
	opts = opts or {}
	if not sid then
		return nil, "no focused session"
	end

	local cwd = session_cwd(sid)
	local root = root_for(cwd)
	if not root then
		return nil, "cannot determine project root"
	end
	local data = data_dir(sid, root)
	local key = data
	if loops[key] then
		return nil, "the workflow is already running for this session"
	end

	local st = load_state(state_path(data))
	if not st then
		st = fresh_state(sid, root)
	end
	st.session = sid
	st.root = root

	if opts.task and opts.task ~= "" then
		st.task = nil
		st.seed_task = opts.task
		st.stop_after_task = true
		st.stage = "select"
		st.blocked_stage = nil
		st.blocked_reason = nil
		st.feedback = opts.note
	elseif opts.resume then
		if not st.blocked_stage then
			return nil, "nothing to resume (no blocked stage)"
		end
		local blocked = st.blocked_stage
		local counter = ({
			research = "plan",
			plan_review = "plan",
			tests = "tests",
			tests_review = "tests",
			implement = "impl",
			verify = "verify",
			finalize = "verify",
			review = "review",
		})[blocked]
		if counter and st.attempts then
			st.attempts[counter] = 0
		end
		if blocked == "prepare" then
			st.stage = "select"
		elseif blocked == "task_size" then
			-- The user was asked to split or reword the task; select fresh.
			st.task = nil
			st.stage = "select"
		else
			st.stage = blocked
		end
		st.blocked_stage = nil
		st.blocked_reason = nil
		st.feedback = opts.note
	elseif opts.restart then
		st.task = nil
		st.feedback = opts.note
		st.stage = "select"
		st.completed = st.completed or {}
	elseif not st.running then
		-- Plain /fm start resumes a persisted, unfinished task if there is one.
		if st.stage == "blocked" then
			return nil, "blocked at " .. tostring(st.blocked_stage) .. "; use resume"
		end
		st.stage = st.stage or "select"
		if st.stage == "idle" then
			st.stage = "select"
		end
		st.feedback = opts.note
	end

	st.running = true
	local s = make_context(sid, st, data)
	mkdirs(s)
	save_state(s)
	spawn_loop(key, s)
	return data
end

local function stop_loop(sid)
	local st, data = existing_state(sid)
	if not st then
		return nil, "no workflow state for this session"
	end
	local key = data
	local entry = loops[key]
	st.running = false
	if st.stage ~= "blocked" then
		st.stage = "idle"
	end
	save_state(make_context(sid, st, data))
	if entry then
		entry.cancelled = true
		if entry.session then
			entry.session:close()
		end
		if entry.task then
			entry.task:cancel()
		end
		loops[key] = nil
	end
	return key
end

local function status_lines(st)
	if not st then
		return { "no formal-methods state for this session", "run /fm start [task-id] to begin" }
	end
	local t = st.task or {}
	local lines = {}
	lines[#lines + 1] = "task: " .. tostring(t.id or "-") .. "  " .. tostring(t.title or "")
	lines[#lines + 1] = "stage: " .. tostring(st.stage or "?") .. "  running: " .. tostring(st.running) .. "  attempts: "
		.. "plan=" .. tostring(attempt(st, "plan")) .. " tests=" .. tostring(attempt(st, "tests"))
		.. " impl=" .. tostring(attempt(st, "impl")) .. " verify=" .. tostring(attempt(st, "verify"))
		.. " review=" .. tostring(attempt(st, "review"))
	lines[#lines + 1] = "worktree: " .. tostring(t.worktree or "-") .. "  branch: " .. tostring(t.branch or "-")
	lines[#lines + 1] = "base: " .. tostring(t.base_rev or "-") .. "  merged: " .. tostring(t.merged_sha or "-")
	lines[#lines + 1] = checks_brief(st.last_checks)
	if st.blocked_stage then
		lines[#lines + 1] = "blocked at: " .. tostring(st.blocked_stage)
		lines[#lines + 1] = "reason: " .. clip(st.blocked_reason or "", 1200)
	end
	if st.feedback then
		lines[#lines + 1] = "feedback: " .. clip(st.feedback, 800)
	end
	lines[#lines + 1] = "updated: " .. tostring(st.updated_at and iso(st.updated_at) or "-")
	local h = st.history or {}
	if #h > 0 then
		lines[#lines + 1] = "history:"
		local from = math.max(1, #h - 6)
		for i = from, #h do
			local e = h[i]
			lines[#lines + 1] = "  " .. tostring(e.at) .. " " .. tostring(e.stage) .. " -> "
				.. tostring(e.outcome) .. "  " .. tostring(e.note or "")
		end
	end
	return lines
end

local function status_text(sid)
	local st = existing_state(sid)
	local lines = status_lines(st)
	return table.concat(lines, "\n")
end

local function show_status(sid)
	local st = existing_state(sid)
	local lines = status_lines(st)
	local buf = maki.ui.buf()
	for _, l in ipairs(lines) do
		buf:line(l)
	end
	local win = maki.ui.open_win(buf, {
		title = "formal-methods",
		width = "78%",
		height = math.min(#lines + 2, 26),
		footer = { { "q", "close" }, { "<Esc>", "close" } },
		border = "rounded",
	})
	maki.async.spawn(function()
		while win:is_open() do
			local ev = win:recv()
			if not ev then
				break
			end
			if ev.type == "key" and (ev.key == "q" or ev.key == "<Esc>") then
				break
			end
			if ev.type == "close" then
				break
			end
		end
		win:close()
	end)
end

local function control(sid, action, opts)
	opts = opts or {}
	if not sid then
		return nil, "no focused session"
	end
	local a = tostring(action or "status"):lower()
	if a == "start" then
		local dir, err = start_loop(sid, { task = opts.task, note = opts.note })
		if not dir then
			return nil, err
		end
		return "started; state in " .. dir, dir
	elseif a == "resume" then
		local dir, err = start_loop(sid, { resume = true, note = opts.note })
		if not dir then
			return nil, err
		end
		return "resumed", dir
	elseif a == "restart" then
		local dir, err = start_loop(sid, { restart = true, note = opts.note })
		if not dir then
			return nil, err
		end
		return "restarted", dir
	elseif a == "stop" then
		local key, err = stop_loop(sid)
		if not key then
			return nil, err
		end
		return "stopped"
	end
	return clip(status_text(sid), 8000)
end

-- ---------------------------------------------------------------------------
-- Registration
-- ---------------------------------------------------------------------------

maki.api.register_tool({
	name = "fm_loop",
	description = [[Control the deterministic formal-methods workflow (the Rust/Agda loop in AGENTS.md).

The loop runs in the background: it picks backlog tasks from todo.md, creates worktrees, spawns
research/tests/implementation/review subagents, runs fmt/clippy/test/agda, commits and merges.
A cheap decision model handles accept/revise verdicts. You are only woken when the loop blocks
(spec ambiguity, exhausted retries, merge conflict, empty backlog, dirty checkout).

Actions:
- status: compact state (always safe, cheap)
- start [task]: start the loop for the whole backlog, or only the named task
- resume [note]: continue after a block, optionally passing guidance for the next attempt
- stop: stop the loop and close its running subagent
- restart [note]: drop the current task and re-select from todo.md

Use this after a wake-up to answer and resume; do not poll it otherwise.]],
	kind = "workflow",
	audiences = { "main" },
	schema = {
		type = "object",
		properties = {
			action = {
				type = "string",
				enum = { "status", "start", "resume", "stop", "restart" },
				description = "What to do.",
				required = true,
			},
			task = { type = "string", description = "Task id for action=start (e.g. T3b). Omit to run the backlog." },
			note = { type = "string", description = "Guidance for resume/restart, appended to the next subagent prompt." },
		},
		required = { "action" },
	},
	handler = function(input, ctx)
		local sid = ctx:session_id()
		local msg, err = control(sid, input.action, { task = input.task, note = input.note })
		if not msg then
			return { llm_output = "error: " .. tostring(err), is_error = true }
		end
		return { llm_output = msg }
	end,
})

maki.api.register_command({
	name = "/fm",
	description = "Formal-methods loop: start [task] | stop | status | resume [note] | restart",
	nargs = "*",
	handler = function(opts)
		local sid = maki.session.current()
		local fargs = opts.fargs or {}
		local action = fargs[1] or "status"
		local rest = {}
		for i = 2, #fargs do
			rest[#rest + 1] = fargs[i]
		end
		local arg = table.concat(rest, " ")

		if action == "status" then
			show_status(sid)
			return
		end
		local is_sub = action == "resume" or action == "restart"
		local msg, err = control(sid, action, { task = arg, note = is_sub and arg or nil })
		if not msg then
			maki.ui.flash("[fm] " .. tostring(err))
			return
		end
		maki.ui.flash("[fm] " .. msg)
	end,
})

-- Resume a persisted running state after /reload, but never auto-resume a
-- blocked one: blocks are for the chat to answer.
maki.api.create_autocmd("SessionFocusChanged", {
	callback = function(ev)
		if not cfg.auto_resume then
			return
		end
		local sid = ev.data.session_id
		if not sid then
			return
		end
		local st, data = existing_state(sid)
		if not st or not st.running then
			return
		end
		if loops[data] then
			return
		end
		local s = make_context(sid, st, data)
		spawn_loop(data, s)
		maki.log.info("[formal-methods] auto-resumed at " .. tostring(st.stage))
	end,
})

function M.setup(opts)
	for k, v in pairs(opts or {}) do
		cfg[k] = v
	end
	return M
end

function M.status(sid)
	return status_text(sid or maki.session.current())
end

return M
