local function half_scroll(dir)
	local view, view_err = maki.fn.winsaveview()
	if not view then
		maki.ui.flash(view_err or "no transcript view")
		return
	end
	local half = math.max(1, math.floor(view.height / 2))
	local ok, err = maki.fn.winrestview({ topline = view.topline + dir * half })
	if not ok then
		maki.ui.flash(err or "scroll failed")
	end
end

maki.keymap.set("n", "<C-d>", function()
	local st = maki.ui.input()
	if st and st.text:match("^%s*$") then
		local ok, err = maki.api.run_command("/exit")
		if not ok then
			maki.ui.flash(err or "exit failed")
		end
	end
end, { desc = "Exit if prompt empty" })

maki.keymap.set("n", "<M-p>", function()
	half_scroll(-1)
end, { desc = "Scroll half page up" })

maki.keymap.set("n", "<M-n>", function()
	half_scroll(1)
end, { desc = "Scroll half page down" })
