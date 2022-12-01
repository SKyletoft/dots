lua <<EOF
local dap = require("dap")
local dapui = require("dapui")
local jdtls = require("jdtls")

dapui.setup()

dap.adapters.cppdbg = {
	id = 'cppdbg',
	type = 'executable',
	command = '${pkgs.vscode-extensions.ms-vscode.cpptools}/share/vscode/extensions/ms-vscode.cpptools/debugAdapters/bin/OpenDebugAD7',
}

local rust_c_cpp = {
	{
		name = "Launch file",
		type = "cppdbg",
		request = "launch",
		program = function()
			return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
		end,
		cwd = "''${workspaceFolder}",
		stopAtEntry = true,
	},
	{
		name = 'Attach to gdbserver :1234',
		type = 'cppdbg',
		request = 'launch',
		MIMode = 'gdb',
		miDebuggerServerAddress = 'localhost:1234',
		miDebuggerPath = '/usr/bin/gdb',
		cwd = "''${workspaceFolder}",
		program = function()
			return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
		end,
	},
}

jdtls.start_or_attach({
	cmd = { "${pkgs.jdt-language-server}/bin/jdt-language-server" },
	root_dir = vim.fs.dirname(vim.fs.find({'.gradlew', '.git', 'mvnw'}, { upward = true })[1]),
	on_attach = function(client, bufnr)
		require('jdtls').setup_dap({ hotcodereplace = 'auto' })
	end
})
jdtls.setup_dap()

local jvm = {
	{
		name = "Launch class",
		type = "java",
		request = "launch",
		program = function()
			return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
		end,
		cwd = "''${workspaceFolder}",
		stopAtEntry = true,
	}
}

dap.configurations.rust    = rust_c_cpp
dap.configurations.c       = rust_c_cpp
dap.configurations.cpp     = rust_c_cpp
dap.configurations.java    = jvm
dap.configurations.scala   = jvm
dap.configurations.clojure = jvm
EOF
