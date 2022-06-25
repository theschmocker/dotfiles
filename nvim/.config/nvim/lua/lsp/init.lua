-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
-- capabilities.textDocument.completion.completionItem.snippetSupport = true

local on_attach = function(client, bufnr)
	-- Mappings.
	-- See `:help vim.lsp.*` for documentation on any of the below functions
	local bufopts = { noremap=true, silent=true, buffer=bufnr }
	vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
	vim.keymap.set('n', 'gh', vim.lsp.buf.hover, bufopts)
	vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
	require'which-key-config'.register_lsp_mappings(bufnr)
end

require("nvim-lsp-installer").setup {}

require'lspconfig'.sumneko_lua.setup {
	capabilities = capabilities,
	on_attach = on_attach,
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = 'LuaJIT',
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = {'vim'},
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
}

require'lspconfig'.gopls.setup{
	capabilities = capabilities,
	on_attach = on_attach,
}

require'lspconfig'.svelte.setup{
	capabilities = capabilities,
	on_attach = on_attach,
}

require'lspconfig'.tailwindcss.setup{
	capabilities = capabilities,
	on_attach = on_attach,
}

require'lspconfig'.jsonls.setup {
	capabilities = capabilities,
}

require'lspconfig'.omnisharp.setup{
	-- TODO TODO do I need any of this with the lsp installer?
	-- TODO this is for work setup; configure for macOS too
	-- local pid = vim.fn.getpid()
	-- cmd = { "~/omnisharp-win-x64/OmniSharp.exe", "--languageserver" , "--hostPID", tostring(pid) };
	on_attach = on_attach,
	capabilities = capabilities,
	-- TODO doesn't seem like this is the write way to pass this config. Would be awesome to have decompilation support
	init_config = {
		RoslynExtensionsOptions = {
			enableDecompilationSupport = true,
			enableImportCompletion = true,
		}
	}
}

require'lspconfig'.eslint.setup{
	on_attach = function (client, bufnr)
		on_attach(client, bufnr)

		local group = vim.api.nvim_create_augroup("EslintFix", { clear = true })
		vim.api.nvim_create_autocmd("BufWritePre", { pattern = "*.tsx,*.ts,*.jsx,*.js,*.vue,*.svelte", group = group, command = "EslintFixAll" })
	end,
	capabilities = capabilities,
}

require'lspconfig'.rust_analyzer.setup{
	capabilities = capabilities,
	on_attach = on_attach,
}

require'lsp/typescript'.setup {
	capabilities = capabilities,
	on_attach = on_attach,
}

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "rounded",
})

