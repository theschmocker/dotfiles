return {
	{
		-- `lazydev` configures Lua LSP for your Neovim config, runtime and plugins
		-- used for completion, annotations and signatures of Neovim apis
		'folke/lazydev.nvim',
		ft = 'lua',
		opts = {
			library = {
				-- Load luvit types when the `vim.uv` word is found
				{ path = '${3rd}/luv/library', words = { 'vim%.uv' } },
			},
		},
	},
	{
		-- Main LSP Configuration
		'neovim/nvim-lspconfig',
		dependencies = {
			-- Automatically install LSPs and related tools to stdpath for Neovim
			-- Mason must be loaded before its dependents so we need to set it up here.
			-- NOTE: `opts = {}` is the same as calling `require('mason').setup({})`
			{ 'williamboman/mason.nvim', opts = {} },
			'williamboman/mason-lspconfig.nvim',
			-- 'WhoIsSethDaniel/mason-tool-installer.nvim',

			-- Useful status updates for LSP.
			{ "j-hui/fidget.nvim", tag = "v1.5.0" },

			-- Allows extra capabilities provided by nvim-cmp
			'hrsh7th/cmp-nvim-lsp',
			'folke/which-key.nvim',
		},
		config = function()
			-- Brief aside: **What is LSP?**
			--
			-- LSP is an initialism you've probably heard, but might not understand what it is.
			--
			-- LSP stands for Language Server Protocol. It's a protocol that helps editors
			-- and language tooling communicate in a standardized fashion.
			--
			-- In general, you have a "server" which is some tool built to understand a particular
			-- language (such as `gopls`, `lua_ls`, `rust_analyzer`, etc.). These Language Servers
			-- (sometimes called LSP servers, but that's kind of like ATM Machine) are standalone
			-- processes that communicate with some "client" - in this case, Neovim!
			--
			-- LSP provides Neovim with features like:
			--  - Go to definition
			--  - Find references
			--  - Autocompletion
			--  - Symbol Search
			--  - and more!
			--
			-- Thus, Language Servers are external tools that must be installed separately from
			-- Neovim. This is where `mason` and related plugins come into play.
			--
			-- If you're wondering about lsp vs treesitter, you can check out the wonderfully
			-- and elegantly composed help section, `:help lsp-vs-treesitter`

			--  This function gets run when an LSP attaches to a particular buffer.
			--    That is to say, every time a new file is opened that is associated with
			--    an lsp (for example, opening `main.rs` is associated with `rust_analyzer`) this
			--    function will be executed to configure the current buffer
			vim.api.nvim_create_autocmd('LspAttach', {
				group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
				callback = function(event)
					-- NOTE: Remember that Lua is a real programming language, and as such it is possible
					-- to define small helper and utility functions so you don't have to repeat yourself.
					--
					-- In this case, we create a function that lets us more easily define mappings specific
					-- for LSP related items. It sets the mode, buffer and description for us each time.
					local map = function(keys, func, desc, mode)
						mode = mode or 'n'
						vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
					end

					vim.keymap.set('n', 'gh', vim.lsp.buf.hover, {
						desc = "LSP: Hover",
						buffer = event.buf,
						silent = true,
					})

					vim.keymap.set('i', '<C-S>', vim.lsp.buf.signature_help, {
						desc = "LSP: Signature Help",
						buffer = event.buf,
						silent = true,
					})

					map('gd', function ()
						vim.diagnostic.open_float()
					end, "Show diagnostic", "n")

					local leader_map = require('util.keymap').leader_map
					leader_map({
						name = "+code",
						prefix = "c",
						buffer = event.buf,
						mode = "n",
						keys = {
							d = { require('telescope.builtin').lsp_definitions, desc = "Find Definitions" },
							f = { require('telescope.builtin').lsp_references, desc = "Find References" },
							r = { vim.lsp.buf.rename, desc = "Rename" },
							a = { vim.lsp.buf.code_action, desc = "Code Actions", mode = { 'n', 'v' } },
							i = { require('telescope.builtin').lsp_implementations, desc = "Find Implementations" }
						}
					})

					-- -- Jump to the definition of the word under your cursor.
					-- --  This is where a variable was first declared, or where a function is defined, etc.
					-- --  To jump back, press <C-t>.
					-- map('gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')
					--
					-- -- Find references for the word under your cursor.
					-- map('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
					--
					-- -- Jump to the implementation of the word under your cursor.
					-- --  Useful when your language has ways of declaring types without an actual implementation.
					-- map('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
					--
					-- -- Jump to the type of the word under your cursor.
					-- --  Useful when you're not sure what type a variable is and you want to see
					-- --  the definition of its *type*, not where it was *defined*.
					-- map('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
					--
					-- -- Fuzzy find all the symbols in your current document.
					-- --  Symbols are things like variables, functions, types, etc.
					-- map('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
					--
					-- -- Fuzzy find all the symbols in your current workspace.
					-- --  Similar to document symbols, except searches over your entire project.
					-- map('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
					--
					-- -- Rename the variable under your cursor.
					-- --  Most Language Servers support renaming across files, etc.
					-- map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
					--
					-- -- Execute a code action, usually your cursor needs to be on top of an error
					-- -- or a suggestion from your LSP for this to activate.
					-- map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction', { 'n', 'x' })
					--
					-- -- WARN: This is not Goto Definition, this is Goto Declaration.
					-- --  For example, in C this would take you to the header.
					-- map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

					-- The following two autocommands are used to highlight references of the
					-- word under your cursor when your cursor rests there for a little while.
					--    See `:help CursorHold` for information about when this is executed
					--
					-- When you move your cursor, the highlights will be cleared (the second autocommand).
					local client = vim.lsp.get_client_by_id(event.data.client_id)
					if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
						local highlight_augroup = vim.api.nvim_create_augroup('kickstart-lsp-highlight', { clear = false })
						vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
							buffer = event.buf,
							group = highlight_augroup,
							callback = vim.lsp.buf.document_highlight,
						})

						vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
							buffer = event.buf,
							group = highlight_augroup,
							callback = vim.lsp.buf.clear_references,
						})

						vim.api.nvim_create_autocmd('LspDetach', {
							group = vim.api.nvim_create_augroup('kickstart-lsp-detach', { clear = true }),
							callback = function(event2)
								vim.lsp.buf.clear_references()
								vim.api.nvim_clear_autocmds { group = 'kickstart-lsp-highlight', buffer = event2.buf }
							end,
						})
					end

					-- The following code creates a keymap to toggle inlay hints in your
					-- code, if the language server you are using supports them
					--
					-- This may be unwanted, since they displace some of your code
					if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
						map('<leader>th', function()
							vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
						end, '[T]oggle Inlay [H]ints')
					end

					-- WORKAROUND for omnisharp token casing
					-- https://github.com/OmniSharp/omnisharp-roslyn/issues/2483#issuecomment-1539809155
					local function toSnakeCase(str)
						return string.gsub(str, "%s*[- ]%s*", "_")
					end

					if client and client.name == 'omnisharp' then
						local tokenModifiers = client.server_capabilities.semanticTokensProvider.legend.tokenModifiers
						for i, v in ipairs(tokenModifiers) do
							tokenModifiers[i] = toSnakeCase(v)
						end
						local tokenTypes = client.server_capabilities.semanticTokensProvider.legend.tokenTypes
						for i, v in ipairs(tokenTypes) do
							tokenTypes[i] = toSnakeCase(v)
						end
					end
					-- /WORKAROUND for omnisharp token casing
				end,
			})

			-- Change diagnostic symbols in the sign column (gutter)
			-- if vim.g.have_nerd_font then
			--   local signs = { ERROR = '', WARN = '', INFO = '', HINT = '' }
			--   local diagnostic_signs = {}
			--   for type, icon in pairs(signs) do
			--     diagnostic_signs[vim.diagnostic.severity[type]] = icon
			--   end
			--   vim.diagnostic.config { signs = { text = diagnostic_signs } }
			-- end

			-- LSP servers and clients are able to communicate to each other what features they support.
			--  By default, Neovim doesn't support everything that is in the LSP specification.
			--  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
			--  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

			local function get_mason_package_install_path(package_name)
				return vim.fn.expand("$MASON/packages/" .. package_name)
			end

			local vue_lang_server_path = vim.fs.joinpath(
				get_mason_package_install_path('vue-language-server'),
				'node_modules',
				'@vue',
				'language-server'
			)

			local svelte_ts_plugin_path = get_mason_package_install_path('svelte-language-server')

			-- Enable the following language servers
			--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
			--
			--  Add any additional override configuration in the following tables. Available keys are:
			--  - cmd (table): Override the default command used to start the server
			--  - filetypes (table): Override the default list of associated filetypes for the server
			--  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
			--  - settings (table): Override the default settings passed when initializing the server.
			--        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/
			local servers = {
				ts_ls = {
					init_options = {
						tsserver = {
							logVerbosity = 'verbose',
							logDirectory = '/Users/jacob/.local/share/nvim/tsserver',
							trace = 'verbose',
						},
						plugins = {
							{
								name = '@vue/typescript-plugin',
								location = vue_lang_server_path,
								languages = {"javascript", "typescript", "vue"},
							},
							{
								name = 'typescript-svelte-plugin',
								location = svelte_ts_plugin_path,
							}
						},
					},
					single_file_support = false,
					filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue' },
				},
				vue_ls = {},
				lua_ls = {
					-- cmd = { ... },
					-- filetypes = { ... },
					-- capabilities = {},
					settings = {
						Lua = {
							completion = {
								callSnippet = 'Replace',
							},
							-- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
							-- diagnostics = { disable = { 'missing-fields' } },
						},
					},
				},
				eslint = {
					on_attach = function (client, bufnr)
						if client.name == 'eslint' then
							vim.api.nvim_create_autocmd('BufWritePre', { buffer = bufnr, command = 'EslintFixAll' })
						end
					end,
					settings = {
						rulesCustomizations = {
							{
								rule = "prettier*",
								severity = "off",
							},
						},
					},
				},
				gdscript = {},
			}

			for server_name, config in pairs(servers) do
				-- This handles overriding only values explicitly passed
				-- by the server configuration above. Useful when disabling
				-- certain features of an LSP (for example, turning off formatting for ts_ls)
				config.capabilities = vim.tbl_deep_extend('force', {}, capabilities, config.capabilities or {})
				vim.lsp.config(server_name, config)
				vim.lsp.enable(server_name)
			end

			-- Mason doesn't detect that it should download the arm build of omnisharp on windows, so it doesn't
			-- work with my setup. As a hack, set it up to use the working build from my Emacs setup.
			if vim.fn.has('win32') == 1 then
				servers['omnisharp'] = {
					cmd = { "C:/Users/schmo/AppData/Roaming/.emacs.d/.local/etc/lsp/omnisharp-roslyn/latest/OmniSharp.exe" }
				}
			end

			require('mason-lspconfig').setup {
				automatic_enable = true,
				ensure_installed = {},
				automatic_installation = false,
			}
		end,
	},
}
