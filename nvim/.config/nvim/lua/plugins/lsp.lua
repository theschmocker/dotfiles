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

			local real_notify_once = vim.notify_once
			---@diagnostic disable-next-line: duplicate-set-field
			vim.notify_once = function (msg, level, opts)
				if msg == 'position_encoding param is required in vim.lsp.util.make_position_params. Defaulting to position encoding of the first client.' then
					return
				end
				real_notify_once(msg, level, opts)
			end

			vim.api.nvim_create_autocmd('LspAttach', {
				group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
				callback = function(event)
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

					-- Automatic lsp document highlight
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

			local base_eslint_on_attach = vim.lsp.config.eslint.on_attach
			local eslint_config = {
				on_attach = function (client, bufnr)
					---@diagnostic disable-next-line: need-check-nil
					base_eslint_on_attach(client, bufnr)
					if client.name == 'eslint' then
						vim.api.nvim_create_autocmd('BufWritePre', { buffer = bufnr, command = 'LspEslintFixAll' })
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
			}

			local base_ts_ls_on_attach = vim.lsp.config.ts_ls.on_attach
			local ts_ls = {
				init_options = {
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
				on_attach = function (client, bufnr)
					---@diagnostic disable-next-line: need-check-nil
					base_ts_ls_on_attach(client, bufnr)
					if client.name == 'ts_ls' then
						vim.keymap.set({ 'n', 'v', 'x' }, '<localleader>ca', '<cmd>LspTypescriptSourceAction<cr>', {
							desc = "TypeScript Source Action",
							buffer = bufnr,
							silent = true,
						})
					end
				end
			}

			local servers = {
				ts_ls = ts_ls,
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
				eslint = eslint_config,
				gdscript = {},
				['rust_analyzer'] = {
					settings = {
						["rust-analyzer"] = {
							imports = {
								granularity = {
									group = "module",
								},
								prefix = "self",
							},
							cargo = {
								buildScripts = {
									enable = true,
								},
							},
							procMacro = {
								enable = true
							},
						}
					}
				},
			}

			-- Mason doesn't detect that it should download the arm build of omnisharp on windows, so it doesn't
			-- work with my setup. As a hack, set it up to use the working build from my Emacs setup.
			if vim.fn.has('win32') == 1 then
				servers['omnisharp'] = {
					cmd = {
						"C:/Users/schmo/AppData/Roaming/.emacs.d/.local/etc/lsp/omnisharp-roslyn/latest/OmniSharp.exe",
						'-z', -- https://github.com/OmniSharp/omnisharp-vscode/pull/4300
						'--hostPID',
						tostring(vim.fn.getpid()),
						'DotNet:enablePackageRestore=false',
						'--encoding',
						'utf-8',
						'--languageserver',
					}
				}
			end


			for server_name, config in pairs(servers) do
				-- This handles overriding only values explicitly passed
				-- by the server configuration above. Useful when disabling
				-- certain features of an LSP (for example, turning off formatting for ts_ls)
				config.capabilities = vim.tbl_deep_extend('force', {}, capabilities, config.capabilities or {})
				vim.lsp.config(server_name, config)
				vim.lsp.enable(server_name)
			end

			require('mason-lspconfig').setup {
				automatic_enable = true,
				ensure_installed = {},
				automatic_installation = false,
			}
		end,
	},
}
