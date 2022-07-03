(module user.lsp.init
        {autoload {ts-lsp user.lsp.typescript
                   : lspconfig
                   cmp-lsp cmp_nvim_lsp
                   : nvim-lsp-installer
                   my-which-key user.which-key
                   a aniseed.core
                   {:default-capabilities default-capabilities
                     :default-on-attach default-on-attach
                     :setup-server setup-server} user.lsp.util}})

(nvim-lsp-installer.setup)

(setup-server :gopls)
(setup-server :svelte)
(setup-server :tailwindcss)
(setup-server :jsonls)
(setup-server :rust_analyzer)

(local eslint-group (vim.api.nvim_create_augroup :EslintFix {:clear true}))
(setup-server :eslint
              {:on_attach (fn [client bufnr]
                            (default-on-attach client bufnr)
                            (vim.api.nvim_create_autocmd :BufWritePre
                                                         {:pattern "*.tsx,*.ts,*.jsx,*.js,*.vue,*.svelte"
                                                          :group eslint-group
                                                          :command :EslintFixAll}))})

(setup-server :sumneko_lua
              {:settings {:Lua {:runtime {:version :LuaJIT}
                                :diagnostics {:globals [:vim]}
                                :workspace {:library (vim.api.nvim_get_runtime_file ""
                                                                                    true)}
                                :telemetry {:enable false}}}})

(ts-lsp.setup)

(tset vim.lsp.handlers :textDocument/hover
      (vim.lsp.with vim.lsp.handlers.hover {:border :rounded}))

