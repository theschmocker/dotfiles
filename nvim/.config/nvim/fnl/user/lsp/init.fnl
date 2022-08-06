(module user.lsp.init
  {autoload {ts-lsp user.lsp.typescript
             : lspconfig
             cmp-lsp cmp_nvim_lsp
             : mason-lspconfig
             a aniseed.core
             : null-ls
             {:default-capabilities default-capabilities
              :default-on-attach default-on-attach
              :setup-server setup-server} user.lsp.util}
   require-macros user.macros})

(mason-lspconfig.setup {:ensure_installed [:gopls
                                           :svelte
                                           :tailwindcss
                                           :jsonls
                                           :sumneko_lua
                                           :eslint
                                           :tsserver
                                           :volar]})

(require :user.lsp.svelte-ts-plugin)

(setup-server :gopls)
(setup-server :svelte)
(setup-server :tailwindcss)
(setup-server :jsonls)
(setup-server :rust_analyzer)

(local eslint-group (augroup! [:EslintFix {:clear true}]))
(setup-server :eslint
              {:on_attach (fn [client bufnr]
                            (default-on-attach client bufnr)
                            (au! :BufWritePre
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

(vim.diagnostic.config {:virtual_text false
                        :signs true
                        :underline true
                        :update_in_insert false
                        :severity_sort false})

(null-ls.setup {:sources [null-ls.builtins.formatting.stylua
                          (null-ls.builtins.formatting.prettierd.with {:filetypes ["javascript"
                                                                                  "javascriptreact"
                                                                                  "typescript"
                                                                                  "typescriptreact"
                                                                                  "vue"
                                                                                  "css"
                                                                                  "scss"
                                                                                  "less"
                                                                                  "html"
                                                                                  "json"
                                                                                  "jsonc"
                                                                                  "yaml"
                                                                                  "markdown"
                                                                                  "graphql"
                                                                                  "handlebars"
                                                                                  "svelte"]})]
                :on_attach (fn [client bufnr]
                             (when (and (= :null-ls client.name)
                                        client.resolved_capabilities.document_formatting)
                               (nnoremap! :<leader>cF vim.lsp.buf.formatting_sync {:desc "format"})
                               (augroup! [:NullLsFmt {:clear true}]
                                         (vim.api.nvim_clear_autocmds {:buffer bufnr
                                                                       :group :NullLsFmt})
                                         (au! :BufWritePre
                                              {:buffer bufnr
                                               :callback vim.lsp.buf.formatting_sync}))))})
