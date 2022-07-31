(module user.lsp.init
  {autoload {ts-lsp user.lsp.typescript
             : lspconfig
             cmp-lsp cmp_nvim_lsp
             : nvim-lsp-installer
             a aniseed.core
             {:default-capabilities default-capabilities
              :default-on-attach default-on-attach
              :setup-server setup-server} user.lsp.util}
   require-macros user.macros})

(nvim-lsp-installer.setup)

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
