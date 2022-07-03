(module user.lsp.init {autoload {ts-lsp user.lsp.typescript
                                 : lspconfig
                                 cmp-lsp cmp_nvim_lsp
                                 : nvim-lsp-installer
                                 my-which-key user.which-key
                                 a aniseed.core}})

(module user.lsp.init)

(nvim-lsp-installer.setup)

(local capabilities
       (cmp-lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities)))

(fn on_attach [client bufnr]
  (let [bufopts {:noremap true :silent true :buffer bufnr}]
    (do
      (vim.keymap.set :n :K vim.lsp.buf.hover bufopts)
      (vim.keymap.set :n :gh vim.lsp.buf.hover bufopts)
      (vim.keymap.set :n :<C-k> vim.lsp.buf.signature_help bufopts)
      (my-which-key.register-lsp-mappings bufnr))))

(lspconfig.sumneko_lua.setup {: capabilities
                              : on_attach
                              :settings {:Lua {:runtime {:version :LuaJIT}
                                               :diagnostics {:globals [:vim]}
                                               :workspace {:library (vim.api.nvim_get_runtime_file ""
                                                                                                   true)}
                                               :telemetry {:enable false}}}})

(fn lsp-setup-with-defaults [...]
  (let [server-names [...]]
    (a.run! #((. lspconfig $1 :setup) {:capabilities capabilities :on_attach on_attach})
            server-names)))

(lsp-setup-with-defaults :gopls :svelte :tailwindcss :jsonls :rust_analyzer)

(ts-lsp.setup {: capabilities : on_attach})

(local eslint-group (vim.api.nvim_create_augroup :EslintFix {:clear true}))
(lspconfig.eslint.setup {: capabilities
                         :on_attach (fn [client bufnr]
                                      (on_attach client bufnr)
                                      (vim.api.nvim_create_autocmd :BufWritePre
                                                                   {:pattern "*.tsx,*.ts,*.jsx,*.js,*.vue,*.svelte"
                                                                    :group eslint-group
                                                                    :command :EslintFixAll}))})

(tset vim.lsp.handlers :textDocument/hover
      (vim.lsp.with vim.lsp.handlers.hover {:border :rounded}))
