(module user.lsp.typescript
        {autoload {a aniseed.core
                   : lspconfig
                   lsputil lspconfig.util
                   {:get-package-json get-package-json
                    :is-vue-project is-vue-project
                    :setup-server setup-server} user.lsp.util
                   {: with-svelte-plugin} user.lsp.svelte-ts-plugin}})

(fn temp-text-document-edit-handler [text_document_edit index offset_encoding]
  "temporary fix for volar's document version issue"
  (let [text_document text_document_edit.textDocument
        bufnr (vim.uri_to_bufnr text_document.uri)]
    (when (= nil offset_encoding)
      (vim.notify_once "apply_text_document_edit must be called with valid offset encoding"
                       vim.log.levels.WARN))
    (vim.lsp.util.apply_text_edits text_document_edit.edits bufnr
                                   offset_encoding)))

(defn setup []
      "In Vue projects, I want to use Volar's Takeover mode for JS/TS files. Otherwise, I want to use tsserver.
  This will set up the relevant server based on the presence of vue in package.json. There may be a better way to handle this.
  config table should have the `capabilities` and `on_attach` keys to pass to lspconfig's setup"

      ;; disables single file support for tsserver, which otherwise overrides root_dir
      (tset lsputil :on_setup
            (lsputil.add_hook_before lsputil.on_setup (fn [config]
                                                         (when (= "tsserver" (?. config :name))
                                                           (tset config :single_file_support false)))))

      (let [default_root_dir (lsputil.root_pattern :package.json)]
        ;; temporary fix for volar's document version issue
        (tset vim.lsp.util :apply_text_document_edit
              temp-text-document-edit-handler)
        (setup-server :tsserver
                      (with-svelte-plugin {
                        :root_dir (fn [filepath]
                                    (if (is-vue-project (get-package-json))
                                      nil
                                      (default_root_dir filepath)))}))
        (setup-server :volar
                      {:filetypes [:typescript
                                   :javascript
                                   :javascriptreact
                                   :typescriptreact
                                   :vue]
                       :root_dir (fn [filepath]
                                   (if (is-vue-project (get-package-json))
                                     (default_root_dir filepath)
                                     nil))})))

