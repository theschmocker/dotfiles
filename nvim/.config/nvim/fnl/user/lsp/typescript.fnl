(module user.lsp.typescript
  {autoload {a aniseed.core
             : lspconfig
             util lspconfig.util}})

(fn get-package-json []
  "Gets the nearest package.json in parent directories. If one exists, returns it as a table"
  (let [current-file (util.path.sanitize (vim.fn.expand "%:p"))
        p-json-dir (util.find_package_json_ancestor current-file)]
    (when p-json-dir
      (with-open [p_json (io.open (.. p-json-dir "/package.json") :r)]
        (vim.json.decode (p_json:read "*a"))))))

(fn get-global-node-modules-path []
  "Returns the path to the system's global node_modules"
  (let [(res) (string.gsub (vim.fn.system "npm root -g") "\n" "")]
    res))

(fn is-vue-project [p_json]
  (not= nil (?. p_json :dependencies :vue)))

(fn temp-text-document-edit-handler [text_document_edit index offset_encoding]
  "temporary fix for volar's document version issue"
  (let [text_document text_document_edit.textDocument
        bufnr (vim.uri_to_bufnr text_document.uri)]
    (do
      (when (= nil offset_encoding)
        (vim.notify_once 
          "apply_text_document_edit must be called with valid offset encoding"
          vim.log.levels.WARN
          ))
      (vim.lsp.util.apply_text_edits text_document_edit.edits bufnr offset_encoding))))

(defn setup [config]
  "In Vue projects, I want to use Volar's Takeover mode for JS/TS files. Otherwise, I want to use tsserver.
  This will set up the relevant server based on the presence of vue in package.json. There may be a better way to handle this.
  config table should have the `capabilities` and `on_attach` keys to pass to lspconfig's setup"
  (let [default_root_dir (util.root_pattern "package.json")]
    (do
      ;; temporary fix for volar's document version issue
      (tset vim.lsp.util :apply_text_document_edit temp-text-document-edit-handler)

      (lspconfig.tsserver.setup {:capabilities config.capabilities
                                 :on_attach config.on_attach

                                 ;; TODO: I wonder how I could hook into init_options more dynamically. There doesn't seem to be an issue if the plugin isn't installed, but it'd be interesting to somehow extract
                                 ;; this to a plugin. Maybe one that handled registering the TS plugin automatically in a Svelte workspace, installing it, etc. like the VSCode plugin. VSCode handles TS plugins
                                 ;; in a special way
                                 :init_options {:plugins [{:name "typescript-svelte-plugin"
                                                           :location (.. (get-global-node-modules-path) "/typescript-svelte-plugin")}]}
                                 :root_dir (fn [filepath]
                                             (if (is-vue-project (get-package-json))
                                               nil
                                               (default_root_dir filepath)))})
      (lspconfig.volar.setup {:capabilities config.capabilities
                              :on_attach config.on_attach
                              :filetypes ["typescript" "javascript" "javascriptreact" "typescriptreact" "vue"]
                              :root_dir (fn [filepath]
                                          (if (is-vue-project (get-package-json))
                                            (default_root_dir filepath)
                                            nil))}))))


