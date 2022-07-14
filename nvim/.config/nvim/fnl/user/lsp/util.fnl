(module user.lsp.util {autoload {ts-lsp user.lsp.typescript
                                 : lspconfig
                                 lsputil lspconfig.util
                                 cmp-lsp cmp_nvim_lsp
                                 : nvim-lsp-installer
                                 a aniseed.core}
                       require-macros [user.macros]})

(local default-capabilities
       (cmp-lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities)))

(defn default-on-attach [client bufnr]
      (let [bufopts {:silent true :buffer bufnr}]
        (do
          (nnoremap! :K vim.lsp.buf.hover bufopts)
          (nnoremap! :gh vim.lsp.buf.hover bufopts)
          (nnoremap! :<C-k> vim.lsp.buf.signature_help bufopts)
          (leader-map! ["+code-actions (lsp)" :c]
                       (:a "<cmd>lua vim.lsp.buf.code_action()<cr>" "lsp code actions" bufopts)
                       (:f "<cmd>Telescope lsp_references<cr>" "find references" bufopts)
                       (:r "<cmd>lua vim.lsp.buf.rename()<cr>" "rename current symbol" bufopts)
                       (:d "<cmd>Telescope lsp_definitions<cr>" "jump to definition" bufopts)
                       (:D "<cmd>lua vim.lsp.buf.declaration()<cr>" "jump to declaration" bufopts)
                       (:i "<cmd>lua vim.lsp.buf.implementation()<cr>" "jump to implmentation" bufopts)
                       (:t "<cmd>lua vim.lsp.buf.type_definition()<cr>" "jump to type definition" bufopts)))))

(defn setup-server [server config]
      (let [setup-fn (. lspconfig server :setup)
            merged-config (a.merge {:capabilities default-capabilities
                                     :on_attach default-on-attach}
                                    (or config {}))]
        (setup-fn merged-config)))

(defn get-package-json []
      "Gets the nearest package.json in parent directories. If one exists, returns it as a table"
      (let [p-json-dir (->> (vim.fn.expand "%:p")
                            (lsputil.path.sanitize)
                            (lsputil.find_package_json_ancestor))]
        (when p-json-dir
          (with-open [p_json (io.open (.. p-json-dir :/package.json) :r)]
            (vim.json.decode (p_json:read :*a))))))

(defn get-global-node-modules-path []
      "Returns the path to the system's global node_modules"
      (pick-values 1 (string.gsub (vim.fn.system "npm root -g") "\n" "")))

(defn is-vue-project [p_json] (not= nil (?. p_json :dependencies :vue)))

