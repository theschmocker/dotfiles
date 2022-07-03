(module user.lsp.util {autoload {ts-lsp user.lsp.typescript
                                 : lspconfig
                                 lsputil lspconfig.util
                                 cmp-lsp cmp_nvim_lsp
                                 : nvim-lsp-installer
                                 my-which-key user.which-key
                                 a aniseed.core}})

(local default-capabilities
       (cmp-lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities)))

(defn default-on-attach [client bufnr]
      (let [bufopts {:noremap true :silent true :buffer bufnr}]
        (do
          (vim.keymap.set :n :K vim.lsp.buf.hover bufopts)
          (vim.keymap.set :n :gh vim.lsp.buf.hover bufopts)
          (vim.keymap.set :n :<C-k> vim.lsp.buf.signature_help bufopts)
          (my-which-key.register-lsp-mappings bufnr))))

(defn setup-server [server config]
      (let [setup-fn (. lspconfig server :setup)
            merged-config (a.merge! {:capabilities default-capabilities
                                     :on_attach default-on-attach}
                                    (or config {}))]
        (setup-fn merged-config)))

(defn get-package-json []
      "Gets the nearest package.json in parent directories. If one exists, returns it as a table"
      (let [current-file (lsputil.path.sanitize (vim.fn.expand "%:p"))
            p-json-dir (lsputil.find_package_json_ancestor current-file)]
        (when p-json-dir
          (with-open [p_json (io.open (.. p-json-dir :/package.json) :r)]
            (vim.json.decode (p_json:read :*a))))))

(defn get-global-node-modules-path []
      "Returns the path to the system's global node_modules"
      (let [(res) (string.gsub (vim.fn.system "npm root -g") "\n" "")]
        res))

(defn is-vue-project [p_json] (not= nil (?. p_json :dependencies :vue)))

