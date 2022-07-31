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
  (print client.name (vim.inspect client.resolved_capabilities))
  (when client.resolved_capabilities.document_highlight
    (augroup! ["lsp_document_highlight" {:clear false}]
              (vim.api.nvim_clear_autocmds {:buffer bufnr
                                            :group "lsp_document_highlight"})

              (au! ["CursorHold" "CursorHoldI"]
                   {:buffer bufnr
                    :callback vim.lsp.buf.document_highlight})

              (au! "CursorMoved"
                   {:buffer bufnr
                    :callback vim.lsp.buf.clear_references}))
    )

  (let [bufopts {:silent true :buffer bufnr}]
    (nnoremap! :K vim.lsp.buf.hover bufopts)
    (nnoremap! :gh vim.lsp.buf.hover bufopts)
    (nnoremap! :gd #(vim.diagnostic.open_float nil {:focus false}))
    (nnoremap! :<C-k> vim.lsp.buf.signature_help bufopts)
    (leader-map! ["+code-actions (lsp)" :c]
                 (:a "<cmd>lua vim.lsp.buf.code_action()<cr>" "lsp code actions" bufopts)
                 (:f "<cmd>Telescope lsp_references<cr>" "find references" bufopts)
                 (:r "<cmd>lua vim.lsp.buf.rename()<cr>" "rename current symbol" bufopts)
                 (:d "<cmd>Telescope lsp_definitions<cr>" "jump to definition" bufopts)
                 (:D "<cmd>lua vim.lsp.buf.declaration()<cr>" "jump to declaration" bufopts)
                 (:i "<cmd>lua vim.lsp.buf.implementation()<cr>" "jump to implmentation" bufopts)
                 (:t "<cmd>lua vim.lsp.buf.type_definition()<cr>" "jump to type definition" bufopts))))

(defn setup-server [server config]
  (let [setup-fn (. lspconfig server :setup)
        merged-config (a.merge {:capabilities default-capabilities}
                               (or config {}))]
    (tset merged-config
          :on_attach
          (if merged-config.on_attach
            (lsputil.add_hook_before merged-config.on_attach default-on-attach)
            default-on-attach))
    (setup-fn merged-config)))

(defn get-package-json []
  "Gets the nearest package.json in parent directories. If one exists, returns it as a table"
  (let [p-json-dir (->> (vim.fn.expand "%:p")
                        (lsputil.path.sanitize)
                        (lsputil.find_package_json_ancestor))]
    (when p-json-dir
      (with-open [p_json (io.open (.. p-json-dir :/package.json) :r)]
        (vim.json.decode (p_json:read :*a))))))

(defn is-vue-project [p_json]
  "Determines whether the given package.json (as a table) belongs to a Vue project."
  (not= nil (?. p_json :dependencies :vue)))

