(module user.lsp.svelte-ts-plugin
  {autoload {a aniseed.core
             util lspconfig.util
             : fennel
             {: sanitize} user.util}
   require {job plenary.job}})

(local ts-plugin-project-root (-> (vim.fn.stdpath "data")
                                  (.. "/svelte-tools/typescript-svelte-plugin")
                                  (sanitize)))

(local ts-plugin-location (-> ts-plugin-project-root
                              (.. "/node_modules/typescript-svelte-plugin")
                              (sanitize)))

(defn- add-tsserver-plugin [config plugin]
  "Adds plugin to the given tsserver config and returns the updated config. Mutates config"
  (let [init-options (or config.init_options {})
        plugins (or init-options.plugins [])]
    (table.insert plugins plugin)
    (tset init-options :plugins plugins)
    (tset config :init_options init-options)
    config))

(defn- ts-plugin-installed? []
  "Check if typescript-svelte-plugin is installed"
  (and (-> ts-plugin-location
           (vim.fn.isdirectory)
           (= 1))
       (not= nil (-?> ts-plugin-project-root
                      (.. "/package.json")
                      (sanitize)
                      (a.slurp true)
                      (vim.json.decode)
                      (?. :dependencies :typescript-svelte-plugin)))))

(defn- install-typescript-svelte-plugin [callback]
  "Installs typescript-svelte-plugin to the svelte-tools data directory. Calls callback with no arguments when complete."
  (when (not (ts-plugin-installed?))
    (vim.fn.mkdir ts-plugin-project-root "p")
    (local npmrc (-> ts-plugin-project-root
                     (.. "/.npmrc")
                     (sanitize)))
    (a.spit npmrc "global-style=true")
    (let [init-project (job:new {:command "npm"
                                 :cwd ts-plugin-project-root
                                 :args ["init" "--yes" "--scope" "svelte-tools"]})
          install (job:new {:command "npm"
                            :cwd ts-plugin-project-root
                            :args ["install" "typescript-svelte-plugin"]})]
      (install:after (vim.schedule_wrap (fn []
                                          (vim.notify "typescript-svelte-plugin installed")
                                          (callback))))
      (vim.notify "Installing typescript-svelte-plugin")
      (doto init-project
            (: :and_then install)
            (: :start)))))

(defn- is-svelte-project? [root_dir]
  "Determines whether the given root_dir is a svelte project"
  (not= nil (-?> root_dir
                 (.. "/package.json")
                 (sanitize)
                 (a.slurp true)
                 (vim.json.decode)
                 (?. :devDependencies :svelte))))

;; Tracks whether or not user's been prompted to install typescript-svelte-plugin this session. used to
;; prevent asking more than once
(var asked-to-install-ts-plugin? false)
(defn with-svelte-plugin [config]
  "Augments tsserver config with typescript-svelte-plugin and handles its installation. Use this function to wrap your tsserver config"
  (add-tsserver-plugin config {:name :typescript-svelte-plugin
                               :location ts-plugin-location})
  (tset config
        :on_attach
        (util.add_hook_before config.on_attach
                              (fn [client bufnr]
                                (when (and (is-svelte-project? (?. client :config :root_dir))
                                           (not (ts-plugin-installed?))
                                           (not asked-to-install-ts-plugin?))
                                  (set asked-to-install-ts-plugin? true)
                                  (vim.ui.select [:yes :no]
                                                 {:prompt "Would you like to install typescript-svelte-plugin?"}
                                                 (fn [choice]
                                                   (when (= :yes choice)
                                                     (install-typescript-svelte-plugin #(vim.cmd "LspRestart tsserver")))))))))
  config)

(vim.api.nvim_create_user_command :SvelteToolsInstallTypeScriptPlugin
                                  #(install-typescript-svelte-plugin #(vim.cmd "LspRestart tsserver"))
                                  {})
