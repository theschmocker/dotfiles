(module user.completion {autoload {: cmp
                                   conjure-source user.completion.conjure
                                   : luasnip}})

(cmp.register_source :conjure ((. (conjure-source.get-source) :new)))

(cmp.setup {:snippet {:expand (fn [args]
                                (luasnip.lsp_expand args.body))}
            :mapping (cmp.mapping.preset.insert {:<C-b> (cmp.mapping.scroll_docs -4)
                                                 :<C-f> (cmp.mapping.scroll_docs 4)
                                                 :<C-Space> (cmp.mapping.complete)
                                                 :<C-e> (cmp.mapping.abort)})
            :sources (cmp.config.sources [{:name :nvim_lsp}
                                          {:name :luasnip}
                                          ;; For luasnip users.
                                          {:name :conjure}]
                                         [{:name :buffer}])})

(cmp.setup.filetype :gitcommit
                    {:sources (cmp.config.sources [{:name :cmp_git}]
                                                  [{:name :buffer}])})

(cmp.setup.cmdline "/"
                   {:sources (cmp.config.sources [{:name :buffer}])
                    :mapping (cmp.mapping.preset.cmdline)})

(cmp.setup.cmdline ":"
                   {:mapping (cmp.mapping.preset.cmdline)
                    :sources (cmp.config.sources [{:name :path}]
                                                 [{:name :cmdline}])})
