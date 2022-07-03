(module user.snippets {require {ls luasnip}})

((. (require :luasnip.loaders.from_vscode) :lazy_load) {:paths {1 :./snippets/vue}})

(local ls (require :luasnip))
(local s ls.snippet)
(local sn ls.snippet_node)
(local isn ls.indent_snippet_node)
(local t ls.text_node)
(local i ls.insert_node)
(local f ls.function_node)
(local c ls.choice_node)
(local d ls.dynamic_node)
(local r ls.restore_node)
(local events (require :luasnip.util.events))
(local ai (require :luasnip.nodes.absolute_indexer))
(local fmt (. (require :luasnip.extras.fmt) :fmt))
(local m (. (require :luasnip.extras) :m))
(local ___lambda-__ (. (require :luasnip.extras) :l))
(local postfix (. (require :luasnip.extras.postfix) :postfix))

(ls.add_snippets :all [(s :ternary
                          [(i 1 :cond)
                           (t " ? ")
                           (i 2 :then)
                           (t " : ")
                           (i 3 :else)])])

(ls.add_snippets :vue [(s :arb
                          [(t ":aria-")
                           (i 1 :attribute)
                           (t "=\"")
                           (i 2 :bool)
                           (t " ? 'true' : 'false'\"")])])

(vim.keymap.set [:i :s] :<c-j>
                (fn []
                  (when (ls.expand_or_jumpable)
                    (ls.expand_or_jump))) {:silent true})

(vim.keymap.set [:i :s] :<c-k>
                (fn []
                  (when (ls.jumpable (- 1))
                    (ls.jump (- 1)))) {:silent true})

(vim.keymap.set :i :<c-l> (fn []
                            (when (ls.choice_active)
                              (ls.change_choice 1))))

(vim.keymap.set :i :<c-u> (require :luasnip.extras.select_choice))

