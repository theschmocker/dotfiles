(module user.completion.conjure
  {autoload {: cmp
             conjure-eval conjure.eval}})

(local source {})
(set source.new (fn []
                  (let [self (setmetatable {} {:__index source})]
                    self)))

(fn source.is_available [self]
  (if (= ((. (require :conjure.client) :current)) nil) false true))

(fn source.get_keyword_pattern [self]
  "\\%([0-9a-zA-Z\\*\\+!\\-_'?<>=\\/.:]*\\)")

(fn source.get_trigger_characters [self]
  {1 "/" 2 "." 3 ":"})

(local kind-tbl
  {:clojure {:C cmp.lsp.CompletionItemKind.Class
             :F cmp.lsp.CompletionItemKind.Function
             :K cmp.lsp.CompletionItemKind.Keyword
             :M cmp.lsp.CompletionItemKind.Function
             :N cmp.lsp.CompletionItemKind.Module
             :S cmp.lsp.CompletionItemKind.Function
             :V cmp.lsp.CompletionItemKind.Variable}
   :fennel {:boolean cmp.lsp.CompletionItemKind.Value
            :function cmp.lsp.CompletionItemKind.Function
            :nil cmp.lsp.CompletionItemKind.Value
            :number cmp.lsp.CompletionItemKind.Value
            :string cmp.lsp.CompletionItemKind.Value
            :table cmp.lsp.CompletionItemKind.Struct}})

(fn lookup-kind [s ft]
  (let [ft-kinds (. kind-tbl ft)]
    (if ft-kinds (. ft-kinds s) nil)))

(fn source.complete [self request callback]
  (let [input (string.sub request.context.cursor_before_line request.offset)]
    (conjure-eval.completions input
                              (fn [results]
                                (let [items {}]
                                  (each [_ completion (ipairs results)]
                                    (table.insert items
                                                  {:label completion.word
                                                   :documentation {:kind cmp.lsp.MarkupKind.PlainText
                                                                   :value completion.info}
                                                   :kind (lookup-kind completion.kind
                                                                      request.context.filetype)}))
                                  (callback items))))))

(defn get-source []
  source)
