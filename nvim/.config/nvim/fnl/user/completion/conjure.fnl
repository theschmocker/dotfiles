(module user.completion.conjure
  {autoload {: cmp
             conjure-eval conjure.eval
             view aniseed.fennel.view}})
(local source {})

(fn source.new []
  (let [self (setmetatable {} {:__index source})]
    self))

(fn source.is_available [self]
  (not= nil (. (require :conjure.client) :current)))

(fn source.get_keyword_pattern [self]
  "\\%([0-9a-zA-Z\\*\\+!\\-_'?<>=\\/.:]*\\)")

(fn source.get_trigger_characters [self]
  ["/" "." ":"])

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

(fn source.complete [self request callback]
  (let [input (string.sub request.context.cursor_before_line request.offset)]
    (conjure-eval.completions input
                              (fn [results]
                                (callback (icollect [_ completion (ipairs results)]
                                            {:label completion.word
                                             :documentation {:kind (or (?. kind-tbl
                                                                           request.context.filetype
                                                                           completion.kind)
                                                                       cmp.lsp.CompletionItemKind.Value)
                                                             :value completion.info}}))))))
(defn get-source []
  source)
