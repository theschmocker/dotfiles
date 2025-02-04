; inherits: css

; seems to fix a bug in Vue style tags with lang="less". There's not currently a Less treesitter parser,
; so the injection is scss. When opening a newline after a declaration in some blocks, I'd get an extra indentation.
; seems to have been caused by Less language constructs that cause errors for the scss parser
(declaration
  ";" @indent.end)
