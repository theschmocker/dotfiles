;;; personal/haskell/config.el -*- lexical-binding: t; -*-

(map!
 :mode haskell-error-mode
 :n "q" #'quit-window)

(map!
 :mode haskell-mode
 (:localleader
  (:prefix ("r" . "repl")
           "r" #'haskell-interactive-switch
           "R" #'haskell-process-restart
           "c" #'haskell-interactive-mode-clear)))

(map!
 :mode haskell-interactive-mode
 :i "<up>" #'haskell-interactive-mode-history-previous
 :i "<down>" #'haskell-interactive-mode-history-next
 :n "<return>" #'haskell-interactive-mode-return
 :n "C-k" #'haskell-interactive-mode-prompt-previous
 :n "C-j" #'haskell-interactive-mode-prompt-next
 (:localleader
  (:prefix ("r" . "repl")
           "R" #'haskell-process-restart
           "c" #'haskell-interactive-mode-clear)))
