;;; personal/eshell/config.el -*- lexical-binding: t; -*-

(defadvice! schmo/+eshell-default-prompt-fn (prompt)
  "Customize last character of doom's eshell prompt.

Sets the prompt regexp anytime its called. It was getting reset when set
outside. Not sure of the timings, so putting it in here works as a hack."
  :filter-return #'+eshell-default-prompt-fn
  (setq eshell-prompt-regexp "^.* » ")
  (string-replace "λ"
                  (propertize "»" 'face (get-text-property (- (length prompt) 2) 'face prompt))
                  prompt))

(map! :leader
      (:prefix "o"
               (:desc "Toggle eshell popup" "e" #'schmo/eshell-toggle)
               (:desc "Open eshell here" "E" #'schmo/eshell-here)))
