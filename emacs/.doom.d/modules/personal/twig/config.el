;;; personal/twig/config.el -*- lexical-binding: t; -*-

(require 'f)

(def-project-mode! twig-minor-mode
  :modes '(web-mode)
  :match "\\.twig$")

(defvar +twig/snippets-dir (f-join (dir!) "snippets"))

(defun +twig/snippets-initialize ()
  (add-to-list 'yas-snippet-dirs +twig/snippets-dir)
  (yas-load-directory +twig/snippets-dir t))

(after! yasnippet
  (+twig/snippets-initialize))

(set-yas-minor-mode! 'twig-minor-mode)

(defun +twig/sp-point-before-closing-bracket-p (_id action context)
  (sp-point-before-same-p "}" action context))

(add-hook! 'twig-minor-mode-hook
  (dolist (twig-pair '(("#" . "#") ("%" . "%")))
    (let ((open (car twig-pair))
          (close (cdr twig-pair)))
      (sp-with-modes 'twig-pairs
        (sp-local-pair open close
                       :when '(+twig/sp-point-before-closing-bracket-p))
        ;; copied from modules/config/default/config.el
        ;; handles turning {%|%} into {% | %} when pressing space
        (sp-local-pair open nil
                       :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
                       ;; Don't autopair opening braces if before a word character or
                       ;; other opening brace. The rationale: it interferes with manual
                       ;; balancing of braces, and is odd form to have s-exps with no
                       ;; whitespace in between, e.g. ()()(). Insert whitespace if
                       ;; genuinely want to start a new form in the middle of a word.
                       :unless '(sp-point-before-word-p sp-point-before-same-p)))))
  (sp-update-local-pairs 'twig-pairs))
