;;; $DOOMDIR/rg/config.el -*- lexical-binding: t; -*-

(defmacro without-eager-macroexpand (&rest body)
  "Prevents macros in BODY from being expanded eagerly and thus triggering load
of the package(s) in which they're defined.

See: https://www.reddit.com/r/emacs/comments/12l1n7c/comment/jg5on6w/"
  `(eval '(progn ,@body) t))

(use-package! rg
  :defer t
  :commands (schmo/rg-project schmo/rg-notes)
  :init
  (map! :leader
        :prefix "s"
        (:desc "Search project with rg.el" "g" 'schmo/rg-project))

  :config
  (without-eager-macroexpand
   (rg-define-search schmo/rg-notes
     "Search `org-directory' with context using rg.el"
     :files "*"
     :dir org-directory
     :flags ("--context=3"))

   (rg-define-search schmo/rg-project
     "Search project with rg.el"
     :files "*"
     :dir project
     :flags ("--hidden" "--glob=!.git/"))

   (rg-define-search schmo/rg-doom
     "Search DOOM files in .emacs.d"
     :files "*"
     :dir doom-emacs-dir)))
