;;; $DOOMDIR/lisp/lib.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)
(require 'json)

;;; Project/Workspace Utilities
(cl-defun schmo/package-json-has-dependency-p (workspace-root dep &key (in 'dependencies))
  (if-let ((package-json (f-join workspace-root "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json)))
      (cl-some (lambda (key)
                 (thread-last config
                              (alist-get key)
                              (alist-get dep)))
               (ensure-list in))
    nil))

(defun schmo/vue-project-p (workspace-root)
  "Check if the 'vue' package is present in the package.json file
in the WORKSPACE-ROOT. Checks dependencies and devDependencies."
  (schmo/package-json-has-dependency-p workspace-root 'vue :in '(dependencies devDependencies)))

(defun schmo/project-has-styled-components-p (workspace-root)
  "Check if the `styled-components' package is present in the package.json file
in the WORKSPACE-ROOT."
  (schmo/package-json-has-dependency-p workspace-root 'styled-components :in '(dependencies devDependencies)))

(defun schmo/svelte-project-p (workspace-root)
  "Check if the `svelte' package is present in the package.json file in the
WORKSPACE-ROOT."
  (schmo/package-json-has-dependency-p workspace-root 'svelte :in 'devDependencies))

;;; General Configuration Utilities

;; Original font notes from DOOM's config.el template:
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(defmacro doom-set-font! (font-var &rest specs)
  "Sets FONT-VAR to the first available font defined in SPECS."
  (let* ((specs (mapcar (lambda (spec-form)
                          (if (and (listp spec-form)
                                   (not (eql 'font-spec (car spec-form))))
                              (cons #'font-spec spec-form)
                            spec-form))
                        specs))
         (spec (cl-find-if (lambda (spec-form)
                             (doom-font-exists-p (eval spec-form)))
                           specs)))
    `(setq ,font-var ,spec)))

(defmacro doom-font! (&rest specs)
  "Sets `doom-font' to the first available font in SPECS."
  `(doom-set-font! doom-font ,@specs))

(defmacro doom-variable-pitch-font! (&rest specs)
  "Sets `doom-variable-pitch-font' to the first available font in SPECS."
  `(doom-set-font! doom-variable-pitch-font ,@specs))


;;; Misc Utilities
(defun schmo/print-float-with-max-places (num max-places)
  "Truncates decimal places of `num' to `max-places' without trailing 0s"
  (number-to-string
   (string-to-number
    (format (concat "%0." (number-to-string max-places) "f")
            num))))

(defmacro schmo/debounce (seconds lambda-list &rest body)
  (declare (indent defun))
  (let ((timer (gensym)))
    `(let (,timer)
       (lambda ,lambda-list
         (when ,timer
           (cancel-timer ,timer)
           (setq ,timer nil))
         (setq ,timer (run-with-idle-timer ,seconds nil (lambda ()
                                                          ,@body)))))))

;; Keeping this around for now. The live minibuffer pattern here might be useful some day
;; (defun schmo/deadgrep-incremental ()
;;   "Alternative to `deadgrep-incremental' that uses the minibuffer instead of a
;; read-char loop"
;;   (interactive)
;;   (when (not (derived-mode-p 'deadgrep-mode))
;;     (error "Not a deadgrep buffer"))
;;   (let ((deadgrep-buffer (current-buffer)))
;;     (progn
;;       (cl-letf (((symbol-function 'update-deadgrep-search)
;;                  (schmo/debounce 0.25 (search-term)
;;                    (message search-term)
;;                    (with-current-buffer deadgrep-buffer
;;                      (setq deadgrep--search-term search-term)
;;                      (deadgrep-restart)))))
;;         (cl-labels ((handle-mb-change (&rest)
;;                                       (let ((search-term (minibuffer-contents)))
;;                                         (with-current-buffer deadgrep-buffer
;;                                           (dlet ((deadgrep--incremental-active t))
;;                                             (update-deadgrep-search search-term)))))
;;                     (mb-setup ()
;;                               (remove-hook 'minibuffer-setup-hook #'mb-setup)
;;                               (add-hook 'after-change-functions #'handle-mb-change nil t)))
;;           (add-hook 'minibuffer-setup-hook #'mb-setup)
;;           (let ((last-search-term deadgrep--search-term))
;;             (condition-case _
;;                 (update-deadgrep-search (read-from-minibuffer "Incremental Search: " last-search-term))
;;               (quit
;;                (update-deadgrep-search last-search-term)))))))))

(provide 'schmo-lib)
