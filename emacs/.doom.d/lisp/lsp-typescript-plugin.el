;;; lsp-typescript-plugin.el --- Automatic TypeScript plugin management for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jacob Schmocker
;;
;; Author: Jacob Schmocker
;; Maintainer: Jacob Schmocker
;; Created: Nov 19, 2022
;; Version: 0.1.0
;; Keywords: tools convenience
;; Homepage: https://github.com/theschmocker/dotfiles/blob/main/emacs/.doom.d/lisp/lsp-typescript-plugin.el
;; Package-Requires: ((emacs "28.2") (lsp-mode "8.0.1") (dash "2.19.1"))

;;; Commentary:
;;
;; Improves on lsp-mode's built-in support for TypeScript plugins
;; (`lsp-clients-typescript-plugins') with automatic installation—either as a
;; dependency of a particular language server or with a `completing-read'
;; prompt—and conditional activation depending on workspace information.
;;
;;; Code:

(require 'dash)
(require 'lsp-mode)

(defvar lsp-typescript-plugin--registered-plugins (make-hash-table :test #'equal))

(cl-defstruct lsp-typescript-plugin--plugin
  name
  dependency
  activation-fn
  download-plugin-fn
  download-in-progress?
  dependency-of)

(defun lsp-typescript-plugin-install (update? &optional plugin-name)
  "Interactively install or re-install typescript plugin.

If PLUGIN-NAME is non-nil, install that if it's registered. Otherwise, offer a
list of plugins to install.

When prefix UPDATE? is t force installation even if the plugin is present."
  (interactive "P")
  (lsp--require-packages)
  (let* ((chosen-plugin (or (gethash plugin-name lsp-typescript-plugin--registered-plugins)
                            (lsp--completing-read
                             "Select plugin to install/re-install: "
                             (or (->> lsp-typescript-plugin--registered-plugins
                                      (hash-table-values)
                                      (-filter (-andfn
                                                (-not #'lsp-typescript-plugin--plugin-download-in-progress?)
                                                #'lsp-typescript-plugin--plugin-download-plugin-fn)))
                                 (user-error "There are no plugins with automatic installation"))
                             (lambda (plugin)
                               (let ((plugin-name (-> plugin lsp-typescript-plugin--plugin-name symbol-name)))
                                 (if (lsp-typescript-plugin--plugin-present-p plugin)
                                     (concat plugin-name " (Already installed)")
                                   plugin-name)))
                             nil
                             t)))
         (update? (or update?
                      (and (not (lsp-typescript-plugin--plugin-download-in-progress? chosen-plugin))
                           (lsp-typescript-plugin--plugin-present-p chosen-plugin)))))
    (lsp-typescript-plugin--install-internal chosen-plugin update?)))

(defun lsp-typescript-plugin--install-internal (plugin &optional update?)
  "This is the internal function that handles async installation.
installing PLUGIN. UPDATE? is passed through to the
`lsp-typescript-plugin--plugin''s download-plugin-fn"
  (unless (lsp-typescript-plugin--plugin-download-plugin-fn plugin)
    (user-error "There is no automatic installation for `%s', you have to install it manually following lsp-mode's documentation"
                (lsp-typescript-plugin--plugin-name plugin)))
  (setf (lsp-typescript-plugin--plugin-download-in-progress? plugin) t)
  ;; (add-to-list 'global-mode-string '(t (:eval (lsp--download-status))))
  (cl-flet ((done
             (success? &optional error-message)
             ;; run with idle timer to make sure the lsp command is executed in
             ;; the main thread, see #2739.
             (run-with-timer
              0.0
              nil
              (lambda ()
                (let ((tsls-workspaces (-filter (lambda (workspace)
                                                  (and (equal 'ts-ls (lsp--workspace-server-id workspace))
                                                       (with-lsp-workspace workspace
                                                         (funcall (lsp-typescript-plugin--plugin-activation-fn plugin)
                                                                  (lsp--workspace-root workspace)))))
                                                (lsp--session-workspaces (lsp-session))))
                      (plugin-name (lsp-typescript-plugin--plugin-name plugin)))
                  (setf (lsp-typescript-plugin--plugin-download-in-progress? plugin) nil)
                  (if success?
                      (lsp--info "Plugin %s downloaded, restarting %s workspaces" plugin-name
                                 (length tsls-workspaces))
                    (lsp--error "Plugin %s install process failed with the following error message: %s.
Check `*lsp-install*' and `*lsp-log*' buffer"
                                plugin-name
                                error-message))
                  (seq-do
                   (lambda (workspace)
                     (when success? (lsp-workspace-restart workspace))
                     ;; (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                     ;;            global-mode-string)
                     )
                   tsls-workspaces)
                  ;; (unless (some #'lsp-typescript-plugin--plugin-download-in-progress? (hash-table-values lsp-typescript-plugin--registered-plugins))
                  ;;   (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                  ;;              global-mode-string))
                  )))))
    (lsp--info "Download %s started." (lsp-typescript-plugin--plugin-name plugin))
    (condition-case err
        (funcall
         (lsp-typescript-plugin--plugin-download-plugin-fn plugin)
         plugin
         (lambda () (done t))
         (lambda (msg) (done nil msg))
         update?)
      (error
       (done nil (error-message-string err))))))

(cl-defun lsp-typescript-plugin-register (&key name activation-fn lsp-dependency-recipe dependency-of)
  "Registers a typescript plugin with NAME for auto-installation/activation.

NAME is a symbol which acts as this plugin's id

ACTIVATION-FN is a function that receives the workspace root directory and
returns t if the plugin should be activated in the workspace, or nil otherwise.
Defaults to a function that always returns t

LSP-DEPENDENCY-RECIPE is a the `lsp-dependency' recipe for installing the
plugin.

DEPENDENCY-OF is a symbol or a list of symbols of other lsp dependency packages.
If set, then this plugin will be installed at the same time."
  (lsp-dependency name lsp-dependency-recipe)
  (puthash name
           (make-lsp-typescript-plugin--plugin
            :name name
            :dependency lsp-dependency-recipe
            :activation-fn (or activation-fn (-const t))
            :download-plugin-fn (lambda (_plugin callback error-callback _update?)
                                  (lsp-package-ensure name callback error-callback))
            :dependency-of dependency-of)
           lsp-typescript-plugin--registered-plugins))

(defun lsp-typescript-plugin--workspace-plugins (&optional workspace-root)
  "Return a list of typescript plugins that should be activated for the workspace.
Optionally, provide a WORKSPACE-ROOT directory. Otherwise, the root
is calculated based on the current buffer."
  (->> (hash-table-values lsp-typescript-plugin--registered-plugins)
       (-filter (lambda (plugin)
                  (funcall (lsp-typescript-plugin--plugin-activation-fn plugin)
                           (or workspace-root
                               (lsp--calculate-root (lsp-session) (buffer-file-name))))))))

(defun lsp-typescript-plugin--get-name (plugin)
  "Return the the package name of PLUGIN."
  (plist-get (cdr (lsp-typescript-plugin--plugin-dependency plugin))
             :package))

(defun lsp-typescript-plugin--get-location (plugin)
  "Return the path to PLUGIN's installation directory."
  (let ((name (lsp-typescript-plugin--get-name plugin))
        (path (plist-get
               (cdr (lsp-typescript-plugin--plugin-dependency plugin))
               :path)))
    (file-name-concat lsp-server-install-dir
                      "npm"
                      path
                      "lib"
                      "node_modules"
                      name)))

(defun lsp-typescript-plugin--plugin-present-p (plugin)
  "Return t when an installation of PLUGIN is detected, nil otherwise."
  (file-exists-p (lsp-typescript-plugin--get-location plugin)))

(defun lsp-typescript-plugin--notify-plugins-available-for-installation ()
  (when (eq 'ts-ls (lsp--workspace-server-id lsp--cur-workspace))
    (when-let ((available-plugins (-filter (-compose #'not #'lsp-typescript-plugin--plugin-present-p)
                                           (lsp-typescript-plugin--workspace-plugins (lsp--workspace-root lsp--cur-workspace)))))
      (lsp--info "tsserver plugin(s) available for this workspace: %s. Install using M-x lsp-install-typescript-server"
                 (mapconcat (-compose #'symbol-name #'lsp-typescript-plugin--plugin-name)
                            available-plugins
                            ", ")))))

(defun lsp-typescript-plugin--add-to-client (original-start-workspace session client root original-init-options)
  "Adds relevant TypeScript plugins to the initilization options
(ORIGINAL-INIT-OPTIONS) of the client that will be started in ROOT. Intended as
advice around `lsp--start-workspace'."
  (let ((init-options (if (not (eq 'ts-ls (lsp--client-server-id client)))
                          original-init-options
                        (let ((plugins (or (plist-get original-init-options :plugins) (vector))))
                          (plist-put original-init-options :plugins
                                     (vconcat plugins (->> (lsp-typescript-plugin--workspace-plugins root)
                                                           (-map (lambda (plugin)
                                                                   (list :name (lsp-typescript-plugin--get-name plugin)
                                                                         :location (lsp-typescript-plugin--get-location plugin))))
                                                           (apply #'vector))))))))
    (funcall original-start-workspace session client root init-options)))

(advice-add 'lsp--start-workspace :around #'lsp-typescript-plugin--add-to-client)

(defun lsp-typescript-plugin--install-plugin-as-dependency (orig-package-ensure dependency callback error-callback)
  "Advice around `lsp-package-ensure' that will install typescript plugins whose dependency-of field names DEPENDENCY."
  (let* ((plugins (if lsp--cur-workspace
                      (lsp-typescript-plugin--workspace-plugins)
                    (hash-table-values lsp-typescript-plugin--registered-plugins)))
         (auto-install (-filter (lambda (plugin)
                                  (let ((dependents (ensure-list (lsp-typescript-plugin--plugin-dependency-of plugin))))
                                    (seq-contains-p dependents dependency)))
                                plugins)))
    (funcall orig-package-ensure
             dependency
             (lambda ()
               (lsp-typescript-plugin--chain-install auto-install callback error-callback))
             error-callback)))

(advice-add 'lsp-package-ensure :around #'lsp-typescript-plugin--install-plugin-as-dependency)

(defun lsp-typescript-plugin--chain-install (plugins callback error-callback &optional update?)
  "Install PLUGINS in sequence.

CALLBACK is function that will be called when all plugins have been installed

ERROR-CALLBACK is a function that receives a message string when a plugin
installation results in an error

UPDATE? is passed through to the `lsp-typescript-plugin--plugin''s
download-plugin-fn"
  (if (null plugins)
      (funcall callback)
    (let ((plugin (car plugins)))
      (funcall
       (lsp-typescript-plugin--plugin-download-plugin-fn plugin)
       plugin
       (lambda ()
         (lsp-typescript-plugin--chain-install (cdr plugins) callback error-callback))
       error-callback
       update?))))

(provide 'lsp-typescript-plugin)
;;; lsp-typescript-plugin.el ends here
