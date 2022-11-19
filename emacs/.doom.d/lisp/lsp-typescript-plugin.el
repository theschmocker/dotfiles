;;; $DOOMDIR/lisp/lsp-typescript-plugin.el -*- lexical-binding: t; -*-

(require 'ht)
(require 'lsp-mode)

(defvar lsp-typescript-plugin--registered-plugins (ht))

(cl-defstruct lsp-typescript-plugin--def
  name
  dependency
  activation-fn
  download-plugin-fn
  download-in-progress?)

(defun lsp-typescript-plugin-install (update? &optional plugin-name)
  "Interactively install or re-install typescript plugin.
When prefix UPDATE? is t force installation even if the plugin is present."
  (interactive "P")
  (lsp--require-packages)
  (let* ((chosen-plugin (or (gethash plugin-name lsp-typescript-plugin--registered-plugins)
                            (lsp--completing-read
                             "Select plugin to install/re-install: "
                             (or (->> lsp-typescript-plugin--registered-plugins
                                      (ht-values)
                                      (-filter (-andfn
                                                (-not #'lsp-typescript-plugin--def-download-in-progress?)
                                                #'lsp-typescript-plugin--def-download-plugin-fn)))
                                 (user-error "There are no plugins with automatic installation"))
                             (lambda (plugin)
                               (let ((plugin-name (-> plugin lsp-typescript-plugin--def-name symbol-name)))
                                 (if (lsp-typescript-plugin--plugin-present-p plugin) ;; TODO(lsp--server-binary-present? client)
                                     (concat plugin-name " (Already installed)")
                                   plugin-name)))
                             nil
                             t)))
         (update? (or update?
                      (and (not (lsp-typescript-plugin--def-download-in-progress? chosen-plugin))
                           (lsp-typescript-plugin--plugin-present-p chosen-plugin)))))
    (lsp-typescript-plugin--install-internal chosen-plugin update?)))

(defun lsp-typescript-plugin--install-internal (plugin &optional update?)
  (unless (lsp-typescript-plugin--def-download-plugin-fn plugin)
    (user-error "There is no automatic installation for `%s', you have to install it manually following lsp-mode's documentation."
                (lsp-typescript-plugin--def-name plugin)))
  (setf (lsp-typescript-plugin--def-download-in-progress? plugin) t)
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
                                                         (funcall (lsp-typescript-plugin--def-activation-fn plugin)
                                                                  (lsp--workspace-root workspace)))))
                                                (lsp--session-workspaces (lsp-session))))
                      (plugin-name (lsp-typescript-plugin--def-name plugin)))
                  (setf (lsp-typescript-plugin--def-download-in-progress? plugin) nil)
                  (if success?
                      (lsp--info "Plugin %s downloaded, restarting %s workspaces" plugin-name
                                 (length tsls-workspaces))
                    (lsp--error "Plugin %s install process failed with the following error message: %s.
Check `*lsp-install*' and `*lsp-log*' buffer."
                                plugin-name
                                error-message))
                  (seq-do
                   (lambda (workspace)
                     (with-lsp-workspace workspace
                       ;; (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                       ;;            global-mode-string)
                       (when success? (lsp-restart-workspace))))
                   tsls-workspaces)
                  ;; (unless (some #'lsp-typescript-plugin--def-download-in-progress? (ht-values lsp-typescript-plugin--registered-plugins))
                  ;;   (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                  ;;              global-mode-string))
                  )))))
    (lsp--info "Download %s started." (lsp-typescript-plugin--def-name plugin))
    (condition-case err
        (funcall
         (lsp-typescript-plugin--def-download-plugin-fn plugin)
         plugin
         (lambda () (done t))
         (lambda (msg) (done nil msg))
         update?)
      (error
       (done nil (error-message-string err))))))

(defun lsp-typescript-plugin-register (name activation-fn dep-definition)
  "Registers a typescript plugin with NAME (a symbol) for
auto-installation/activation.

ACTIVATION-FN is a function that receives the workspace root directory and
returns t if the plugin should be activated in the workspace, or nil otherwise.

DEP-DEFINITION is a the `lsp-dependency' recipe for installing the plugin."
  (lsp-dependency name dep-definition)
  (ht-set lsp-typescript-plugin--registered-plugins
          name
          (make-lsp-typescript-plugin--def
           :name name
           :dependency dep-definition
           :activation-fn activation-fn
           :download-plugin-fn (lambda (_plugin callback error-callback _update?)
                                 (lsp-package-ensure name callback error-callback)))))

(defun lsp-typescript-plugin--workspace-plugins (&optional workspace-root)
  "Returns a list of typescript plugins that should be activated for the
workspace. Optionally, provide a WORKSPACE-ROOT directory. Otherwise, the root
is calculated based on the current buffer."
  (->> (ht-values lsp-typescript-plugin--registered-plugins)
       (-filter (lambda (plugin)
                  (funcall (lsp-typescript-plugin--def-activation-fn plugin)
                           (or workspace-root
                               (lsp--calculate-root (lsp-session) (buffer-file-name))))))))

(defun lsp-typescript-plugin--get-name (plugin)
  (plist-get (cdr (lsp-typescript-plugin--def-dependency plugin))
             :package))

(defun lsp-typescript-plugin--get-location (plugin)
  (let ((name (lsp-typescript-plugin--get-name plugin))
        (path (plist-get
               (cdr (lsp-typescript-plugin--def-dependency plugin))
               :path)))
    (f-join lsp-server-install-dir
            "npm"
            path
            "lib"
            "node_modules"
            name)))

(defun lsp-typescript-plugin--plugin-present-p (plugin)
  (file-exists-p (lsp-typescript-plugin--get-location plugin)))

(defun lsp--notify-typescript-plugins-available-for-installation ()
  (when (eq 'ts-ls (lsp--workspace-server-id lsp--cur-workspace))
    (when-let ((available-plugins (-filter (-compose #'not #'lsp-typescript-plugin--plugin-present-p)
                                           (lsp-typescript-plugin--workspace-plugins (lsp--workspace-root lsp--cur-workspace)))))
      (lsp--info "tsserver plugin(s) available for this workspace: %s. Install using M-x lsp-install-typescript-server"
                 (mapconcat (-compose #'symbol-name #'lsp-typescript-plugin--def-name)
                            available-plugins
                            ", ")))))

;; defun lsp--start-workspace (session client-template root &optional initialization-options)
(defun lsp-typescript-plugin--add-to-client (original-start-workspace session client root original-init-options)
  ""
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

;; (with-current-buffer (get-buffer "pointing-session.ts")
;;   (let* ((client (ht-get lsp-clients 'ts-ls))
;;          (init-options (lsp--client-initialization-options client)))
;;     (funcall init-options)))


;; (add-hook 'lsp-workspace-folders-changed-functions #'lsp-typescript-plugin--add-to-client)

(advice-add 'lsp--start-workspace :around #'lsp-typescript-plugin--add-to-client)

(provide 'lsp-typescript-plugin)
;;; lsp-typescript-plugin.el ends here
