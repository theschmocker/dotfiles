;;; lsp-ts-plugin-manager.el --- Automatic TypeScript plugin management for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jacob Schmocker
;;
;; Author: Jacob Schmocker
;; Maintainer: Jacob Schmocker
;; Created: Nov 19, 2022
;; Version: 0.1.0
;; Keywords: tools convenience
;; Homepage: https://github.com/theschmocker/dotfiles/blob/main/emacs/.doom.d/lisp/lsp-ts-plugin-manager.el
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

(defvar lsp-ts-plugin-manager--registered-plugins (make-hash-table :test #'equal))

(defgroup lsp-ts-plugin-manager nil
  "Settings for lsp-ts-plugin-manager."
  :group 'lsp-ts-plugin-manager-module)

(defcustom lsp-ts-plugin-manager-non-npm-install-path (file-name-concat lsp-server-install-dir "ts-plugin-manager")
  "Directory in which non-npm TypeScript plugins will be installed."
  :group 'lsp-ts-plugin-manager
  :type '(string))

(defclass lsp-ts-plugin-manager-plugin ()
  ((name :initarg :name
         :type symbol
         :documentation "Symbolic name of the plugin. Should be unique.")
   (package :initarg :package
            :type (or lsp-ts-plugin-manager-package string)
            :documentation "An instance of some subclass of
            `lsp-ts-plugin-manager-package' or a string naming an npm package.
            Determines how/where the plugin will be installed.")
   (activation-fn :initarg :activation-fn
                  :initform #'always
                  :documentation "A function of one argument that returns a
                  truthy value when a plugin should be activated/installed; nil
                  otherwise. Receives the workspace root path as an argument.")
   (download-in-progress? :initarg :download-in-progress?
                          :initform nil
                          :documentation "Whether or not the plugin is currently
                          being downloaded.")
   (dependency-of :initarg :dependency-of
                  :initform nil
                  :documentation "A symbol or list of symbols of packages
                  registered with `lsp-dependency'. The plugin will be installed
                  automatically when packages in this field are installed and
                  ACTIVATION-FN returns true in the current workspace."))
  :documentation "TypeScript plugin definition")

(cl-defmethod lsp-ts-plugin-manager-plugin-installed-p ((plugin lsp-ts-plugin-manager-plugin))
  "Return t when an installation of PLUGIN is detected, nil otherwise."
  (file-exists-p (lsp-ts-plugin-manager-package-install-location (oref plugin package))))

(cl-defmethod lsp-ts-plugin-manager-plugin--install ((plugin lsp-ts-plugin-manager-plugin) callback error-callback)
  "Insall PLUGIN, passing CALLBACK and ERROR-CALLBACK to `lsp-package-ensure'."
  (lsp-package-ensure (oref plugin name) callback error-callback))

(defclass lsp-ts-plugin-manager-package ()
  ((package-name :initarg :package-name
                 :type string
                 :documentation "String name of the package definition."))
  :documentation "Base class for TypeScript plugin package definitions, ie. recipes for how to install and/or where to find TS plugins."
  :abstract t)

(cl-defgeneric lsp-ts-plugin-manager-package-install-location (package)
  "Return the installation path for PACKAGE.")

(cl-defgeneric lsp-ts-plugin-manager-package--get-lsp-dependency (package)
  "Return the `lsp-dependency' dependency recipe for PACKAGE.")

(cl-defgeneric lsp-ts-plugin-manager-package-name (package)
  "Return the name of PACKAGE.")

(cl-defmethod lsp-ts-plugin-manager-package-name ((package lsp-ts-plugin-manager-package))
  "Return the name of PACKAGE."
  (oref package package-name))

(cl-defmethod lsp-ts-plugin-manager-package-name ((package string))
  "Return the name of PACKAGE."
  package)

(defclass lsp-ts-plugin-manager-npm-package (lsp-ts-plugin-manager-package)
  ((package-name :initarg :package-name
                 :documentation "Name of the package on npm."))
  :documentation "Recipe for a TypeScript plugin downloaded from npm.")

(cl-defmethod lsp-ts-plugin-manager-package-install-location ((package lsp-ts-plugin-manager-npm-package))
  "Return the install location of npm PACKAGE."
  (with-slots (package-name) package
    (file-name-concat lsp-server-install-dir
                      "npm"
                      package-name
                      "lib"
                      "node_modules"
                      package-name)))

(cl-defmethod lsp-ts-plugin-manager-package--get-lsp-dependency ((package lsp-ts-plugin-manager-npm-package))
  "Return the `lsp-dependency' dependency recipe for npm PACKAGE."
  (with-slots (package-name) package
    `(:npm
      :package ,package-name
      :path ,package-name)))

(cl-defmethod lsp-ts-plugin-manager-package-install-location ((package string))
  "Return the install location of npm PACKAGE."
  (lsp-ts-plugin-manager-package-install-location (lsp-ts-plugin-manager-npm-package :package-name package)))

(cl-defmethod lsp-ts-plugin-manager-package--get-lsp-dependency ((package string))
  "Return the `lsp-dependency' dependency recipe for npm PACKAGE."
  (lsp-ts-plugin-manager-package--get-lsp-dependency (lsp-ts-plugin-manager-npm-package :package-name package)))

(defclass lsp-ts-plugin-manager-downloaded-package (lsp-ts-plugin-manager-package)
  ((url :initarg :url
        :type string
        :documentation "URL from which to download the package.")
   (archive-type :initarg :archive-type
                 :type (member :gzip :zip :targz)
                 :documentation "Type of the downloaded archive. Tells lsp-mode how to handle the downloaded archive."))
  :documentation "Recipe for a TypeScript plugin download from an arbitrary URL.")

(cl-defmethod lsp-ts-plugin-manager-package-install-location ((package lsp-ts-plugin-manager-downloaded-package))
  "Return the install location of downloaded PACKAGE."
  (with-slots (package-name) package
    (file-name-concat lsp-ts-plugin-manager-non-npm-install-path
                      package-name
                      package-name)))

(cl-defmethod lsp-ts-plugin-manager-package--get-lsp-dependency ((package lsp-ts-plugin-manager-downloaded-package))
  "Return the `lsp-dependency' dependency recipe for downloaded PACKAGE."
  (with-slots (package-name url archive-type) package
    `(:download
      :url ,url
      :store-path ,(lsp-ts-plugin-manager-package-install-location package)
      :decompress ,archive-type)))

(defun lsp-ts-plugin-manager-install-plugin (&optional plugin-name)
  "Interactively install or re-install typescript plugin.

If PLUGIN-NAME is non-nil, install that if it's registered. Otherwise, offer a
list of plugins to install."
  (interactive "P")
  (lsp--require-packages)
  (let* ((chosen-plugin (or (gethash plugin-name lsp-ts-plugin-manager--registered-plugins)
                            (lsp--completing-read
                             "Select plugin to install/re-install: "
                             (or (->> lsp-ts-plugin-manager--registered-plugins
                                      (hash-table-values)
                                      (-filter (-andfn
                                                (lambda (plugin)
                                                  (not (oref plugin download-in-progress?)))
                                                (lambda (plugin)
                                                  (lsp-ts-plugin-manager-package--get-lsp-dependency (oref plugin package))))))
                                 (user-error "There are no plugins with automatic installation"))
                             (lambda (plugin)
                               (let ((plugin-name (-> plugin (oref name) symbol-name)))
                                 (if (lsp-ts-plugin-manager-plugin-installed-p plugin)
                                     (concat plugin-name " (Already installed)")
                                   plugin-name)))
                             nil
                             t))))
    (lsp-ts-plugin-manager--install-plugin-internal chosen-plugin)))

(defun lsp-ts-plugin-manager--install-plugin-internal (plugin)
  "This is the internal function that handles async installation of PLUGIN."
  (with-slots (package download-in-progress? activation-fn) plugin
    (unless (lsp-ts-plugin-manager-package--get-lsp-dependency package)
      (user-error "There is no automatic installation for `%s', you have to install it manually following lsp-mode's documentation"
                  (lsp-ts-plugin-manager-package-name package)))
    (setf download-in-progress? t)
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
                                                           (funcall activation-fn
                                                                    (lsp--workspace-root workspace)))))
                                                  (lsp--session-workspaces (lsp-session))))
                        (plugin-name (lsp-ts-plugin-manager-package-name package)))
                    (setf download-in-progress? nil)
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
                    ;; (unless (some #'lsp-ts-plugin-manager--plugin-download-in-progress? (hash-table-values lsp-ts-plugin-manager--registered-plugins))
                    ;;   (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                    ;;              global-mode-string))
                    )))))
      (lsp--info "Download %s started." (lsp-ts-plugin-manager-package-name package))
      (condition-case err
          (lsp-ts-plugin-manager-plugin--install plugin
                                                 (lambda () (done t))
                                                 (lambda (msg) (done nil msg)))
        (error
         (done nil (error-message-string err)))))))

(cl-defmethod lsp-ts-plugin-manager-register ((plugin lsp-ts-plugin-manager-plugin))
  "Registers PLUGIN for auto-installation/activation."
  (with-slots (name package) plugin
    (lsp-dependency name (lsp-ts-plugin-manager-package--get-lsp-dependency package))
    (puthash name
             plugin
             lsp-ts-plugin-manager--registered-plugins)))

(defun lsp-ts-plugin-manager--workspace-plugins (&optional workspace-root)
  "Return a list of typescript plugins that should be activated for the workspace.
Optionally, provide a WORKSPACE-ROOT directory. Otherwise, the root
is calculated based on the current buffer."
  (->> (hash-table-values lsp-ts-plugin-manager--registered-plugins)
       (-filter (lambda (plugin)
                  (funcall (oref plugin activation-fn)
                           (or workspace-root
                               (lsp--calculate-root (lsp-session) (buffer-file-name))))))))

;; (defun lsp-ts-plugin-manager--notify-plugins-available-for-installation ()
;;   (when (eq 'ts-ls (lsp--workspace-server-id lsp--cur-workspace))
;;     (when-let ((available-plugins (-filter (-compose #'not #'lsp-ts-plugin-manager-plugin-installed-p)
;;                                            (lsp-ts-plugin-manager--workspace-plugins (lsp--workspace-root lsp--cur-workspace)))))
;;       (lsp--info "tsserver plugin(s) available for this workspace: %s. Install using M-x lsp-install-typescript-server"
;;                  (mapconcat (-compose #'symbol-name #'lsp-ts-plugin-manager--plugin-name)
;;                             available-plugins
;;                             ", ")))))

(defun lsp-ts-plugin-manager--add-to-client (original-start-workspace session client root original-init-options)
  "Adds relevant TypeScript plugins to the initilization options
(ORIGINAL-INIT-OPTIONS) of the client that will be started in ROOT. Intended as
advice around `lsp--start-workspace'."
  (let ((init-options (if (not (eq 'ts-ls (lsp--client-server-id client)))
                          original-init-options
                        (let ((plugins (or (plist-get original-init-options :plugins) (vector))))
                          (plist-put original-init-options :plugins
                                     (vconcat plugins (->> (lsp-ts-plugin-manager--workspace-plugins root)
                                                           (-map (lambda (plugin)
                                                                   (with-slots (package) plugin
                                                                     (list :name (lsp-ts-plugin-manager-package-name package)
                                                                           :location (lsp-ts-plugin-manager-package-install-location package)))))
                                                           (apply #'vector))))))))
    (funcall original-start-workspace session client root init-options)))

(advice-add 'lsp--start-workspace :around #'lsp-ts-plugin-manager--add-to-client)

(defun lsp-ts-plugin-manager--install-plugin-as-dependency (orig-package-ensure dependency callback error-callback)
  "Advice around `lsp-package-ensure' that installs typescript plugins whose
dependency-of field names DEPENDENCY."
  (let* ((plugins (if lsp--cur-workspace
                      (lsp-ts-plugin-manager--workspace-plugins)
                    (hash-table-values lsp-ts-plugin-manager--registered-plugins)))
         (auto-install (-filter (lambda (plugin)
                                  (let ((dependents (ensure-list (oref plugin dependency-of))))
                                    (seq-contains-p dependents dependency)))
                                plugins)))
    (funcall orig-package-ensure
             dependency
             (lambda ()
               (lsp-ts-plugin-manager--chain-install auto-install callback error-callback))
             error-callback)))

(advice-add 'lsp-package-ensure :around #'lsp-ts-plugin-manager--install-plugin-as-dependency)

(defun lsp-ts-plugin-manager--chain-install (plugins callback error-callback)
  "Install PLUGINS in sequence.

CALLBACK is function that will be called when all plugins have been installed

ERROR-CALLBACK is a function that receives a message string when a plugin
installation results in an error"
  (if (null plugins)
      (funcall callback)
    (let ((plugin (car plugins)))
      (lsp-ts-plugin-manager-plugin--install plugin
                                             (lambda ()
                                               (lsp-ts-plugin-manager--chain-install (cdr plugins) callback error-callback))
                                             error-callback))))

(provide 'lsp-ts-plugin-manager)
;;; lsp-ts-plugin-manager.el ends here
