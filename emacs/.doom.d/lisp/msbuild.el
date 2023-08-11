;;; msbuild.el --- Simple commands for building .NET Projects with MSBuild -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jacob Schmocker
;;
;; Author: Jacob Schmocker
;; Maintainer: Jacob Schmocker
;; Homepage: https://github.com/theschmocker
;; Created: Jul 14, 2023
;; Version: 0.1.0
;; Keywords: tools convenience csharp msbuild .net
;; Package-Requires: ((emacs "28") (compat "29"))

;;; Commentary:
;;
;; By default, guess where to run the MSBuild command by searching up the directory tree for a .sln file.
;; This can be overridden by setting the `msbuild-solution-file' variable.
;;
;; To select and build a project interactively, invoke the `msbuild-build-project' command.
;; To build the enter solution, invoke the `msbuild-build-all' command.
;;
;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'msbuild-project-type)
(require 'compat) ;; still have one environment running 28

(defvar msbuild-command "MSBuild.exe"
  "Command used to shell out to MSBuild.
Assumes Windows and defaults to MSBuild.exe")

(defvar-local msbuild-solution-file nil
  "Absolute path to the target solution's .sln file.")

(defvar msbuild-format-command-function #'msbuild-format-command-default
  "Produce the full MSBuild command.
Receives the command name as the first argument and a list of command line
arguments as the second")

(defconst msbuild--sln-project-regexp "^Project(\\\"{\\(.*?\\)}\\\".*= \\\"\\(.*?\\)\\\", \\\"\\(.*?\\)\\\""
  "Regexp used to extract project information from `msbuild-solution-file'.")

(defvar msbuild-buffer-name-function #'msbuild-buffer-name-function-default
  "Set this function to change how MSBuild compilation buffers are named.

Receives the target `msbuild-project' as an argument")

(defvar msbuild-display-buffer-function #'display-buffer-at-bottom
  "Set as `display-buffer-function' when running an msbuild compile command.")

(defvar msbuild-execute-shell-command-function #'msbuild--execute-shell-command-with-compile
  "Set this to determine how to handle the MSBuild command.

Called with the shell command string and the `msbuild-project' as arguments.")

(defun msbuild--execute-shell-command-with-compile (command project)
  "Execute msbuild COMMAND for PROJECT using `compile'.

Sets appropriate `default-directory' and some opinionated values for various
compilation buffer variables.

This is the default value of `msbuild-execute-shell-command-function'."
  (let ((compile-b-name (funcall msbuild-buffer-name-function project)))
    (let ((compile-p (or (not (buffer-live-p compile-b-name))
                         (kill-buffer-ask (get-buffer compile-b-name)))))
      (when compile-p
        (dlet ((default-directory (file-name-directory (msbuild-get-solution-file)))
               (compilation-buffer-name-function (lambda (&rest _) compile-b-name))
               (compilation-scroll-output t)
               (display-buffer-alist (cons (list compile-b-name (list msbuild-display-buffer-function)) display-buffer-alist)))
          (compile command t))
        (with-current-buffer compile-b-name
          (msbuild-compilation-minor-mode 1)
          ;; TODO: how can I a) make this call only if evil is installed and
          ;; active and b) get flycheck to be quiet?
          (evil-normal-state))))))

(defun msbuild--execute-shell-command-debug (command project)
  "Print COMMAND and PROJECT info.

Set this as `msbuild-execute-shell-command-function' to see what command will be
run without actually running it."
  (message "Command: %s
Project:
  Name: %s
  Type: %s
  Path: %s"
           command
           (msbuild-project-name project)
           (msbuild-project-type project)
           (msbuild-project-path project)))

(cl-defstruct msbuild-project
  "Basic .NET project data."
  name
  type
  path)

(defconst msbuild-project-all (make-msbuild-project :name "All" :type "Build Solution")
  "Dummy `msbuild-project' which encompasses all projects in the current solution.")

;;;###autoload
(defun msbuild-build-project (project)
  "Build PROJECT with MSBuild."
  (interactive (list (msbuild--project-completing-read)))
  (let ((command (funcall msbuild-format-command-function
                          msbuild-command
                          (list (msbuild--msbuild-project-name-for-command project)
                                "-verbosity:minimal"))))
    (funcall msbuild-execute-shell-command-function command project)))

;;;###autoload
(defun msbuild-build-all ()
  "Build all projects in the current solution with MSBuild."
  (interactive)
  (msbuild-build-project msbuild-project-all))

(defun msbuild--get-compilation-buffer-windows ()
  "Get a list of windows visiting msbuild compilation buffers."
  (let ((msbuild-compilation-buffers-having-windows
         (cl-remove-if-not (lambda (buf)
                             (and (buffer-local-value 'msbuild-compilation-minor-mode buf)
                                  (get-buffer-window buf)))
                           (buffer-list))))
    (mapcar #'get-buffer-window msbuild-compilation-buffers-having-windows)))

;;;###autoload
(defun msbuild-select-compilation-window ()
  "Select the window of the first msbuild compilation buffer in the buffer list."
  (interactive)
  (let ((comp-window (cl-find-if-not (lambda (window)
                                       (eq window (selected-window)))
                                     (msbuild--get-compilation-buffer-windows))))
    (if (not comp-window)
        (error "No %s windows contain msbuild buffers"
               (if (bound-and-true-p msbuild-compilation-minor-mode)
                   "other"
                 "active"))
      (select-window comp-window))))

;;;###autoload
(defun msbuild-close-compilation-windows ()
  "Close all windows visiting msbuild compilation buffers using `delete-window'."
  (interactive)
  (dolist (window (msbuild--get-compilation-buffer-windows))
    (delete-window window)))

(defun msbuild-format-command-default (executable command-args)
  "Create the full build command, combining EXECUTABLE iwth COMMAND-ARGS.
Any members of COMMAND-ARGS with spaces are wrapped in quotes."
  (format "%s %s"
          executable
          (string-join (mapcar #'shell-quote-argument command-args)
                       " ")))

(defun msbuild-buffer-name-function-default (project &optional target)
  "Create the name of the msbuild buffer targeting PROJECT.

TARGET isn't used at the moment, but may be in the future to support other
targets like rebuild, clean, etc."
  (format "*msbuild: %s %s*"
          (or target "Build")
          (msbuild-project-name project)))

(defun msbuild--list-projects (&optional sln-file)
  "Parse `msbuild-project's out of SLN-FILE.

Fall back to the return value of `msbuild-get-solution-file' if SLN file is not
provided.

To see how projects are matched, see `msbuild--sln-project-regexp'."
  (let ((sln (or sln-file msbuild-solution-file))
        projects)
    (with-temp-buffer
      (insert-file-contents sln)
      (goto-char (point-min))
      (while (ignore-errors (re-search-forward msbuild--sln-project-regexp))
        (let* ((guid (match-string 1))
               (name (match-string 2))
               (path (match-string 3))
               (type (msbuild-project-type-label guid)))
          (push (make-msbuild-project :name name
                                      :type type
                                      :path path)
                projects))))
    projects))

(defun msbuild--project-completing-read ()
  "Prompt the user to select an `msbuild-project' by name."
  (let* ((items (cl-remove-if (lambda (p)
                                (string= (msbuild-project-type p) "Solution Folder")) ;; Solution folder projects not currently support. Not sure how to get MSBuild to do this without manually specifying sub-projects
                              (msbuild--list-projects)))
         (annotation-function (msbuild--make-project-annotation-function items))
         (project-name (completing-read
                        "Project: "
                        (lambda (s pred action)
                          (cond
                           ((eq 'metadata action) `(metadata (annotation-function . ,annotation-function)))
                           (t (all-completions s (mapcar #'msbuild-project-name items) pred)))))))
    (msbuild--find-project-by-name items project-name)))

(defun msbuild--make-project-annotation-function (projects)
  "Return a function used to annotate `msbuild--project-completing-read' options.
PROJECTS is a list of `msbuild-project's"
  (lambda (project-name)
    (let* ((project (msbuild--find-project-by-name projects project-name))
           (type (format " %s" (msbuild-project-type project))))
      (propertize type 'face 'completions-annotations))))

(defun msbuild--find-project-by-name (projects name)
  "From the list of PROJECTS, return the `msbuild-project' with NAME."
  (cl-find-if (lambda (p)
                (string= name (msbuild-project-name p)))
              projects))

(defun msbuild--msbuild-project-name-for-command (project)
  "Format PROJECT's name for MSBuild's first argument."
  (if (string= "All" (msbuild-project-name project))
      ""
    (thread-last project
                 (msbuild-project-path)
                 ((lambda (path)
                    (or (file-name-directory path) path)))
                 (replace-regexp-in-string "\\(/\\|\\\\\\)$" ""))))

(defun msbuild-get-solution-file ()
  "Return `msbuild-solution-file' or try to find one higher in the directory tree."
  (or msbuild-solution-file
      (msbuild-find-closest-sln-file)))

(defun msbuild-find-closest-sln-file ()
  "Try to find a solution file higher in the directory tree."
  (let* ((sln-files-in-directory (lambda (directory)
                                   (and directory
                                        (directory-files directory nil "\\.sln\\'"))))
         (closest-sln-dir (locate-dominating-file default-directory sln-files-in-directory))
         (sln-files (funcall sln-files-in-directory closest-sln-dir)))
    (when (consp (cdr sln-files))
      (warn "Found multiple .sln files in the same directory: %s. Selecting the first."
            (string-join sln-files ", ")))
    (when closest-sln-dir
      (file-truename (concat closest-sln-dir
                             (car sln-files))))))

(defvar-keymap msbuild-window-prefix-map
  :doc "Bindings to interact with windows visiting msbuild compilation buffers."
  :prefix 'msbuild-window-prefix-map
  "w" '("Select compilation window" . msbuild-select-compilation-window)
  "k" '("Close compilation window" . msbuild-close-compilation-windows))

(defvar-keymap msbuild-prefix-map
  :doc "Bindings for various msbuild commands."
  :prefix 'msbuild-prefix-map
  "B" '("Build All" . msbuild-build-all)
  "b" '("Build Project" . msbuild-build-project)
  "w" '("window" . msbuild-window-prefix-map))

(defvar msbuild-project-mode-map-prefix-key
  "C-c m"
  "Prefix key for `msbuild-project-mode-map'.

Note that `msbuild-project-mode-map' will need to be re-evaluated if you change
this. Need to look into custom vars to see about making this more easily
configurable.")

(defvar-keymap msbuild-project-mode-map
  msbuild-project-mode-map-prefix-key '("msbuild" . msbuild-prefix-map))

(define-minor-mode msbuild-project-mode
  "Bind keys for various msbuild commands.

This can be automatically enabled for buffers in projects with an sln file using
`msbuild-auto-project-mode' or by using a dir local variable."
  :keymap msbuild-project-mode-map)

(defun msbuild--enable-msbuild-project-mode-in-project-buffers ()
  "Enable msbuild-project-mode if an sln file can be found from the current buffer."
  (when (msbuild-get-solution-file)
    (msbuild-project-mode 1)))

;;;###autoload
(define-minor-mode msbuild-auto-project-mode
  "Enable `msbuild-project-mode' automatically in .NET project buffers."
  :global t
  (if msbuild-auto-project-mode
      (add-hook 'after-change-major-mode-hook #'msbuild--enable-msbuild-project-mode-in-project-buffers)
    (remove-hook 'after-change-major-mode-hook #'msbuild--enable-msbuild-project-mode-in-project-buffers)))

(define-minor-mode msbuild-compilation-minor-mode
  "Simply denotes msbuild compilation buffers.")

(provide 'msbuild)

;;; msbuild.el ends here
