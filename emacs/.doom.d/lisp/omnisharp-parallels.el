;;; ../Documents/dotfiles/emacs/.doom.d/lisp/omnisharp-parallels.el -*- lexical-binding: t; -*-

;; Some WIP code in here that attempts to start and connect to an OmniSharp
;; instance in Parallels while working with Blain Apps in a network share on the
;; macOS side.
;;
;; Working:
;; Modifying the :new-connection property of the OmniSharp client to
;; run "prlctl ..."
;;
;; Not working:
;; Basically everything else. OmniSharp starts (I think) but then exits
;; immediately with the error "The network path was not found." I've attempted
;; to encode various permutations of rootPath and rootUri, with no success so
;; far. Not sure that those are actually the issue, but those are the only
;; things like "network paths" I can see in the lsp logs. All that shows up is
;; the "initialize" request body that gets sent to OmniSharp. Would be helpful
;; if I could get my hands on OmniSharp's stdout, but the buffer that gets
;; created for the process is empty. Maybe the error is coming from something
;; other than OmniSharp 🤔.
;;
;; Alternatively, I could try running OmniSharp on the macOS side with Mono. I
;; /think/ Mono has some support for .NET Framework. Uncertain.

(defvar schmo/parallels-network-drive-prefix "/Volumes/[C] Windows 11.hidden/")

(defun schmo/replace-parallels-network-drive-prefix (path)
  (string-replace schmo/parallels-network-drive-prefix "C:/"
                  path))

(defun schmo/get-parallels-omnisharp-command ()
  (let* ((parallels-network-drive-prefix "/Volumes/[C] Windows 11.hidden/")
         (windows-project-root (string-replace parallels-network-drive-prefix
                                               "/"
                                               (projectile-project-root)))
         (escaped-sln-path (concat "\\\"C:" ; \"C:
                                   (string-replace "/"
                                                   "\\\\"
                                                   windows-project-root)
                                   "Blain Apps.sln"
                                   "\\\"")))
    ;; (list "sh" "-c" ">&2 prlctl list")
    (list
     "prlctl" "exec" "Windows 11" "--current-user" "cmd" "/S" "/C"
          (concat "\\\\Users\\\\schmo\\\\omnisharp-win-arm64\\\\OmniSharp.exe -s "
                  escaped-sln-path
                  " useModernNet:false"))
    ))

(defun schmo/modify-omnisharp-config-for-blains (original-init-options)
  (message "modifying omnisharp options")
  (let ((upd (map-delete
              (plist-put (cl-copy-list original-init-options)
                         :new-connection
                         ;; prlctl exec "Windows 11" --current-user cmd /S /C "\\Users\\schmo\\omnisharp-win-arm64\\OmniSharp.exe -s \"C:\\Projects\\Blain Apps\\Blain Apps.sln\" useModernNet:false"
                         ;;
                         ;;
                         (lsp-stdio-connection
                          #'schmo/start-omnisharp-in-parallels))
              :download-server-fn)))
    (print upd)))

(defun schmo/blain-apps-omnisharp-p (client)
  (and (eq 'omnisharp (lsp--client-server-id client))
       (s-contains-p "Blain Apps" (projectile-project-root))))

(defadvice! schmo/omnisharp-binary-present-advice (fn client)
  :around #'lsp--server-binary-present?
  (or (schmo/blain-apps-omnisharp-p client)
      (funcall fn client)))

(defun schmo/start-omnisharp-in-parallels (original-start-workspace session client root init-options)
  (let ((client (if (not (schmo/blain-apps-omnisharp-p client))
                    client
                  (let ((client-copy (copy-lsp--client client)))
                    (setf (lsp--client-new-connection client-copy)
                          (lsp-stdio-connection #'schmo/get-parallels-omnisharp-command))
                    (setf (lsp--client-download-server-fn client-copy)
                          nil)
                    (setf (lsp--client-download-in-progress? client-copy)
                          nil)
                    (setf (lsp--client-path->uri-fn client-copy)
                          (lambda (path)
                            (schmo/replace-parallels-network-drive-prefix path)
                            ;; (concat lsp--uri-file-prefix
                            ;;         (string-replace "%3A" ":"
                            ;;                         (url-hexify-string (schmo/replace-parallels-network-drive-prefix path)
                            ;;                                            lsp--url-path-allowed-chars)))
                            ))
                    client-copy))))
    (funcall original-start-workspace session client root init-options)))

(advice-add 'lsp--start-workspace :around #'schmo/start-omnisharp-in-parallels)

(defadvice! schmo/lsp-file-local-name (fn file)
  :around #'lsp-file-local-name
  (if (s-contains-p "Blain Apps" file)
      (thread-last file
                   (schmo/replace-parallels-network-drive-prefix)
                   (string-replace "/" "\\"))
    (funcall fn file)))
