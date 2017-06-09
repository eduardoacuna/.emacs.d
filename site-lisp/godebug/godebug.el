(require 'go-mode)

(defvar go-debug-package-history nil)

(defvar *go-debug-packages* (go-packages-go-list))

(defun go-debug-scan-packages ()
  (setq *go-debug-packages* (go-packages-go-list)))

(defun go-debug--prompt-package ()
  (completing-read "Go package: " *go-debug-packages* nil nil nil 'go-debug-package-history))

(define-derived-mode go-debug-mode go-mode
  "go-debug-mode"
  "A variant of go mode for a debugging session."
  ;; (font-lock-add-keywords nil '(("\\(\\w+_test.go\\):[0-9]+" 1 font-lock-doc-face)
  ;;                               ("[0-9]" . font-lock-constant-face)))
  )

(member "scratch/hello/greet" (go-packages-go-list))

;;;###autoload
(defun go-debug-start ()
  (interactive)
  (go-debug--prompt-package))


;; (defun go-debug-start ()
;;   (interactive)
;;   (let ((buf (generate-new-buffer "*go debug*")))
;;     (switch-to-buffer buf)
;;     (message "switch!")
;;     (funcall 'go-debug-mode)))

(defun --parent-dir (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun go-debug--path-package-name (dir)
  (car (split-string (shell-command-to-string (concat "cd " dir " && go list -e -f '{{ .Name }}'")))))

(defun go-debug--directory-package (dir)
  (car (split-string (shell-command-to-string (concat "cd " dir " && go list -e")))))

(defun go-debug--find-main-package-directory (dir)
  (when dir
    (let ((package-name (go-debug--path-package-name dir)))
      (if (equal package-name "main")
          dir
        (go-debug--find-main-package-directory (--parent-dir dir))))))

(defun go-debug--find-main-package (dir)
  (go-debug--directory-package (go-debug--find-main-package-directory dir)))

(defun go-debug--prompt-project-packages ()
  (interactive)
  (completing-read "Go package: "
                   (split-string
                    (shell-command-to-string
                     (concat "cd "
                             (go-debug--find-main-package-directory (--parent-dir buffer-file-name))
                             " && go list ./...")))))

(defun go-debug--current-func-name+loc ()
  (let (current-func-name current-func-loc)
    (save-excursion
      (when (go-beginning-of-defun)
        (setq current-func-loc (format "%s:%d" buffer-file-name (line-number-at-pos)))
        (when (looking-at go-func-regexp)
          (let ((func-name (match-string-no-properties 1)))
            (setq current-func-name func-name)))))
    (if current-func-loc
        (list current-func-name current-func-loc)
      (message "Not in a function"))))

;;;###autoload
(defun go-debug-main ()
  (interactive)
  (let ((main-pkg (go-debug--find-main-package (--parent-dir buffer-file-name))))
    (if main-pkg
        (message (concat "debugging... " main-pkg))
      (message "fail"))))

;;;###autoload
(defun go-debug-current-func ()
  (interactive)
  (let ((name+loc (go-debug--current-func-name+loc))
        (main-pkg (go-debug--find-main-package (--parent-dir buffer-file-name))))
    (if (and (listp name+loc) main-pkg)
        (let ((func-name (car name+loc))
              (func-loc  (cadr name+loc)))
          (message (concat "debugging... in " main-pkg " function " func-name " at " func-loc)))
      (message "fail"))))

;;;###autoload
(defun go-debug-current-test ()
  (interactive)
  (let ((name+loc (go-debug--current-func-name+loc))
        (main-pkg (go-debug--find-main-package (--parent-dir buffer-file-name))))
    (if (and (listp name+loc)
             main-pkg
             (string-match-p "_test\.go$" buffer-file-name)
             (string-match-p "^Test\\|^Example" (car name+loc)))
        (let ((test-name (car name+loc))
              (test-loc  (cadr name+loc)))
          (message (concat "debugging... in " main-pkg " test " test-name " at " test-loc)))
      (message "fail"))))

(provide 'godebug)
