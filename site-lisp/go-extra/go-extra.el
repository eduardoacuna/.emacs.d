(require 'projectile)
(require 'go-mode)

(defun go-extra--parent-dir (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun go-extra--dir-command-to-string (dir command)
  (shell-command-to-string (concat "cd " dir " && " command)))

(defun go-extra--dir-package-name (dir)
  (car (split-string (go-extra--dir-command-to-string dir "go list -e -f '{{ .Name }}'"))))

(defun go-extra--find-package-directory (package dir)
  (when dir
    (let ((package-name (go-extra--dir-package-name dir)))
      (if (equal package-name package)
          dir
        (go-extra--find-package-directory package (go-extra--parent-dir dir))))))

(defun go-extra--project-packages-complete ()
  (interactive)
  (completing-read "Go project packages: "
                   (go-extra-project-packages)))

(defun go-extra--all-packages-complete ()
  (interactive)
  (completing-read "Go packages: "
                   (go-extra-all-packages)))

(defun go-extra--project-files ()
  (interactive)
  (let ((default-directory (go-extra-project-path)))
    (split-string (shell-command-to-string "find . -type f -iregex '.*\\.go'"))))

(defun go-extra--project-command-for-each (cmd)
  (interactive)
  (let ((default-directory (go-extra-project-path)))
    (dolist ((file (go-extra--project-files)))
      (shell-command (concat cmd " " file)))))

;;;###autoload
(defun go-extra-project-path ()
  (interactive)
  (go-extra--find-package-directory "main" (go-extra--parent-dir buffer-file-name)))

;;;###autoload
(defun go-extra-project-packages ()
  (interactive)
  (split-string (go-extra--dir-command-to-string (go-extra-project-path) "go list ./...")))

;;;###autoload
(defun go-extra-all-packages ()
  (interactive)
  (go-packages-go-list))

;;;###autoload
(defun go-extra-run-script ()
  (interactive)
  (let ((default-directory (go-extra-project-path)))
    (let ((file (completing-read "Script: " (directory-files (concat default-directory "scripts/")))))
      (with-output-to-temp-buffer "*go-script*"
        (shell-command (concat "./scripts/" file) "*go-script*" "*go-script*")
        (pop-to-buffer "*go-script*")))))

;;;###autoload
(defun go-extra-regenerate-tags ()
  (interactive)
  (let ((default-directory (go-extra-project-path)))
    (when default-directory
      (shell-command "gogtags" nil))))

(provide 'go-extra)
