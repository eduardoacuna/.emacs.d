;; start measuring emacs startup time
(defconst emacs-start-time (current-time))

;; raise error when using an old emacs version
(let ((min-version "25"))
  (when (version< emacs-version min-version)
    (error "Your Emacs version is too old -- this config requires v%s or higher"
	   min-version)))

(eval-and-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (defvar use-package-verbose t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;; separate the hand-written initialization code from the automatic code written by Customize
(let ((settings-path (expand-file-name "settings.el" user-emacs-directory)))
  (setq custom-file settings-path)
  (load settings-path))

;; post initialization
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
					    emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed (float-time (time-subtract (current-time)
							 emacs-start-time))))
		 (message "Loading %s...done (%.3fs) [after-init]"
			  ,load-file-name elapsed)))
	    t))

