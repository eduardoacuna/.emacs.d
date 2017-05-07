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

;; global shortcuts
(defun mark-line (&optional arg allow-extend)
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero lines"))
  (cond ((and allow-extend
							(or (and (eq last-command this-command) (mark t))
									(and transient-mark-mode mark-active)))
				 (set-mark
					(save-excursion
						(goto-char (mark))
						(forward-line arg)
						(point))))
				(t
				 (forward-line arg)
				 (push-mark nil t t)
				 (forward-line (- arg)))))

(bind-key* "<C-return>" #'other-window)
(bind-key "M-W" #'mark-word)
(bind-key "M-P" #'mark-paragraph)
(bind-key "M-L" #'mark-line)

(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g l" #'goto-line)

;; find files with sudo and ssh
(defun find-file/sudo (file-name)
  (interactive "F(sudo) Find file: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(bind-key "C-x C-v" #'find-file/sudo)

(require 'compile)
(bind-key "C-c c" #'compile)

;; packages
(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
	(exec-path-from-shell-copy-env "GOPATH")
	(exec-path-from-shell-copy-env "GOROOT"))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y TAB" . yas-expand)
				 ("C-c y s"   . yas-insert-snippet)
				 ("C-c y n"   . yas-new-snippet)
				 ("C-c y v"   . yas-visit-snippet-file))
  :preface
  (defun yas-new-snippet (&optional choose-instead-of-guess)
    (interactive "P")
    (let ((guessed-directories (yas-guess-snippet-directories)))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (set (make-local-variable 'yas-guessed-modes)
           (mapcar #'(lambda (d) (intern (yas-table-name (car d))))
                   guessed-directories))
      (unless (and choose-instead-of-guess
                   (not (y-or-n-p "Insert a snippet with useful headers? ")))
        (yas-expand-snippet
         (concat "\n"
                 "# -*- mode: snippet -*-\n"
                 "# name: $1\n"
                 "# --\n"
                 "$0\n")))))

  :config
  (yas-load-directory "~/.emacs.d/snippets/")
  (yas-global-mode 1)
  (bind-key "C-i" #'yas-next-field-or-maybe-expand yas-keymap))

(use-package auto-yasnippet
  :ensure t
  :bind (("C-c y w" . aya-create)
				 ("C-c y y" . aya-expand)
				 ("C-c y o" . aya-open-line)))

(use-package company
  :ensure t
  :defer 2
  :diminish company-mode
  :commands company-mode
  :config
  (global-company-mode 1))


(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode))

(use-package page-break-lines
	:ensure t
	:config
	(global-page-break-lines-mode 1))

(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :config
  (setq dashboard-banner-logo-title "Welcome to Nearsoft Emacs")
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

(use-package ido
  :config
  (setq ido-use-filename-at-point 'guess)
  (setq ido-everywhere t)
  (setq ido-virtual-buffers t)
  (setq ido-file-extensions-order '(".org" ".go" ".html" ".js" ".css"))
  (setq ido-ignore-extensions t)
  (ido-mode 1))

(use-package flx-ido
  :ensure t
  :requires ido
  :config
  (flx-ido-mode))

(use-package ido-hacks
  :ensure t
  :requires ido
  :config
  (ido-hacks-mode))

(use-package ido-ubiquitous
  :ensure t
  :requires ido
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
				 ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(use-package go-eldoc
	:ensure t
	:config
	(add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
	:bind (:map go-mode-map
							("C-c C-g d" . godoc-at-point)
							("C-c C-g i" . go-import-add)
							("C-c C-g r" . gorepl-run)
							("C-c C-g x" . go-run)
							("C-c C-g p" . go-test-current-project)
							("C-c C-g t" . go-test-current-test))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda () (setq company-backends '(company-go))))
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode)
	(add-hook 'go-mode-hook (lambda ()
														(set (make-local-variable 'compile-command)
																 "go build"))))

(use-package company-go
  :ensure t
  :commands company-go)

(use-package gorepl-mode
	:ensure t
	:commands gorepl-run)

(use-package gotest
	:ensure t
	:commands (go-test-current-project go-test-current-test go-run))

(use-package solarized-theme
	:ensure t
	:config
	(load-theme 'solarized-light t))

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

