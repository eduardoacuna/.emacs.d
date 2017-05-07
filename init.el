;; start measuring emacs startup time
(defconst emacs-start-time (current-time))

;; raise error when using an old emacs version
(let ((min-version "25"))
  (when (version< emacs-version min-version)
    (error "Your Emacs version is too old -- this config requires v%s or higher"
					 min-version)))


;; INITIALIZATION AND CONFIGURATION

(eval-and-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (defvar use-package-verbose t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

;; separate the hand-written initialization code from the automatic code written by Customize
(let ((settings-path (expand-file-name "settings.el" user-emacs-directory)))
  (setq custom-file settings-path)
  (load settings-path))

;; disable unfortunate defaults
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
	(tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
	(scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
	(horizontal-scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t
      ring-bell-function 'ignore)



;; GLOBAL SHORTCUTS

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

(defun open-user-init-file ()
	(interactive)
	(find-file user-init-file))

(bind-key "C-c i" #'open-user-init-file)


;; PACKAGES


;;;;;;;;;;;;;;;;;;
;; Visual Style ;;
;;;;;;;;;;;;;;;;;;

(use-package solarized-theme
	:ensure t
	:config
	(load-theme 'solarized-light t))

(use-package page-break-lines
	:ensure t
	:config
	(global-page-break-lines-mode 1))

(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :config
  (setq dashboard-banner-logo-title "GNU Emacs")
  (setq dashboard-startup-banner
        (expand-file-name "imgs/nearsoft-symbol.png" user-emacs-directory))
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda   . 5)))
  (dashboard-setup-startup-hook))

(use-package spaceline-config
  :ensure spaceline)

(use-package all-the-icons
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-toggle-all-the-icons-git-ahead-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
	(exec-path-from-shell-copy-env "GOPATH")
	(exec-path-from-shell-copy-env "GOROOT"))

;;;;;;;;;;;;;;;;;;;;;;
;; Restarting Emacs ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package restart-emacs
	:ensure t
	:bind (("C-c r" . restart-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets/Skeletons/Templates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-expand yas-insert-snippet yas-new-snippet yas-visit-snippet-file yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
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
  :ensure t)

(bind-keys :prefix-map yasnippet-prefix
					 :prefix "C-c y"
					 ("TAB" . yas-expand)
					 ("s"   . yas-insert-snippet)
					 ("n"   . yas-new-snippet)
					 ("v"   . yas-visit-snippet-file)
					 ("w"   . aya-create)
					 ("y"   . aya-expand)
					 ("o"   . aya-open-line))

;;;;;;;;;;;;;
;; Editing ;;
;;;;;;;;;;;;;

(use-package mwim
  :ensure t
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-line-or-code))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-y") 'yank-and-indent)

(use-package multiple-cursors
  :ensure t)

(bind-keys :prefix-map cursor-prefix
					 :prefix "C-c C-c"
           ("e" . mc/edit-lines)
           ("n" . mc/mark-next-like-this)
           ("p" . mc/mark-previous-like-this)
           ("a" . mc/mark-all-like-this))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-Complete Engine ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :demand t
  :diminish company-mode
  :commands company-mode
  :config
  (global-company-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git & Source Version Control ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; Completion Engine ;;
;;;;;;;;;;;;;;;;;;;;;;;

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
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

;;;;;;;;;;;;;;;;;;;;;;
;; Smooth Scrolling ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

;;;;;;;;;;;;;;;;;;;;
;; Key Cheatsheet ;;
;;;;;;;;;;;;;;;;;;;;

(use-package which-key
	:ensure t
  :diminish which-key-mode
	:config
	(which-key-mode)
	(setq which-key-idle-delay 0.5))

;;;;;;;;;;;;;;;;;
;; Go Language ;;
;;;;;;;;;;;;;;;;;

(use-package go-eldoc
	:ensure t
	:config
	(add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
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


(bind-keys :prefix-map golang-prefix
					 :prefix "C-c C-g"
					 :map go-mode-map
					 ("d" . godoc-at-point)
					 ("i" . go-import-add)
					 ("r" . gorepl-run)
					 ("x" . go-run)
					 ("p" . go-test-current-project)
					 ("t" . go-test-current-test))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :commands projectile-global-mode
  :config
  (projectile-global-mode))

;;;;;;;;;;;;;;;;;;;
;; Lisp Language ;;
;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'ielm-mode-hook       'enable-paredit-mode))

;;;;;;;;;;;;;;
;; Web Mode ;;
;;;;;;;;;;;;;;

(use-package smartparens-config
  :ensure smartparens)

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.js\\'"   . web-mode)
         ("\\.css\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset    4
        web-mode-code-indent-offset   4)
  (add-hook 'web-mode-hook #'smartparens-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx"))))
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-enable-auto-pairing nil))))

(use-package restclient
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;; Terminal support ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package shell-pop
  :ensure t)

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-src-tab-acts-natively          t
        org-src-preserve-indentation       t
        org-fontify-whole-heading-line     t
        org-fontify-done-headline          t
        org-fontify-quote-and-verse-blocks t))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))


;; POST INITIALIZATION

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

