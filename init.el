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
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(show-paren-mode 1)
(global-auto-revert-mode t)
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
            (end-of-line)
						(point))))
				(t
				 (forward-line arg)
         (backward-char)
				 (push-mark nil t t)
				 (forward-line (+ (- arg) 1)))))

(bind-key* "<C-return>" #'other-window)
(bind-key* "<C-S-return>" #'(lambda ()
                              (interactive)
                              (other-window -1)))
(bind-key "M-W" #'mark-word)
(bind-key "M-P" #'mark-paragraph)
(bind-key "M-L" #'mark-line)

(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g l" #'goto-line)

(defun open-user-init-file ()
	(interactive)
	(find-file user-init-file))

(bind-key "C-c i" #'open-user-init-file)


;; PACKAGES

;;;;;;;;;;;;;;;;;;;;
;; Package update ;;
;;;;;;;;;;;;;;;;;;;;

(use-package auto-package-update
  :ensure t
  :bind ("C-c u" . auto-package-update-now))

;;;;;;;;;;;;;;;;;;
;; Visual Style ;;
;;;;;;;;;;;;;;;;;;

(use-package solarized-theme
	:ensure t
	:config
	;;(load-theme 'solarized-light t)
  )

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-one-brighter-modeline nil
        doom-one-brighter-comments nil)
  (load-theme 'doom-vibrant t)
  (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
  (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
  (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
  (doom-themes-nlinum-config))

(use-package nlinum
  :ensure t
  :config
  (doom-themes-nlinum-config))

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
        (expand-file-name "imgs/nearsoft-points-symbol.png" user-emacs-directory))
  (setq dashboard-items '((recents  . 15)
                          (projects . 10)))
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

;;;;;;;;;
;; RCP ;;
;;;;;;;;;

(use-package json-rpc
  :ensure t)

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
  :ensure t
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(bind-keys :prefix-map cursor-prefix
					 :prefix "C-c C-c"
           ("e" . mc/edit-lines)
           ("n" . mc/mark-next-like-this)
           ("p" . mc/mark-previous-like-this)
           ("a" . mc/mark-all-like-this))

(use-package drag-stuff
  :ensure t
  :bind (("<s-up>"    . drag-stuff-up)
         ("<s-down>"  . drag-stuff-down)
         ("<s-right>" . drag-stuff-right)
         ("<s-left>"  . drag-stuff-left))
  :config
  (drag-stuff-global-mode 1))

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

;;;;;;;;;;;;;;;;;;;;
;; Tagging system ;;
;;;;;;;;;;;;;;;;;;;;

(use-package helm-gtags
  :ensure t)

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

(use-package helm-config
  :ensure helm
  :demand t
  :bind (("C-c h" . helm-command-prefix)
         ("C-h a" . helm-apropos)
         ("C-x f" . helm-multi-files)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-M-s" . helm-occur)
         ("M-x" . helm-M-x))
  :config
  (global-unset-key (kbd "C-x c"))
  (use-package helm-commands)
  (use-package helm-files)
  (use-package helm-buffers)
  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))
  (helm-autoresize-mode 1)
  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  (setq helm-split-window-in-side-p t))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-company
  :ensure t
  :bind (:map
         company-mode-map
         ("C-:" . helm-company)
         :map
         company-active-map
         ("C-:" . helm-company)))

(use-package helm-themes
  :ensure t)

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

(use-package helm-c-yasnippet
  :ensure t
  :config
  (setq helm-yas-space-match-any-greedy t))

(bind-keys :prefix-map yasnippet-prefix
					 :prefix "C-c y"
					 ("TAB" . yas-expand)
					 ("s"   . yas-insert-snippet)
					 ("n"   . yas-new-snippet)
					 ("v"   . yas-visit-snippet-file)
					 ("w"   . aya-create)
					 ("e"   . aya-expand)
					 ("o"   . aya-open-line)
           ("y"   . helm-yas-complete))

;;;;;;;;;;;;;;;;;;;;;;
;; Smooth Scrolling ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

;;;;;;;;;;;;;;;;;;;;
;; Key Cheatsheet ;;
;;;;;;;;;;;;;;;;;;;;

(use-package which-key
	:ensure t
  :diminish which-key-mode
	:config
	(which-key-mode)
	(setq which-key-idle-delay 0.5))

(use-package indicators
  :ensure t)

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'nlinum-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(bind-keys :prefix-map programming-prefix
           :prefix "C-.")

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
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda () (setq company-backends '(company-go))))
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode)
  (add-hook 'go-mode-hook 'helm-gtags-mode))

(use-package company-go
  :ensure t
  :commands company-go)

(use-package gotest
	:ensure t
	:commands (go-test-current-file go-test-current-test go-test-current-project go-test-current-benchmark go-run))

;; (use-package godebug
;;   :load-path "site-lisp/godebug/"
;;   :commands (go-debug-start go-debug-current-file go-debug-current-test go-debug-current-project))

(use-package go-extra
  :load-path "site-lisp/go-extra/"
  :commands (go-extra-project-path
             go-extra-project-packages
             go-extra-all-packages
             go-extra-run-script
             go-extra-regenerate-tags))

;; (use-package go-dlv
;;   :ensure t)

(bind-keys :prefix-map golang-file-prefix
           :prefix "C-. f"
           :map go-mode-map
           ("p" . helm-projectile)
           ("f" . helm-browse-project))

(bind-keys :prefix-map golang-exec-prefix
           :prefix "C-. x"
           :map go-mode-map
           ("r" . go-run))

(bind-keys :prefix-map golang-test-prefix
           :prefix "C-. t"
           :map go-mode-map
           ("f" . go-test-current-file)
           ("t" . go-test-current-test)
           ("p" . go-test-current-project)
           ("b" . go-test-current-benchmark))

(bind-keys :prefix-map golang-doc-prefix
           :prefix "C-. d"
           :map go-mode-map
           ("p" . godoc-at-point)
           ("d" . godoc)
           ("?" . godef-describe))

(bind-keys :prefix-map golang-imports-prefix
           :prefix "C-. i"
           :map go-mode-map
           ("a" . go-import-add)
           ("r" . go-remove-unused-imports))

(bind-keys :prefix-map golang-goto-prefix
           :prefix "C-. g"
           :map go-mode-map
           ("i" . go-goto-imports)
           ("f" . go-goto-function)
           ("a" . go-goto-arguments)
           ("d" . go-goto-docstring)
           ("r" . go-goto-return-values)
           ("n" . go-goto-function-name)
           ("m" . go-goto-method-receiver)
           ("?" . go))

(bind-keys :prefix-map golang-jump-prefix
           :prefix "C-. j"
           :map go-mode-map
           ("j" . godef-jump)
           ("o" . godef-jump-other-window))

(bind-keys :prefix-map golang-debug-prefix
           :prefix "C-. b"
           :map go-mode-map
           ("d" . dlv))

(bind-keys :prefix-map golang-symbols-prefix
           :prefix "C-. s"
           :map go-mode-map
           ("d" . helm-gtags-find-tag)
           ("r" . helm-gtags-find-rtag)
           ("a" . helm-gtags-select)
           ("s" . helm-gtags-show-stack)
           ("g" . go-extra-regenerate-tags))

(bind-keys :map go-mode-map
           ("M-." . helm-gtags-find-tag-from-here)
           ("M-," . helm-gtags-pop-stack))

(defun dlv-current-test ()
  (interactive)
  (let (current-test-name current-func-loc)
    (save-excursion
      (when (go-beginning-of-defun)
        (setq current-func-loc (format "%s:%d" buffer-file-name (line-number-at-pos)))
        (when (looking-at go-func-regexp)
          (let ((func-name (match-string 1)))
            (when (and (string-match-p "_test\.go$" buffer-file-name)
                       (string-match-p "^Test\\|^Example" func-name))
              (message func-name)
              (setq current-test-name func-name))))))
    (if current-func-loc
        (let (gud-buffer-name dlv-command)
          (if current-test-name
              (progn
                (setq gud-buffer-name "*dlv-test*")
                (setq dlv-command (concat go-dlv-command-name " test")))
            (error "Not in a test"))
          (let ((gud-buffer (get-buffer gud-buffer-name)))
            (when gud-buffer (kill-buffer gud-bufer)))
          (dlv dlv-command)
          (gud-call (format "break %s" current-func-loc))
          (gud-call "continue"))
      (error "Not in a function"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp Language ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))

(setf slime-lisp-implementations
      `((sbcl ("ros" "-Q" "-l" "~/.sbclrc" "-L" "sbcl" "run"))
        (ccl  ("ros" "-Q" "-l" "~/.ccl-init.lisp" "-L" "ccl-bin" "run"))))

(setf slime-default-lisp 'sbcl)

(setq slime-net-coding-system 'utf-8-unix)

(defun custom-lisp-hook ()
  (interactive)
  (slime-mode)
  (local-set-key [tab] 'slime-complete-symbol)
  (local-set-key (kbd "M-q") 'slime-reindent-defun)
  (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
  (setq slime-load-failed-fasl 'never))

(add-hook 'lisp-mode-hook 'custom-lisp-hook)

;;;;;;;;;;;;;;;;;;;;;;;;
;; project Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
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
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset    2
        web-mode-code-indent-offset   2)
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
  (use-package ob-restclient :ensure t)
  (use-package htmlize :ensure t)
  (setq org-src-tab-acts-natively          t
        org-src-preserve-indentation       t
        org-fontify-whole-heading-line     t
        org-fontify-done-headline          t
        org-fontify-quote-and-verse-blocks t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))

;;;;;;;;;;;;;;;;;;;
;; Markdown Mode ;;
;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;
;; YML Mode ;;
;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))


;; POST INITIALIZATION

(when window-system
  (toggle-frame-maximized)
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

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
