;; -*- lexical-binding: t; -*-

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(set-frame-font "Triplicate A Code 18" nil t)

(custom-set-faces
 `(markdown-code-face ((t :inherit default))))

(use-package org
  :defer t

  :hook
  (org-mode . visual-line-mode)

  :config
  (setq org-startup-indented t)
  (setq org-directory "~/notes")
  (setq org-default-notes-file (concat org-directory "/ideas.org"))
  (setq org-agenda-files '("~/notes"))
  (setq org-capture-templates
        '(("i" "Idea" item (file+headline "~/notes/ideas.org" "Ideas")
           "- [ ] %?\n"))))

(use-package org-roam
  :ensure t
  :config
  (make-directory "~/notes/roam" t)
  (setq org-roam-directory (file-truename "~/notes/roam"))
  (org-roam-db-autosync-mode))

(use-package magit
  :ensure t)

(use-package transient
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package eat
  :ensure t
  :defer t

  :config
  (setq eat-term-name "xterm"))

(use-package markdown-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package odin-ts-mode
  :vc (:url "https://github.com/sampie159/odin-ts-mode"
            :rev :newest)
  :ensure t
  :mode "\\.odin\\'")

(use-package php-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package tuareg
  :ensure t)

(use-package paredit
  :ensure t
  :defer t)

(use-package which-key
  :config
  (setq which-key-show-eary-on-c-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; this procedure was shamelessly stolen from internet
(defun mode-line-render (left right)
  "return a string of `window-width' length containing left and right aligned respectively."
  (let* ((available-width (- (window-width) (length left) 3)))
    (format (format " %%s %%%ds " available-width) left right)))

(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-to-list 'eshell-modules-list 'eshell-smart))

(use-package emacs
  :custom
  (frame-resize-pixelwise t)
  (enable-recursive-minibuffers t)
  (tab-always-indent 'complete)
  (cursor-type 'box)

  :config
  ;; fido
  (require 'icomplete)

  (defun fido-hook ()
    (setq-local completion-styles '(orderless basic)))

  (add-hook 'icomplete-minibuffer-setup-hook 'fido-hook)
  (fido-vertical-mode)

  ;; address mode
  (global-goto-address-mode 1)

  ;; fonts

  (set-face-attribute 'fixed-pitch nil :family "Triplicate A Code")

  ;; whitespace mode
  (setq whitespace-style '(face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))
  (global-whitespace-mode)

  ;; electric
  (electric-indent-mode -1)

  ;; minibuffer size
  (setq max-mini-window-height 0.35)

  ;; mode line
  (setq-default mode-line-format
                '((:eval
                   (mode-line-render
                    (format-mode-line
                     '(" "
                       mode-line-modified
                       " "
                       (:propertize (:eval (meow-indicator)) face italic)
                       (:propertize " %b " face bold)
                       " (%l, %c) "
                       " %@ "))
                    (format-mode-line
                     '(" %i "
                       " %p%% "
                       (vc-mode vc-mode)
                       " %m "))))))
  ;; scratch buffer
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)

  ;; dired
  (setq dired-dwim-target t)
  ;; misc
  (setq echo-keystrokes 0.001))

;; theming
(use-package ef-themes
  :ensure t
  :defer t

  :config
  ;; (set-face-underline 'ef-themes-underline-warning '(:style line :color "#c0b000"))
  ;; (set-face-underline 'ef-themes-underline-error '(:style line :color "#df2f2f"))
  ;; (set-face-underline 'ef-themes-underline-info '(:style line :color "#22b022"))
  )

(use-package tao-theme
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t
  :config
  (custom-set-faces (whitespace-tab ((t (:background "#3F3F3F" :foreground "#888888"))))))

(use-package doom-themes
  :ensure t
  :defer t

  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package monokai-theme
  :ensure t
  :defer t)

;; faces
;; (set-face-attribute 'font-lock-builtin-face nil :weight 'bold :slant 'normal)
;; (set-face-attribute 'font-lock-keyword-face nil :weight 'normal :slant 'italic)
;; (set-face-attribute 'font-lock-type-face nil :weight 'bold :slant 'normal)
;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default indent-line-function 'insert-tab)

(setq-default c-ts-mode-indent-offset 4)
(setq-default c-basic-offset 4)

(setq-default go-ts-mode-indent-offset 8)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq ring-bell-function 'ignore)

(global-hl-line-mode)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq inhibit-startup-screen t)

(column-number-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(defvar auto-mode-pairs
  '(("\\.rs\\'" . rust-ts-mode)
    ("\\.yml\\'" . yaml-ts-mode)
    ("\\(dockerfile\\|containerfile\\)" . dockerfile-ts-mode)
    ("\\(cmakelists\\.txt\\|\\.cmake\\)" . cmake-ts-mode)
    ("\\.go\\'" . go-mode)
    ("\\.mod\\'" . go-mod-ts-mode)
    ("\\.cjs\\'" . js-ts-mode)
    ("\\.mjs\\'" . js-ts-mode)
    ("\\.ts\\'" . typescript-ts-mode)
    ("\\.mts\\'" . typescript-ts-mode)
    ("\\.tsx\\'" . tsx-ts-mode)
    ("\\.odin\\'" . odin-mode)
    ("\\.yaml\\'" . yaml-ts-mode)))

(dolist (pair auto-mode-pairs)
  (add-to-list 'auto-mode-alist pair))

(setq major-mode-remap-alist
      '((javascript-mode . js-ts-mode)
        (js-mode . js-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (mhtml-mode . html-ts-mode)
        (html-mode . html-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)))

(add-hook 'yaml-ts-mode-hook (lambda ()
                               (setq-local tab-width 2)))

(add-hook 'typescript-ts-mode-hook (lambda ()
                                     (setq-local typescript-ts-mode-indent-offset 4)))

(defun atm/c++-ts-mode-indent-style ()
  `(
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
    ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))

(setq-default c-ts-mode-indent-style #'atm/c++-ts-mode-indent-style)

;; misc
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(defun atm/prog-mode-hook ()
  (interactive)
  (electric-indent-local-mode +1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook #'atm/prog-mode-hook)
(add-hook 'html-mode-hook #'atm/prog-mode-hook)

(autoload 'enable-paredit-mode "paredit")

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

(setq projectile-project-search-path
      '("~/personal"))

(setq-default treesit-font-lock-level 4)

(setq
 scroll-step 1
 scroll-conservatively 999)

(add-to-list 'load-path (concat user-emacs-directory "atm"))

(require 'keys)

(require 'didko-theme)
(load-theme 'didko t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'eset)
