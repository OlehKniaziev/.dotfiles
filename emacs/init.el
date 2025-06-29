(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(set-frame-font "Iosevka Cozy 17" nil t)

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

(use-package elixir-ts-mode
  :ensure t)

(use-package odin-mode
  :vc (:url "https://git.sr.ht/~mgmarlow/odin-mode"
            :rev :newest)
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package tuareg
  :ensure t)

(use-package paredit
  :ensure t
  :defer t)

;; This procedure was shamelessly stolen from internet
(defun mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT aligned respectively."
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

  :config
  ;; Fido
  (require 'icomplete)

  (defun fido-hook ()
    (setq-local completion-styles '(substring)))

  (add-hook 'icomplete-minibuffer-setup-hook 'fido-hook)
  (fido-vertical-mode)

  ;; Address mode
  (global-goto-address-mode 1)

  ;; Fonts
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Cozy")

  ;; Whitespace mode
  (setq whitespace-style '(face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))
  (global-whitespace-mode)

  ;; Electric
  (electric-indent-mode -1)

  ;; Mode line
  (setq-default mode-line-format
                '((:eval
                   (mode-line-render
                    (format-mode-line
                     '(" "
                       mode-line-modified
                       (:propertize "  %b " face bold)
                       " (%l, %c) "
                       " %@ "))
                    (format-mode-line
                     '(" %I "
                       " %p%% "
                       (vc-mode vc-mode)
                       " %m "))))))

  ;; Dired
  (setq dired-dwim-target t)
  ;; Misc
  (setq echo-keystrokes 0.001))

;; theming
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   't
   '(                                   ; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
                                        ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
                                        ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
                                        ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
                                        ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
                                        ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
                                        ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package ef-themes
  :ensure t
  :defer t

  :config
  (set-face-underline 'ef-themes-underline-warning '(:style line :color "#c0b000"))
  (set-face-underline 'ef-themes-underline-error '(:style line :color "#df2f2f"))
  (set-face-underline 'ef-themes-underline-info '(:style line :color "#22b022")))

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
    ("\\(Dockerfile\\|Containerfile\\)" . dockerfile-ts-mode)
    ("\\(CMakeLists\\.txt\\|\\.cmake\\)" . cmake-ts-mode)
    ("\\.go\\'" . go-ts-mode)
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

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'keys)
;; (require 'ef-themes)
;; (load-theme 'ef-kassio t)
;; (load-theme 'ef-tritanopia-dark t)
;; (load-theme 'ef-eagle t)
;; (load-theme 'ef-trio-dark t)
;; (load-theme 'ef-elea-dark t)
;; (load-theme 'monokai t)
;; (load-theme 'ef-autumn t)

(require 'didko-theme)
(require 'hmm-theme)
(load-theme 'didko t)
;; (load-theme 'hmm t)

;; (require 'doom-themes)
;; (load-theme 'doom-one t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages '((odin-mode :url "https://git.sr.ht/~mgmarlow/odin-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
