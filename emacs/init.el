(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(set-frame-font "Codelia Ligatures 17" nil t)

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

;; (use-package meow
;;   :ensure t
;;   :config
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("<escape>" . ignore))
;;     (meow-leader-define-key
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))
;;   (meow-setup)
;;   (setq meow-use-clipboard t)
;;   (meow-global-mode 1))

(use-package magit
  :ensure t)

(use-package transient
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-go-hover-kind "FullDocumentation")
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-peek-enable t)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-quit-no-match t)
  (add-hook 'corfu-mode-hook
          (lambda ()
            ;; Settings only for Corfu
            (setq-local completion-styles '(basic)
                        completion-category-overrides nil
                        completion-category-defaults nil))))

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

(use-package odin-ts-mode
  :vc (:url "https://github.com/Sampie159/odin-ts-mode"
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
  (setq which-key-show-eary-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

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
  (tab-always-indent 'complete)

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

  (set-face-attribute 'fixed-pitch nil :family "Input Mono Narrow")

  ;; Whitespace mode
  (setq whitespace-style '(face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))
  (global-whitespace-mode)

  ;; Electric
  (electric-indent-mode -1)

  ;; Minibuffer size
  (setq max-mini-window-height 0.35)

  ;; Mode line
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
                     '(" %I "
                       " %p%% "
                       (vc-mode vc-mode)
                       " %m "))))))

  ;; Custom
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'keys)
(require 'ef-themes)
;; (load-theme 'ef-kassio t)
;; (load-theme 'ef-tritanopia-dark t)
;; (load-theme 'ef-eagle t)
;; (load-theme 'ef-trio-dark t)
(load-theme 'ef-elea-dark t)
;; (load-theme 'monokai t)
;; (load-theme 'ef-autumn t)

;; (load-theme 'ef-eagle t)
;; (load-theme 'hmm t)

;; (require 'doom-themes)
;; (load-theme 'doom-one t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(load custom-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((odin-ts-mode :url "https://github.com/Sampie159/odin-ts-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
