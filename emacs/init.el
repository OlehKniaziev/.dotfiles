;; elpaca setup
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca-no-symlink-mode)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

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

(use-package magit
  :ensure t)

(use-package transient
  :ensure t)

(use-package eat
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package tuareg
  :ensure t)

(use-package yasnippet
  :ensure t

  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package emacs
  :config
  ;; Ido mode
  ;; (setq ido-enable-flex-matching t)
  ;; (ido-mode 1)
  ;; (ido-everywhere 1)
  (require 'icomplete)

  (defun fido-hook ()
    (setq-local completion-styles '(substring)))

  (add-hook 'icomplete-minibuffer-setup-hook 'fido-hook)
  (fido-vertical-mode)

  (global-goto-address-mode 1)

  ;; Whitespace mode
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
  (global-whitespace-mode 1)

  ;; Mode line
  (setq-default mode-line-format
                '(" "
                  mode-line-modified
                  " %b "
                  " (%l, %c) "
                  " %p "
                  " %m "
                  (vc-mode vc-mode))))

;; theming
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   't
   '(; Group A
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

(use-package cmake-mode
  :ensure t)

(use-package ef-themes
  :ensure t
  :defer t

  :config
  (set-face-underline 'ef-themes-underline-warning '(:style line :color "#c0b000"))
  (set-face-underline 'ef-themes-underline-error '(:style line :color "#df2f2f"))
  (set-face-underline 'ef-themes-underline-info '(:style line :color "#22b022")))

;; faces
;; (set-face-attribute 'font-lock-builtin-face nil :weight 'bold :slant 'normal)
;; (set-face-attribute 'font-lock-keyword-face nil :weight 'normal :slant 'italic)
;; (set-face-attribute 'font-lock-type-face nil :weight 'bold :slant 'normal)
;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)

(elpaca-wait)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indent-line-function 'insert-tab)

(setq-default c-ts-mode-indent-offset 4)
(setq-default c-basic-offset 4)

(setq-default go-ts-mode-indent-offset 4)

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
    ("\\.go\\'" . go-ts-mode)
    ("\\.mod\\'" . go-mod-ts-mode)
    ("\\.cjs\\'" . js-ts-mode)
    ("\\.mjs\\'" . js-ts-mode)
    ("\\.yaml\\'" . yaml-ts-mode)))

(dolist (pair auto-mode-pairs)
  (add-to-list 'auto-mode-alist pair))

(setq major-mode-remap-alist
      '((javascript-mode . js-ts-mode)
        (js-mode . js-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)))

;; misc
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(setq projectile-project-search-path
      '("~/personal"))

(setq-default treesit-font-lock-level 4)

(setq
 scroll-step 1
 scroll-conservatively 999)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'keys)
;; (require 'didko-theme)
;;
;; (load-theme 'didko t)

(require 'ef-themes)
;; (load-theme 'ef-kassio t)
;; (load-theme 'ef-tritanopia-dark t)
(load-theme 'ef-melissa-light t)
