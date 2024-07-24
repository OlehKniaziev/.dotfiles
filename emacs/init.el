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

(defmacro --evil-normal-bind (keys callback)
  "define the evil key binding in the normal mode"
  `(evil-define-key nil evil-normal-state-map
     (kbd ,keys) ',callback))

(use-package evil
  :ensure t
 
  :custom
  (evil-undo-system 'undo-redo)
 
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
 
  :config
  ;; bindings
  (evil-set-leader 'normal (kbd "SPC"))

  (--evil-normal-bind "K" eldoc-box-help-at-point)
  (--evil-normal-bind "<leader>s" consult-imenu)
  (--evil-normal-bind "<leader>S" consult-imenu-multi)

  (--evil-normal-bind "<leader>fr" recentf-open)
  (--evil-normal-bind "<leader>ff" consult-fd)
  (--evil-normal-bind "<leader>fg" consult-ripgrep)
  (--evil-normal-bind "<leader>fG" consult-git-grep)
  (--evil-normal-bind "<leader>fb" consult-buffer)

  (--evil-normal-bind "<leader>ca" eglot-code-actions)
  (--evil-normal-bind "<leader>cr" eglot-rename)
  (--evil-normal-bind "<leader>cf" eglot-format)

  (evil-mode 1))
 
(use-package evil-collection
  :ensure t
 
  :init
  (setq evil-collection-key-blacklist '("K"))
  
  :config
  (evil-collection-init))
 
(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:update-interval 0.2)

  (pcase system-type
    ('windows-nt nil)
    (_ (global-git-gutter-mode 1))))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package vterm
  :ensure t
  :defer t)

(use-package consult
  :ensure t
  :demand t)
 
;; some lsp stuff
(use-package flycheck
  :ensure t)
 
(use-package markdown-mode
  :ensure t)

(use-package eldoc-box
  :ensure t)

(use-package eglot
  :hook
  ((c-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure)
   (go-mode . eglot-ensure))

  :config
  (add-hook 'eglot-connect-hook 'flycheck-eglot-mode))

(use-package flycheck-eglot
  :ensure t)
 
(use-package orderless
  :ensure t
 
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file styles basic partial-completion))))
 
(use-package vertico
  :ensure t
 
  :custom
  (vertico-scroll-margin 3)
  (vertico-cycle t)
 
  :init
  (vertico-mode))
 
(use-package corfu
  :ensure t
 
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 3)
 
  :init
  (global-corfu-mode))
 
(use-package cape
  :ensure t
 
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package projectile
  :ensure t

  :bind
  (:map projectile-mode-map
	("<leader>p" . projectile-command-map))
    
  :init
  (projectile-mode 1))

(use-package emacs
  :custom
  (tab-always-indent 'complete))

;; theming
(use-package ligature
  :ensure t
  :config
(ligature-set-ligatures
   'prog-mode
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
    
(use-package go-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package catppuccin-theme
  :ensure (:wait t)
  :demand t
 
  :init
  (setq catppuccin-flavor 'mocha))

(use-package dracula-theme
  :ensure (:wait t)
  :demand t)
 
;; (load-theme 'catppuccin t)
(load-theme 'dracula t)

;; faces
(set-face-attribute 'font-lock-builtin-face nil :weight 'bold :slant 'normal)
(set-face-attribute 'font-lock-keyword-face nil :weight 'normal :slant 'italic)
(set-face-attribute 'font-lock-type-face nil :weight 'bold :slant 'normal)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

(set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :slant 'italic :inherit 'shadow)
(set-face-underline 'eglot-highlight-symbol-face t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indent-line-function 'insert-tab)

(setq-default c-ts-mode-indent-offset 4)
(setq-default c-basic-offset 4)
(setq c-indentation-style "k&r")

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

(set-frame-font "Berkeley Mono 19" nil t)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(defvar auto-mode-pairs
  '(("\\.rs\\'" . rust-ts-mode)
    ("\\.yml\\'" . yaml-ts-mode)
    ("\\.yaml\\'" . yaml-ts-mode)))

(dolist (pair auto-mode-pairs)
  (add-to-list 'auto-mode-alist pair))

(setq major-mode-remap-alist
    '((javascript-mode . js-ts-mode)
      (js-mode . js-ts-mode)
      (c-mode . c-ts-mode)))

;; misc
(setq eldoc-echo-area-use-multiline-p nil)

(setq eglot-ignored-server-capabilities
  '(:inlayHintProvider))

(setq projectile-project-search-path
  '("~/personal"))
