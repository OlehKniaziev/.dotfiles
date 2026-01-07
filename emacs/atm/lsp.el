;;; -*- lexical-binding: t -*-

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto-prefix 1
   corfu-auto t
        corfu-auto-trigger "."
        corfu-auto-delay 0.2
        corfu-quit-no-match t)
  (add-hook 'corfu-mode-hook
            (lambda ()
              ;; settings only for corfu
              (setq-local completion-styles '(orderless basic)
                          completion-category-overrides nil
                          completion-category-defaults nil))))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (go-ts-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-provider :none)
  (setq lsp-go-hover-kind "FullDocumentation")
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-enable-on-type-formatting nil)
  (add-hook 'lsp-completion-mode-hook (lambda ()
                                        (setq-local completion-styles '(orderless)
                                                    completion-category-defaults nil)))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-show-with-mouse nil)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode)

(provide 'atm/lsp)
