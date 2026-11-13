;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l = =" . eglot-format-buffer)
              ("C-c l g d" . xref-find-definitions)
              ("C-c l g r" . xref-find-references)
              ("C-c l g i" . eglot-find-implementation)
              ("C-c l h h" . eldoc-doc-buffer)
              ("C-c l r o" . eglot-code-action-organize-imports)
              ("C-c l r r" . eglot-rename)
              ("C-c l w q" . eglot-shutdown)
              ("C-c l w r" . eglot-reconnect))
  :hook ((go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (eglot-mode . flycheck-mode)))

(provide 'eglot-config)
