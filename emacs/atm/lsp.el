;; -*- lexical-binding: t; -*-

(require 'corfu-config)
(require 'eglot-config)

(require 'flycheck)

(global-flycheck-eglot-mode 1)
(global-flycheck-annotate-mode)

(provide 'lsp)
