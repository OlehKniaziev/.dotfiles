;; -*- lexical-binding: t; -*-

(use-package agent-shell
  :ensure t
  :defer t

  :bind (:map project-prefix-map
              ("a" . agent-shell))

  :config
  (setq agent-shell-inhibit-system-sleep nil))

(provide 'slop)
