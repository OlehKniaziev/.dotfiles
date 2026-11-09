;; -*- lexical-binding: t; -*-

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

  (keymap-unset corfu-map "TAB")
  (keymap-unset corfu-map "<remap> <next-line>")
  (keymap-unset corfu-map "<remap> <previous-line>")

  (add-hook 'corfu-mode-hook
            (lambda ()
              ;; settings only for corfu
              (setq-local completion-styles '(orderless basic)
                          completion-category-overrides nil
                          completion-category-defaults nil))))

(provide 'corfu-config)
