;;; keys.el --- my custom keybindings -*- lexical-binding: t -*-

(require 'recentf)

(require 'vc-git)

;; Misc mappings
(keymap-global-set "C-c f r" 'recentf-open)
(keymap-global-set "C-c f p" 'find-file-at-point)
(keymap-global-set "C-c s g" 'rgrep)
(keymap-global-set "C-c s G" 'vc-git-grep)

;; Window mappings
(keymap-global-set "C-c w h" 'windmove-left)
(keymap-global-set "C-c w j" 'windmove-down)
(keymap-global-set "C-c w k" 'windmove-up)
(keymap-global-set "C-c w l" 'windmove-right)

(provide 'keys)
