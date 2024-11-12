;;; keys.el --- my custom keybindings -*- lexical-binding: t -*-

(require 'recentf)

(require 'vc-git)

;; Mappings
(keymap-global-set "C-c f r" 'recentf-open)
(keymap-global-set "C-c s g" 'rgrep)
(keymap-global-set "C-c s G" 'vc-git-grep)

(provide 'keys)
