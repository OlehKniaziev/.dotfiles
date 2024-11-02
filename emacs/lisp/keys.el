;;; keys.el --- my custom keybindings -*- lexical-binding: t -*-

(require 'recentf)

;; Additional functions and macros
(defun ido-recentf ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (unless (find-file (ido-completing-read "Open file: " recentf-list))
    (message "No such file or directory")))

(require 'vc-git)

;; Mappings
(keymap-global-set "C-c f r" 'ido-recentf)
(keymap-global-set "C-c s g" 'rgrep)
(keymap-global-set "C-c s G" 'vc-git-grep)

(provide 'keys)

