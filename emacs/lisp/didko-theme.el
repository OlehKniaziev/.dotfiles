;;; didko-theme.el --- Didko theme for Emacs 24. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Didko theme for Emacs made by Oleh Kniaziev aka atmatm9182.

;;; Code:

(deftheme didko
  "Didko theme for Emacs 24.")

(let (;; (didko-fg "#E4E4E4")
      (didko-fg "#FFFFD7")
      (didko-bg "#0F0F0F")
      (didko-bg2 "#5A616A")
      (didko-bg3 "#2F3136")
      (didko-gray "#828282")
      (didko-yellow "#FFD700")
      (didko-quartz "#F4ABFF")
      (didko-grape "#B65DE3")
      (didko-pink "#FF66D8")
      (didko-pink3 "#DB6079")
      (didko-main "#FFFAA6")
      (didko-red "#DE1A1A")
      (didko-red2 "#FC2856")
      (didko-brown "#A77464")
      (didko-light-brown "#C79484")
      (didko-green "#659157")
      (didko-olive "#84AB5B")
      (didko-indigo "#9740D9")
      (didko-pumpkin "#FF8C42")
      (didko-pumpkin2 "#FCAC32")
      (didko-coral "#FC573A")
      (didko-cocoa "#C96F36")
      (didko-alice "#E3F2FD")
      (didko-blue "#42CAFD")
      (didko-dark-blue "#3F88C5")
      (didko-light-blue "#ACD7EC"))

  (custom-theme-set-variables
   'didko
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'didko

   ;; Basic
   `(default ((t ,(list :background didko-bg
                        :foreground didko-fg))))
   `(cursor ((t ,(list :background didko-main))))
   `(link ((t ,(list :foreground didko-main
                     :underline t))))
   `(region ((t ,(list :background didko-bg2))))
   `(hl-line ((t ,(list :background didko-bg3))))
   `(minibuffer-prompt ((t ,(list :foreground didko-main
                                  :bold t))))
   `(highlight ((t ,(list :background didko-pink3))))
   `(match ((t ,(list :background didko-pink3))))

   ;; Magit related
   `(diff-removed ((t ,(list :foreground didko-red
                             :background nil))))
   `(diff-added ((t ,(list :foreground didko-green
                           :background nil))))
   `(git-commit-overlong-summary ((t ,(list :foreground didko-pumpkin
                                            :italic t
                                            :bold t
                                            :background nil))))

   ;; Mode line
   `(mode-line ((t ,(list :background didko-bg3
                          :foreground didko-fg))))
   `(mode-line-inactive ((t ,(list :background didko-bg))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background didko-bg
                                 :foreground didko-bg2))))
   `(whitespace-tab ((t ,(list :background didko-bg
                               :foreground didko-bg2))))
   `(whitespace-hspace ((t ,(list :background didko-bg
                                  :foreground didko-bg2))))
   `(whitespace-newline ((t ,(list :background didko-bg
                                   :foreground didko-bg2))))
   `(whitespace-trailing ((t ,(list :background didko-red
                                    :foreground didko-red))))
   `(whitespace-empty ((t ,(list :background didko-main
                                 :foreground didko-main))))
   `(whitespace-indentation ((t ,(list :background didko-main
                                       :foreground didko-red))))
   `(whitespace-space-after-tab ((t ,(list :background didko-main
                                           :foreground didko-main))))
   `(whitespace-space-before-tab ((t ,(list :background didko-brown
                                            :foreground didko-brown))))

   ;; Searching
   `(isearch ((t ,(list :background didko-pink
                        :bold t))))
   `(isearch-fail ((t ,(list :background didko-red))))

   ;; Org mode
   `(org-done ((t ,(list :foreground didko-green
                         :bold t))))
   `(org-todo ((t ,(list :foreground didko-red
                         :bold t))))
   `(org-headline-done ((t ,(list :foreground didko-gray))))
   `(org-code ((t ,(list :foreground didko-main))))
   `(org-verbatim ((t ,(list :foreground didko-green))))

   ;; Eshell
   `(eshell-prompt ((t ,(list :foreground didko-main
                              :bold t))))
   `(eshell-ls-executable ((t ,(list :foreground didko-blue
                                     :bold t))))
   `(eshell-ls-directory ((t ,(list :foreground didko-cocoa
                                    :bold t))))

   ;; Compilation mode
   `(compilation-info ((t ,(list :foreground didko-dark-blue
                                 :bold t))))
   `(compilation-error ((t ,(list :foreground didko-coral
                                 :bold t))))
   `(compilation-warning ((t ,(list :foreground didko-pumpkin
                                  :bold t))))
   `(compilation-line-number ((t ,(list :foreground didko-quartz
                                        :bold t))))
   `(compilation-column-number ((t ,(list :foreground didko-grape
                                        :bold t))))

   ;; Font lock
   `(font-lock-keyword-face ((t ,(list :foreground didko-main
                                       :bold t))))
   `(font-lock-function-name-face ((t ,(list :foreground didko-gray))))
   `(font-lock-comment-face ((t ,(list :foreground didko-brown))))
   `(font-lock-doc-face ((t ,(list :foreground didko-light-brown))))
   `(font-lock-doc-string-face ((t ,(list :foreground didko-light-brown))))
   `(font-lock-string-face ((t ,(list :foreground didko-green))))
   `(font-lock-warning-face ((t ,(list :foreground didko-pumpkin
                                       :bold t))))
   `(font-lock-variable-name-face ((t ,(list :foreground didko-grape))))
   `(font-lock-variable-use-face ((t ,(list :foreground didko-fg))))
   `(font-lock-type-face ((t ,(list :foreground didko-pink3))))
   `(font-lock-number-face ((t ,(list :foreground didko-alice))))
   `(font-lock-escape-face ((t ,(list :foreground didko-pumpkin
                                      :bold t))))
   `(font-lock-constant-face ((t ,(list :foreground didko-quartz))))
   `(font-lock-preprocessor-face ((t ,(list :foreground didko-light-blue))))
   `(font-lock-builtin-face ((t ,(list :foreground didko-blue))))

   ;; Misc
   `(show-paren-match ((t ,(list :background didko-blue))))
   `(show-paren-mismatch ((t ,(list :background didko-red
                                    :bold t))))
   `(outline-1 ((t ,(list :foreground didko-quartz))))
   `(outline-2 ((t ,(list :foreground didko-pumpkin2))))
   `(outline-3 ((t ,(list :foreground didko-blue))))
   `(outline-4 ((t ,(list :foreground didko-light-blue))))
   `(outline-5 ((t ,(list :foreground didko-pumpkin))))
   `(outline-6 ((t ,(list :foreground didko-green))))
   `(outline-7 ((t ,(list :foreground didko-cocoa))))
   `(outline-8 ((t ,(list :foreground didko-dark-blue))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'didko)
;;; didko-theme.el ends here
