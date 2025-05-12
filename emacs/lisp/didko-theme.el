;;; didko-theme.el --- Didko theme for Emacs 24. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Didko theme for Emacs made by Oleh Kniaziev aka atmatm9182.

;;; Code:

(deftheme didko
  "Didko theme for Emacs 24.")

(let* ((didko-fg "#FFFFE7")
      (didko-bg "#151514")
      ;; (didko-bg "#101012")
      (didko-bg2 "#303030")
      (didko-bg3 "#505050")
      (didko-gray "#b3b3b3")
      (didko-yellow "#f2de4b")
      (didko-blue "#208FD0")
      (didko-quartz "#F4ABFF")
      (didko-pink "#FF66D8")
      (didko-pink3 "#FF5B4D")
      (didko-pale-yellow "#fff7b3")
      (didko-red "#DE1A1A")
      (didko-red2 "#FC2856")
      (didko-brown "#A77464")
      (didko-light-brown "#C79484")
      (didko-olive "#B8C480")
      (didko-green "#84AB5B")
      (didko-forest-green "#82BD92")
      (didko-indigo "#9740D9")
      (didko-pumpkin "#FF8C42")
      (didko-pumpkin2 "#FCAC32")
      (didko-coral "#FC573A")
      (didko-cocoa "#C96F36")
      (didko-snow "#F0F8FC")
      (didko-dark-blue "#3F88C5")
      (didko-acid "#9AFE22")
      (didko-very-green "#108040")
      ;; (didko-swamp "#91c928")
      (didko-night-blue "#4059AD")
      (didko-azure "#007FFF")
      (didko-grape "#665687")
      (didko-swamp "#77b500")
      (didko-ash "#c3c3c3")
      (didko-rose "#CA2E55")
      (didko-blueish "#6dafce")
      (didko-vista "#10A6FF")
      ;; (didko-light-blue "#ABE4FF")
      (didko-light-blue "#98abb1")
      (didko-main didko-pumpkin))

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
   `(region ((t ,(list :background didko-bg3))))
   `(hl-line ((t ,(list :background didko-bg2))))
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
   `(magit-keyword ((t ,(list :inherit 'font-lock-keyword-face))))
   `(magit-branch-remote ((t ,(list :foreground didko-olive))))
   `(magit-branch-upstream ((t ,(list :italic t))))
   `(magit-branch-local ((t ,(list :foreground didko-light-blue))))
   `(magit-branch-current ((t ,(list :box 1
                                     :inherit 'magit-branch-local))))
   `(magit-diff-added ((t ,(list :background didko-green
                                 :foreground didko-snow
                                 :extend t))))
   `(magit-diff-removed ((t ,(list :background didko-coral
                                   :foreground didko-snow
                                   :extend t))))

   `(magit-diff-added-highlight ((t ,(list :background didko-green
                                           :foreground didko-fg
                                           :bold t
                                           :extend t))))
   `(magit-diff-removed-highlight ((t ,(list :background didko-coral
                                             :foreground didko-fg
                                             :bold t
                                             :extend t))))

   ;; Mode line
   `(mode-line ((t ,(list :background didko-bg2
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
                                       :bold nil))))
   `(font-lock-function-name-face ((t ,(list :foreground didko-green))))
   `(font-lock-comment-face ((t ,(list :foreground didko-brown))))
   `(font-lock-doc-face ((t ,(list :foreground didko-light-brown))))
   `(font-lock-doc-string-face ((t ,(list :foreground didko-light-brown))))
   `(font-lock-string-face ((t ,(list :foreground didko-swamp))))
   `(font-lock-warning-face ((t ,(list :foreground didko-pumpkin
                                       :bold t))))
   `(font-lock-variable-name-face ((t ,(list :foreground didko-quartz))))
   `(font-lock-variable-use-face ((t ,(list :foreground didko-fg))))
   `(font-lock-type-face ((t ,(list :foreground didko-blueish))))
   `(font-lock-number-face ((t ,(list :foreground didko-snow))))
   `(font-lock-escape-face ((t ,(list :foreground didko-vista))))
   `(font-lock-constant-face ((t ,(list :foreground didko-red2
                                        :bold nil))))
   `(font-lock-preprocessor-face ((t ,(list :foreground didko-yellow))))
   `(font-lock-builtin-face ((t ,(list :foreground didko-azure))))
   `(font-lock-operator-face ((t ,(list :foreground didko-ash))))
   `(font-lock-delimiter-face ((t ,(list :foreground didko-ash))))

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
