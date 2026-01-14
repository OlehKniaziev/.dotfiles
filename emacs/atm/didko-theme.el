;;; didko-theme.el --- Didko theme for Emacs 24. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Didko theme for Emacs made by Oleh Kniaziev aka atmatm9182.

;;; Code:

(deftheme didko
  "Didko theme for Emacs 24.")

(let* ((didko-fg "#FFFFFF")
      (didko-bg "#000000")
      ;; (didko-bg "#101012")
      (didko-bg2 "#303030")
      (didko-bg3 "#505050")
      (didko-gray "#b3b3b3")
      (didko-yellow "#f2de4b")
      (didko-blue "#0000FF")
      (didko-quartz "#F4ABFF")
      (didko-pink "#FF66D8")
      (didko-pomidor "#FF5B4D")
      (didko-pale-yellow "#ffff88")
      (didko-red "#DE1A1A")
      (didko-red2 "#FC2856")
      (didko-brown "#A77464")
      (didko-light-brown "#C79484")
      (didko-olive "#B8C480")
      (didko-green "#00FF00")
      (didko-purple "#bd93f9")
      (didko-purple2 "#9d93d9")
      (didko-magenta "#FD3DB5")
      (didko-magenta-dimmed "#451131")
      (didko-forest-green "#82BD92")
      (didko-indigo "#9740D9")
      (didko-pumpkin "#FF8C42")
      (didko-pumpkin2 "#FCAC32")
      (didko-cocoa "#C96F36")
      (didko-snow "#F0F8FC")
      (didko-dark-blue "#3F88C5")
      (didko-acid "#BDFE22")
      (didko-very-green "#108040")
      ;; (didko-swamp "#91c928")
      (didko-night-blue "#4059AD")
      (didko-azure "#007FFF")
      (didko-grape "#8080C0")
      (didko-swamp "#77b500")
      (didko-ash "#808080")
      (didko-rose "#CA2E55")
      (didko-blueish "#6dafce")
      (didko-vista "#10A6FF")
      ;; (didko-light-blue "#ABE4FF")
      (didko-light-blue "#98bfb1")
      (didko-hl didko-bg3)
      (didko-main didko-yellow))

  (custom-theme-set-variables
   'didko
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'didko

   ;; Basic
   `(default ((t ,(list :background didko-bg
                        :foreground didko-fg))))
   `(cursor ((t ,(list :background didko-green))))
   `(link ((t ,(list :foreground didko-main
                     :underline t))))
   `(region ((t ,(list :background didko-bg3))))
   `(hl-line ((t ,(list :background didko-bg2))))
   `(minibuffer-prompt ((t ,(list :foreground didko-main
                                  :bold t))))
   `(highlight ((t ,(list :background didko-hl
                          :underline t))))
   `(match ((t ,(list :background didko-pomidor))))

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
   `(magit-diff-added ((t ,(list :background didko-very-green
                                 :foreground didko-snow
                                 :extend t))))
   `(magit-diff-removed ((t ,(list :background didko-pomidor
                                   :foreground didko-snow
                                   :extend t))))

   `(magit-diff-added-highlight ((t ,(list :background didko-very-green
                                           :foreground didko-fg
                                           :bold t
                                           :extend t))))
   `(magit-diff-removed-highlight ((t ,(list :background didko-pomidor
                                             :foreground didko-fg
                                             :bold t
                                             :extend t))))

   ;; Mode line
   `(mode-line ((t ,(list :background didko-night-blue
                          :foreground didko-fg))))
   `(mode-line-inactive ((t ,(list :background didko-dark-blue))))

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
   `(org-document-title ((t ,(list :foreground didko-green
                                   :bold t))))

   ;; Eshell
   `(eshell-prompt ((t ,(list :foreground didko-main
                              :bold t))))
   `(eshell-ls-executable ((t ,(list :foreground didko-blue
                                     :bold t))))
   `(eshell-ls-directory ((t ,(list :foreground didko-cocoa
                                    :bold t))))

   ;; Dired
   `(dired-directory ((t ,(list :foreground didko-ash))))
   `(dired-flagged ((t ,(list :foreground didko-pomidor
                                :bold t))))
   ;; Compilation mode
   `(compilation-info ((t ,(list :foreground didko-dark-blue
                                 :bold t))))
   `(compilation-error ((t ,(list :foreground didko-pomidor
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
   `(font-lock-function-name-face ((t ,(list :foreground didko-fg))))
   `(font-lock-comment-face ((t ,(list :foreground didko-brown
                                       :italic t))))
   `(font-lock-doc-face ((t ,(list :foreground didko-light-brown))))
   `(font-lock-doc-string-face ((t ,(list :foreground didko-light-brown))))
   `(font-lock-string-face ((t ,(list :foreground didko-swamp))))
   `(font-lock-warning-face ((t ,(list :foreground didko-pumpkin
                                       :bold t))))
   `(font-lock-variable-name-face ((t ,(list :foreground didko-fg))))
   `(font-lock-variable-use-face ((t ,(list :foreground didko-fg))))
   `(font-lock-type-face ((t ,(list :foreground didko-grape))))
   `(font-lock-number-face ((t ,(list :foreground didko-snow))))
   `(font-lock-escape-face ((t ,(list :foreground didko-acid))))
   `(font-lock-constant-face ((t ,(list :foreground didko-grape))))
   `(font-lock-preprocessor-face ((t ,(list :foreground didko-night-blue))))
   `(font-lock-builtin-face ((t ,(list :foreground didko-pomidor))))
   `(font-lock-operator-face ((t ,(list :foreground didko-fg))))
   `(font-lock-delimiter-face ((t ,(list :foreground didko-gray))))

   ;; Misc
   `(show-paren-match ((t ,(list :background didko-night-blue))))
   `(show-paren-mismatch ((t ,(list :background didko-red
                                    :bold t))))
   `(outline-1 ((t ,(list :foreground didko-main))))
   `(outline-2 ((t ,(list :foreground didko-purple))))
   `(outline-3 ((t ,(list :foreground didko-blue))))
   `(outline-4 ((t ,(list :foreground didko-light-blue))))
   `(outline-5 ((t ,(list :foreground didko-pumpkin))))
   `(outline-6 ((t ,(list :foreground didko-green))))
   `(outline-7 ((t ,(list :foreground didko-vista))))
   `(outline-8 ((t ,(list :foreground didko-dark-blue))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'didko)
;;; didko-theme.el ends here
