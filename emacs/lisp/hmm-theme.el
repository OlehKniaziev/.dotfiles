;;; hmm-theme.el --- Hmm theme for Emacs 24. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Hmm theme for Emacs made by Oleh Kniaziev aka atmatm9182.

;;; Code:

(deftheme hmm
  "Hmm theme for Emacs 24.")

(let ((hmm-fg "#FFFFD7")
      (hmm-bg "#202020")
      (hmm-bg+1 "#343434")
      (hmm-bg+2 "#484848")
      (hmm-gray "#828282")
      (hmm-yellow "#FFDD33")
      (hmm-quartz "#F4ABFF")
      (hmm-purpur "#FFA1F1")
      ;; (hmm-grape "#B65DE3")
      (hmm-grape "#dd64f5")
      (hmm-pink "#FF66D8")
      (hmm-pink3 "#FF5B4D")
      ;; (hmm-red "#FC2856")
      (hmm-red "#FF426B")
      (hmm-brown "#A77464")
      (hmm-green "#84AB5B")
      (hmm-indigo "#9740D9")
      (hmm-pumpkin "#FF8C42")
      (hmm-coral "#FC573A")
      (hmm-peach "#FFD059")
      (hmm-alice "#E3F2FD")
      (hmm-blue "#42CAFD")
      (hmm-dark-blue "#4390d1")
      (hmm-light-blue "#ACD7EC"))

  (custom-theme-set-variables
   'hmm
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'hmm

   ;; Basic
   `(default ((t ,(list :background hmm-bg
                        :foreground hmm-fg))))
   `(cursor ((t ,(list :background hmm-red))))
   `(link ((t ,(list :foreground hmm-red
                     :underline t))))
   `(region ((t ,(list :background hmm-bg+2))))
   `(hl-line ((t ,(list :background hmm-bg+1))))
   `(minibuffer-prompt ((t ,(list :foreground hmm-red
                                  :bold t))))
   `(highlight ((t ,(list :background hmm-red))))
   `(match ((t ,(list :background hmm-red))))

   ;; Magit related
   `(diff-removed ((t ,(list :foreground hmm-red
                             :background nil))))
   `(diff-added ((t ,(list :foreground hmm-green
                           :background nil))))
   `(git-commit-overlong-summary ((t ,(list :foreground hmm-pumpkin
                                            :italic t
                                            :bold t
                                            :background nil))))
   `(magit-keyword ((t ,(list :inherit 'font-lock-keyword-face))))
   `(magit-branch-remote ((t ,(list :foreground hmm-green))))
   `(magit-branch-upstream ((t ,(list :italic t))))
   `(magit-branch-local ((t ,(list :foreground hmm-light-blue))))
   `(magit-branch-current ((t ,(list :box 1
                                     :inherit 'magit-branch-local))))
   `(magit-diff-added ((t ,(list :background hmm-green
                                 :foreground hmm-alice
                                 :extend t))))
   `(magit-diff-removed ((t ,(list :background hmm-coral
                                   :foreground hmm-alice
                                   :extend t))))

   `(magit-diff-added-highlight ((t ,(list :background hmm-green
                                           :foreground hmm-fg
                                           :bold t
                                           :extend t))))
   `(magit-diff-removed-highlight ((t ,(list :background hmm-coral
                                             :foreground hmm-fg
                                             :bold t
                                             :extend t))))

   ;; Mode line
   `(mode-line ((t ,(list :background hmm-bg+2
                          :foreground hmm-fg))))
   `(mode-line-inactive ((t ,(list :background hmm-bg))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background hmm-bg
                                 :foreground hmm-bg+2))))
   `(whitespace-tab ((t ,(list :background hmm-bg
                               :foreground hmm-bg+2))))
   `(whitespace-hspace ((t ,(list :background hmm-bg
                                  :foreground hmm-bg+2))))
   `(whitespace-newline ((t ,(list :background hmm-bg
                                   :foreground hmm-bg+2))))
   `(whitespace-trailing ((t ,(list :background hmm-red
                                    :foreground hmm-red))))
   `(whitespace-empty ((t ,(list :background hmm-red
                                 :foreground hmm-red))))
   `(whitespace-indentation ((t ,(list :background hmm-red
                                       :foreground hmm-red))))
   `(whitespace-space-after-tab ((t ,(list :background hmm-red
                                           :foreground hmm-red))))
   `(whitespace-space-before-tab ((t ,(list :background hmm-brown
                                            :foreground hmm-brown))))

   ;; Searching
   `(isearch ((t ,(list :background hmm-pink
                        :bold t))))
   `(isearch-fail ((t ,(list :background hmm-red))))

   ;; Org mode
   `(org-done ((t ,(list :foreground hmm-green
                         :bold t))))
   `(org-todo ((t ,(list :foreground hmm-red
                         :bold t))))
   `(org-headline-done ((t ,(list :foreground hmm-gray))))
   `(org-code ((t ,(list :foreground hmm-red))))
   `(org-verbatim ((t ,(list :foreground hmm-green))))

   ;; Eshell
   `(eshell-prompt ((t ,(list :foreground hmm-red
                              :bold t))))
   `(eshell-ls-executable ((t ,(list :foreground hmm-blue
                                     :bold t))))
   `(eshell-ls-directory ((t ,(list :foreground hmm-red
                                    :bold t))))

   ;; Compilation mode
   `(compilation-info ((t ,(list :foreground hmm-dark-blue
                                 :bold t))))
   `(compilation-error ((t ,(list :foreground hmm-coral
                                 :bold t))))
   `(compilation-warning ((t ,(list :foreground hmm-pumpkin
                                  :bold t))))
   `(compilation-line-number ((t ,(list :foreground hmm-quartz
                                        :bold t))))
   `(compilation-column-number ((t ,(list :foreground hmm-indigo
                                        :bold t))))

   ;; Font lock
   `(font-lock-keyword-face ((t ,(list :foreground hmm-red
                                       :bold t))))
   `(font-lock-function-name-face ((t ,(list :foreground hmm-dark-blue))))
   `(font-lock-comment-face ((t ,(list :foreground hmm-brown))))
   `(font-lock-doc-face ((t ,(list :foreground hmm-brown))))
   `(font-lock-doc-string-face ((t ,(list :foreground hmm-brown))))
   `(font-lock-string-face ((t ,(list :foreground hmm-green))))
   `(font-lock-warning-face ((t ,(list :foreground hmm-pumpkin
                                       :bold t))))
   `(font-lock-variable-name-face ((t ,(list :foreground hmm-fg))))
   `(font-lock-variable-use-face ((t ,(list :foreground hmm-fg))))
   `(font-lock-type-face ((t ,(list :foreground hmm-grape))))
   `(font-lock-number-face ((t ,(list :foreground hmm-alice))))
   `(font-lock-escape-face ((t ,(list :foreground hmm-pumpkin
                                      :bold t))))
   `(font-lock-constant-face ((t ,(list :foreground hmm-quartz
                                        :bold t))))
   `(font-lock-preprocessor-face ((t ,(list :foreground hmm-light-blue))))
   `(font-lock-builtin-face ((t ,(list :foreground hmm-blue))))

   ;; Misc
   `(show-paren-match ((t ,(list :background hmm-blue))))
   `(show-paren-mismatch ((t ,(list :background hmm-red
                                    :bold t))))
   `(outline-1 ((t ,(list :foreground hmm-quartz))))
   `(outline-2 ((t ,(list :foreground hmm-grape))))
   `(outline-3 ((t ,(list :foreground hmm-blue))))
   `(outline-4 ((t ,(list :foreground hmm-light-blue))))
   `(outline-5 ((t ,(list :foreground hmm-pumpkin))))
   `(outline-6 ((t ,(list :foreground hmm-green))))
   `(outline-7 ((t ,(list :foreground hmm-coral))))
   `(outline-8 ((t ,(list :foreground hmm-dark-blue))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hmm)
;;; hmm-theme.el ends here
