;;; didko-theme.el --- Didko theme for Emacs 24. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Didko theme for Emacs made by Oleh Kniaziev aka atmatm9182.

;;; Code:

(deftheme didko
  "Didko theme for Emacs 24.")

(let ((didko-fg "#E4E4E4")
      (didko-bg "#353535")
      (didko-bg2 "#5A616A")
      (didko-bg3 "#2F3136")
      (didko-gray "#505050")
      (didko-yellow "#FFDF00")
      (didko-quartz "#F4ABFF")
      (didko-light-quartz "#AA6B9D")
      (didko-pink "#FF66D8")
      (didko-pink2 "#AC80A0")
      (didko-red "#DE1A1A")
      (didko-brown "#A77464")
      (didko-green "#659157")
      (didko-indigo "#9740D9")
      (didko-pumpkin "#FF8C42")
      (didko-pumpkin2 "#FCAC32")
      (didko-cocoa "#C96F36")
      (didko-mustard "#FFDF5F")
      (didko-alice "#E3F2FD")
      (didko-dark-blue "#3F88C5")
      (didko-blue "#42CAFD")
      (didko-light-blue "#ACD7EC"))

  (custom-theme-set-variables
   'didko
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'didko

   ;; Basic
   `(default ((t ,(list :background didko-bg
                        :foreground didko-fg))))
   `(cursor ((t ,(list :background didko-yellow))))
   `(link ((t ,(list :foreground didko-yellow
                     :underline t))))
   `(region ((t ,(list :background didko-bg2))))
   `(hl-line ((t ,(list :background didko-bg3))))
   `(minibuffer-prompt ((t ,(list :foreground didko-yellow
                                  :bold t))))

   ;; Magit related
   `(diff-removed ((t ,(list :foreground didko-red
                             :background nil))))
   `(diff-added ((t ,(list :foreground didko-green
                           :background nil))))

   ;; Mode line
   `(mode-line ((t ,(list :backround didko-bg3
                         :foreground didko-fg))))
   `(mode-line-inactive ((t ,(list :background didko-bg3
                                  :foreground didko-quartz))))

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
   `(whitespace-empty ((t ,(list :background didko-yellow
                                 :foreground didko-yellow))))
   `(whitespace-indentation ((t ,(list :background didko-yellow
                                       :foreground didko-red))))
   `(whitespace-space-after-tab ((t ,(list :background didko-yellow
                                           :foreground didko-yellow))))
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

   ;; Eshell
   `(eshell-prompt ((t ,(list :foreground didko-yellow
                              :bold t))))
   `(eshell-ls-executable ((t ,(list :foreground didko-blue
                                     :bold t))))
   `(eshell-ls-directory ((t ,(list :foreground didko-cocoa
                                    :bold t))))

   ;; Compilation mode
   `(compilation-info ((t ,(list :foreground didko-dark-blue
                                 :bold t))))

   ;; Font lock
   `(font-lock-keyword-face ((t ,(list :foreground didko-yellow
                                       :bold t))))
   `(font-lock-function-name-face ((t ,(list :foreground didko-quartz))))
   `(font-lock-comment-face ((t ,(list :foreground didko-brown))))
   `(font-lock-doc-face ((t ,(list :foreground didko-green))))
   `(font-lock-doc-string-face ((t ,(list :foreground didko-green))))
   `(font-lock-string-face ((t ,(list :foreground didko-green))))
   `(font-lock-warning-face ((t ,(list :foreground didko-pumpkin
                                       :bold t))))
   `(font-lock-variable-name-face ((t ,(list :foreground didko-pumpkin2))))
   `(font-lock-variable-use-face ((t ,(list :foreground didko-fg))))
   `(font-lock-type-face ((t ,(list :foreground didko-indigo))))
   `(font-lock-number-face ((t ,(list :foreground didko-alice))))
   `(font-lock-escape-face ((t ,(list :foreground didko-blue
                                      :bold t))))
   `(font-lock-constant-face ((t ,(list :foreground didko-dark-blue
                                        :bold t))))
   `(font-lock-preprocessor-face ((t ,(list :foreground didko-light-blue))))
   `(font-lock-builtin-face ((t ,(list :foreground didko-light-quartz))))

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
