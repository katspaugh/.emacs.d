(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-backends
   '(company-tide company-nxml company-css company-capf company-files))
 '(counsel-find-file-ignore-regexp "\\`\\.")
 '(css-indent-offset 2)
 '(cursor-type '(bar . 2))
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-replace nil)
 '(desktop-buffers-not-to-save ".")
 '(desktop-files-not-to-save ".")
 '(diff-hl-side 'right)
 '(fill-column 120)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-checkers
   '(typescript-tide javascript-tide jsx-tide css-csslint emacs-lisp haml javascript-eslint json-jsonlint yaml-jsyaml))
 '(flycheck-eslint-rules-directories nil)
 '(flycheck-idle-change-delay 10.0)
 '(highlight-symbol-foreground-color nil)
 '(highlight-symbol-idle-delay 0.1)
 '(imenu-use-popup-menu t)
 '(inhibit-startup-screen t)
 '(ivy-count-format "")
 '(ivy-fixed-height-minibuffer t)
 '(ivy-height 10)
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(js-switch-indent-offset 4)
 '(js2-mode-assume-strict t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(ns-function-modifier 'alt)
 '(ns-pop-up-frames nil)
 '(ns-right-alternate-modifier 'none)
 '(package-selected-packages
   '(mmm-mode ov fence-edit guide-key-tip es-lib json-mode flycheck typescript-mode smart-jump flx mark-multiple helpful atom-dark-theme smart-mode-line ivy tern js2-mode company powerline spaceline-all-the-icons powerline-evil spaceline all-the-icons fundamental-mode js2-refactor wgrep tj-mode web-mode ag yaml-mode which-key use-package tide smex smart-mode-line-powerline-theme sass-mode rjsx-mode rainbow-mode project-explorer markdown-mode js2-highlight-vars highlight-symbol helm-git-grep helm flycheck-title editorconfig diff-hl counsel company-tern coffee-mode anzu))
 '(recentf-menu-filter 'recentf-arrange-by-dir)
 '(safe-local-variable-values '((sgml-basic-offset . 2) (standard-indent . 2)))
 '(scroll-margin 3)
 '(scroll-step 5)
 '(smart-jump-peek-key "M-p")
 '(sml/theme 'respectful)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(use-package-always-ensure t)
 '(web-mode-attr-indent-offset 2)
 '(window-divider-default-right-width 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Roboto Mono"))))
 '(hl-line ((t (:extend t :background "#B0BEC5" :foreground "#FFFFFF"))))
 '(ivy-current-match ((t (:extend t :background "#B2BDC4" :foreground "white"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.3 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0 :weight bold))))
 '(minibuffer-prompt ((t (:background "#886DB6" :foreground "#FFFFFF")))))
