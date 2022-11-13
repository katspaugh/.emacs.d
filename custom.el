(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#cccccc"))
 '(blink-cursor-mode nil)
 '(company-backends
   '(company-tide company-nxml company-css company-capf company-files))
 '(company-idle-delay 1000)
 '(copilot-idle-delay 0.1)
 '(counsel-find-file-ignore-regexp "\\`\\.")
 '(css-indent-offset 2)
 '(cursor-type '(bar . 2))
 '(custom-enabled-themes '(vscode-dark-plus))
 '(custom-safe-themes
   '("0ed28b0694dd2c7a2407598e63650a8562b9e833a1a136ee74790a74d3776d3b" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-replace nil)
 '(desktop-buffers-not-to-save ".")
 '(desktop-files-not-to-save ".")
 '(diff-hl-side 'right)
 '(fill-column 120)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-checkers
   '(typescript-tide javascript-tide jsx-tide css-csslint emacs-lisp haml javascript-eslint json-jsonlint yaml-jsyaml))
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-eslint-rules-directories nil)
 '(flycheck-idle-change-delay 10.0)
 '(frame-background-mode 'dark)
 '(highlight-symbol-foreground-color nil)
 '(highlight-symbol-idle-delay 0.1)
 '(ido-everywhere t)
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
 '(mini-frame-show-parameters '((top . 0.1) (left . 0.5) (height . 15) (width . 0.9)))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(ns-function-modifier 'alt)
 '(ns-pop-up-frames nil)
 '(ns-right-alternate-modifier 'none)
 '(package-selected-packages
   '(vscode-dark-plus-theme mini-frame editorconfig smart-dash flycheck nlinum rainbow-delimiters paredit rainbow-blocks smart-mode-line project-explorer ns-auto-titlebar color-theme-sanityinc-tomorrow ujelly-theme vs-dark-theme which-key rainbow-mode counsel flx smex ivy yaml-mode tide typescript-mode mmm-mode web-mode markdown-mode anzu company diff-hl use-package))
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
 '(window-divider-default-right-width 1)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(copilot-overlay-face ((t (:inherit shadow :foreground "#777777"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.3 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0 :weight bold)))))
