;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; ;; Smart mode line
(use-package smart-mode-line
  :config (smart-mode-line-enable))

;; (use-package sanityinc-tomorrow-night
;;   :init (load-theme 'sanityinc-tomorrow-night))

(use-package diff-hl
  :defer 1
  :init (global-diff-hl-mode t))

;; company-mode
(use-package company
  :defer 1
  :init (setq
         company-tooltip-align-annotations t
         company-tooltip-minimum-width 30)
  :config (global-company-mode)
  :bind ("M-<tab>" . company-complete))

;; Count matched lines
(use-package anzu
  :defer 1
  :init (global-anzu-mode))

;; Markdown
(use-package markdown-mode
  :defer 1
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (use-package web-mode
;;   :mode ("\\.jsx\\'" . web-mode)
;;   :config
;;   (progn
;;     (flycheck-add-mode 'typescript-tslint 'web-mode)
;;     (add-hook 'web-mode-hook 'setup-tide-mode)))

;; mmm-mode (for JSX)
(use-package mmm-mode
  :config
  (progn
    (setq mmm-global-mode t)
    (setq mmm-submode-decoration-level 0) ;; Turn off background highlight
    (mmm-add-classes
     '((mmm-styled-mode
        :submode css-mode
        :front "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
        :back "`;")))
    (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)

    (mmm-add-classes
     '((mmm-jsx-mode
        :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
        :front-offset -1
        :back ">\n?\s*)"
        :back-offset 1
        :submode web-mode)))
    (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)))

(defun mmm-reapply ()
  (mmm-mode)
  (mmm-mode))

(add-hook 'after-save-hook
          (lambda ()
            (when (string-match-p "\\.tsx?" buffer-file-name)
              (mmm-reapply))))

;; TypeScript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :mode (("\\.tsx\\'" . typescript-mode)))

(defun setup-tide-mode ()
  (interactive)
  (defun tide-imenu-index () nil)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :config
  (progn
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    (add-hook 'rjsx-mode-hook #'setup-tide-mode)))

;; Yaml
(use-package yaml-mode
  :defer 1)

;; Flycheck
(use-package flycheck
  :defer 1
  :init (setq
         flycheck-checkers
         '(typescript-tide
           javascript-tide
           jsx-tide
           javascript-eslint
           css-csslint
           emacs-lisp
           haml
           json-jsonlint
           yaml-jsyaml))
  :config (global-flycheck-mode))

;; Project explorer
(use-package project-explorer
  :bind (("C-c C-p" . project-explorer-open))
  :config (progn
            (add-hook 'project-explorer-mode-hook (lambda ()
                                                    (setq-local left-fringe-width 6)
                                                    (setq-local right-fringe-width 6)))
            (add-hook 'project-explorer-mode-hook 'hl-line-mode)

            (defun highlight-file-line (&rest args) (hl-line-highlight))
            (advice-add 'pe/goto-file :after 'highlight-file-line))
  :init (setq
         pe/follow-current t
         pe/omit-gitignore t
         pe/width 30))
(load "~/.emacs.d/project-explorer.el")

;; Ivy
(use-package ivy
  :config (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
  :bind (("C-x C-f" . find-file))
  :init (ivy-mode 1))

(use-package smex)
(use-package flx)

(use-package counsel
  :init (counsel-mode 1)
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-x C-d" . counsel-git)
         ("C-x C-g" . counsel-ag)
         ("C-x C-r" . counsel-recentf)))

(defun counsel-ag-from-isearch ()
  "Invoke `counsel-ag' from isearch."
  (interactive)
  (let ((input (if isearch-regexp isearch-string (regexp-quote isearch-string))))
    (isearch-exit)
    (counsel-ag input)))

(define-key isearch-mode-map (kbd "C-x C-g") 'counsel-ag-from-isearch)

;; Transparent titlebar
(use-package ns-auto-titlebar
  :init (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

;; CSS colors
(use-package rainbow-mode
  :defer 1
  :config (progn
            (setq rainbow-html-colors nil)
            (add-hook 'css-mode-hook #'rainbow-mode)))

;;; packages.el ends here
