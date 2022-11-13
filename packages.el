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

;; Smart mode line
;; (use-package smart-mode-line
;;   :config (smart-mode-line-enable))

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
  :bind (
         :map company-mode-map
         ("M-<tab>" . company-complete)
         :map company-active-map
         ("<return>" . nil)))

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

;; TypeScript
(use-package typescript-mode
  :mode (("\\.tsx?\\'" . typescript-mode)))

;; (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

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

;; (use-package web-mode
;;   :mode ("\\.tsx\\'" . web-mode)
;;   :config
;;   (progn
;;     (flycheck-add-mode 'typescript-tslint 'web-mode)
;;     (add-hook 'web-mode-hook 'setup-tide-mode)
;;     (setq sgml-basic-offset 2
;;           css-indent-offset 2
;;           web-mode-markup-indent-offset 2
;;           web-mode-css-indent-offset 2
;;           web-mode-code-indent-offset 2
;;           web-mode-attr-indent-offset 2)))

;; Yaml
(use-package yaml-mode
  :defer 1)

;; Paredit
(use-package paredit
  :config
  (progn
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))

;; Flycheck
(use-package flycheck
  :defer 1
  :init (setq
         flycheck-checkers
         '(javascript-eslint
           typescript-tide
           javascript-tide
           jsx-tide
           css-csslint
           emacs-lisp
           haml
           json-jsonlint
           yaml-jsyaml))
  :config (global-flycheck-mode))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

;; Project explorer
(use-package project-explorer
  :bind (("C-c C-p" . project-explorer-open))
  :config (progn
            (add-hook 'project-explorer-mode-hook 'hl-line-mode)
            (defun highlight-file-line (&rest args) (hl-line-highlight))
            (advice-add 'pe/goto-file :after 'highlight-file-line))
  :init (setq
         pe/follow-current t
         pe/omit-gitignore t
         pe/width 50))

;; Disable ido
(ido-mode nil)

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
         ("s-p" . counsel-git)
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

;; Copilot
(load "~/.emacs.d/copilot/copilot.el")
(add-hook 'prog-mode-hook 'copilot-mode)


;; Linum
(add-hook 'prog-mode-hook (lambda ()
                            (linum-mode 1)
                            (set-face-attribute 'linum nil :background "#262A2D")
                            (set-face-attribute 'fringe nil :background "#262A2D")))

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab)
  (define-key prog-mode-map (kbd "C-<tab>") #'indent-for-tab-command))

;; Mini-frame
(use-package mini-frame
  :init (progn (mini-frame-mode)
         (custom-set-variables
          '(mini-frame-show-parameters
            '((top . 0.2)
              (left . 0.5)
              (height . 15))))))

;;; packages.el ends here
