(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer 1
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Yaml
(use-package yaml-mode
  :ensure t
  :defer 1)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))

;; Project explorer
(use-package project-explorer
  :ensure t
  :bind (("C-c C-p" . project-explorer-open))
  :config (progn
            (add-hook 'project-explorer-mode-hook 'hl-line-mode)
            (defun highlight-file-line (&rest args) (hl-line-highlight))
            (advice-add 'pe/goto-file :after 'highlight-file-line))
  :init (setq
         pe/follow-current t
         pe/omit-gitignore t
         pe/width 50))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; CSS colors
(use-package rainbow-mode
  :ensure t
  :defer 1
  :config (progn
            (setq rainbow-html-colors nil)
            (add-hook 'css-mode-hook #'rainbow-mode)))

;; Copilot
(use-package s :ensure t)
(use-package editorconfig :ensure t)
(load "~/.emacs.d/copilot.el/copilot.el")
(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))
(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab)
  (define-key prog-mode-map (kbd "C-<tab>") #'indent-for-tab-command))
(add-hook 'prog-mode-hook 'copilot-mode)

;; Mini-frame
(use-package mini-frame
  :ensure t
  :init (progn (mini-frame-mode)
               (custom-set-variables
		'(mini-frame-show-parameters
		  '((top . 0.2)
		    (left . 0.5)
		    (width . 0.9)
		    (height . 15))))))

;; Make the minibuffer prompt's font bigger
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 250 :family "San Francisco"))))

;; Vertico
(ido-mode -1)
(icomplete-mode -1)

(use-package vertico
  :ensure t
  :config (setq vertico-count-format nil)
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Counsel
(use-package counsel
  :ensure t
  :bind (("C-x C-d" . counsel-git)
         ("C-x C-g" . counsel-ag)))

(defun counsel-ag-from-isearch ()
  "Invoke `counsel-ag' from isearch."
  (interactive)
  (let ((input (if isearch-regexp isearch-string (regexp-quote isearch-string))))
    (isearch-exit)
    (counsel-ag input)))

(define-key isearch-mode-map (kbd "C-x C-g") 'counsel-ag-from-isearch)

;; Code completion at point
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (
         :map company-mode-map
              ("M-<tab>" . company-complete)
              :map company-active-map
              ("<return>" . nil))
  :custom
  (company-idle-delay 0))

;; Typescript
(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Flycheck
(use-package flycheck
  :ensure t
  :defer 1
  :config (global-flycheck-mode))
