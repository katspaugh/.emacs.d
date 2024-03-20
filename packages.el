;;; Code:

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
  :hook
  (emacs-lisp-mode-hook . rainbow-delimiters-mode)
  (ielm-mode-hook . rainbow-delimiters-mode)
  (lisp-interaction-mode-hook . rainbow-delimiters-mode)
  (lisp-mode-hook . rainbow-delimiters-mode))

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

;; CSS colors
(use-package rainbow-mode
  :ensure t
  :defer 1
  :config (progn
            (setq rainbow-html-colors nil)
            (add-hook 'css-mode-hook #'rainbow-mode)))

;; Copilot
(defun my/copilot-tab ()
 "Copilot autocomplete."
 (interactive)
 (or (copilot-accept-completion)
     (indent-for-tab-command)))

(use-package f :ensure t)
(use-package editorconfig :ensure t)
(use-package jsonrpc :ensure t)

(use-package copilot
  :load-path "site-lisp/copilot"
  :ensure nil
  :hook ((prog-mode . copilot-mode))
  :bind (
         :map copilot-mode-map
              ("<tab>" . my/copilot-tab)
              :map prog-mode-map
              ("C-<tab>" . indent-for-tab-command)))

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
;
;; Make the minibuffer prompt's font bigger
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 250 :family "San Francisco"))))

(ido-mode -1)
(icomplete-mode -1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package ivy
  :ensure t
  :config (ivy-mode 1)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

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
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Code completion at point
(use-package company
  :ensure t
  :init (company-mode 1)
  :bind (
         :map company-mode-map
         ("M-<tab>" . company-complete)
         :map company-active-map
         ("<return>" . nil)))

;; Flycheck
(use-package flycheck
  :ensure t
  :defer 1
  :config (global-flycheck-mode))

;; Typescript
(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (tsx-ts-mode . tide-setup)
         (js-mode . tide-setup)
         (js-mode . (lambda ()
                      (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))
         (before-save . tide-format-before-save)))

;; Prettier
(use-package prettier-js
  :ensure t
  :defer 1
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)))

;; Diff-hl
(use-package diff-hl
  :ensure t
  :defer 1
  :config (global-diff-hl-mode))

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((go-mode . lsp) (lua-mode . lsp))
;;   :commands lsp)
;; (use-package lsp-ui :commands lsp-ui-mode :ensure t)


;;; packages.el ends here
