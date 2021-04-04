;; Save desktop
(desktop-save-mode)

;; Backup and autosave files in temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)

;; Set system PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq frame-title-format "%f")
(setq ns-use-proxy-icon nil)

;; y-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set tabs to 2 spaces
(setq-default
 indent-tabs-mode nil
 js-indent-level 2
 standard-indent 2
 tab-width 2
 sgml-basic-offset 2)

;; Electric
(electric-indent-mode -1)
(electric-pair-mode -1)

;; Display tabs and trailing spaces
(global-whitespace-mode t)
(setq-default whitespace-style '(face tab trailing))

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)

;; Auto-revert buffer on file change
(global-auto-revert-mode)

;; Display column number
(column-number-mode)

;; Hide scrollbars
(scroll-bar-mode -1)

;; Mute bell
(setq ring-bell-function #'ignore)

;; Allow upcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Make the minibuffer prompt's font bigger
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 250 :family "San Francisco"))))

;; Session
(load "~/.emacs.d/session.el")

;; Load packages
(run-with-idle-timer 0.1 nil #'load "~/.emacs.d/packages.el")

;; Keybindings
(run-with-idle-timer 0.1 nil #'load "~/.emacs.d/keybindings.el")

;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(run-with-idle-timer 0.3 nil #'load custom-file)

;; Start the Emacs server
(run-with-idle-timer 1 nil #'server-start)
(add-hook 'server-switch-hook #'raise-frame)
