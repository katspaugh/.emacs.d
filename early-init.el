;; Performance tweaking for modern machines
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Save desktop
(desktop-save-mode)

;; Backup and autosave files in temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Set system PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Mac OS frame title
(setq frame-title-format "%f")
(setq ns-use-proxy-icon nil)

;; y-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; Move between windows with Meta+arrows
(windmove-default-keybindings 'meta)

;; Display tabs and trailing spaces
(global-whitespace-mode t)
(setq-default whitespace-style '(face tab trailing))

;; Set tabs to 2 spaces
(setq-default
 indent-tabs-mode nil
 tab-width 2
 standard-indent 2
 js-indent-level 2
 css-indent-offset 2
 sgml-basic-offset 2)

;; Auto-revert buffer on file change
(global-auto-revert-mode)

;; Hide scrollbars and toolbar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Mute bell
(setq ring-bell-function #'ignore)

;; Cursor type
(setq-default cursor-type '(bar . 2))

;; Start the Emacs server
(run-with-idle-timer 1 nil #'server-start)
(add-hook 'server-switch-hook #'raise-frame)

;; Load packages
(run-with-idle-timer 0.1 nil #'load "~/.emacs.d/packages.el")

;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(run-with-idle-timer 0.1 nil #'load custom-file)

;; Load keybindings
(run-with-idle-timer 0.2 nil #'load "~/.emacs.d/keybindings.el")

;;; early-init.el ends here
