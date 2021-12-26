;; Save miscellaneous history
(setq savehist-additional-variables
      '(kill-ring
        command-history
	      set-variable-value-history
	      custom-variable-history
	      query-replace-history
	      read-expression-history
	      minibuffer-history
	      read-char-history
	      face-name-history
	      bookmark-history
        ivy-history
	      counsel-M-x-history
	      file-name-history
        counsel-minibuffer-history))
(setq history-length 100)
(setq kill-ring-max 25)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 25)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)

(savehist-mode 1)

;; Remove text properties for kill ring entries
;; See https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;; Recentf files
(setq recentf-max-menu-items 25)
(recentf-mode 1)

;; Backup
(setq backup-by-copying t     ; don't clobber symlinks
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
                                        ;  backup is made (default: 2)
      kept-new-versions 9)    ; newest versions to keep when a new numbered
