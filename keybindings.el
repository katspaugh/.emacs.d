;;; Keybindings

;;; Code:

;; Comment/uncomment
(bind-key "C-c C-/" 'comment-region)
(bind-key "C-c C-\\" 'uncomment-region)

;; Jump to definition of symbol at point
;; (defun jump-to-definition ()
;;   "Jump to definition of symbol at point."
;;   (interactive)
;;   (imenu (symbol-name (symbol-at-point))))
;; (bind-key "M-." 'jump-to-definition)

;; Kill/copy region or line
(defun flash-region (start end &optional timeout)
  "Flash the region between START and END with an optional TIMEOUT."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'region)
    (run-with-timer (or timeout 0.1) nil 'delete-overlay overlay)))

(defun copy-region-or-line ()
  "Copy region or a single line."
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (region-beginning) (region-end) t)
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (copy-region-as-kill start end)
      (flash-region start end))))

(defun kill-region-or-line ()
  "Kill region or a single line."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end) t)
    (kill-whole-line)))

(bind-key "M-w" 'copy-region-or-line)
(bind-key "C-w" 'kill-region-or-line)


(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(bind-key "M-<backspace>" 'backward-delete-word minibuffer-local-map)

;;; keybindings.el ends here
