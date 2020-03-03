;; == functions

(defun ds-kill-line ()
  "Kills current line"
  (interactive)
  (kill-line 1))

(defun ds-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun ds-move-line-down ()
  "Move down the current line."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun ds-copy-line ()
  (interactive)
  (let ((beg (line-beginning-position))
      	(end (line-end-position)))
    (copy-region-as-kill beg end)))

(defun ds-duplicate-line (&optional arg)
  (interactive "p")
  (let ((ccolumn (current-column)))
    (progn
      (or arg (setq arg 1))
      (if (< arg 0)
          (setq tomove (1+ arg))
        (setq tomove arg))
      (ds-copy-line)
      (end-of-line tomove)
      (newline)
      (yank)
      (next-line (- arg))
      (move-to-column ccolumn))))

(defun ds-duplicate-line-down (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (ds-duplicate-line arg))

(defun ds-duplicate-line-up (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (ds-duplicate-line (- arg)))

(defun ds-transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun ds-cool-home-key ()
  "Move to the first non-space character of current line or to
   the beginning of the line, if we are already there"
  (interactive)
  (let ((col (current-column)))
    (back-to-indentation)
    (if (= (current-column) col)
        (beginning-of-line))))

(defun ds-display-buffer-prefferin-hz-split (buffer force-other-window)
  "Prefer horizontal splitting while displaying buffers if there are
   more than given number of columns"
  (or (get-buffer-window buffer)
      (if (one-window-p)
          (let ((new-win (if (> (window-width) 130)
                             (split-window-horizontally)
                           (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))

(defun ds-ido-imenu (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ds-ido-imenu (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ds-ido-imenu symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun ds-toggle-window-split ()
  "Changes vertical split to horizontal and vice versa,
works only when 2 windows are present"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun ds-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; Kill and copy text only if there is an active region (the default
;; behavior when C-w kills the region between the mark and the point
;; even if the region is not active is irritating).
(defun ds-kill-region-if-mark-active ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (message "There is no active region")))

(defun ds-kill-ring-save-if-mark-active ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (message "There is no active region")))

;;; The functions below has been copied from `ag` and slightly
;;; modified.
(defun ds-dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun ds-read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (ds-dwim-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

(defun ds-search-rust-docs (query)
  "Search Rust docs for a given search STRING,
with STRING defaulting to the symbol under point."
  (interactive (list (ds-read-from-minibuffer "Search string")))
  (browse-url (concat "https://doc.rust-lang.org/std/index.html?search="
			(url-hexify-string query))))
