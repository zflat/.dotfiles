;; Utility functions

(defun sudo-shell-command (command)
  (interactive "MShell command (root): ")
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command command)))


(defun window-and-buffer-kill ()
  "Kill the active buffer and its window pane"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))


;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))



;; http://www.emacswiki.org/emacs/buffer-extension.el
;; http://stackoverflow.com/a/9411825
(defun copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javadoc helper
;; http://stackoverflow.com/a/19567306
(defun prefix-javadoc-return () 
  "Advanced C-m for Javadoc multiline comments.   
Inserts `*' at the beggining of the new line if 
unless return was pressed outside the comment"
  (interactive)
  (setq last (point))
  (setq is-inside
        (if (search-backward "*/" nil t)
        ;; there are some comment endings - search forward
            (search-forward "/*" last t)
          ;; it's the only comment - search backward
          (goto-char last)
          (search-backward "/*" nil t)
      )
    )
  ;; go to last char position
  (goto-char last)
  ;; the point is inside some comment, insert `* '
  (if is-inside
      (progn 
    (insert "\n* ")
    (indent-for-tab-command))
    ;; else insert only new-line
    (insert "\n")))
(add-hook 'c-mode-common-hook (lambda () 
  (local-set-key "\r" 'prefix-javadoc-return)))
(add-hook 'php-mode-hook (lambda () 
  (local-set-key "\r" 'prefix-javadoc-return)))
(add-hook 'js-mode-hook (lambda () 
  (local-set-key "\r" 'prefix-javadoc-return)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

