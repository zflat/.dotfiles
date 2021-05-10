;; Utility functions

;; Pull from PRIMARY (same as middle mouse click)
;; http://stackoverflow.com/a/29793391
(defun paste-primary-selection ()
  (interactive)
  (insert
   (x-get-selection 'PRIMARY)))
(global-set-key (kbd "S-<insert>") 'paste-primary-selection)

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
;; https://stackoverflow.com/a/2382677
;; https://stackoverflow.com/a/35392142
(defun get-buffer-file-path (arg &optional file)
  (let ((new-kill-string)
        (name (or file (if (eq major-mode 'dired-mode)
                           (dired-get-filename)
                         (or (buffer-file-name) ""))))
        (proj-name (if (and (fboundp 'projectile-project-root)
                            (projectile-project-root))
                       (concat
                        (file-name-as-directory
                         (file-name-nondirectory (directory-file-name (projectile-project-root))))
                        (string-remove-prefix (projectile-project-root) name))
                     "")))
    (cond ((string-equal arg "p")
           proj-name)
          ((string-equal arg "r")
           (string-remove-prefix (file-name-as-directory
                                  (file-name-nondirectory (directory-file-name (projectile-project-root))))
                                 proj-name))
          ((string-equal arg "f")
           name)
          ((string-equal arg "d")
           (file-name-directory name))
          ((string-equal arg "n")
           (file-name-nondirectory name))
          (t nil))))
(defun copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive ;; TODO: pre-compute projectile root  values and present them as options
   (list (let ((completions
                (let ((choice-index 0))
                  (mapcar (lambda (arg) (append (list (concat (number-to-string (cl-incf choice-index)) "-" (car arg))) (last arg)))
                          (append (if (and (fboundp 'projectile-project-root)
                                           (projectile-project-root))
                                      '(("Project Path" "p")
                                        ("Path from project root" "r")))
                                  '(("Name" "n") ("Full" "f") ("Directory" "d")))))))
           (cadr (assoc (completing-read "Copy buffer name as kill: " completions) completions)))))
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (file-truename (or (buffer-file-name) ""))))
        (suffix (if (and
                     (not (equal current-prefix-arg nil)) ; C-u argument given
                     (or
                      (string-equal choice "f")
                      (string-equal choice "r")
                      (string-equal choice "p")))
                    (concat ":" (number-to-string (line-number-at-pos)))))
        (new-kill-string))
        (setq new-kill-string (concat (get-buffer-file-path choice name) suffix))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))
(global-set-key (kbd "<f3>") `copy-buffer-file-name-as-kill)

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
Inserts `*' at the beggining of the new line
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
    (newline-and-indent)
))
(add-hook 'c-mode-common-hook (lambda ()
  (local-set-key "\r" 'prefix-javadoc-return))) ; M-j keyboard sequence
(add-hook 'php-mode-hook (lambda ()
  (local-set-key "\r" 'prefix-javadoc-return)))
(add-hook 'js-mode-hook (lambda ()
  (local-set-key "\r" 'prefix-javadoc-return)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sql-format-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "fsqlf --config-file ~/.dotfiles/config/fsqlf/formatting.conf" nil t)))




(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Windows Explorer, Linux file manager)
 This command can be called when in a file or in `dired'.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2018-09-29"
  (interactive)
  (let (($path default-directory))
    (cond
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" $path t t)))
     ((string-equal system-type "darwin")
      (if (eq major-mode 'dired-mode)
          (let (($files (dired-get-marked-files )))
            (if (eq (length $files) 0)
                (progn
                  (shell-command
                   (concat "open " default-directory)))
              (progn
                (shell-command
                 (concat "open -R " (shell-quote-argument (car (dired-get-marked-files ))))))))
        (shell-command
         (concat "open -R " $path))))
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram $path))
      ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for opening a remote file (w/ sudo)
;; See https://github.com/syl20bnr/spacemacs/issues/10969#issuecomment-409929968
;; https://www.reddit.com/r/emacs/comments/5v0w6h/question_how_can_i_get_magit_to_use_my_ssh_key/
(defun open-remote (username host)
  (interactive
   (open-remote-args))
  (counsel-find-file (concat "/ssh:" username "@" host ":")))
(defun open-remote-protected (username host)
  (interactive
   (open-remote-args))
  (find-remote-file username host t))
(defun find-remote-file (username host protected &optional filepath)
  (counsel-find-file (concat "/" (filepath-remote-protected username host filepath))))
(defun filepath-remote-protected (username host &optional filepath)
  (concat "ssh:" username "@" host "|sudo:" host ":/" (if filepath (get-buffer-file-path "p" filepath) nil)))
(defun open-remote-args ()
    (or
     (if (boundp 'tramp-hosts) ; See load tramp_remote_hosts.el below
         (let* ((choices (append tramp-hosts '(("Manual entry"))))
                (choice (cadr (assoc (completing-read "Select host: " choices) choices))))
           (if (listp choice) choice (list (user-login-name) choice))))
     (list (read-string "Username: ") (read-string "Hostname: "))))
; Load local file that sets 'tramp-hosts
; List format is: '(("name1" ("user" "host1")) ("name2" "host2"))
; Falls back to (user-login-name) if a user is not specified for a host
(let ((path "~/.emacs.d")
      (file "tramp_remote_hosts.el"))
  (if (locate-file file (list path))
      (load (concat path "/" file))))

(defun sudo-find-file (file-name) ; https://stackoverflow.com/a/7043786
"Like find file, but opens the file as root."
(interactive "FSudo Find File: ")
(let ((tramp-file-name (concat "/sudo:" "root"  "@localhost:" (expand-file-name file-name))))
(find-file tramp-file-name)))

; TODO function to copy a region without indentation for pasting into code snippets
; https://emacs.stackexchange.com/questions/34966/copy-region-without-leading-indentation


;; http://www.credmp.org/indexphp/2006/11/01/working_with_c/index.html
(defun cxx-create-gaurds ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq guard (concat (upcase
                         (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name)))
                        "_H"))
    (insert "#ifndef " guard  "\n")
    (insert "#define " guard  "\n")
    (goto-char (point-max))
    (insert "\n#endif // " guard "\n")))




;; Some emacs-fu
;; Insert a column of incrementing numbers in emacs
;; https://stackoverflow.com/a/9932745
;; (rectangle-number-lines)
