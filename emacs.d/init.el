;; TODO: http://cachestocaches.com/2015/8/getting-started-use-package/

(setq-default gc-cons-threshold 100000000)

;; Configure emacs
;; Also can invoke `M-x Custom`
;;
;; Usefull init options
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html
;; -q Do not load any initialization file
;; --debug-init Enable the Emacs List Debugger

  ;;;;;;;;;;;;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; Check if running with X support (featurep 'x)


;; Set font
;; http://askubuntu.com/questions/23603/how-to-change-font-size-in-emacs
(set-default-font "DejaVu Sans Mono 12")
(set-face-attribute 'default nil :height 130)

  ;;;;;;;;;;;;;;
;; All indentation made with spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2) ;; Use 2 spaces for javascript files
(setq js2-basic-offset 2) ;; Use 2 spaces for javascript files in js2-mode?
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(setq js2-strict-trailing-comma-warning nil)


;; Move cursor to different Panes by Arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; show the current directory in the frame bar
;; see http://stackoverflow.com/a/8945306
(setq frame-title-format '((:eval default-directory)))


;; Customizing backup settings
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (ignore-errors 
        (delete-file file)))))

;;; coding systems
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; disable the toolbar
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable the scrollbar
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1)
  (if (boundp 'toggle-scroll-bar)
      (toggle-scroll-bar -1)))

;; disable the menu bar
;; Can get the meny with C-<mouse-3>
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))

(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  MINOR MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-mark-mode t) ; Standard selection-highlighting behavior of other editors.

(if (and
     (>= emacs-major-version 24)
     (>= emacs-minor-version 4))
    (electric-pair-mode 1) ; Provides a way to easily insert matching delimiters
  )
(global-hl-line-mode t) ; highlgiht


;; parenthesis customization
;; consider also http://www.emacswiki.org/emacs/HighlightParentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; How to show the matching paren when it is offscreen
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

(desktop-save-mode 1)

(when (fboundp 'winner-mode)
  (winner-mode 1)) ; C-c <left> ; for prev window layout


;; For M-x align
;; See also M-x align-regexp
;; and adding an "alignment rule" to the
;; variable align-rules-list
;; Align with spaces only
;; http://stackoverflow.com/a/8129994
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(global-auto-revert-mode t)


; make vertical split the default for edif
; NOTE
;   Open ediff from magit: press e on an unmerged (due to conflicts)
;   file from the status window during a merge/rebase/cherry-pick
(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))


(require 'cask "~/.cask/cask.el")
(cask-initialize)


;;;
;;; Packages configuration / Initialization
;;;


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode t)


;; Muliple cursors
(require 'multiple-cursors)
                                        ; Cursor at each line in selected region
                                        ; Note: <S> is shift
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) 
                                        ; Use arrow keys to quickly mark/skip next/previous occurances.
(global-set-key (kbd "C-S-c C-s") 'mc/mark-more-like-this-extended) 
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;; Line duplication
(global-set-key (kbd "M-<up>") 'md/move-lines-up)
(global-set-key (kbd "M-<down>") 'md/move-lines-down)
(global-set-key (kbd "C-S-d <down>") 'md/duplicate-down)
(global-set-key (kbd "C-S-d <up>") 'md/duplicate-up)


;; Transpose Frame
(require 'transpose-frame)

;; Close popup windows with C-g
(require 'popwin)
(popwin-mode 1)
(push '(ag-mode :dedicated t :stick t :position bottom) popwin:special-display-config)
                                        ;(pop popwin:special-display-config)

;; Automatic find file customizations:
;;
;; Stop IDO mode find file by typing C-f
;;
;; Slow down time delay for automatic file search
;; (setq ido-auto-merge-delay-time 9)
;;
;; Completely disable automatic find file
;; (setq ido-auto-merge-work-directories-length -1)


(require 'neotree)
(global-set-key [f7] 'neotree-find)
(global-set-key (kbd "<S-f7>") 'neotree-toggle)

;; TODO: Show the buffer directory in the mode-line
;; http://www.emacswiki.org/emacs/ModeLineDirtrack

;; Visual Bookmarks
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(require 'ag)
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'helm-mini)
;; optional fuzzy matching 
(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-autoresize-mode 1)
(setq helm-buffer-max-length nil)
(helm-mode 1)



;; Enable Projectile
;;
;; list of commands: C-c p C-h
(require 'projectile)
(require 'helm-projectile)
;; TODO: optimize projectile-generic-command (like use mlocate),
;; then override function (projectile-get-ext-command)
(defun projectile-get-ext-command () "find . -type f -print0")
;; Speed up? find-file
;; See https://github.com/syl20bnr/spacemacs/issues/4207
(setq shell-file-name "/bin/sh")

(projectile-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;; (setq helm-projectile-fuzzy-match nil)
(global-set-key [f9] 'helm-do-ag-project-root)
(global-set-key (kbd "C-<f6> s") 'helm-do-ag-project-root)
;(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key [f8] 'helm-projectile-find-file)
(global-set-key (kbd "C-<f6> f") 'helm-projectile-find-file)
;; Note: Invalidate Projectile cache with  [C-c p i]

(custom-set-variables
                                        ; '(helm-gtags-prefix-key "\C-t")
                                        ; '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-auto-update t))
                                        ; Updating tags via git hook: https://stackoverflow.com/q/42680131

(with-eval-after-load 'helm-gtags
  (global-set-key (kbd "M-t") 'helm-gtags-find-tag)
  (global-set-key (kbd "M-r") 'helm-gtags-find-rtag)
  (global-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
  (global-set-key (kbd "C-c <") 'helm-gtags-previous-history)
  (global-set-key (kbd "C-c >") 'helm-gtags-next-history)
  (global-set-key (kbd "M-,") 'helm-gtags-pop-stack))
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(require 'helm-gtags)
(helm-gtags-mode t)

(require 'avy)
(global-set-key (kbd "M-s") 'avy-goto-char)
(setq avy-background t)

(require 'php-mode)
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(require 'web-mode) 
(add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist '
      (("php" . "\\.phtml\\'")
       ("blade" . "\\.blade\\."))
      )
                                        ; web-mode customization
(setq web-mode-markup-indent-offset 2)
(defun my-web-mode-hook ()
  "Hooks for Web mode." 
  (setq web-mode-markup-indent-offset 2)
                                        ; (editorconfig-apply)
  ) 
(add-hook 'web-mode-hook 'my-web-mode-hook)
(set-face-attribute 'web-mode-symbol-face nil :foreground "SeaGreen")

(require 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

;; ggtags config:
;; http://emacs.stackexchange.com/q/14685
;; https://www.reddit.com/r/emacs/comments/4qerou/programmatically_createupdate_a_tags_file/d5b7m23/
;; http://spacemacs.org/layers/+tags/gtags/README.html
;; Exclude dirs: https://github.com/syl20bnr/spacemacs/issues/3273#issuecomment-145984669
;;
;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
(setq ggtags-completing-read-function nil)
                                        ; (setenv "GTAGSLIBPATH" "/showclix/config:/showclix/src:/showclix/tests/active_tests:/showclix/settings:/showclix/schema_evolutions:/showclix/public_html/classes:/showclix/public_html/actions:/showclix/public_html/templates:/showclix/public_html/controller")
(setenv "GTAGSLIBPATH" nil)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'smooth-scrolling)
;; Also increase speed by changing X-window repeat rate
;; xset r rate 500 75

;; Getting markdown preview to work...
;; http://www.maheshsubramaniya.com/article/install-markdown-for-emacs.html
(require 'markdown-mode)
(setq markdown-command "multimarkdown")

(global-set-key (kbd "C-x g") 'magit-status)
(global-magit-file-mode t)

                                        ; (setq tramp-mode nil)

(add-hook 'web-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'php-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/snippets-official"        ;; Collection from "official" submodule
        ))

(global-set-key (kbd "\M-/") 'hippie-expand)
(global-set-key (kbd "<f6>") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(setq-local hippie-expand-try-functions-list
            (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))

(require 'wolfram)
(setq  wolfram-alpha-app-id "TBD") ; Need to sign up with an account & keep this key a secret :(


;; Auto-complete notes:
;; https://github.com/lehoff/emacs-cask/blob/master/configs/init-auto-complete.el
;; http://crypt.codemancers.com/tags/emacs.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Themes
;;
;; Change with M-x load-theme RET {themename}
(require `zenburn-theme)
(load-theme 'zenburn t)
;;(require `ample-theme)
;;(load-theme 'ample-light t t)
;;(enable-theme 'ample-light)
;;(require `solarized-theme)
;;(load-theme 'solarized-light t)



;; Note: Setup windows splitting preferences
;; 
;; `customize-group [RET] Windows`
;; Split Height Threshold:
;;   default: 80
;;   always split horizontal: nil 
;; Split Width Threshold 
;;   default: 160
;;   always split horizontal: 0


(global-set-key (kbd "M-<f6>") 'imenu)

;; Keybindings Notes
;;
;; Key Help
;; get information about a key sequence
;;   C-h c (describe-key-briefly) 
;;   C-h k (describe-key).
;;
;; Note: 'C-M-...' can be captured as '<esc>, C-...'
;;
;; Zoom in/out with
;; C-x C-+
;; C-x C--
;; Tabbar explained
;; C-Alt-p 'tabbar-backward-group
;; C-Alt-n 'tabbar-forward-group
;;
;; Jump to beginning/end of file
;; C-Home
;; C-End
;;
;; List matching lines for `isearch-...`
;; Shows list in new buffer
;; Alt-s o
;;
;; Move cursor to different panes
;;
;; [Shift]+[Up Arrow]
;; [Shift]+[Down Arrow]
;; [Shift]+[Left Arrow]
;; [Shift]+[Right Arrow]
;;
;; Move to parenthesis pair
;; C-M-right ;; Also can be <esc>, C+right
;; C-M-left ;; Also can be <esc>, C+left
;;
;; C-x b<RET> ;; Opens the last edited buffer
;;
;; M-/ ;; Dyanmic Abbreviations (pressed multiple times to cycle)
;;
;; Commands notes
;;
;; After M-x cd stops emacs from tracking directories, run: M-x dirtrack-mode
;;
;;
;; C-x TAB ;; (indent-rigidly)
;; Adjust the text indentation in the region
;; <left>, <right>, <S-left>, and <S-right>
;;
;; Git Blame
;; C-x v g
;; Or
;; M-x magit-blame
;;
;; Copy/Paste from cliboard
;; See also https://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; [Shift]+[Insert]
;; M-x clipboard-yank
;; Copy to clipboard
;; M-x clipboard-kill-ring-save
;;
;; Shell mode
;;
;; previous command M-p
;; next command M-n
;;
;; ANSI mode
;;
;; C-c C-j to go into line mode
;; C-c C-k to get back into char mode


;;
;; Load additional init files
(load "~/.emacs.d/util.el")


;; emacs paste hang workaround, kinda (still need to restart)
;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://ergoemacs.org/misc/emacs_bug_cant_paste_2015.html
(setq x-selection-timeout 300)

;; Setup diminish after all modes have finished initializing
;; To list minor modes: M-x describe-mode 
(require 'diminish)
(diminish 'auto-revert-mode)
(diminish 'abbrev-mode "Abv")
(diminish 'auto-complete-mode)
(diminish 'helm-gtags-mode)
(diminish 'projectile)
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'yas-minor-mode)
(diminish 'editorconfig-mode)



;; Speed up emacs start:
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; https://github.com/jschaf/esup
;; byte-compile .emacs.d directory: C-u 0 M-x byte-recompile-directory



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffers-fuzzy-matching t)
 '(helm-buffers-list-fuzzy-match t)
 '(helm-find-files-fuzzy-match t)
 '(helm-imenu-fuzzy-match t)
 '(helm-mini-fuzzy-matching t)
 '(helm-recentf-fuzzy-match t)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "gray20" :foreground "gold"))))
 '(hi-blue ((t (:foreground "light blue" :background "MidnightBlue"))))
 '(hi-blue-b ((t (:foreground "light blue" :background "MidnightBlue" :weight bold))))
 '(hi-green ((t (:foreground "PaleGreen1" :background "DarkOliveGreen"))))
 '(hi-pink ((t (:foreground "pink" :background "gray20"))))
 '(hi-red-b ((t (:background "dark red" :foreground "white" :weight bold))))
 '(hi-yellow ((t (:foreground "yellow1" :weight bold)))))
