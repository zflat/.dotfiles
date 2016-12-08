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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Themes
;;
;; Change with M-x load-theme RET {themename}
(load-theme 'zenburn t)
;;(load-theme 'solarized-dark t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(helm-buffers-fuzzy-matching t)
 '(helm-buffers-list-fuzzy-match t)
 '(helm-find-files-fuzzy-match t)
 '(helm-imenu-fuzzy-match t)
 '(helm-mini-fuzzy-matching t)
 '(helm-recentf-fuzzy-match t)
 '(inhibit-startup-screen t)
 '(tramp-mode nil))

;; Note: Setup windows splitting preferences
;; 
;; `customize-group [RET] Windows`
;; Split Height Threshold:
;;   default: 80
;;   always split horizontal: nil 
;; Split Width Threshold 
;;   default: 160
;;   always split horizontal: 0



;;;;;;;;;;;;;;



;; Set font
;; http://askubuntu.com/questions/23603/how-to-change-font-size-in-emacs
(set-default-font "DejaVu Sans Mono 12")
(set-face-attribute 'default nil :height 130)


;; current buffer name in title bar
(setq frame-title-format "%b")

;; All indentation made with spaces
(setq-default indent-tabs-mode nil) ;; setq vs setq-default? 
(setq tab-width 2)
(setq default-tab-width 2)
(setq js-indent-level 2) ;; Use 2 spaces for javascript files
(setq js2-basic-offset 2) ;; Use 2 spaces for javascript files in js2-mode?
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(setq js2-strict-trailing-comma-warning nil)


;; Start emacs to fill the screen
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system ; see also http://stackoverflow.com/a/5795518
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 100)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)
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

;; Move cursor to different Panes by Arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; show the current directory in the frame bar
;; see http://stackoverflow.com/a/8945306
(setq frame-title-format '((:eval default-directory)))

;; Customizing backup settings
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

; (global-linum-mode 1) ; always show line numbers

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))


(require 'cask "~/.cask/cask.el")
(cask-initialize)

; (setq tramp-default-method "ftp")
;(eval-after-load "tramp"
;  '(debug))

;; consider also using pallet 
;; https://github.com/rdallasgray/pallet

;;;
;;; Packages configuration / Initialization
;;;


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode t)

(require 'geben)
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)
(global-set-key [f5] 'geben)

;; Muliple cursors
(require 'multiple-cursors)
; Cursor at each line in selected region
; Note: <S> is shift
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) 
; Use arrow keys to quickly mark/skip next/previous occurances.
(global-set-key (kbd "C-S-c C-s") 'mc/mark-more-like-this-extended) 
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Iedit - Edit multiple regions in the same way simultaneously
(require 'iedit)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Line duplication
(global-set-key (kbd "M-<up>") 'md/move-lines-up)
(global-set-key (kbd "M-<down>") 'md/move-lines-down)
(global-set-key (kbd "C-S-d <down>") 'md/duplicate-down)
(global-set-key (kbd "C-S-d <up>") 'md/duplicate-up)


;; Code folding
(require 'origami)

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

(require `helm-swoop)


;; Enable Projectile
;;
;; list of commands: C-c p C-h
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(helm-projectile-on)
;; (setq helm-projectile-fuzzy-match nil)
(global-set-key [f9] 'helm-do-ag-project-root)
(global-set-key (kbd "C-<f6> s") 'helm-do-ag-project-root)
;(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key [f8] 'helm-projectile-find-file)
(global-set-key (kbd "C-<f6> f") 'helm-projectile-find-file)
;; Note: Invalidate Projectile cache with  [C-c p i]

(require 'helm-gtags)
(helm-gtags-mode 1)

;; Auto-complete
(require 'auto-complete-config)
;;  Loading dictionary based on directory location...
;;  https://github.com/lehoff/emacs-cask/blob/master/configs/init-auto-complete.el
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/.cask/24.4.1/elpa/auto-complete-20160107.8/dict")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 4 characters of a word
(setq ac-auto-start 4)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)


(require 'avy)
(global-set-key (kbd "M-s") 'avy-goto-char)
(setq avy-background t)


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
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))
        ))


(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)

(require 'php-mode)
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook (lambda()(ggtags-mode 1)))
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'ac-php)
             ;(company-mode 0)
             (auto-complete-mode t)
             (setq ac-sources  '(ac-source-php ) )
             ;(add-to-list 'company-backends 'company-ac-php-backend )
             ))
             
;; http://prak5190.github.io/p/jsemacs/
;; npm install -g tern
(require 'tern)
(eval-after-load 'tern
    '(progn
    (require 'tern-auto-complete)
    (tern-ac-setup)))
(add-to-list 'tern-command "--no-port-file" 'append) ;; from https://github.com/syl20bnr/spacemacs/pull/3465
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'tern-mode)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(require 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

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

(setq tramp-mode nil)

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

;;  Keybindings Notes
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
(diminish 'helm-mode)
(diminish 'yas-minor-mode)
