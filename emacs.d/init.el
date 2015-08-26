;; Configure emacs
;; Also can invoke `M-x Custom`


;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(inhibit-startup-screen t)

;; '(split-height-threshold nil)
;; '(split-width-threshold 0)

)

;; Setup windows splitting preferences
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
(set-face-attribute 'default nil :height 140)


;; current buffer name in title bar
(setq frame-title-format "%b")

;; All indentation made with spaces
(setq-default indent-tabs-mode nil) ;; setq vs setq-default? 
(setq tab-width 2)
(setq default-tab-width 2)
(setq js-indent-level 2) ;; Use 2 spaces for javascript files


;; Start emacs to fill the screen
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
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
 )

;; Move cursor to different Panes by Arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; disable the toolbar
(tool-bar-mode -1)

;; disable the scroll bar
(scroll-bar-mode -1)


;; show the current directory in the frame bar
;; see http://stackoverflow.com/a/8945306
(setq frame-title-format '((:eval default-directory)))

(show-paren-mode t)


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
      (delete-file file))))

;;; coding systems
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  MINOR MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (global-linum-mode 1) ; always show line numbers

(transient-mark-mode t) ; Standard selection-highlighting behavior of other editors.

(electric-pair-mode 1) ; Provides a way to easily insert matching delimiters

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cask "~/.cask/cask.el")
(cask-initialize)



;; consider also using pallet 
;; https://github.com/rdallasgray/pallet

;;;
;;; Packages configuration / Initialization
;;;

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

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Line duplication
(global-set-key (kbd "M-<up>") 'md/move-lines-up)
(global-set-key (kbd "M-<down>") 'md/move-lines-down)
(global-set-key (kbd "C-S-d <down>") 'md/duplicate-down)
(global-set-key (kbd "C-S-d <up>") 'md/duplicate-up)


;; Code folding
(require 'origami)


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
(global-set-key [f8] 'neotree-toggle)

;; TODO: Show the buffer directory in the mode-line
;; http://www.emacswiki.org/emacs/ModeLineDirtrack

; (require 'ag)



;; Visual Bookmarks
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'helm-mini)
;; optional fuzzy matching 
(setq helm-M-x-fuzzy-match t)
(custom-set-variables '(helm-recentf-fuzzy-match t)
                      '(helm-mini-fuzzy-matching t)
                      '(helm-buffers-fuzzy-matching t)
                      '(helm-find-files-fuzzy-match t)
                      '(helm-buffers-list-fuzzy-match t)
                      '(helm-imenu-fuzzy-match t))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-autoresize-mode 1)
(helm-mode 1)

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
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)


(require 'web-mode) 
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
  (setq web-mode-markup-indent-offset 2) ) 
(add-hook 'web-mode-hook 'my-web-mode-hook)
(set-face-attribute 'web-mode-symbol-face nil :foreground "SeaGreen")

(require 'php-mode)
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'smooth-scrolling)
;; Also increase speed by changing X-window repeat rate
;; xset r rate 500 75

(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Themes
;;
;; Change with M-x load-theme RET {themename}
(load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)


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
;; Commands notes
;;
;; After M-x cd stops emacs from tracking directories, run: M-x dirtrack-mode
;;
;;
;; C-x TAB ;; (indent-rigidly)
;; Adjust the text indentation in the region
;; <left>, <right>, <S-left>, and <S-right> 


;;
;; Load additional init files
(load "~/.emacs.d/util.el")


