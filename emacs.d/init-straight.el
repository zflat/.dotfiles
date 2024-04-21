;; (setq package-enable-at-startup nil) ;; emacs.d/early-init.el
;; emacs -q -l ~/.emacs.d/init-straight.el

;; emacs --script ~/.emacs.d/init-straight.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://countvajhula.com/2020/12/27/turn-your-emacs-d-into-an-emacs-distribution-with-straight-el/
;; https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; Custmizations
;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)

;; disable the toolbar
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))

;; disable the menu bar
;; Can get the menu with C-<mouse-3> or F10
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))

;; disable the scrollbar
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1)
  (if (boundp 'toggle-scroll-bar)
      (toggle-scroll-bar -1)))
(if (boundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)
  (if (boundp 'toggle-horizontal-scroll-bar)
      (toggle-horizontal-scroll-bar -1)))

;; Turn off the bell
;; https://www.emacswiki.org/emacs/AlarmBell#h5o-5
 (setq ring-bell-function 'ignore)

;; Move cursor to different Panes by Arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; show the current directory in the frame bar
;; see http://stackoverflow.com/a/8945306
;; current directory similar to displaying in mode-line
;; http://www.emacswiki.org/emacs/ModeLineDirtrack
(setq frame-title-format '("-emacs- " (:eval default-directory)))


;;; coding systems
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Making buffer names unique
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)



;; Keyboard shortcut to toggle show/hide function content
;; See https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display column (or column 1))))
(global-set-key (kbd "<f12>") 'jao-toggle-selective-display)

;;;;;;;;;;;;;;
;; Minor modes

(transient-mark-mode t) ; Standard selection-highlighting behavior of other editors.

(if (or (>= emacs-major-version 25)
        (and (>= emacs-major-version 24)
             (>= emacs-minor-version 4)))
    (electric-pair-mode 1)) ; Provides a way to easily insert matching delimiters


(global-hl-line-mode t) ; highlight the line at point
(set-face-underline hl-line-face nil)


(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)


; (desktop-save-mode nil)

;; Customizing backup settings
;; TODO don't auto-save sensitive files https://stackoverflow.com/a/18330742
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

;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;

(straight-register-package '(transient :build (:not compile))) ;; emacs25 workaround https://github.com/magit/magit/issues/3732#issuecomment-464058617

(straight-use-package 'avy)
(straight-use-package 'beacon)
(straight-use-package 'beginend)
(straight-use-package 'cmake-font-lock)
(straight-use-package 'counsel)
(straight-use-package 'docker)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'dynamic-spaces)
(straight-use-package 'editorconfig)
(straight-use-package 'emmet-mode)
(straight-use-package 'expand-region)
(straight-use-package 'flx)
(straight-use-package 'ivy)
(straight-use-package 'ivy-hydra)
(straight-use-package 'justl)
(straight-use-package 'just-mode)
(straight-use-package 'literate-calc-mode)
(straight-use-package
 '(gc-buffers :type git
              :repo "https://codeberg.org/akib/emacs-gc-buffers.git"))
(straight-use-package 'magit)
(straight-use-package 'modern-cpp-font-lock)
(straight-use-package 'move-dup)
(straight-use-package 'multiple-cursors)
(straight-use-package 'mwim)
(straight-use-package 'neotree)
(straight-use-package 'solarized-theme)
(straight-use-package 'swiper)
(straight-use-package 'yaml-mode)
(straight-use-package 'vlf) ; very large file
(straight-use-package 'web-mode)

(require 'avy)
(global-set-key (kbd "M-s") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "<home>") 'avy-goto-char-timer)
(setq avy-background t)

;; Smart move to the beginning or end of lines and top of buffer
(require 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

(require 'beacon)
; see also https://github.com/rolandwalker/nav-flash
(beacon-mode 1)
(setq beacon-dont-blink-commands
   (quote
    (next-line previous-line forward-line mwheel-scroll)))


;; Muliple cursors
(require 'multiple-cursors)
;; Cursor at each line in selected region
;; Note: <S> is shift
;; See also https://github.com/fgallina/region-bindings-mode/ for a possibly easier way to invoke mc on selected region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; Use arrow keys to quickly mark/skip next/previous occurances.
(global-set-key (kbd "C-S-c C-s") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "s-/") `set-rectangular-region-anchor)
                                        ; setup multiple-cursors-hydra https://iqss.github.io/IQSS.emacs/init.html

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region) ;; Also can use minus to contract


;; Line duplication
(require 'move-dup)
(global-set-key (kbd "M-<up>")       'move-dup-move-lines-up)
(global-set-key (kbd "ESC <up>")       'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>")     'move-dup-move-lines-down)
(global-set-key (kbd "ESC <down>")     'move-dup-move-lines-down)
(global-set-key (kbd "C-S-d <down>") 'move-dup-duplicate-down)
(global-set-key (kbd "ESC C-<down>") 'move-dup-duplicate-down)
(global-set-key (kbd "C-S-d <up>")   'move-dup-duplicate-up)
(global-set-key (kbd "ESC C-<up>")   'move-dup-duplicate-up)

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)


(require 'flx) ; scoring mechanism from flx is used by ivy–regex-fuzzy
(require 'ivy-hydra)
(require 'ivy)

(require 'counsel)
(require 'swiper)
(add-to-list 'swiper-font-lock-exclude 'php-mode)
(ivy-mode 1)
; see https://oremacs.com/2016/01/06/ivy-flx/
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (counsel-projectile-find-file . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
(setq ivy-format-function 'ivy-format-function-line)
(setq ivy-use-virtual-buffers t) ;; see also https://emacs.stackexchange.com/questions/36836/how-to-remove-files-from-recentf-ivy-virtual-buffers
(setq ivy-virtual-abbreviate 'full) ;; helps to know files are from recentf instead of an open buffer
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(defun wrapped-ivy-immediate-done (&rest ignore)
    (interactive)
    (if ( > (minibuffer-depth) 0) (ivy-immediate-done) nil))
(global-set-key (kbd "C-c C-f") 'wrapped-ivy-immediate-done) ;; useful for creating a new file
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(straight-use-package 'ivy-xref)
(require 'ivy-xref)

(require 'beginend)
(beginend-global-mode)

(straight-use-package 'dumb-jump)
(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(straight-use-package 'flycheck)
(require 'flycheck)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'dynamic-spaces)

(require 'cmake-font-lock)

(require 'web-mode)

(require 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(require 'docker)
(global-set-key (kbd "C-c d") 'docker)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$'" . yaml-mode))

(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)
(add-to-list 'modern-c++-operators "=")
(add-to-list 'modern-c++-operators "->")
(add-to-list 'modern-c++-operators "&&")
(add-to-list 'modern-c++-operators "||")
(add-to-list 'modern-c++-operators "<<")


(require 'just-mode)
(require 'justl)

;; Set font
;; http://askubuntu.com/questions/23603/how-to-change-font-size-in-emacs
(if (find-font (font-spec :name "Hack"))
               (set-frame-font "Hack 12")
               (if (find-font (font-spec :name "DejaVu Sans Mono"))
                   (set-frame-font "DejaVu Sans Mono 12")))
(set-face-attribute 'default nil :height 130)

(gc-buffers-mode)

(defun load-theme-solarized-dark ()
  (interactive)
  (progn
    (require `solarized-theme)
    (load-theme 'solarized-dark-high-contrast t)

    (face-spec-set 'bm-face '((t (:foreground "gold" :overline nil))))

    (setq beacon-color "LightGoldenrod3")

    (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
    (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link))
    (set-face-attribute 'web-mode-current-column-highlight-face nil :background (face-attribute 'hl-line :background)))
  (when (fboundp 'sml/setup) (sml/setup)))

(defun load-theme-solarized-light ()
  (interactive)
  (progn
    (require `solarized-theme)
    (load-theme 'solarized-light t)

    (face-spec-set 'bm-face '((t (:foreground "maroon" :overline nil))))

    (setq beacon-color (face-foreground 'menu))

    (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
    (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link))
    (set-face-attribute 'web-mode-current-column-highlight-face nil :background (face-attribute 'hl-line :background)))
  (when (fboundp 'sml/setup) (sml/setup)))



; (add-hook 'after-init-hook 'load-theme-solarized-dark)
(if (display-graphic-p)
    (load-theme-solarized-dark)
  (load-theme 'solarized-zenburn t))

(message "")
