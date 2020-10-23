;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;
;; Configure Emacs
;; Also can invoke `M-x Custom`
;;
;; Usefull init options
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html
;; -q Do not load any initialization file
;; --debug-init Enable the Emacs List Debugger
;;
;; Apply changes
;; `M-x load-file` init.el
;;
;; TODO: http://cachestocaches.com/2015/8/getting-started-use-package/

;;; Code:

;; TODO http://cachestocaches.com/2015/8/getting-started-use-package/

(setq load-prefer-newer t)

;; Debugging triggers
;; (setq debug-on-error t)
;; (setq debug-on-quit t) to let C-g trigger debug
(defun clear-debug-triggers ()
  (interactive)
  (setq debug-on-error nil
        debug-on-signal nil
        debug-on-quit nil))


;; slow-down due to TRAMP bug: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=810640
(setq tramp-ssh-controlmaster-options nil)

(setq-default gc-cons-threshold 100000000)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

;; Check if running with X support (featurep 'x)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(inhibit-startup-screen t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838"))))


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


;; Buffer switching
;; See https://www.emacswiki.org/emacs/SwitchingBuffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
; (global-set-key (kbd "<f1>") 'switch-to-previous-buffer)
(global-set-key (kbd "<backtab>") 'switch-to-buffer)


;; show the current directory in the frame bar
;; see http://stackoverflow.com/a/8945306
(setq frame-title-format '("-emacs- " (:eval default-directory)))


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

;;; coding systems
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Scrolling tweaks
;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)
;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)


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


;;; auth-source config
;;; use pass (~/.password-store)
;;; (see The Unix password store)
(setq auth-sources '(password-store))


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
(global-hl-line-mode t) ; highlight the line at point

;; parenthesis customization
;; consider also http://www.emacswiki.org/emacs/HighlightParentheses
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
; (set-face-background 'show-paren-match-face (face-background 'default))

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
    ; (when matching-text (message matching-text)) ;; see https://emacs.stackexchange.com/a/28532
))


;; TODO use git-gutter ?
; (global-set-key (kbd "M-p") 'git-gutter:previous-hunk)
; (global-set-key (kbd "M-n") 'git-gutter:next-hunk)

(setq auto-save-visited-interval 5)
(setq auto-save-visited-file-name nil) ; explicitly disable a setting which would disable in-place autosaving.
(auto-save-visited-mode 1)

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

;; (when (version<= "26.0.50" emacs-version )
;;   (global-display-line-numbers-mode))


;; Making buffer names unique
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; See also http://www.lonecpluspluscoder.com/2014/08/23/unique-buffer-names-in-emacs/


; (setq vc-git-annotate-switches '("--minimal -w -c --date=short --abbrev=0"))


; make vertical split the default for edif
; NOTE
;   Open ediff from magit: press e on an unmerged (due to conflicts)
;   file from the status window during a merge/rebase/cherry-pick


(setq-default show-trailing-whitespace nil)

(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))


(require 'cask "~/.cask/cask.el")
(cask-initialize)
; (or (load-file (buffer-file-name)) (package-initialize))

;;;
;;; Packages configuration / Initialization
;;;


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode t)

;; Muliple cursors
(require 'multiple-cursors)
;; Cursor at each line in selected region
;; Note: <S> is shift
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; Use arrow keys to quickly mark/skip next/previous occurances.
(global-set-key (kbd "C-S-c C-s") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "s-/") `set-rectangular-region-anchor)
                                        ; setup multiple-cursors-hydra https://iqss.github.io/IQSS.emacs/init.html

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; Line duplication
(require 'move-dup)
(global-set-key (kbd "M-<up>")       'md-move-lines-up)
(global-set-key (kbd "M-<down>")     'md-move-lines-down)
(global-set-key (kbd "C-S-d <down>") 'md-duplicate-down)
(global-set-key (kbd "C-S-d <up>")   'md-duplicate-up)


;; Elscreen
(require 'elscreen)
(elscreen-start)
(setq elscreen-display-tab nil)
(global-set-key (kbd "C-z C-z") 'elscreen-toggle)

;; Transpose Frame
(require 'transpose-frame)

;; Close popup windows with C-g
(require 'popwin)
(popwin-mode 1)
(push '(ag-mode :dedicated t :stick t :position bottom) popwin:special-display-config)
                                        ;(pop popwin:special-display-config)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/mule-info nil)
(line-number-mode t)

;; (car (vc-git-branches))
;; (mood-line--update-vc-segment)
;; (message mood-line--vc-text)

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
(setq bm-highlight-style 'bm-highlight-line-and-fringe)

;; Visual Mark -- highlights the current mark, and N-previous marks
;; http://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "dark green"))) "")
(defface visible-mark-face1
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "plum4"))) "")
(defface visible-mark-face2
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "tan4"))) "")
(defface visible-mark-face3
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "DarkSlateGray4"))) "")
(setq visible-mark-max 3)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2 visible-mark-face3))
(require 'visible-mark)


(require 'zoom)
(zoom-mode 1)
(setq zoom-size '(0.666 . 0.666))
(global-set-key (kbd "C-<f1>") 'zoom-mode) ; toggle zoom mode easily

(require 'ag)

(require 'flx) ; scoring mechanism from flx is used by ivy–regex-fuzzy
(require 'ivy-hydra)
(require 'ivy)
(require 'ivy-pass)
(global-set-key (kbd "<C-f12>") 'password-store-copy)

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



(require 'ripgrep)

(add-to-list 'ripgrep-arguments "-M 120")
(defun my-projectile-ripgrep (regexp)
  "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root. Copied from projectile-ripgrep."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))))
  (ripgrep-regexp regexp
                  (projectile-project-root)
                  (mapcar (lambda (val) (concat "--glob \!" val))
                          (append projectile-globally-ignored-files
                                  projectile-globally-ignored-directories
                                  (projectile-project-ignored)
                                  ; (projectile-paths-to-ignore)
                                  ))))

;; Open search result in the same window
;; https://emacs.stackexchange.com/a/33908
(defun my-compile-goto-error-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))
(defun my-compilation-mode-hook ()
  (local-set-key (kbd "o") #'my-compile-goto-error-same-window))
(add-hook 'compilation-mode-hook #'my-compilation-mode-hook)


(require 'editorconfig)
(editorconfig-mode 1)

;; Enable Projectile
;;
;; list of commands: C-c p C-h
(require 'projectile)
(require 'counsel-projectile)
(counsel-projectile-mode)
(setq projectile-completion-system 'ivy)
;; command used to get the file for projectile
;; also consider https://www.emacswiki.org/emacs/FileSets
;; (defun projectile-get-ext-command () "find . -type f -print0")
(defun projectile-get-ext-command (&optional arg) "rg . --no-require-git --null --files") ;; See https://emacs.stackexchange.com/a/29200
;; note that I added the optional arg to projectile-get-ext-command after upgrading ?

;; Speed up? find-file
;; See https://github.com/syl20bnr/spacemacs/issues/4207
(setq shell-file-name "/bin/sh")

(projectile-mode)
(setq projectile-enable-caching t)
;; (setq helm-projectile-fuzzy-match nil)
(global-set-key [f9] 'ag-project)
(global-set-key (kbd "C-<f9>") 'ag-project-regexp)
(global-set-key (kbd "C-<f6> s") 'my-projectile-ripgrep)
(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
;(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
  ;; (global-set-key [f8] 'helm-projectile-find-file)
  ;(global-set-key (kbd "C-<f6> f") 'projectile-find-file-dwim)
(global-set-key (kbd "C-<f6> f") 'counsel-projectile-find-file)


;; Note: Invalidate Projectile cache with  [C-c p i]

; (require 'find-file-in-project) ;; TODO install and configure and compare w/ projectile
; (global-set-key (kbd "C-<f6> e") 'find-file-in-project)

; (global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(defun wrapped-ivy-immediate-done (&rest ignore)
    (interactive)
    (if ( > (minibuffer-depth) 0) (ivy-immediate-done) nil))
(global-set-key (kbd "C-c C-f") 'wrapped-ivy-immediate-done) ;; useful for creating a new file
(global-set-key (kbd "C-c C-r") 'ivy-resume)


                                        ; Updating tags via git hook: https://stackoverflow.com/q/42680131

;; (with-eval-after-load 'helm-gtags
;;   (global-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;   (global-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;   (global-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
;;   (global-set-key (kbd "C-c <") 'helm-gtags-previous-history)
;;   (global-set-key (kbd "C-c >") 'helm-gtags-next-history)
;;   (global-set-key (kbd "M-,") 'helm-gtags-pop-stack))
;; (add-hook 'php-mode-hook 'helm-gtags-mode)
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (require 'helm-gtags)
;; (helm-gtags-mode t)

(require 'avy)
(global-set-key (kbd "M-s") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(setq avy-background t)

(require 'beacon)
(beacon-mode 1)
(setq beacon-dont-blink-commands
   (quote
    (next-line previous-line forward-line mwheel-scroll)))
(setq beacon-color "LightGoldenrod3")

(require 'company)
(setq company-idle-delay 0.2)

(require 'geben)
(setq geben-display-window-function 'popwin:switch-to-buffer)

(require 'php-extras)
(require 'php-mode)
(setq ac-php-php-executable (executable-find "php7"))  ; NOTE: use php-build to build php 7 and then symlink the binary to /usr/local/bin
(add-hook 'php-mode-hook 'ggtags-mode)
; (add-hook 'php-mode-hook 'visible-mark-mode)

(progn
  (remove-hook 'php-mode-hook 'php-mode-hook-codestyle)
  (defun php-mode-hook-codestyle ()
    "Set up coding style for php-mode"
    (progn ;; Make sure the editorconfig happens after psr2 coding style is applied
      (php-enable-psr2-coding-style)
      (editorconfig-apply)))
  (add-hook 'php-mode-hook 'php-mode-hook-codestyle))

(progn
  (remove-hook 'php-mode-hook 'php-mode-hook-autocomplete)
  (defun php-mode-hook-autocomplete ()
    "Set up autocomplete for php-mode"
    (unless (file-remote-p default-directory 'host) ; Get hostname when using TRAMP mode
      ;; Enable company-mode
      (company-mode t)
      (require 'company-php)
      ;; Enable ElDoc support (optional)
      ;; (ac-php-core-eldoc-setup)
      (set (make-local-variable 'company-backends)
           '(company-ac-php-backend company-capf))))
  (add-hook 'php-mode-hook 'php-mode-hook-autocomplete))

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(require 'js2-mode)
;; Enable js2-mode like this instead of with js-mode-hook so that
;; qml-mode, which uses js-mode does not also include js2-mode. May
;; need to add more file extensions in the future, but for now it is
;; nice that js2-mode is not enabled for JSON files too.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
; (add-hook 'js-mode-hook 'visible-mark-mode)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.qrc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . web-mode))
(setq web-mode-engines-alist '
      (("php" . "\\.phtml\\'")
       ("blade" . "\\.blade\\.")
       ("riot" . "\\.tag\\'"))
      )

(add-to-list 'auto-mode-alist '("/checkout/src/.*\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist
  '(("jsx"  . "/src.*/components/.*\\.js[x]?\\'")))


; web-mode customization
(setq web-mode-enable-auto-indentation nil)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (editorconfig-apply))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(set-face-attribute 'web-mode-current-column-highlight-face nil :background (face-attribute 'hl-line :background))


(require 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'qml-mode)
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(add-hook 'qml-mode-hook '(lambda ()
                           (js2-mode-exit)
                           (message "js2-mode exited")))
(define-key qml-mode-map (kbd "<f5>") 'recompile)

(require 'flycheck)

; C++ w/ RTags (sort of automated with cmake-ide...)
; https://github.com/atilaneves/cmake-ide
; http://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html
;
; https://oracleyue.github.io/2017/12/04/emacs-init-cc-irony/
(require 'rtags)
(require 'flycheck-rtags)
(require 'ivy-rtags)
(setq rtags-display-result-backend 'ivy)
(if
    (and (rtags-executable-find "rc") (rtags-executable-find "rc"))
    (progn
      (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
      (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
      (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
      (define-key c-mode-base-map (kbd "M-*") 'rtags-location-stack-back)
      (rtags-enable-standard-keybindings)
      (add-hook 'c++-mode-hook
                '(lambda ()
                   (require 'company-rtags)
                   (company-mode t)
                   (setq rtags-autostart-diagnostics t)
                   (rtags-diagnostics)
                   (setq rtags-completions-enabled t)
                   (add-to-list 'company-backends 'company-rtags)))
      (defun setup-flycheck-rtags ()
        (flycheck-select-checker 'rtags)
        (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
        (setq-local flycheck-check-syntax-automatically nil)
        (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
        )
      (add-hook 'c++-mode-hook #'setup-flycheck-rtags))
  ;; (progn
  ;;   (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
  ;;   (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!")))
  nil
  )
(progn
  (remove-hook 'c++-mode-hook 'c++-mode-hook-codestyle)
  (defun c++-mode-hook-codestyle ()
    "Set up coding style for c++-mode"
    (progn ;; Make sure the editorconfig happens after other coding style is applied
      (define-key c-mode-base-map [ret] 'newline-and-indent)
      (editorconfig-apply)))
  (add-hook 'c++-mode-hook 'c++-mode-hook-codestyle))
(define-key c++-mode-map (kbd "<f5>") 'recompile)

(require 'cmake-ide)
;(setq cmake-ide-cmake-command "/home/local/RESQUARED/william.wedler/cmake-install/bin/cmake")
(setq cmake-ide-cmake-command "/home/local/RESQUARED/william.wedler/.dotfiles-private/bin/cmake-catkin")
; (list "cmake" "*cmake*" cmake-ide-cmake-command)
(cmake-ide-setup)

; (global-flycheck-mode t)
(require 'cmake-mode)
; (require 'cmake-font-lock) ;; see https://github.com/gonsie/dotfiles/blob/master/emacs/init.el
;; CMake
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; See ROSmacs
;; https://wiki.ros.org/rosemacs
(add-to-list 'auto-mode-alist '("\\.msg\\'" . gdb-script-mode))


;; ggtags config:
;; http://emacs.stackexchange.com/q/14685
;; https://www.reddit.com/r/emacs/comments/4qerou/programmatically_createupdate_a_tags_file/d5b7m23/
;; http://spacemacs.org/layers/+tags/gtags/README.html
;; Exclude dirs: https://github.com/syl20bnr/spacemacs/issues/3273#issuecomment-145984669
;;
;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
(ggtags-mode 1)
;(global-set-key (kbd "M-*") 'xref-pop-marker-stack) ; could also be 'pop-tag-mark
;; TODO also use (rtags-location-stack-back) when in c++ mode?

(setq ggtags-completing-read-function nil)
                                        ; (setenv "GTAGSLIBPATH" "/showclix/config:/showclix/src:/showclix/tests/active_tests:/showclix/settings:/showclix/schema_evolutions:/showclix/public_html/classes:/showclix/public_html/actions:/showclix/public_html/templates:/showclix/public_html/controller")
(setenv "GTAGSLIBPATH" nil)


(require 'smooth-scrolling)
(smooth-scrolling-mode t)
;; todo https://superuser.com/questions/134921/smooth-scrolling-in-emacs-windows

;; Also increase speed by changing X-window repeat rate
;; xset r rate 500 75

;; Getting markdown preview to work...
;; http://www.maheshsubramaniya.com/article/install-markdown-for-emacs.html
(require 'markdown-mode)
(setq markdown-command "multimarkdown")

(global-set-key (kbd "C-x g") 'magit-status)
(global-magit-file-mode t)
(setq magit-completing-read-function 'ivy-completing-read)
; (setq magit-completing-read-function 'magit-ido-completing-read)

(add-hook 'web-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'php-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))

(require 'string-inflection)
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

(require 'ess)

(require 'protobuf-mode)

(setq clang-format-executable (concat (getenv "HOME") "/.pyenv/versions/3.8.5/bin/clang-format"))
(require 'clang-format)

;; c-mode indentation https://stackoverflow.com/a/664525
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro 2)
  )
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; Auto-complete notes:
;; https://github.com/lehoff/emacs-cask/blob/master/configs/init-auto-complete.el
;; http://crypt.codemancers.com/tags/emacs.html


;; Note: Setup windows splitting preferences
;;
;; `customize-group [RET] Windows`
;; Split Height Threshold:
;;   default: 80
;;   always split horizontal: nil
;; Split Width Threshold
;;   default: 160
;;   always split horizontal: 0



;; (global-set-key (kbd "M-<f6>") nil)
(global-set-key (kbd "M-<f6>") 'imenu)
(global-set-key (kbd "C-x C-q") 'kill-buffer-and-window)


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
;; Indent region after yank:
;; Indent region on active text:
;;   C-M-\
;; Or highlight yanked then tab:
;;   C-x C-x, TAB
;;
;; Adjust the text indentation in the region using indent-rigidly
;; C-x TAB, <left> OR <right> OR <S-left> OR <S-right>
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
;; clone-indirect-buffer-other-window
;; C-x 4 c
;; kill-buffer-and-window
;; C-x 4 0
;;
;; iconify-frame (minimize)
;; C-x C-z
;;
;; Moving around with the Mark
;; C-u C-space move point to previous mark
;;
;; Org-mode
;; C-c C-t cycle item status from none to TODO to DONE
;;

;;
;; Load additional init files
(load "~/.emacs.d/util.el")

(global-set-key (kbd "C-s-e") 'xah-show-in-desktop)


(require 'go-back-buffer "~/.emacs.d/packages/go-back-buffer/go-back-buffer.el")
(global-set-key (kbd "<f1>") 'gbb--display-prev-buffer)
(advice-add 'set-window-buffer :before 'gbb--update-history)
(advice-add 'delete-window :before 'gbb--cleanup-history)

;; mouse vs keyboard
;; https://www.reddit.com/r/emacs/comments/4f2iee/efficient_use_of_multibutton_mice_with_emacs/d260em2/

;; See https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/cznzmh9/
(require 'mouse-copy)
(global-set-key [C-down-mouse-1] 'mouse-drag-secondary-pasting)
(global-set-key [C-S-down-mouse-1] nil)
(global-set-key [C-M-down-mouse-1] 'mouse-drag-secondary-moving)



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
(diminish 'company-mode)
(diminish 'projectile)
(diminish 'projectile-mode)
(diminish 'yas-minor-mode)
(diminish 'editorconfig-mode)
(diminish 'ivy-mode)
(diminish 'flycheck-mode)
(diminish 'eldoc-mode)
(diminish 'ggtags-mode)
(diminish 'beacon-mode)
(diminish 'emmet-mode)



;; Speed up emacs start:
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; https://github.com/jschaf/esup
;; byte-compile .emacs.d directory: C-u 0 M-x byte-recompile-directory

;; TODO only consider the top 3 marks on the mark ring
;; ALSO TODO consider using AutoMark
(defun buffer-order-next-mark (arg)
    (interactive "p")
    (when (mark)
    (let* ((p (point))
         (m (mark))
         (n p)
         (count (if (null arg) 1 arg))
         (abscount (abs count))
         (rel
          (funcall
           (if (< 0 count) 'identity 'reverse)
           (sort (cons (cons 0 p)
                       (cons (cons (- m p) m)
                             (if mark-ring
                                 (mapcar (lambda (mrm)
                                           (cons (- mrm p) mrm))
                                         mark-ring)
                               nil)))
                 (lambda (c d) (< (car c) (car d))))))
          (cur rel))
         (while (and (numberp (caar cur)) (/= (caar cur) 0))
           (setq cur (cdr cur)))
         (while (and (numberp (caadr cur)) (= (caadr cur) 0))
           (setq cur (cdr cur)))
         (while (< 0 abscount)
           (setq cur (cdr cur))
           (when (null cur) (setq cur rel))
           (setq abscount (- abscount 1)))
         (when (number-or-marker-p (cdar cur))
           (goto-char (cdar cur))))))
(defun buffer-order-prev-mark (arg)
  (interactive "p")
  (buffer-order-next-mark
   (if (null arg) -1 (- arg))))
(global-set-key [s-down] 'buffer-order-next-mark)
(global-set-key [s-up] 'buffer-order-prev-mark)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Themes
;;

(defun load-theme-solarized-dark ()
  (interactive)
  (progn
    (require `solarized-theme)
    (load-theme 'solarized-dark t)

    (face-spec-set 'bm-face '((t (:foreground "gold" :overline nil))))

    (setq beacon-color "LightGoldenrod3")

    (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
    (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link)))
  (when (fboundp 'sml/setup) (sml/setup)))

(defun load-theme-solarized-light ()
  (interactive)
  (progn
    (require `solarized-theme)
    (load-theme 'solarized-light t)

    (face-spec-set 'bm-face '((t (:foreground "maroon" :overline nil))))

    (setq beacon-color (face-foreground 'menu))

    (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
    (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link)))
  (when (fboundp 'sml/setup) (sml/setup)))

(defun load-theme-zenburn ()
  (interactive)
  (progn
    (require `zenburn-theme)
    (load-theme 'zenburn t)

    (setq beacon-color "LightGoldenrod3")

    (set-face-background 'avy-background-face (face-background 'menu))
    (set-face-foreground 'avy-background-face (face-foreground 'menu))
    (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
    (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link))

    (set-face-attribute 'ivy-current-match nil :background (face-background 'default))
    (set-face-attribute 'ivy-minibuffer-match-face-1 nil :background "plum4")
    (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground "white smoke")
    (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background "RosyBrown4")
    (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground "white smoke")
    (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background "DarkSlateGray4")
    (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background "DodgerBlue4")
    (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground "white smoke")

    (set-face-attribute 'web-mode-symbol-face nil :foreground "SeaGreen")

    (face-spec-set 'bm-face   '((t (:foreground "gold" :background "gray20" :overline nil))))
    (face-spec-set 'linum     '((t (:background "#3F3F3F" :foreground "#808080"))))
    (face-spec-set 'hi-blue   '((t (:foreground "light blue" :background "MidnightBlue"))))
    (face-spec-set 'hi-blue-b '((t (:foreground "light blue" :background "MidnightBlue" :weight bold))))
    (face-spec-set 'hi-green  '((t (:foreground "PaleGreen1" :background "DarkOliveGreen"))))
    (face-spec-set 'hi-pink   '((t (:foreground "pink" :background "gray20"))))
    (face-spec-set 'hi-red-b  '((t (:foreground "white" :background "dark red" :weight bold))))
    (face-spec-set 'hi-yellow '((t (:foreground "yellow1" :background "gray20" :weight bold))))

    (customize-set-variable 'vc-annotate-background "#2B2B2B")
    (customize-save-variable 'vc-annotate-color-map
                             (quote
                              ((20 . "#BC8383")
                               (40 . "#CC9393")
                               (60 . "#DFAF8F")
                               (80 . "#D0BF8F")
                               (100 . "#E0CF9F")
                               (120 . "#F0DFAF")
                               (140 . "#5F7F5F")
                               (160 . "#7F9F7F")
                               (180 . "#8FB28F")
                               (200 . "#9FC59F")
                               (220 . "#AFD8AF")
                               (240 . "#BFEBBF")
                               (260 . "#93E0E3")
                               (280 . "#6CA0A3")
                               (300 . "#7CB8BB")
                               (320 . "#8CD0D3")
                               (340 . "#94BFF3")
                               (360 . "#DC8CC3"))))
    (customize-set-variable 'vc-annotate-very-old-color "#DC8CC3")
    (customize-set-variable 'ansi-color-names-vector
                            ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
    (customize-set-variable 'fci-rule-color "#383838")
    (customize-set-variable 'nrepl-message-colors
                            (quote
                             ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))))
  (when (fboundp 'sml/setup)
    (sml/setup)
    (sml/apply-theme 'respectful)))

(color-values (face-background 'highlight)) ; debug a face background


;; Set font
;; http://askubuntu.com/questions/23603/how-to-change-font-size-in-emacs
(if (find-font (font-spec :name "Hack"))
               (set-frame-font "Hack 12")
               (if (find-font (font-spec :name "DejaVu Sans Mono"))
                   (set-frame-font "DejaVu Sans Mono 12")))
(set-face-attribute 'default nil :height 130)


(add-hook 'after-init-hook 'load-theme-solarized-dark)
;; Note: Change theme with M-x load-theme RET {themename}


(message "load_end")

; (provide 'init)
;;; init.el ends here

