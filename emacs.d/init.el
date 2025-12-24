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

;; Prevent *Compile-Log* warnings during package loading
(setq byte-compile-warnings nil)
;; For good measure, also silence comp async warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; TODO http://cachestocaches.com/2015/8/getting-started-use-package/

(setq load-prefer-newer t)

;; Debugging triggers
;; (setq debug-on-error t)
;; (setq debug-on-quit t) to let C-g trigger debug
(defun set-debug-triggers ()
  (interactive)
  (setq debug-on-error t
        debug-on-signal t
        debug-on-quit t))
(defun clear-debug-triggers ()
  (interactive)
  (setq debug-on-error nil
        debug-on-signal nil
        debug-on-quit nil))

;; Profile expressions to evaluate via C-x C-e to help with badly
;; hanging emacs. Start the profiler, switch to the problem buffer,
;; then switch back and stop the profiler.
;;
;; (profiler-start 'cpu)
;; (profiler-stop)

;; slow-down due to TRAMP bug: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=810640
(setq tramp-ssh-controlmaster-options nil)

(setq-default gc-cons-threshold 100000000)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb


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
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("eab123a5ed21463c780e17fc44f9ffc3e501655b966729a2d5a2072832abd3ac" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fci-rule-color "#073642" t)
 '(inhibit-startup-screen t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4") t)
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background nil t)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4466ec20b5")
     (60 . "#c11679431550")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed992380000")
     (140 . "#96bf94d00000")
     (160 . "#8e5497440000")
     (180 . "#859900")
     (200 . "#77689bfc4636")
     (220 . "#6d449d475bfe")
     (240 . "#5fc09ea47093")
     (260 . "#4c69a01784aa")
     (280 . "#2aa198")
     (300 . "#303598e7affc")
     (320 . "#2fa1947dbb9b")
     (340 . "#2c889009c736")
     (360 . "#268bd2")) t)
 '(vc-annotate-very-old-color nil t)
 '(warning-suppress-types '(((package reinitialization)))))


;;;;;;;;;;;;;;
;; All indentation made with spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2) ;; Use 2 spaces for javascript files
(setq js2-basic-offset 2) ;; Use 2 spaces for javascript files in js2-mode?
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(setq js2-strict-trailing-comma-warning nil)


;; ;; Move cursor to different Panes by Arrow
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))


;; ;; Buffer switching
;; ;; See https://www.emacswiki.org/emacs/SwitchingBuffers
;; (defun switch-to-previous-buffer ()
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))
;; ; (global-set-key (kbd "<f1>") 'switch-to-previous-buffer)
;; (global-set-key (kbd "<backtab>") 'switch-to-buffer)


;; Scrolling tweaks
;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)
;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)


;;; auth-source config
;;; use pass (~/.password-store)
;;; (see The Unix password store)
(setq auth-sources '(password-store))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  MINOR MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; parenthesis customization
;; consider also http://www.emacswiki.org/emacs/HighlightParentheses
;; (require 'paren)
;; (show-paren-mode t)
;; (setq show-paren-delay 0)
;; ; (set-face-background 'show-paren-match-face (face-background 'default))

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

(defun ws-precedes-point-p()
  "Checks if the cursor is at the end of the line and immediately preceded by whitespace"
  (let* ((at-line-start (eq (line-beginning-position) (point)))
         (at-line-end (eq (line-end-position) (point)))
         (prior-char (+ (point) -1))
         (prior-nonw (save-excursion (re-search-backward "[^ \t]" (line-beginning-position) t)))
         ;; Have any whitespace in front of point
         (prior-ws (not (eq prior-char prior-nonw))))
    (and (not at-line-start) at-line-end prior-ws)))

(setq auto-save-visited-interval 30)
(setq auto-save-visited-file-name nil) ; explicitly disable a setting which would disable in-place autosaving.
(auto-save-visited-mode 1)
;; Not autosaving files opened as sudo
(setq auto-save-visited-predicate
      (lambda () (not (string-match "^/sudo:" buffer-file-name))))


;; Automatically save files on lose focus in Emacs
(add-function :after after-focus-change-function
              (lambda ()
                (unless (or (frame-focus-state) (ws-precedes-point-p)) (save-some-buffers t nil))))

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

;; Avoid high CPU usage when many buffers are open
(setq auto-revert-avoid-polling t)
(global-auto-revert-mode t)


;; ;; Making buffer names unique
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; ;; See also http://www.lonecpluspluscoder.com/2014/08/23/unique-buffer-names-in-emacs/


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

(load-file (concat emacs-config-dir "init-straight.el"))


;;;
;;; Packages configuration / Initialization
;;;


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode t)


;; Elscreen
(straight-use-package 'elscreen)
(require 'elscreen)
(elscreen-start)
(setq elscreen-display-tab nil)
(global-set-key (kbd "C-z C-z") 'elscreen-toggle)

;; Transpose Frame
(straight-use-package 'transpose-frame)
(require 'transpose-frame)

;; Close popup windows with C-g and bring them back with C-S-t
(straight-use-package 'popwin)
(require 'popwin)
(popwin-mode 1)
(push '(ag-mode :dedicated t :stick t :position bottom) popwin:special-display-config)
                                        ;(pop popwin:special-display-config)
(global-set-key (kbd "C-S-t") 'popwin:display-last-buffer)

(straight-use-package 'smart-mode-line)
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

(straight-use-package 'neotree)
(require 'neotree)
(setq neo-window-fixed-size nil)
;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
                (lambda (frame)
                  (let ((neo-window (neo-global--get-window)))
                    (unless (null neo-window)
                      (setq neo-window-width (window-width neo-window)))))))
(global-set-key [f7] 'neotree-find)
(global-set-key (kbd "<S-f7>") 'neotree-toggle)

;; Visual Bookmarks
(straight-use-package 'bm)
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<s-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)

;; Visual Mark -- highlights the current mark, and N-previous marks
;; http://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "LightGoldenrod3"))) "")
(defface visible-mark-face1
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "slateblue4"))) "")
(defface visible-mark-face2
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "slateblue3"))) "")
(defface visible-mark-face3
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "slateblue2"))) "")
(setq visible-mark-max 1)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2 visible-mark-face3))
(straight-use-package 'visible-mark)
(require 'visible-mark)
(visible-mark-mode t)
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 4)

(straight-use-package 'zoom)
(require 'zoom)

(setq zoom-size '(0.618 . 0.618))
;; TODO customize '(zoom-ignored-major-modes '(neotree-mode))
(zoom-mode 1)

 ; Toggle zoom and auto-save-visited modes. Having zoom mode disabled is a
 ; visual reminder that auto-save-visited-mode is not enabled
(global-set-key (kbd "C-<f1>")
                (lambda ()
                  (interactive)
                  (if (default-value 'zoom-mode)
                      (progn
                        (zoom-mode -1)
                        (auto-save-visited-mode nil))
                    (progn
                      (zoom-mode 1)
                      (auto-save-visited-mode 1)))
                  ))

(straight-use-package 'ag)
(require 'ag)

(straight-use-package 'vterm)

;; (require 'flx) ; scoring mechanism from flx is used by ivy–regex-fuzzy
;; (require 'ivy-hydra)
;; (require 'ivy)
(straight-use-package 'ivy-hydra)
(straight-use-package 'ivy-pass)
(require 'ivy-pass)
(global-set-key (kbd "<C-f12>") 'password-store-copy)

;; magit ssh auth issue with SSH_AUTH_SOCK
;; See https://github.com/syl20bnr/spacemacs/issues/10969
;; (straight-use-package 'keychain-environment)
;; (require 'keychain-environment)
;; (keychain-refresh-environment)
;; Possibly more permanent than this?
;; https://github.com/syl20bnr/spacemacs/issues/10969#issuecomment-409929968

(straight-use-package 'ripgrep)
(require 'ripgrep)

(add-to-list 'ripgrep-arguments "-M 120")
(defun my-projectile-ripgrep (regexp)
  "Run a Ripgrep search with `REGEXP' rooted at the current projectile
project root. Copied from projectile-ripgrep. Searches hidden dotfiles
when the prefix argument is given."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))))
  (ripgrep-regexp regexp
                  (projectile-project-root)
                  (mapcar (lambda (val)
                            (concat
                             (if (not (equal current-prefix-arg nil)) "--hidden " : "") ; C-u argument given
                             "--no-require-git --glob \!" val))
                          (append projectile-globally-ignored-files
                                  projectile-globally-ignored-directories
                                  (projectile-project-ignored)
                                  ; (projectile-paths-to-ignore)
                                  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Improve on the compile command and compilation defaults
;; See https://endlessparentheses.com/better-compile-command.html for more tips.
;; Glad I'm not the only one not afraid of saving everything!
(customize-set-variable 'compilation-ask-about-save nil) ;; Always auto-save files before recompile
(setq compilation-skip-threshold 2) ;; Don't stop on info or warnings. Use (compilation-next-error) to navigate

;; Can also follow compilation output when set to t
;; See https://zeekat.nl/articles/making-emacs-work-for-me.html
(setq compilation-scroll-output t) ;; 'first-error to Stop on the first error, or t to scroll

;; Note: for troubleshooting compilation mode finding files, adding to
;; 'compilation-search-path may be a [solution](https://emacs.stackexchange.com/a/10248)
;; (add-to-list 'compilation-search-path "/path/to/build")

;; Color output from compilation
;; See https://zeekat.nl/articles/making-emacs-work-for-me.html
;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  ;; Only apply to the *compilation* buffer
  (if (string-equal (buffer-name) "*compilation*")
      (progn
           (read-only-mode nil)
           (ansi-color-apply-on-region (point-min) (point-max))
           (read-only-mode t))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-always-kill t)

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


;; Enable Projectile
;;
;; list of commands: C-c p C-h
(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)
(require 'projectile)
(require 'counsel-projectile)
(counsel-projectile-mode)
(setq projectile-completion-system 'ivy)
;; command used to get the file for projectile
;; also consider https://www.emacswiki.org/emacs/FileSets
;; (defun projectile-get-ext-command () "find . -type f -print0")
(defun projectile-get-ext-command (&optional arg) "rg . --hidden --no-require-git --null --files") ;; See https://emacs.stackexchange.com/a/29200
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
(global-set-key (kbd "<pause>") 'my-projectile-ripgrep)
(global-set-key (kbd "s-<f6> s") 'my-projectile-ripgrep)
(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
;(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
  ;; (global-set-key [f8] 'helm-projectile-find-file)
  ;(global-set-key (kbd "C-<f6> f") 'projectile-find-file-dwim)
(global-set-key (kbd "C-<f6> f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-<f6> C-<f6>") 'counsel-projectile-find-file)
(global-set-key (kbd "s-<f6> f") 'counsel-projectile-find-file)

; TODO find a better keybinding than C-x b to swith buffer to buffer
;(global-set-key (kbd "C-<f1>") 'counsel-projectile-switch-to-buffer)


;; Note: Invalidate Projectile cache with  [C-c p i]

; (require 'find-file-in-project) ;; TODO install and configure and compare w/ projectile
; (global-set-key (kbd "C-<f6> e") 'find-file-in-project)


                                        ; Updating tags via git hook: https://stackoverflow.com/q/42680131


(straight-use-package 'company)
(require 'company)
(setq company-idle-delay 0.2)
(setq company-backends '((company-capf company-dabbrev-code)))

(straight-use-package 'geben)
(require 'geben)
(setq geben-display-window-function 'popwin:switch-to-buffer)

(straight-use-package 'lsp-mode)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]install\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]logs\\'")
  ;; Do not load lsp-lense because it is slow on large files. Also not helpful
  (setq lsp-lens-enable nil))
(setq lsp-idle-delay 0.500)
(setq lsp-lens-enable nil)
(require 'lsp-mode)

;; LSP over Tramp
;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "clangd-8")
;;                      :major-modes '(c++-mode)
;;                      :remote? t
;;                      :server-id 'bro_system))


;                     :activation-fn nil)
; (start-file-process-shell-command "my-remote" "*my-remote*" '("stty" "raw" ";" "clangd-9" "2>/tmp/my-remote-16-stderr"))

;; see also  https://github.com/emacs-lsp/lsp-mode/issues/1741
;; see also, https://github.com/emacs-lsp/lsp-mode/issues/2375
; Maybe dir local with  lsp-enabled-clients/lsp-disabled-clients


(straight-use-package 'ccls)
(straight-use-package 'lsp-docker)
(require 'lsp-docker)
(require 'ccls)
(setq lsp-docker-client-packages
      '(lsp-bash lsp-pyls ccls))
;; Load a file that defines
;; * lsp-docker-client-configs
;;   * See lsp-docker usage
;; * lsp-docker-default-configs
;;   * A plist for lsp-docker-init-clients keyword arguments
(let ((path "~/.emacs.d")
      (file "lsp_docker_client_config.el"))
  (if (locate-file file (list path))
      (load (concat path "/" file))))

(gethash 'ccls lsp-clients)
(gethash 'ccls-docker lsp-clients)

;; Prevent semgrep-ls and pyls from being used
(setq lsp-disabled-clients '(pyls pylsp semgrep-ls))
;; Other examples are:
;; (setq lsp-enabled-clients '(foo-bar)) will run only client foo-bar
;; (setq lsp-disabled-clients '(foo-bar)) will disable client foo-bar
;; (setq lsp-client-packages '(lsp-foo-bar)) will load only the definition for foo-bar.
;; Note: May also want to force c++ to use the ccls-docker client, disable clangd
;;       and ccls and to force python to use pyls-docker-xxx client(s)

; (setq lsp-file-watch-threshold 3000)
(setq lsp-enable-file-watchers nil)

; Enable lsp-mode for c++ only if the lang server is installed
(if (locate-file "ccls" exec-path)
    (add-hook 'c++-mode-hook #'lsp-deferred))


(straight-use-package 'docker)
(require 'docker)
(global-set-key (kbd "C-c C-d") 'docker)

(straight-use-package
'(php-extras :type git :host github :repo "arnested/php-extras"))
(straight-use-package 'php-mode)
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

(straight-use-package 'company-php)
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

(straight-use-package 'js2-mode)
(require 'js2-mode)
;; Enable js2-mode like this instead of with js-mode-hook so that
;; qml-mode, which uses js-mode does not also include js2-mode. May
;; need to add more file extensions in the future, but for now it is
;; nice that js2-mode is not enabled for JSON files too.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
; (add-hook 'js-mode-hook 'visible-mark-mode)

(straight-use-package 'sass-mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(straight-use-package 'web-mode)
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
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)) ; see also https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh
(add-to-list 'auto-mode-alist '("\\.qrc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(setq web-mode-engines-alist '
      (("php" . "\\.phtml\\'")
       ("blade" . "\\.blade\\.")
       ("riot" . "\\.tag\\'")))
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


(straight-use-package 'typescript-mode)
(require 'typescript-mode)


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

;; Requires a valid yaml checker
(add-hook 'yaml-mode-hook 'flycheck-mode)

;; (require 'dockerfile-mode)
;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(straight-use-package 'qml-mode)
(require 'qml-mode)
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(add-hook 'qml-mode-hook '(lambda ()
                           (js2-mode-exit)
                           (message "js2-mode exited")))

(define-key qml-mode-map (kbd "<f5>") (lambda () (interactive) (save-some-buffers t) (recompile)))


; C++ w/ RTags (sort of automated with cmake-ide...)
; https://github.com/atilaneves/cmake-ide
; http://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html
;
; https://oracleyue.github.io/2017/12/04/emacs-init-cc-irony/
;
; TODO https://nilsdeppe.com/posts/emacs-c++-ide2 LSP mode for completion (and possibly replace rtags?)
(straight-use-package 'rtags)
(require 'rtags)
(straight-use-package 'flycheck-rtags)
(require 'flycheck-rtags)
(straight-use-package 'ivy-rtags)
(require 'ivy-rtags)
(setq rtags-display-result-backend 'ivy)
(if
    (and nil
         (fboundp 'rtags-executable-find) (rtags-executable-find "rc") (rtags-executable-find "rc"))
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

(defun clang-format-on-save ()
  (interactive)
  (add-hook 'before-save-hook #'clang-format-buffer nil 'local))
;; (defun clang-format-on-save-remove ()
;;   (interactive)
;;   (remove-hook 'before-save-hook #'clang-format-buffer))
(add-hook 'c++-mode-hook 'clang-format-on-save 10)
(add-hook 'c-mode-hook 'clang-format-on-save)

(straight-use-package 'cmake-ide)
(require 'cmake-ide)
(setq cmake-ide-cmake-command (concat (getenv "HOME") "/.dotfiles-private/bin/cmake-catkin"))
; (cmake-ide-setup)

; (global-flycheck-mode t)
(straight-use-package 'cmake-mode)
(require 'cmake-mode)
; (require 'cmake-font-lock) ;; see https://github.com/gonsie/dotfiles/blob/master/emacs/init.el
;; CMake
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
(require 'cycle-at-point-find-alphabet)
;; (setopt cycle-at-point-preset-override nil)
(defun cycle-at-point-preset-cmake-mode-custom ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (declare (important-return-value t))
  ;; Note that CMAKE is case insensitive.
  (list
   (list :data (list "hpp" "cpp") :case-fold t)
   (list :data (list "TRUE" "FALSE") :case-fold t)
   (list :data (list "ON" "OFF") :case-fold t)
   (list :data (list "AND" "OR") :case-fold t)
   (list :data (list "VERSION_LESS" "VERSION_GREATER") :case-fold t)
   (list :data (list "VERSION_LESS_EQUAL" "VERSION_GREATER_EQUAL") :case-fold t)
   (lambda () (cycle-at-point-find-alphabet-ascii))))
(provide 'cycle-at-point-preset-cmake-mode-custom)
(add-hook 'cmake-mode-hook '(lambda ()
                              (setq-local cycle-at-point-preset-override "cmake-mode-custom")))

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
;; (ggtags-mode 1)
(global-set-key (kbd "M-*") 'xref-pop-marker-stack) ; could also be 'pop-tag-mark
;; TODO also use (rtags-location-stack-back) when in c++ mode?
;; (setq ggtags-completing-read-function nil)
                                        ; (setenv "GTAGSLIBPATH" "/showclix/config:/showclix/src:/showclix/tests/active_tests:/showclix/settings:/showclix/schema_evolutions:/showclix/public_html/classes:/showclix/public_html/actions:/showclix/public_html/templates:/showclix/public_html/controller")
;; (setenv "GTAGSLIBPATH" nil)

(straight-use-package 'smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode t)
;; todo https://superuser.com/questions/134921/smooth-scrolling-in-emacs-windows

;; Also increase speed by changing X-window repeat rate
;; xset r rate 500 75

;; Getting markdown preview to work...
;; http://www.maheshsubramaniya.com/article/install-markdown-for-emacs.html
(straight-use-package 'markdown-mode)
(require 'markdown-mode)
(setq markdown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
;; Use outline minor mode to expand/collapse markdown document sections like you can in org-mode
(add-hook 'markdown-mode-hook  '(lambda ()
                            (outline-minor-mode 1)))

(global-set-key (kbd "C-x g") 'magit-status-quick)

; (global-magit-file-mode t) ; this is no longer a thing? what did this do before
(setq magit-completing-read-function 'ivy-completing-read)
; (setq magit-completing-read-function 'magit-ido-completing-read)


;; Working with conventional commits in magit
;; http://www.modernemacs.com/post/pretty-magit/
;; https://gist.github.com/sebastiencs/f03129ade6a699f7c596e13b0aae31f2
;; https://www.reddit.com/r/emacs/comments/6jegis/pretty_magit_integrating_commit_leaders/
;;
;; Maps commit type to configured unicode icon. Use a font like Noto
;; Color Emoji to get icons for the unicode characters.
;;
(defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
  "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
  `(prog1
     (add-to-list 'pretty-magit-alist
                  (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                        ,ICON ',PROPS))
     (unless ,NO-PROMPT?
       (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

;; Map to unicode only if we have a font that can render nicely.
;; Find more emoji to use at https://github.com/yodamad/gitlab-emoji
;; Check to see if emoji font is found.
(if (find-font (font-spec :name "Noto Color Emoji"))
    (progn
      (setq pretty-magit-alist nil)
      (setq pretty-magit-prompt nil)
      ;; (pretty-magit "main"    ?✩ nil t)
      (pretty-magit "origin/"  ?🌐 (:height 0.75) t) ;; :globe_with_meridians:
      (pretty-magit "build" ?🔨 nil) ;; :hammer:
      (pretty-magit "ci" ?🚧 (:foreground "#3F681C" :height 1.2))
      (pretty-magit "chore" ?🧹 nil) ;; :broom:
      (pretty-magit "docs" ?📖 (:foreground "#3F681C" :height 1.2)) ;; :book:
      (pretty-magit "feat!:" ?☑ (:box t :foreground "slate gray" :background "black" :height 1.2) t) ;; :ballot_box_with_check:
      (pretty-magit "feat" ?☑ nil) ;; :ballot_box_with_check:
      (pretty-magit "fix"  ?🦋 nil) ;; :butterfly:
      (pretty-magit "perf" ?📈 nil) ;; :chart_with_upwards_trend:
      (pretty-magit "refactor" ?✂ (:foreground "#375E97" :height 1.2)) ;; :scissors:
      (pretty-magit "style"    ?🖌 nil) ;; :paintbrush:
      (pretty-magit "test"    ?🧪 nil) ;; :test_tube:
      (set-fontset-font t 'symbol
                        (font-spec :family "Noto Color Emoji") nil 'prepend)))

(defun add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (when (derived-mode-p 'magit-log-mode)
    (with-silent-modifications
      (--each (if (boundp 'pretty-magit-alist) pretty-magit-alist '())
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props)))))))))

;; (advice-add 'magit-status :after 'add-magit-faces)
;; (advice-add 'magit-refresh-buffer :after 'add-magit-faces)

(setq use-magit-commit-prompt-p nil)
(defun use-magit-commit-prompt (&rest args)
  (setq use-magit-commit-prompt-p t))

(defun magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when use-magit-commit-prompt-p
    (setq use-magit-commit-prompt-p nil)
    (insert (ivy-read "Commit Type " pretty-magit-prompt
                      :require-match t :sort t :preselect "feat: "))))

;; (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
;; (add-hook 'git-commit-setup-hook #'magit-commit-prompt)
;; (advice-add 'magit-commit :after 'use-magit-commit-prompt)


(add-hook 'web-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'php-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))

(straight-use-package 'string-inflection)
(require 'string-inflection)
(global-set-key (kbd "ESC M-p") 'string-inflection-cycle)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(require 'yasnippet)
(require 'string-inflection) ;; string-inflection is used for some advanced snippets
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


(straight-use-package 'ess)
(require 'ess)


(straight-use-package 'protobuf-mode)
(require 'protobuf-mode)

(setq clang-format-executable (locate-file "clang-format-11" exec-path))
(straight-use-package 'clang-format)
(require 'clang-format)

;; c-mode indentation https://stackoverflow.com/a/664525
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro 4))
(straight-use-package 'modern-cpp-font-lock)
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

;; Use dimmer with god-mode for visual cue of editing mode
(straight-use-package '(dimmer :host github :repo "emacsmirror/dimmer" :branch "master"))
(require 'dimmer)

; Set up god-mode for modal editing
; See also https://github.com/jmorag/kakoune.el
(straight-use-package 'god-mode)
;; (setq god-exempt-major-modes nil)
;; (setq god-exempt-predicates nil)
(setq god-mode-enable-function-key-translation nil)
(require 'god-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(god-mode)
; https://emacs.zdx.cat/#org16c18e2
; (defun my-god-mode-update-cursor-type ()
;  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
; (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
(add-hook 'god-mode-enabled-hook '(lambda ()
                                    (progn
                                      (dimmer-mode -1)
                                      (setq cursor-type '(hbar . 8)))))
(add-hook 'god-mode-disabled-hook '(lambda ()
                                     (progn
                                       (dimmer-mode t)
                                       ;; (dimmer-dim-all)
                                       (dimmer-process-all)
                                       (setq cursor-type 'box))))
;; Toggle (god-local-mode)
(defun god-mode-toggle ()
  "Toggles god local mode in the current buffer"
  (interactive)
  (if god-local-mode
      (or (god-local-mode-pause) (god-local-mode-resume))
    (god-local-mode)))
(global-set-key (kbd "<end>") 'god-mode-toggle);
(define-key god-local-mode-map (kbd "i") (lambda () (interactive) (god-local-mode-pause)))
(define-key god-local-mode-map (kbd ".") #'repeat)
;; god-mode is greedy and tries to prefix everything with C- so
;; modify some common keybindings to work better in god-mode.
(define-key god-local-mode-map (kbd "C-x C-1") #'delete-other-windows)
(define-key god-local-mode-map (kbd "C-x C-2") #'split-window-below)
(define-key god-local-mode-map (kbd "C-x C-3") #'split-window-right)
(define-key god-local-mode-map (kbd "C-x C-0") #'delete-window)
(define-key god-local-mode-map (kbd "C-x C-o") #'other-window)
(define-key god-local-mode-map (kbd "C-x C-b") #'ivy-switch-buffer)
(define-key god-local-mode-map (kbd "C-x C-c") #'list-buffers) ;; Re-bind to prevent accidental exit
(define-key god-local-mode-map (kbd "C-x C-m") #'kmacro-end-and-call-macro)

;; (global-set-key (kbd "M-<f6>") nil)
(global-set-key (kbd "M-o") #'ivy-switch-buffer)
(global-set-key (kbd "M-<f6>") 'imenu)
(global-set-key (kbd "s-x") 'imenu)
(global-set-key (kbd "C-x C-q") 'kill-buffer-and-window)

;; Remove some keybindings I don't use and do not want to trigger
;; accidentally or conflict with window manager shortcuts
(global-set-key (kbd "C-<home>") nil)
(global-set-key (kbd "C-<end>") nil)
(global-set-key (kbd "C-<prior>") nil)
(global-set-key (kbd "C-<next>") nil)
(global-set-key (kbd "<prior>") nil)
(global-set-key (kbd "<next>") nil)
(global-set-key (kbd "C-M-b") nil)

;; Ctrl+Alt+d shows desktop in Unubntu

;; Sometimes a window steals focus away from the minibuffer while the
;; minibuffer is in a prompt (like asking to create a directory that
;; does not exist when saving a new file).
;;
;; Call 'minibuffer-keyboard-quit to force the prompt to close after
;; focus has been stolen.
(global-set-key (kbd "C-M-g") 'minibuffer-keyboard-quit)

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
;; Under Ubuntu, there may be keybinding conflicts with C+; and C+. due to IBus emojii selection
;; IBus can be configured to disable the shortcut key with `ibus-setup`
;; See https://askubuntu.com/a/1436302
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

;; Switching to recent buffers
(defun display-prev-buffer-in-window ()
  "Changes to the most recent buffer in the current window"
  (interactive)
  (if (window-next-buffers)
      ;; When called from a buffer that is not at the top of the
      ;; window's buffer history, the current buffer is placed as the
      ;; first previous buffer in the window history. The history
      ;; update is done by storing a reference to the current buffer
      ;; as "old", then visiting the top of the history and storing a
      ;; reference to that buffer as new, then switch to the "old" and
      ;; then back to "new".
      (let
          ;; Keep a reference to the "old" buffer
          ((old-buffer (window-buffer))
           (old-start (window-start))
           (old-point (window-point)))
        ;; Navigate back to the buffer at the top of the window's history
        (while (window-next-buffers) (switch-to-next-buffer))
        (let
            ;; Keep a reference to the "new" buffer
            ((new-buffer (window-buffer))
             (new-start (window-start))
             (new-point (window-point)))
          ;; Set the window to the "old" buffer so it is first in the history
          (set-window-buffer-start-and-point
           (selected-window) old-buffer old-start old-point)
          ;; Set the window to the "new" buffer so that it is current
          ;; and then "old" becomes the first previous buffer in the
          ;; window history
          (set-window-buffer-start-and-point
           (selected-window) new-buffer new-start new-point)))
    (switch-to-prev-buffer)))
(global-set-key (kbd "<f1>") 'display-prev-buffer-in-window)
;; (require 'go-back-buffer "~/.emacs.d/packages/go-back-buffer/go-back-buffer.el")
;; (go-back-buffer-mode t)
;; (global-set-key (kbd "<f1>") 'go-back-buffer--display-prev-buffer-in-window)

(global-set-key (kbd "<f9>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f10>") 'switch-to-next-buffer)
(global-set-key (kbd "M-o") 'switch-to-buffer)

;; TODO also try these:
;; https://github.com/jrosdahl/iflipb
;; https://github.com/killdash9/buffer-flip.el

;; mouse vs keyboard
;; https://www.reddit.com/r/emacs/comments/4f2iee/efficient_use_of_multibutton_mice_with_emacs/d260em2/

;; See https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/cznzmh9/
(straight-use-package 'mouse-copy)
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
(straight-use-package 'diminish)
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
(diminish 'god-local-mode)
(diminish 'gc-buffers-mode)
(dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
  (diminish mode))
(diminish 'zoom-mode)

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

(straight-use-package 'package-lint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Themes
;;

;; (defun load-theme-solarized-dark ()
;;   (interactive)
;;   (progn
;;     (require `solarized-theme)
;;     (load-theme 'solarized-dark t)

;;     (face-spec-set 'bm-face '((t (:foreground "gold" :overline nil))))

;;     (setq beacon-color "LightGoldenrod3")

;;     (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
;;     (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link)))
;;   (when (fboundp 'sml/setup) (sml/setup)))

;; (defun load-theme-solarized-light ()
;;   (interactive)
;;   (progn
;;     (require `solarized-theme)
;;     (load-theme 'solarized-light t)

;;     (face-spec-set 'bm-face '((t (:foreground "maroon" :overline nil))))

;;     (setq beacon-color (face-foreground 'menu))

;;     (set-face-background 'avy-goto-char-timer-face (face-background 'menu))
;;     (set-face-foreground 'avy-goto-char-timer-face (face-foreground 'link)))
;;   (when (fboundp 'sml/setup) (sml/setup)))

(straight-use-package 'zenburn-theme)
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

  (set-face-attribute 'web-mode-current-column-highlight-face nil :background (face-attribute 'hl-line :background))

  (when (fboundp 'sml/setup)
    (sml/setup)
    (sml/apply-theme 'respectful)))

(color-values (face-background 'highlight)) ; debug a face background


;; Set font
;; http://askubuntu.com/questions/23603/how-to-change-font-size-in-emacs
(if (find-font (font-spec :name "Hack"))
    (progn (set-frame-font "Hack 12")
           (set-face-attribute 'default nil :height 130))
  (if (find-font (font-spec :name "DejaVu Sans Mono"))
      (set-frame-font "DejaVu Sans Mono 12")))

(if (find-font (font-spec :name "VictorMono NF"))
    (progn
      (set-frame-font "VictorMono NF")
      (set-face-attribute 'default nil :height 130))
  (if (find-font (font-spec :name "DejaVu Sans Mono"))
      (set-frame-font "DejaVu Sans Mono 12")))

(add-hook 'after-init-hook 'load-theme-solarized-dark)
(load-theme-solarized-dark)
;; Note: Change theme with M-x load-theme RET {themename}


(defun quit-compile-log ()
  (interactive)
  (progn
    (quit-window nil (get-buffer-window "*Compile-Log*" 'visible))
    ;; Restore *Compile-Log* warnings
    (setq byte-compile-warnings t)))
(add-hook 'after-init-hook 'quit-compile-log)

(message "load_end")


; (provide 'init)
;;; init.el ends here
