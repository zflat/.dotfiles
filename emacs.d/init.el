;; Configure emacs

;; Set font
;; http://askubuntu.com/questions/23603/how-to-change-font-size-in-emacs
(set-face-attribute 'default nil :height 120)

;; disable splash screen
(load "~/elisp/autoloads" 'install)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(inhibit-startup-screen t))

;; current buffer name in title bar
(setq frame-title-format "%b")


;;;;;;;;;;;;;


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
 '(default ((t (:inherit nil :stipple "" :background "linen" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; Enable IDO 
;; http://www.masteringemacs.org/article/introduction-to-ido-mode
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-mode 1)

;; Automatic find file customizations:
;;
;; Stop IDO mode find file by typing C-f
;;
;; Slow down time delay for automatic file search
;; (setq ido-auto-merge-delay-time 9)
;;
;; Completely disable automatic find file
;(setq ido-auto-merge-work-directories-length -1)


;; disable the toolbar
(tool-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; consider also using pallet 
;; https://github.com/rdallasgray/pallet


;;; Packages configuration / Initialization

(require 'ag)

(smex-initialize)


(require 'helm-config)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-autoresize-mode 1)
(helm-mode 1)

(projectile-global-mode)
;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)

(define-key projectile-mode-map [?\s-d] 'helm-projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'helm-projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'helm-projectile-find-file)
(define-key projectile-mode-map [?\s-s] 'helm-do-ag-project-root)

(tabbar-mode)
(global-set-key (kbd "C-M->") 'tabbar-backward-group)
(global-set-key (kbd "C-M-<") 'tabbar-forward-group)
(global-set-key (kbd "C-<") 'tabbar-backward)
(global-set-key (kbd "C->") 'tabbar-forward) ;; tabbar.el, put all the buffers on the tabs.


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

