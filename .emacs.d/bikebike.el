;;
;; Load this file by running:
;; M-x eval-expression<RET>(load "~/.emacs.d/")
;; 
;; Understanding emacs tabs
;; http://www.pement.org/emacs_tabs.htm
(define-key ruby-mode-map (kbd "TAB") 'self-insert-command); # only in text-mode
(define-key text-mode-map (kbd "TAB") 'self-insert-command); # only in text-mode


