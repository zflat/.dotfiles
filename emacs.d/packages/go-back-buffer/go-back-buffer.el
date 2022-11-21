;;; go-back-buffer.el --- Visit the previous buffer in a window  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021  william.wedler

;; Author: William Wedler <wwedler@riseup.net>
;; Version: 0.0.1
;; Keywords: convenience, window
;; URL: https://github.com/zflat/go-back-buffer
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; go-back-buffer is a package to provides a minor mode and
;; interactive function for easily switching to the most recently
;; visited buffer in the active window.  Think of it like alt+tab for
;; buffer switching.

;; Installation and Setup:
;; install go-back-buffer from MELPA, or download it manually from GitHub.
;; If you download manually, add these lines to your init file:
;;    (add-to-list 'load-path "/path/to/go-back-buffer")
;;    (require 'go-back-buffer)
;; To activate Purpose at start-up, add this line to your init file:
;;    (go-back-buffer-mode)
;; Recommended key binding: bind a function key to 'gbb-display-prev-buffer
;;    (global-set-key (kbd "<f1>") 'gbb--display-prev-buffer)

;; Basic Usage:
;; 1. Switch to the most recently viewed buffer in the given window
;;    `gbb--display-prev-buffer'

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(defun go-back-buffer--prev-history (&optional window)
  "Get or set the history entry for the given WINDOW."
  (let* ((curr-window (or window (selected-window)))
         (curr-buffer (window-buffer curr-window))
         (curr-screen (if (fboundp 'elscreen-get-current-screen) (elscreen-get-current-screen) "0"))
         (screen-history (or (get 'go-back-buffer--prev-history 'history) '())) ; get or initialize history alist
         (prev-history (assoc (go-back-buffer--history-key curr-screen curr-window) screen-history))) ; history entry for this window
    (if (not prev-history)
        (put 'go-back-buffer--prev-history 'history
             (append
              (list (go-back-buffer--history-item curr-screen curr-window curr-buffer))
              screen-history)))
    (assoc
     (go-back-buffer--history-key curr-screen curr-window)
     (get 'go-back-buffer--prev-history 'history))))

(defun go-back-buffer--update-history (&optional window)
  "Store the window's current buffer in the previous buffer.
Optional argument WINDOW overrides the default, `selected-window', to get the current buffer of the window."
  (let* ((curr-window (or window (selected-window)))
         (curr-buffer (window-buffer curr-window))
         (prev-history (go-back-buffer--prev-history curr-window)))
    (if (and
         (buffer-live-p curr-buffer)
         (not (equal
               (go-back-buffer--history-buffer prev-history)
               curr-buffer)))
        (setcdr
         prev-history
         (list (go-back-buffer--history-val
                curr-buffer
                (window-start curr-window)
                (window-point curr-window)))))))

(defun go-back-buffer--display-prev-buffer-in-window (&optional window)
  "Toggle to the previous buffer in WINDOW."
  (interactive)
  (let* ((curr-window (or window (selected-window)))
         (curr-buffer (window-buffer curr-window))
         (prev-history (go-back-buffer--prev-history curr-window)))
    (if (and
         (buffer-live-p (go-back-buffer--history-buffer prev-history))
         (not (equal
               (go-back-buffer--history-buffer prev-history)
               curr-buffer)))
        (set-window-buffer-start-and-point
         curr-window
         (go-back-buffer--history-buffer prev-history)
         (go-back-buffer--history-win-start prev-history)
         (go-back-buffer--history-win-point prev-history))
      (previous-buffer 1))))

(defun go-back-buffer--cleanup-history (&optional window)
  "Clean up buffer history.
Delete the buffer history for any window that is no longer valid
or for screens that no longer exist.  Argument WINDOW is provided
for compatibility with advice."
  (mapcar
   (lambda (history)
     (let* ((win-obj (go-back-buffer--history-window history))
            (screen-ref (go-back-buffer--history-screen history)))
       (if (and
            (not (eq window win-obj))
            (or
             (not (and (fboundp 'elscreen-get-conf-list)
                       (member
                        screen-ref
                        (elscreen-get-conf-list 'screen-history))))
             (not (window-valid-p win-obj))))
           (put 'go-back-buffer--prev-history 'history
                (delq
                 history
                 (get 'go-back-buffer--prev-history 'history))))))
   (get 'go-back-buffer--prev-history 'history)))

;; Maintaining state
;; (http://ergoemacs.org/emacs/elisp_toggle_command.html)
(defun go-back-buffer--history-key (screen window)
  "Object used as the alist key in the buffer history list.
Argument SCREEN is the elscreen screen object used for the list
entry key.  Argument WINDOW is the window object used for the list
entry key"
  (list screen window))
(defun go-back-buffer--history-window (item)
  "The window object in the alist element ITEM."
  (nth 1 (car item)))
(defun go-back-buffer--history-screen (item)
  "The screen referenced in the alist element ITEM."
  (nth 0 (car item)))
(defun go-back-buffer--history-val (buffer win-start win-point)
  "Object used as the alist val in the buffer history list.
Argument BUFFER is the buffer to return to when visiting this
history entry.
Argument WIN-START is the starting line of the window to return
to when visiting the history entry.
Argument WIN-POINT is the location of point to return to when
visiting the history entry."
  (list buffer win-start win-point))
(defun go-back-buffer--history-buffer (item)
  "Get the buffer stored in the alist element ITEM."
  (nth 0 (car (cdr item))))
(defun go-back-buffer--history-win-start (item)
  "Get the window start stored in the alist element ITEM."
  (nth 1 (car (cdr item))))
(defun go-back-buffer--history-win-point (item)
  "Get the window point stored in the alist element ITEM."
  (nth 2 (car (cdr item))))
(defun go-back-buffer--history-item (screen window buffer)
  "Key-val pair used for the alist element in the buffer history list.
Argument SCREEN is used for the entry's key.
Argument WINDOW is used for the entry's key.
Argument BUFFER is used as the entry's value."
  (list
   (go-back-buffer--history-key screen window)
   (go-back-buffer--history-val
    buffer
    (window-start window)
    (window-point window))))

(defun go-back-buffer--add-advices ()
    "Add all advices needed for go-back-buffer to work.
This function is called when `go-back-buffer-mode' is activated."
  (advice-add 'set-window-buffer :before #'go-back-buffer--update-history)
  (advice-add 'delete-window :before #'go-back-buffer--cleanup-history))

(defun go-back-buffer--remove-advices ()
    "Remove all advices needed for Purpose to work.
This function is called when `go-back-buffer-mode' is deactivated."
    (advice-remove 'set-window-buffer #'go-back-buffer--update-history)
  (advice-remove 'delete-window #'go-back-buffer--cleanup-history))

;;;###autoload
(defun display-prev-buffer-in-window ()
  "Toggle to the previous buffer in the current window."
  (interactive)
  (go-back-buffer--display-prev-buffer-in-window))

(defvar go-back-buffer--active-p nil)

;;;###autoload
(define-minor-mode go-back-buffer-mode nil
  :global t
  (if go-back-buffer-mode
      (progn
        (go-back-buffer--add-advices)
        (setq go-back-buffer--active-p t))
    (go-back-buffer--remove-advices)
    (setq go-back-buffer--active-p nil)))

(provide 'go-back-buffer)

;;; go-back-buffer.el ends here
