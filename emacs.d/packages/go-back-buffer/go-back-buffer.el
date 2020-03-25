
;;; go-back-buffer.el --- Visit the previous buffer in a window  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  william.wedler

;; Author: William Wedler <wwedler@riseup.net>
;; Version: 0.0.1
;; Keywords: convenience, window

;; This file is NOT part of GNU Emacs.

;;; Commentary:

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


(defun gbb--prev-history (&optional window)
  "Get or set the history entry for the given window"
  (let* ((curr-window (or window (selected-window)))
         (curr-buffer (window-buffer curr-window))
         (curr-screen (if (fboundp 'elscreen-get-current-screen) (elscreen-get-current-screen) "0"))
         (screen-history (or (get 'gbb--prev-history 'history) '())) ; get or initialize history alist
         (prev-history (assoc (gbb--history-key curr-screen curr-window) screen-history))) ; history entry for this window
    (if (not prev-history)
        (put 'gbb--prev-history 'history
             (append
              (list (gbb--history-item curr-screen curr-window curr-buffer))
              screen-history)))
    (assoc
     (gbb--history-key curr-screen curr-window)
     (get 'gbb--prev-history 'history))))

(defun gbb--update-history (&optional window next-buffer)
  "Store the previous buffer in history"
  (let* ((curr-window (or window (selected-window)))
         (curr-buffer (window-buffer curr-window))
         (prev-history (gbb--prev-history curr-window)))
    (if (and
         (buffer-live-p curr-buffer)
         (not (equal
               (gbb--history-buffer prev-history)
               curr-buffer)))
        (setcdr
         prev-history
         (list (gbb--history-val
                curr-buffer
                (window-start curr-window)
                (window-point curr-window)))))))

(defun gbb--display-prev-buffer-in-window (&optional window)
  "Toggle to the previous buffer in WINDOW"
  (let* ((curr-window (or window (selected-window)))
         (curr-buffer (window-buffer curr-window))
         (prev-history (gbb--prev-history curr-window)))
    (if (and
         (buffer-live-p (gbb--history-buffer prev-history))
         (not (equal
               (gbb--history-buffer prev-history)
               curr-buffer)))
        (set-window-buffer-start-and-point
         curr-window
         (gbb--history-buffer prev-history)
         (gbb--history-win-start prev-history)
         (gbb--history-win-point prev-history))
      (mode-line-other-buffer))))

(defun gbb--cleanup-history (&optional window)
  "Clean up buffer history.
Delete the buffer history for any window that is no longer valid
or for screens that no longer exist.  Argument WINDOW provided
for compatibility with advice."
  (mapcar
   (lambda (history)
     (let* ((win-obj (gbb--history-window history))
            (screen-ref (gbb--history-screen history)))
       (if(or
           (not (member
                 screen-ref
                 (elscreen-get-conf-list 'screen-history)))
           (not (window-valid-p win-obj)))
           (put 'gbb--prev-history 'history
                (delq
                 history
                 (get 'gbb--prev-history 'history))))))
   (get 'gbb--prev-history 'history)))

;; Maintaining state
;; (http://ergoemacs.org/emacs/elisp_toggle_command.html)
; (get 'gbb--prev-history 'history)
; (put 'gbb--prev-history 'history nil)
(defun gbb--history-key (screen window)
  "Object used as the alist key"
  (list screen window))
(defun gbb--history-window (item)
  "The window object in the alist element ITEM."
  (nth 1 (car item)))
(defun gbb--history-screen (item)
  "The screen referenced in the alist element ITEM."
  (nth 0 (car item)))
(defun gbb--history-val (buffer win-start  win-point)
  "Object used as the alist val"
  (list buffer win-start win-point))
(defun gbb--history-buffer (item)
  "Get the buffer stored in the alist element ITEM."
  (nth 0 (car (cdr item))))
(defun gbb--history-win-start (item)
  "Get the window start stored in the alist element ITEM."
  (nth 1 (car (cdr item))))
(defun gbb--history-win-point (item)
  "Get the window point stored in the alist element ITEM."
  (nth 2 (car (cdr item))))
(defun gbb--history-item (screen window buffer)
  "Key-val pair used for the alist element"
  (list
   (gbb--history-key screen window)
   (gbb--history-val
    buffer
    (window-start window)
    (window-point window))))

(defun gbb--display-prev-buffer ()
  "Toggle to the previous buffer in the current window"
  (interactive)
  (gbb--display-prev-buffer-in-window))

; (advice-add 'set-window-buffer :before 'gbb--update-history)
; (advice-add 'delete-window :before 'gbb--cleanup-history)
; (advice-remove 'set-window-buffer #'gbb--update-history)


(provide 'go-back-buffer)

;;; go-back-buffer.el ends here
