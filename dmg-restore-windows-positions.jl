;; dmg-restore-windows-positions.jl 0.1 -- restore windows to position when changing back to a given screen size

;; Copyright (C) 2016 Daniel M German <dmg@turingmachine.org>

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;; Commentary:
;;
;; This module provides the ability to save the position of windows
;; for different sizes of screens and simply restore them at a
;; later time.
;;
;; every time the size of the head monitor changes, the sizes of
;; the windows would change accordingly.
;;
;; All operations are transparent to the user
;;
;;
;; TODO:
;;   - Blacklist a given head (based on dimensions) so windows are never restored
;;   - Add the ability to ignore certain windows
;;
;;  BUGS:
;;   - A window that no longer exist might have the same id as a newer window, hence
;;     such window will be reposition in the wrong place

(defvar dmg-rwp-head-windows-positions nil "Keep track of sizes of windows for different sizes of heads")

;; keep track of the current size of the head
(defvar dmg-rwp-current-head-dim (head-dimensions (current-head)) "Current dimensions of the head.")

;; reset currently stored sizes
(define (dmg-rwp-reset)
  (progn
    (setq dmg-rwp-current-head-dim (head-dimensions (current-head))
    (setq dmg-rwp-head-windows-positions nil))
  ))

(define (dmg-rwp-current-window-position w)
  (cons w  (list (window-head-any-viewport w) (window-dimensions w) (window-position w))
        ))

;; predicate to indicate which windows to ignore
(define (dmg-rwp-ignore-window-p w)
  (or
   (window-transient-p w)
   (window-get w 'cycle-skip )
   ))

;; build the cons pair for the dim of the screen and the list of windows it has
(define (dmg-rwp-current-windows-position)
  (cons dmg-rwp-current-head-dim (mapcar dmg-rwp-current-window-position
                                          ;; ignore certain windows
                                          (remove-if dmg-rwp-ignore-window-p
                                                     (stacking-order))
                                          )
        ))

;; save the current positions
(define (dmg-rwp-save-windows-positions)
  (let* (
         (winpos (dmg-rwp-current-windows-position))
         (newlist (remove-if (lambda (x) (equal dmg-rwp-current-head-dim (car x))) dmg-rwp-head-windows-positions))
         )
    (setq dmg-rwp-head-windows-positions (cons winpos newlist))
    ))

;; restore a particular window, only if not in the expected location
(define (dmg-rwp-window w p) 
  (if (windowp w)
      (let* ((h   (car p))
             (pos (caddr p))
             (size (cadr p)))
        (progn
          (if (not (equal (window-position w) pos))
              (move-window-to w (car pos) (cdr pos)))
          (if (not (equal (window-dimensions w) size))
              (resize-window-to w (car size) (cdr size)))
          ))))
; restore all the windows for the current size of head
(define (dmg-rwp-restore-windows-positions)
  (let * ((winpos (cdr (assoc (head-dimensions (current-head)) dmg-rwp-head-windows-positions)))
          )
    (mapcar (lambda (pos) (dmg-rwp-window (car pos) (cdr pos)) ) winpos)
    ))

;; hook to attach to the randr change notify event
(defun dmg-rwp-hook ()
  (progn
    (dmg-rwp-save-windows-positions)
    (setq dmg-rwp-current-head-dim (head-dimensions (current-head)))
    (dmg-rwp-restore-windows-positions)
    ))

(add-hook 'randr-change-notify-hook dmg-rwp-hook)

(provide 'dmg-restore-windows-positions)
