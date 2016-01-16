;;;; dmg-quadrants.jl --  version 0.1
;;;;
;;;; Define quadrants in the screen and move windows to them.
;;;;
;;;; (C) Daniel M. German <dmg@uvic.ca>  July 2008.
;;;;
;;;; *  This program is free software; you can redistribute it and/or
;;;; *  modify it under the terms of the GNU General Public
;;;; *  License as published by the Free Software Foundation; either
;;;; *  version 2 of the License, or (at your option) any later version.
;;;; *
;;;; *  This software is distributed in the hope that it will be useful,
;;;; *  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; *  General Public License for more details.
;;;; *
;;;; *  You should have received a copy of the GNU General Public
;;;; *  License along with this software; if not, write to the Free Software
;;;; *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;; *
;;;; *
;;;; *  Author: Daniel M German dmg@uvic.ca
;;;;
;;;

;;;;  Main interactive functions

;;;  dmg-quad-send (w quad)  "Send a window to a given quad".
;;;  dmg-quad-swap-window (w quad) "Send a window with a given quad
;;;; dmg-window-display-quad find out the quad in which a window is (useful to remember the quads)
;;;;

;;;; You need to define your own list of quads (dmg-quadrants) to match your
;;;; displays and style


(defvar dmg-quadrants '((0 25 1366 768) (0 25 1024 768) (0 25 1023 715) (1024 25 370 500) (1024 501 370 500)
                        (0 25 650 768)  (651 25 650 1025)
                        )
  "Quadrants to use. It is a list of lists of 4 components: x,y, sizex, sizey. They can
overlap. First one is quad 0.
")


;; compute the size of the frame in the given direction
(defun  dmg-window-frame-size-direction (w dir)
  (let*  (
          (fdims (window-frame-dimensions w))
          (dims (window-dimensions w))
          )
    (cond 
     ((equal dir 'h)
      (- (car fdims) (car dims))
      )
     ((equal dir 'v)
      (- (cdr fdims) (cdr dims))
      )
     )
    )
  )
  
; size of the window for given quad
;; takes into account the size of the frame
(defun dmg-window-quad-size-direction (w quad dir)
  (let* (
          (size-frame (dmg-window-frame-size-direction w dir))
          (loc (nth quad dmg-quadrants))
          (x (nth 2 loc))
          (y (nth 3 loc))
          )
    
    (cond 
     ((equal dir 'h)
      (- x size-frame)
      )
     ((equal dir 'v)
      (- y size-frame)
      )
     )
    )
)

; send the current window to a given quad
(defun dmg-quad-send (w quad)
  "Send a window to a given quad"
  (interactive "%W\nnQuad")
  (let* (
         (loc (nth quad dmg-quadrants))
         (x (nth 0 loc))
         (y (nth 1 loc))
         (dimx (nth 2 loc))
         (dimy (nth 3 loc))
         (newx (dmg-window-quad-size-direction w quad 'h))
         (newy (dmg-window-quad-size-direction w quad 'v))
         (prev-quad (window-get w 'dmg-window-quad))
         )
    ; only do this is there is a purpose
    (when (not (null loc))
      (unmaximize-window w)
      ; save previous quad --for restore
      (when prev-quad
        (window-put w 'dmg-window-quad-prev prev-quad)
        )
      (window-put w 'dmg-window-quad quad)
      (move-resize-window-to w 
                             x y
                             newx newy
                             )
      )
    )
  )

; is window in given quad?
(defun dmg-window-in-quad-p (w quad)
  (equal (window-get w 'dmg-window-quad) quad)
  )
  
; display window's quad, if any
(defun dmg-window-display-quad (w)
  "Display the quad in which a window is placed"
  (interactive "%W")
  (let (
        (quad (window-get w 'dmg-window-quad))
        )
    (progn
      (if quad
          (display-message (number->string quad))
        (display-message "current window is not in a quad")
        )
      )
    )
  )
  

; find windows in a given quad, in the order of MRU
(defun dmg-quad-windows-in (quad)
  (filter (lambda (w) (dmg-window-in-quad-p w quad)) (window-order))
)

;; swap the MRU used window in a given quad
(defun dmg-quad-swap-window (w quad)
  "Swap the current window with the MRU in the given quad"
  (interactive "%W\nnQuad")
  (let* (
         (cur-quad (window-get w 'dmg-window-quad))
         (other (car (dmg-quad-windows-in quad)))
         )
    (cond
     ((and cur-quad other)
      ; we have a window to move to and
      ; the current one is in a quadrant
      (dmg-quad-send other cur-quad)
      (dmg-quad-send w quad)
      )
     ((null cur-quad)
      (display-message "Current window is not in a quad")
      )
     ((null other)
      (display-message "Other quad had no window")
      )
     )     
    )
  )


(provide 'dmg-quadrants)

