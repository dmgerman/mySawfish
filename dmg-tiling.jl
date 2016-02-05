;; dmg-tiling.jl 0.2 -- Tiling functions.
;;
;; Copyright (C) 2009 Daniel M. German <dmg@uvic.ca> 
;;              and Eugene Morozov <john_morozov@yahoo.com> 
;;
;; Based on tiling-functions by Eugene Morozov <john_morozov@yahoo.com> 
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the same terms as sawfish (the window manager), or at your
;; choice, under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; Simple tiling function. Tiles windows in a single row or column
;;  I recommend you have undo installed before using it.
;;
;; Known Bugs:
;;   Assumes the borders of all windows are the same.
;;
;; ChangeLog:
;;
;; 2015-02-05  dmg  <dmg@uvic.ca>
;;      * Do the tiling in the order in which the windows are in the screen
;; 2010-01-03  dmg <dmg@uvic.ca>
;;      * Tiling area is only the current head (where the pointer is), even if the viewport is
;;      larger than the head area
;; 2010-01-02  dmg  <dmg@uvic.ca>
;;	*  Create module


(defun dmg-tile-win-left-of-p (w1 w2)
  (let ((x1 (car (window-position w1)))
        (x2 (car (window-position w2)))
          )
    (< x1 x2)
    )
  )

(defun dmg-tile-win-above-of-p (w1 w2)
  (let ((y1 (cdr (window-position w1)))
        (y2 (cdr (window-position w2)))
          )
    (< y1 y2)
    )
  )

(defun dmg-tile-windows-horizontally ()
  "Tile windows horizontally"
  (interactive)
  (let* ((unsortedWins (workspace-windows current-workspace))
         (wins (sort unsortedWins dmg-tile-win-left-of-p))
         (calculated-width 0)
         ;; start placing windows in the x offset of the left corner of the workarea
         (x (nth 0 (calculate-workarea)))
         (border-width 0)
         (borders-width 0)
         (work-area (calculate-workarea))
         (y-offset (nth 1 work-area))
         (screen-x-size (car (head-dimensions (pointer-head))))
         )
    ;; Calculate width of the window border
    (setq border-width (- (car (window-frame-dimensions (car wins))) 
                          (car (window-dimensions (car wins)))))
    ;; Calculate width of borders of all windows on current viewport
    (setq borders-width 
          (* (length wins) border-width))
    (setq calculated-width 
          (ceiling (/ 
                    (- screen-x-size (+ x borders-width))
                    (length wins)
                    )))
    (while wins
      (let ((win (car wins)))
        (move-window-to win x y-offset)
        (resize-window-to win calculated-width (cdr (window-dimensions win)))
        (maximize-window-vertically win)
	(maximize-discard win)
        (setq x (+ x calculated-width border-width))
        (setq wins (cdr wins))
        )
      )
    )
  )

(defun dmg-tile-windows-vertically ()
  "Tile windows vertically in one column. Each window is maximized horizontally"
  (interactive)
  (let* ((unsortedWins (workspace-windows current-workspace))
         (wins (sort unsortedWins dmg-tile-win-above-of-p))
         (calculated-height 0)
         (border-height 0)
         (borders-heights 0)
         ;; the x of the left corner of the work area
         (x-offset (nth 0 (calculate-workarea)))
         ;; start placing windows in the y offset of the left corner of the workarea
         (y (nth 1 (calculate-workarea)))
         (screen-y-size (cdr (head-dimensions (pointer-head))))
         ) ; let*
    ;; Calculate height of the window border
    (setq border-height (- (cdr (window-frame-dimensions (car wins))) 
                           (cdr (window-dimensions (car wins)))))
    ;; Calculate height of borders of all windows on current viewport
    (setq borders-height 
          (* (length wins) border-height))
    (setq calculated-height 
          (ceiling (/ 
                    (- 
                     (- screen-y-size y)
                     borders-height 28)
                    (length wins))))
    (while wins
      (let ((win (car wins)))
        (move-window-to win x-offset y)
        (resize-window-to win (nth 0 (window-dimensions win)) calculated-height)
        (maximize-window-horizontally win)
	(maximize-discard win)
        (setq y (+ y calculated-height border-height))
        (setq wins (cdr wins))
        )
      )
    )
  )

(provide 'dmg-tiling)
