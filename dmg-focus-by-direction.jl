;    dmg-focus-by-direction.jl Focus by direction. Based on the idea
;    of direction.jl by Ives Aerts.
;
;    Version 1.1
;
;    This sawfish script allows you to move to the "next" window in
;    the current workspace in any given direction (north, sound, east,
;    west).  It uses the left-top corner as to create an order. For
;    example, window A is "east" of B
;      1. Its A's x coordinate is larger than B's, 
;      2. Otherwise, if width of A is larger than width of B
;      3. Otherwise, if A is lower (y-coord) than  B's,
;      3. If still tied, if the window-id of A bis larger than B
;
;;   I like binding the operations to meta-control A, P, F, N (similar to 
;    moving lines in emacs.
;
;    This script makes some changes over Ives. First, it considers all
;    windows in the current workspace. Second, it breaks ties in a
;    deterministic way when two windows have the same top-corner
;    location.
;
;    Copyright (C) 2013 Daniel German dmg@uvic.ca
;
;    This program is free software; you can redistribute it and/or modify
;    it under either:
;
;      *  the same terms as the sawfish window manager (any version), or 
;      *  under the terms of the  GPL version 2 or any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Affero General Public License for more details.
;
;    You should have received a copy of the GNU Affero General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;2013-01-19  dmg  <dmg@mn.cs.uvic.ca>
;
;   * Added width/height as a paramater. If two windows in same corner
;    then If window is wider than another one is it to the right/below of it 
;    Updated to version 1.1


(defun dmg-max-list (m l compareW compF)
; takes a list 
; return the max value
  (cond ((null (car l)) m);; if m  is the only value, we are done
        ;; if m is null then do the list
        ((null m) 
         (dmg-max-list (car l) (cdr l) compareW compF)
         )
        ;; compare head of list to m. If head of list, then
        ;; max is in head and do tail
        ((compareW (car l) m compF) 
         (dmg-max-list (car l) (cdr l) compareW compF) 
         )
        ;; otherwise the max is still the current and compare to the tail
        (t 
         (dmg-max-list m (cdr l) compareW compF)
         )
        ) ;; cond
)

(defun dmg-horizontal-order (w1 w2 compF)
  "Compare location of windows. true if w1 is to the right of w2."
  (let ((w1_x (if (null w1) nil (car (window-position w1))))
        (w2_x (if (null w2) nil (car (window-position w2))))
        (w1_y (if (null w1) nil (cdr (window-position w1))))
        (w2_y (if (null w2) nil (cdr (window-position w2))))
        (w1_w (if (null w1) nil (car (window-dimensions w1))))
        (w2_w (if (null w2) nil (car (window-dimensions w2))))
        )
    (cond 
     ((null w1) nil)
     ((null w2) t)
     ((= w1_x w2_x) (cond ;; if same location, use other dimmensions
                          ;; unless it is the same, in that case use window-id to be
                     ;; deterministic
                     ((/= w1_w w2_w) (compF w1_w w2_w))
                     ((/= w1_y w2_y) (compF w1_y w2_y))
                     (t (compF (window-id w1) (window-id w2)))
                      )
      )
     (t (compF w1_x w2_x))
     )
  )
)

(defun dmg-vertical-order (w1 w2 compF)
  "Compare location of windows. true if w1 is to the right of w2."
  (let ((w1_x (if (null w1) nil (car (window-position w1))))
        (w2_x (if (null w2) nil (car (window-position w2))))
        (w1_y (if (null w1) nil (cdr (window-position w1))))
        (w2_y (if (null w2) nil (cdr (window-position w2))))
        (w1_h (if (null w1) nil (cdr (window-dimensions w1))))
        (w2_h (if (null w2) nil (cdr (window-dimensions w2))))
        )
    (cond 
     ((null w1) nil)
     ((null w2) t)
     ((= w1_y w2_y) (cond ;; if same location, use other dimmensions
                          ;; unless it is the same, in that case use window-id to be
                     ;; deterministic
                     ((/= w1_h w2_h) (compF w1_h w2_h))
                     ((/= w1_x w2_x) (compF w1_x w2_x))
                     (t (compF (window-id w1) (window-id w2)))
                      )
      )
     (t (compF w1_y w2_y))
     )
  )
)


(defun dmg-window-dfocusable-p (w)
  "Is window focusable by direction?"
  (and (window-visible-p w)
       (window-mapped-p w)
       (window-in-cycle-p w)
       (not (window-ignored-p w))
       )
)

(defun dmg-select-window-direction-hor (w minF compareW compareF)
  ;; the workhorse. Given a window, a function to compute the "next" (and some parameters to it)
  ;; determine the next window to move to
  ;;
  ;; parameters:
  ;; 
  ;; w        is the current window
  ;; minF     is the function used to do find the "minimum" values (or maximum) depending on
  ;;          the ordering
  ;; compareF is the function that is used to compare positions.
  (let* ((wx (car (window-position w)))
         (wy (cdr (window-position w)))
         (ww (car (window-dimensions w)))
         (dfocusable-windows (filter (lambda (thisWindow)
                                       (let* ((thisx (car (window-position thisWindow)))
                                              (thisy (cdr (window-position thisWindow)))
                                              (thisw (car (window-dimensions thisWindow)))
                                              )
                                         (and (not (= (window-id thisWindow)  (window-id w)))
                                              (dmg-window-dfocusable-p thisWindow)
                                              ;; one of this should be satisfied
                                              (or (compareF thisx wx) ; left of it
                                                  ;; same position but less width
                                                  (and (= thisx wx) ;; 
                                                       (compareF thisw ww)
                                                       )
                                                  (and (= thisx wx) 
                                                       (= thisw ww) 
                                                       (compareF thisy wy)
                                                       )
                                                  (and (= thisx wx)
                                                       (= thisw ww)
                                                       (= thisy wy) ;; same position exactly
                                                       ;;;;;;;;;;;;;;; break the tie using window-id
                                                       (compareF (window-id thisWindow) (window-id w))
                                                       )
                                                  )
                                              )
                                        )
                                       )
                                     (managed-windows)))
         (newWindow (minF nil dfocusable-windows compareW (lambda (x y ) (not (compareF x y)))))
         )
    (if newWindow
      (set-input-focus newWindow)
      )
    )
  )

(defun dmg-select-window-direction-ver (w minF compareW compareF)
  ;; the workhorse. Given a window, a function to compute the "next" (and some parameters to it)
  ;; determine the next window to move to
  ;;
  ;; parameters:
  ;; 
  ;; w        is the current window
  ;; minF     is the function used to do find the "minimum" values (or maximum) depending on
  ;;          the ordering
  ;; compareF is the function that is used to compare positions.
  (let* ((wx (car (window-position w)))
         (wy (cdr (window-position w)))
         (wh (cdr (window-dimensions w)))
         (dfocusable-windows (filter (lambda (thisWindow)
                                       (let* ((thisx (car (window-position thisWindow)))
                                              (thisy (cdr (window-position thisWindow)))
                                              (thish (cdr (window-dimensions thisWindow)))
                                              )
                                         (and (not (= (window-id thisWindow)  (window-id w)))
                                              (dmg-window-dfocusable-p thisWindow)
                                              (or (compareF thisy wy) ; above it
                                                  (and (= thisy wy) ;; same position but less y
                                                       (compareF thish wh)
						       )
                                                  (and (= thisy wy) ;; same position but less y
                                                       (= thish wh)
                                                       (compareF thisx wx)
                                                       )
                                                  (and (= thisx wx)
                                                       (= thish wh)
                                                       (= thisy wy) ;; same position exactly
                                                       ;;;;;;;;;;;;;;; break the tie using window-id
                                                       (compareF (window-id thisWindow) (window-id w))
                                                       )
                                                  )
                                              )
                                        )
                                       )
                                     (managed-windows)))
         (newWindow (minF nil dfocusable-windows compareW (lambda (x y ) (not (compareF x y)))))
         )
    (if newWindow
      (set-input-focus newWindow)
      )
    )
  )



(defun dmg-focus-warp-west (w)
  "Return nearest window west of w."
  (interactive (list (input-focus)))
  ; if we filter those to the left <, then we need to order them in
  ; inverse order 
  (if w
      (dmg-select-window-direction-hor w dmg-max-list dmg-horizontal-order <)
    
    ))

(defun dmg-focus-warp-east (w)
  "Return nearest window east of w."
  (interactive (list (input-focus)))
  (if w
      (dmg-select-window-direction-hor w dmg-max-list dmg-horizontal-order >)
    ))

(defun dmg-focus-warp-north (w)
  "Return nearest window north of w."
  (interactive (list (input-focus)))
  ; if we filter those to the left <, then we need to order them in
  ; inverse order 
  (if w
      (dmg-select-window-direction-ver w dmg-max-list dmg-vertical-order <)
    ))

(defun dmg-focus-warp-south (w)
  "Return nearest window south of w."
  (interactive (list (input-focus)))
  (if w
      (dmg-select-window-direction-ver w dmg-max-list dmg-vertical-order >)
    ))

(provide 'dmg-focus-by-direction)
