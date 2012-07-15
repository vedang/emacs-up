;;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))

        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(define-key ctl-x-4-map (kbd "t") 'swap-windows)

;;; And because diversity is a good thing, here is Stephen Gildea’s version of the same:
(defun swap-window-positions ()         ; Stephen Gildea
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))


(setq swapping-buffer nil)
(setq swapping-window nil)

;;; ChrisWebber provides a similar function, but this one allows you to select which two you want to swap
;;; (perhaps if you have 3 or more windows open)

(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))

(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)

;;; Yet another window-altering function by Robert Bost slightly based on Steve Yegge’s swap-windows. This one
;;; will handle > 1 windows.
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1)) (message "You can't rotate a sinlge window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
