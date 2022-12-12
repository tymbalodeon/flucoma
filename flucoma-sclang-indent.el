;;; flucoma-sclang-indent.el --- custom indentation for sclang-mode
;;; Commentary:
;;
;; Modifies `sclang-indent-line' to indent chained methods an extra level.
;;
;;; Code:

(defun flucoma--beginning-of-line-point ()
  "Return the value of point at the beginning of the line."
  (beginning-of-line)
  (point))

(defun flucoma--indent (value)
  "Increase VALUE by `sclang-indent-level'."
  (+ value sclang-indent-level))

(defun flucoma--goto-indentation (original-points-from-end)
  "Go to ORIGINAL-POINTS-FROM-END if greater than point."
  (let ((current-points-from-end (- (point-max) original-points-from-end)))
    (when (> current-points-from-end (point))
      (goto-char current-points-from-end))))

(defun sclang-indent-line ()
  "Indent the current line as sclang code, returning points shifted."
  (let* ((beginning-of-line-point (flucoma--beginning-of-line-point))
         (indent (calculate-sclang-indent))
         (points-to-shift (- indent (current-column)))
         (original-points-from-end (- (point-max) (point)))
         (case-fold-search nil))
    (skip-chars-forward " \t")
    (when (looking-at "\\.")
      (progn (setq indent
                   (flucoma--indent indent))
             (setq points-to-shift
                   (flucoma--indent points-to-shift))))
    (when (not (zerop points-to-shift))
      (delete-region beginning-of-line-point (point))
      (indent-to indent))
    (flucoma--goto-indentation original-points-from-end)
    points-to-shift))

(provide 'flucoma-sclang-indent)
;;; flucoma-sclang-indent.el ends here
