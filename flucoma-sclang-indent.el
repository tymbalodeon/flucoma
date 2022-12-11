(defun flucoma--beginning-of-line-point ()
  (beginning-of-line)
  (point))

(defun flucoma--indent (value)
  (+ value sclang-indent-level))

(defun flucoma--position-after-indentation (points-from-end)
  (let ((current-points-from-end (- (point-max) points-from-end)))
    (when (> current-points-from-end (point))
      (goto-char current-points-from-end))))

(defun sclang-indent-line ()
  "Indent the current line as sclang code, returning points shifted."
  (let* ((beginning-of-line-point (flucoma--beginning-of-line-point))
         (indent (calculate-sclang-indent))
         (points-to-shift (- indent (current-column)))
         (points-from-end (- (point-max) (point)))
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
    (flucoma--position-after-indentation points-from-end)
    points-to-shift))

(provide 'flucoma-sclang-indent)
;;; flucoma-sclang-indent.el ends here
