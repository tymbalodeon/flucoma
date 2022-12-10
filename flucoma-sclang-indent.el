(defun flucoma--get-beginning-of-line-point ()
  (beginning-of-line)
  (point))

(defun flucoma--increase-indent-level (value)
  (+ value sclang-indent-level))

(defun flucoma--position-after-indentation (points-to-end)
  (let ((current-points-to-end (- (point-max) points-to-end)))
    (when (> current-points-to-end (point))
      (goto-char current-points-to-end))))

(defun sclang-indent-line ()
  "Indent the current line as sclang code, returning points shifted."
  (let* ((beginning-of-line-point (flucoma--get-beginning-of-line-point))
         (indent (calculate-sclang-indent))
         (points-shifted (- indent (current-column)))
         (points-to-end (- (point-max) (point)))
         (case-fold-search nil))
    (skip-chars-forward " \t")
    (when (looking-at "\\.")
      (progn (setq indent
                   (flucoma--increase-indent-level indent))
             (setq points-shifted
                   (flucoma--increase-indent-level points-shifted))))
    (if (zerop points-shifted)
        (flucoma--position-after-indentation points-to-end)
      (delete-region beginning-of-line-point (point))
      (indent-to indent)
      (flucoma--position-after-indentation points-to-end))
    points-shifted))

(provide 'flucoma-sclang-indent)
;;; flucoma-sclang-indent.el ends here
