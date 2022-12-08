(defcustom sclang-indent-level 4
  "*Indentation offset for SCLang statements."
  :group 'sclang-mode
  :type 'integer)

(defun goto-start (start)
  (if start
      (goto-char start)
    (beginning-of-defun)))

(defun get-base (sexp)
  (if sexp
      (save-excursion
        (goto-char sexp)
        (+ (current-indentation) sclang-indent-level))
    0))

(defun is-multiline-comment ()
  (save-excursion
    (back-to-indentation)
    (looking-at "\\*/")))

(defun get-comment-offset ()
  (if (is-multiline-comment)
      1
    0))

(defun get-offset (comment)
  (let (comment-offset (get-comment-offset))
    (* sclang-indent-level (- comment comment-offset))))

(defun get-open-parenthesis ()
  (and (looking-at "\\s)")
       (matching-paren (char-after))))

(defun calculate-sclang-indent (&optional start)
  "Calculate indentation column for current line as sclang code."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          (case-fold-search nil))
      (goto-start start)
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      (let* ((sexp (nth 1 state))
             (string (nth 3 state))
             (comment (nth 4 state)))
        (cond ((null sexp) 0)
              (string (current-indentation))
              ((integerp string) (let ((base (get-base sexp))
                                       (offset (get-offset comment)))
                                   (+ base offset)))
              (t
               (back-to-indentation)
               (let ((open-parenthesis (get-open-parenthesis))
                     (indentation (current-indentation)))
                 (goto-char sexp)
                 (if (or (not open-parenthesis) (eq open-parenthesis (char-after)))
                     (cond ((progn
                              (beginning-of-line)
                              (looking-at sclang-block-regexp))
                            0)
                           (open-parenthesis (current-indentation))
                           (t (+ (current-indentation) sclang-indent-level)))
                   indentation))))))))

(defun get-position-from-end (&optional point)
  (let ((current-point (or point (point))))
    (- (point-max) current-point)))

(defun get-shift-amount (indent)
  (- indent (current-column)))

(defun goto-point (position-from-end)
  (let ((point (get-position-from-end position-from-end)))
    (when (> point (point))
      (goto-char point))))

(defun sclang-indent-line ()
  "Indent current line as sclang code, returning points shifted."
  (let (beginning
        shift-amount
        (indent (calculate-sclang-indent))
        (position-from-end (get-position-from-end))
        (case-fold-search nil))
    (beginning-of-line)
    (setq beginning (point))
    (skip-chars-forward " \t")
    (setq shift-amount (get-shift-amount indent))
    (when (zerop shift-amount)
      (goto-point position-from-end)
      (delete-region beginning (point))
      (indent-to indent)
      ;; if initial point was within line's indentation, position
      ;; after the indentation, else stay at same point in text.
      (goto-point position-from-end))
    shift-amount))
