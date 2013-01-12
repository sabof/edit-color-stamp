(defvar es-color-picker-exec "color_picker")
(defvar es-get-color-function 'es-get-color-qt)
;; Relevant: widget-color--choose-action

(defun es-color-list-to-hex (color-list)
  (apply 'format "#%02X%02X%02X" color-list))

(defun es-color-normalize-hex (hex-string)
  (if (string-match-p "^#" hex-string)
      (upcase
       (if (= (length hex-string) 4)
           (apply 'concat "#"
                  (mapcar
                   (lambda (pair)
                     (make-string
                      2 (string-to-char
                         (substring
                          hex-string (car pair) (cdr pair)))))
                   '((1 . 2) (2 . 3) (3 . 4))))
           hex-string))
      hex-string))

(defun es-color-hex-to-list (hex-color)
  (let ((hex-color (es-color-normalize-hex hex-color)))
    (list (string-to-int (substring hex-color 1 3) 16)
          (string-to-int (substring hex-color 3 5) 16)
          (string-to-int (substring hex-color 5 7) 16))))

(defun* es-get-color-qt (&optional (color-list (list 0 0 0)) (callback 'ignore))
  (let* (( result-string
           (shell-command-to-string
            (apply 'format "%s %s %s %s"
                   es-color-picker-exec
                   color-list))))
    (save-match-data
      (if (string-match
           "NEW_COLOR \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)"
           result-string)
          (mapcar (lambda (num)
                    (string-to-int
                     (match-string-no-properties
                      num result-string)))
                  (list 1 2 3))
          (return-from es-color-edit-stamp)))))

(defun* es-color-edit-stamp ()
  (interactive)
  (save-excursion
    (when (and (or (eq (char-after) ?\# )
                   (search-backward "#" (- (point) 6) t))
               (looking-at
                "#\\(?1:\\(?:[A-Fa-f[:digit:]]\\)\\{3,6\\}\\)[^A-Fa-f[:digit:]]"))
      (let* (( color-list (es-color-hex-to-list (match-string 1)))
             ( new-color-list
               (or (funcall es-get-color-function color-list)
                   (return-from es-color-edit-stamp)))
             ( new-stamp (es-color-list-to-hex new-color-list)))
        (replace-match new-stamp t t nil 1)))))

(provide 'es-color)