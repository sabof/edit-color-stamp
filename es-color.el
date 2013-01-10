
(defun es-color-list-to-hex (color-list)
  (apply 'format "#%02X%02X%02X" color-list))

(defun es-color-normalize-hex (hex-string)
  (if (string-match-p "^#" hex-string)
      (capitalize
       (if (equal (length hex-string) 4)
           (concat
            "#" (mapcar*
                 (lambda (b e)
                   (make-string
                    2 (string-to-char
                       (substring hex-string b e))))
                 (list 1 2 3)
                 (list 2 3 4)))
           hex-string))
      hex-string))

(defun es-color-hex-to-list (hex-color)
  (let ((hex-color (es-color-normalize-hex hex-color)))
    (list (string-to-int (substring hex-color 1 3) 16)
          (string-to-int (substring hex-color 3 5) 16)
          (string-to-int (substring hex-color 5 7) 16))))

(provide 'es-color)