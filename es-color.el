;;;  -*- lexical-binding: t -*-
(require 'es-lib)

(defvar es-color-picker-exec "color_picker")
(defvar es-get-color-function 'es-color-launch-qt-picker)
(defvar es-get-color-function 'es-color-launch-internal-picker)
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

(defun es-color-emacs-color-to-hex (color)
  (let ((color-values (color-values color)))
    (apply 'format "#%02x%02x%02x"
           (mapcar (lambda (c) (lsh c -8))
                   color-values))))

(defun es-color--change-stamp (buffer overlay color)
  (when (buffer-live-p buffer)
    (save-excursion
      (with-current-buffer buffer
        (save-excursion
          (goto-char (overlay-start overlay))
          (delete-region (overlay-start overlay) (overlay-end overlay))
          (insert (es-color-list-to-hex color))
          (delete-overlay overlay))))))

(defun* es-color-launch-qt-picker (&optional (color-list (list 0 0 0)) (callback 'ignore))
  (let* (( process
           (apply
            'start-process
            es-color-picker-exec
            "*Messages*"
            es-color-picker-exec
            (mapcar 'int-to-string color-list)))
         ( process-output ""))
    (set-process-filter
     process (lambda (process output)
               (setq process-output
                     (concat process-output output))))
    (set-process-sentinel
     process (lambda (process change)
               (when (string-match
                      "NEW_COLOR \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)"
                      process-output)
                 (funcall
                  callback
                  (mapcar (lambda (num)
                            (string-to-int
                             (match-string-no-properties
                              num process-output)))
                          (list 1 2 3))))))))

(defun es-color-launch-internal-picker (&optional (color-list (list 0 0 0)) (callback 'ignore))
  (list-colors-display
   nil nil
   `(lambda (color)
      (quit-window t)
      (funcall
       (function ,callback)
       (es-color-hex-to-list
        (es-color-emacs-color-to-hex
         color))))))

(defun* es-color-edit-stamp ()
  (interactive)
  (save-excursion
    (when (and (or (eq (char-after) ?\# )
                   (search-backward "#" (- (point) 6) t))
               (looking-at
                "\\(?1:#\\(?:\\(?:[A-Fa-f[:digit:]]\\)\\{3\\}\\)\\{1,2\\}\\)[^A-Fa-f[:digit:]]"))
      (let* (( color-list (es-color-hex-to-list (match-string 1)))
             ( overlay (make-overlay (match-beginning 1) (match-end 1)))
             ( initial-buffer (current-buffer)))
        (overlay-put overlay 'face '(:background "#888888" :foreground "#dddddd"
                                     :box (:line-width 1 :color "#dddddd")))
        (overlay-put overlay 'priority 100)
        (funcall es-get-color-function
                 color-list
                 (apply-partially
                  'es-color--change-stamp
                  initial-buffer
                  overlay))))))

(provide 'es-color)