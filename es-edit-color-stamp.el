;;; es-edit-color-stamp.el --- Edit a hex color stamp, using a QT or the internal color picker  -*- lexical-binding: t -*-
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/es-edit-color-stamp
;;; Package-Requires: ((es-lib "0.2"))

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-edit-color-stamp
;; The latest version, and all the relevant information can be found there.
;;
;; You should find a qt_color_picker folder in the same directory as this file.
;; If you want to use the QT color picker, go to that folder and run:

;; $ qmake qt_color_picker.pro; make

;; then move the qt_color_picker executable somewhere in your path, or point the
;; es-color-qt-picker-exec variable to it's location.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'es-lib)

(defface es-color-stamp-highlight
    '((t (:background "#888888" :foreground "#dddddd"
          :box (:line-width 1 :color "#dddddd"))))
  "Face used to highighlight color stamps while editing them.")

(defvar es-color-qt-picker-exec "qt_color_picker")
(defvar es-color-picker-function
  #'(lambda (&rest args)
    (if (executable-find es-color-qt-picker-exec)
        (apply 'es-color-launch-qt-picker args)
        (apply 'es-color-launch-internal-picker args))))
(defvar es-color-at-point-function
  #'(lambda (&rest args)
      (or (es-color-color-at-point-hex)
          (and (bound-and-true-p rainbow-mode)
               (es-color-color-at-point-rainbow))))
  "The function must returns a list like this ((R G B) beginning end). The RGB
values should be from the 0-255 range.")

(defun es--color-change-stamp (buffer overlay color)
  (when (buffer-live-p buffer)
    (save-excursion
      (with-current-buffer buffer
        (save-excursion
          (when color
            (goto-char (overlay-start overlay))
            (delete-region (overlay-start overlay) (overlay-end overlay))
            (insert (es-color-list-to-hex color)))
          (delete-overlay overlay))))))

(defun* es-color-launch-qt-picker (&optional (color-list (list 0 0 0)) (callback 'ignore))
  (let* (( process
           (apply
            'start-process
            es-color-qt-picker-exec
            "*Messages*"
            es-color-qt-picker-exec
            (mapcar 'int-to-string color-list)))
         ( process-output ""))
    (set-process-filter
     process (lambda (process output)
               (setq process-output
                     (concat process-output output))))
    (set-process-sentinel
     process (lambda (process change)
               (funcall
                callback
                (if (string-match
                     "NEW_COLOR \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)"
                     process-output)
                    (mapcar (lambda (num)
                              (string-to-int
                               (match-string-no-properties
                                num process-output)))
                            (list 1 2 3))
                    nil))))))

(defun* es-color-launch-internal-picker (&optional (color-list (list 0 0 0)) (callback 'ignore))
  (list-colors-display
   nil nil
   `(lambda (color)
      (quit-window t)
      (funcall
       (function ,callback)
       (es-color-hex-to-list
        (es-color-emacs-color-to-hex
         color))))))

(defun es-color-color-at-point-hex ()
  (save-excursion
    (and (or (eq (char-after) ?\# )
             (search-backward "#" (- (point) 6) t))
         (looking-at
          "\\(?1:#\\(?:\\(?:[A-Fa-f[:digit:]]\\)\\{3\\}\\)\\{1,2\\}\\)\\(?:[^A-Fa-f[:digit:]]\\|\\'\\)")
         (list (es-color-hex-to-list (match-string 1))
               (match-beginning 1) (match-end 1)))))

(defun* es-color-color-at-point-rainbow ()
  "Will pick up any face that has set it's background explicitly.
Will replace it with a color stamp, disregarding any possible alpha value."
  (save-excursion
    (let* ((face (or (getf (text-properties-at (point)) 'face)
                     (return-from es-color-color-at-point-rainbow)))
           (bg (or (and (consp face)
                        (second (assoc :background face)))
                   (return-from es-color-color-at-point-rainbow)))
           beginning end)
      (while (eq (getf (text-properties-at (point)) 'face)
                 face)
        (forward-char -1))
      (forward-char 1)
      (setq beginning (point))
      (while (eq (getf (text-properties-at (point)) 'face)
                 face)
        (forward-char 1))
      ;; (forward-char -1)
      (setq end (point))
      (list (es-color-hex-to-list (es-color-emacs-color-to-hex bg))
            beginning end))))

;;;###autoload
(defun* es-edit-color-stamp ()
  (interactive)
  (multiple-value-bind (color-list beginning end) (funcall es-color-at-point-function)
    (unless color-list
      (return-from es-edit-color-stamp))
    (let* (( overlay (make-overlay beginning end))
           ( initial-buffer (current-buffer)))
      (overlay-put overlay 'face 'es-color-stamp-highlight)
      (overlay-put overlay 'priority 100)
      (funcall es-color-picker-function
               color-list
               (apply-partially
                'es--color-change-stamp
                initial-buffer
                overlay)))))

(defalias 'es-color-edit-stamp 'es-edit-color-stamp)

(provide 'es-edit-color-stamp)
;;; es-edit-color-stamp.el ends here
