;;; unitime.el --- Emacs interface for Unitime -*- lexical-binding: t -*-

;;; This piece of code it made to be used with the Unitime API and get your schedule for
;;; you LNU Växjö courses.
;;; https://github.com/Kodkollektivet/unitime-api/tree/celery-docker

;; Author: John Herrlin <jherrlin@gmail.com>
;; URL: https://github.com/jherrlin/unitime-el
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.2

;;; Code:

(require 'request)
(require 'json)


(defcustom unitime/courses nil
  "Unitime courses list.

Config example: (setq unitime/courses '(2DV50E))"
  :type 'list)


(defun unitime--courses ()
  "Converts the courses symbols to string."
  (map 'list
       (lambda (x) (symbol-name x))
       unitime/courses))


(defun unitime--request (type course)
  "Get data from Unitime API."
  (let* ((url (if (eq type 'course) "https://unitime.jherrlin.se/api/course/"
                "https://unitime.jherrlin.se/api/lectures/"))
         (response (request
                    url
                    :type "POST"
                    :sync t
                    :data (json-encode `(("course" . ,course)))
                    :headers '(("Content-Type" . "application/json"))
                    :parser 'json-read))
         (data (request-response-data response)))
    (if data
        `(,course . ,data))))


(defun unitime--get (type)
  "Return a list with results from unitime--request.

This function iterates over the courses and get the relavant
data.

Example call:
(unitime--get 'courses)"
  (map 'list (lambda (course)
               (if (eq type 'courses)
                   (unitime--request 'course course)
                 (unitime--request 'lectures course)))
       (unitime--courses)))


(defun unitime--lecture-table-row-header ()
  "Return a string with lecture headers for the orgmode table"
  (concatenate 'string
               "|" "Start datetime" "|" "End datetime"
               "|" "Teacher" "|" "Info" "|" "Description"
               "|" "Room name" "|" "Room floor" "|" "Room lat"
               "|" "Room lon" "|"))


(defun unitime--get-buffer ()
  "Returns the unitime buffer."
  (get-buffer-create "unitime-buffer"))


(defun unitime--parse-lecture-to-table-row-str (course)
  "Parse a lecture alist into a orgmode looking table row string."
  (let ((room (cdr (assoc 'room course))))
    (concatenate 'string
                 "|" (cdr (assoc 'start_datetime course))
                 "|" (cdr (assoc 'end_datetime course))
                 "|" (cdr (assoc 'teacher course))
                 "|" (cdr (assoc 'info course))
                 "|" (cdr (assoc 'description course))
                 "|" (cdr (assoc 'name room))
                 "|" (number-to-string (cdr (assoc 'floor room)))
                 "|" (number-to-string (cdr (assoc 'lat room)))
                 "|" (number-to-string (cdr (assoc 'lon room)))
                 "|")))


(defun unitime--insert-buffer-header ()
  (let ((unitime-buffer (get-buffer-create "unitime-buffer")))
    (with-current-buffer unitime-buffer
      (erase-buffer)
      (funcall 'org-mode)
      (insert "#+TITLE: Unitime-el\n")
      (insert "#+STARTUP: content\n")
      (insert "\n\n"))))


(defun unitime--insert-lecture-table (lectures)
  (let ((unitime-buffer (get-buffer-create "unitime-buffer"))
        (course (car lectures))
        (lectures (cdr lectures)))
    (with-current-buffer unitime-buffer
      (insert (concatenate 'string "* " course "\n"))
      (insert (unitime--lecture-table-row-header))
      (insert "\n|-\n")
      (mapc ;; Iterate over lectures
       (lambda (x)
         (insert (unitime--parse-lecture-to-table-row-str x))
         (insert "\n"))
       lectures)
      (backward-char 1)
      (org-ctrl-c-ctrl-c)
      (point-max)
      (insert "\n\n"))))


(defun unitime ()
  (interactive)
  (if unitime/courses
      (progn
        (unitime--insert-buffer-header)
        (mapc
         (lambda (x)
           (unitime--insert-lecture-table x))
         (unitime--get 'lectures))
        (switch-to-buffer (get-buffer-create "unitime-buffer")))
    (princ "No courses in course list")))


(provide 'unitime)

;;; unitime.el ends here
