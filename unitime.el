;;; unitime.el --- Emacs interface for Unitime -*- lexical-binding: t -*-

(require 'request)
(require 'json)


(defvar unitime/courses-list (list "2DV50E")
  "Unitime courses as lists in a string.i

Example:
(defvar unitime/courses-list (list \"2DV50E\"))")

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
    data))

(defun unitime--courses ()
  (map 'list (lambda (course)
            (unitime--request 'course course))
          unitime/courses-list))

(defun unitime--lectures ()
  (map 'list (lambda (course)
               (unitime--request 'lectures course))
       unitime/courses-list))

(defun unitime--lecture-table-row-header ()
  "Return a string with lecture headers for the orgmode table"
  (concatenate 'string
               "|"
               "Start datetime"
               "|"
               "End datetime"
               "|"
               "Teacher"
               "|"
               "Info"
               "|"
               "Description"
               "|"
               "Room name"
               "|"
               "Room floor"
               "|"
               "Room lat"
               "|"
               "Room lon"
               "|"
               ))


(defun unitime--parse-lecture-to-table-row-str (course)
  "Parse a lecture alist into a orgmode looking table row string."
  (setq room (cdr (assoc 'room course)))
  (concatenate 'string
               "|"
               (cdr (assoc 'start_datetime course))
               "|"
               (cdr (assoc 'end_datetime course))
               "|"
               (cdr (assoc 'teacher course))
               "|"
               (cdr (assoc 'info course))
               "|"
               (cdr (assoc 'description course))
               "|"
               (cdr (assoc 'name room))
               "|"
               (number-to-string (cdr (assoc 'floor room)))
               "|"
               (number-to-string (cdr (assoc 'lat room)))
               "|"
               (number-to-string (cdr (assoc 'lon room)))
               "|"))


(defun unitime--insert-buffer-header ()
  (let ((unitime-buffer (get-buffer-create "unitime-buffer")))
    (with-current-buffer unitime-buffer
      (erase-buffer)
      (funcall 'org-mode)
      (insert "#+TITLE: Unitime-el\n")
      (insert "#+AUTHOR: John Herrlin\n")
      (insert "#+EMAIL: jherrlin@gmail.com\n")
      (insert "#+STARTUP: content\n")
      (insert "\n\n"))))


(defun unitime--insert-lecture-table (lectures)
  (let ((unitime-buffer (get-buffer-create "unitime-buffer")))
    (with-current-buffer unitime-buffer
      (insert (unitime--lecture-table-row-header))
      (insert "\n|-\n")
      (mapc
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
  (unitime--insert-buffer-header)
  (mapc
   (lambda (x)
     (unitime--insert-lecture-table x))
   (unitime--lectures))
  (switch-to-buffer (get-buffer-create "unitime-buffer")))


(provide 'unitime)

;;; unitime.el ends here
