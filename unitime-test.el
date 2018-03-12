;;; unitime-test.el --- ERT test for unitime.el

(load-file "unitime.el")

;; Data

(defun unitime--request-lectures-test-data ()
  "Return lectures dummy data"
  `("2DV50E" .
    [((start_datetime . "2018-03-06T10:15:00")
      (end_datetime . "2018-03-06T12:00:00")
      (teacher . "Jesper Andersson")
      (info . "Seminar")
      (description . "")
      (room
       (name . "D1167")
       (floor . 1)
       (lat . 56.855651)
       (lon . 14.828812)))
     ((start_datetime . "2018-05-29T10:15:00")
      (end_datetime . "2018-05-29T12:00:00")
      (teacher . "Jesper Andersson")
      (info . "Seminar")
      (description . "")
      (room
       (name . "D1167")
       (floor . 1)
       (lat . 56.855651)
       (lon . 14.828812)))]))

(defun unitime--request-course-test-data ()
  "Return course dummy data"
  `("2DV50E"
    (code . "2DV50E")
    (name . "Degree Project at Bachelor Level")
    (speed . "50%")
    (points . "15 hp")
    (syllabus . "http://api.kursinfo.lnu.se/GenerateDocument.ashx?templatetype=coursesyllabus&code=2DV50E&documenttype=pdf&lang=en")))


;; Test cases

(ert-deftest unitime--courses ()
  "Test that unitime--courses returns list of strings"
  (setq unitime/courses '(2DV50E))
  (should
   (equal (car (unitime--courses))
          "2DV50E")))


(ert-deftest unitime--request-valid-course ()
  "Test a valid course"
  (let* ((data (unitime--request 'course "2DV50E"))
         (course (car data))
         (data (cdr data)))
    (should
     (equal course
            "2DV50E"))
    (should
     (assoc 'name data))))


(ert-deftest unitime--request-not-valid-course ()
  "Test a valid course"
    (should-not
     (unitime--request 'course "HEJSAN")))


(ert-deftest unitime--lecture-table-row-header ()
  (should
   (string="|Start datetime|End datetime|Teacher|Info|Description|Room name|" (unitime--lecture-table-row-header))))


;;; unitime-test.el ends here
