;;; dot-org.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;;; Org

;;;; Org Export

(require 'xml)

(defun dot/org-inline-css (exporter)
  "Insert custom inline css"
  (if (and (eq exporter 'html) dot/org-css-style
           (file-exists-p dot/org-css-style))
      (progn
        (setq org-html-head-include-default-style nil)
        (setq org-html-head (concat
			     "<style type=\"text/css\">\n"
			     "<!--/*--><![CDATA[/*><!--*/\n"
			     (with-temp-buffer
			       (insert-file-contents dot/org-css-style)
			       (buffer-string))
			     "/*]]>*/-->\n"
			     "</style>\n")))
    (setq org-html-head-include-default-style t)
    (setq org-html-head "")))
       
(add-hook 'org-export-before-processing-hook 'dot/org-inline-css)

(defvar dot/org-css-style nil)

(defun dot/org-set-css-style (style)
  "Set org export css style."
  (interactive
   (let ((style (read-file-name "Style: ")))
     (list style)))
  (if (string= style "")
      (setq dot/org-css-style nil)
    (setq dot/org-css-style style)))

;;;; Org Capture

;; (defvar dot/org nil
;;   "Symbol used to save data.")

;; (defvar dot/org-study-completion-parts '("HEAD" "EYES" "NOSE" "MOUTH"
;; 				      "EARS" "ARMS" "HANDS" "LEG" "FOOT"
;; 				      "TORSO" "FULLBODY"))

;; ;; (setq org-agenda-files "~/.emacs.d/agenda-files")

;; (setq org-todo-keyword-faces
;;       '(("SCULPTING" . "red") ("BREAK" . "brightred")
;; 	("FINISHED" . "green")))

;; (defun dot/org-study-read-part ()
;;   "Read study part. Function for `org-capture-templates'."
;;   (let ((part (completing-read
;; 		  "Part: " dot/org-study-completion-parts)))
;;     part))

;; (defun dot/org-study-read-timeframe ()
;;   "Return study timeframe. Function for `org-capture-templates'."
;;   (let* ((timeframe  (completing-read
;; 		      "Timeframe: " '() nil nil nil nil "30s")))
;;     timeframe))

;; (defun dot/org-study-read (prefix)
;;   (let ((time (format-time-string "%y%m%d-%H%M"))
;; 	(part (dot/org-study-read-part))
;; 	(timeframe (dot/org-study-read-timeframe)))

;;     (dot/org-put :part part)
;;     (dot/org-put :timeframe timeframe)
;;     (dot/org-put :headline
;; 	 (format "%s%s-%s-%s" prefix time part timeframe))
;;     nil))

;; (defun dot/org-put (key val)
;;   (put 'dot/org key val))

;; (defun dot/org-get (key)
;;   (get 'dot/org key))

;; (defun dot/org-work-upwork-read ()
;;   (let ((id (read-string "Contract ID: ")))
;;     (dot/org-put :headline id)
;;     (dot/org-put :title (read-string "Title: "))
;;     (dot/org-put :contract (format "[[https://www.upwork.com/jobs/%s][%s]]" id id))
;;     (dot/org-put :type (read-string "Type: "))
;;     (dot/org-put :budget (read-string "Budget: "))
;;     (dot/org-put :level (read-string "Level: ")))
;;   nil)

;; (defun dot/org-play-bell ()
;;   "Play bell with `paplay', a workaround in wsl."
;;   (call-process
;;    "paplay" "/usr/share/sounds/ubuntu/stereo/bell.ogg"))

;; (add-hook 'org-timer-done-hook 'dot/org-play-bell)

;; (setq org-capture-before-finalize-hook 'org-align-all-tags)

;; (setq org-capture-templates
;;       '(
;; 	;; Capture templates for Studies
;; 	("s" "Studies")

;; 	;; Sculpting Studies
;; 	("ss" "Sculpting" entry (file "/mnt/d/studies/sculpting-journal.org")
;; 	 "
;; %(dot/org-study-read \"S\")
;; * %(dot/org-get :headline)%? :sculpting:
;;   :PROPERTIES:
;;   :PART: %(dot/org-get :part)
;;   :TIMEFRAME: %(dot/org-get :timeframe)
;;   :ZPROJECT: [[sculpting/%(dot/org-get :headline).zpr]]
;;   :ZTOOL: [[sculpting/%(dot/org-get :headline).ztl]]
;;   :RESULT: [[sculpting/res-%(dot/org-get :headline).png]]
;;   :END:"
;; 	 :empty-lines 1)

;; 	;; Drawing Studies
;; 	("sd" "Drawing" entry (file "/mnt/d/studies/drawing-journal.org")

;; 	 "
;; %(dot/org-study-read \"S\")
;; * %(dot/org-get :headline)%? :drawing:
;;   :PROPERTIES:
;;   :PART: %(dot/org-get :part)
;;   :TIMEFRAME: %(dot/org-get :timeframe)
;;   :REFERENCE: [[drawing/ref-%(dot/org-get :headline).png]]
;;   :RESULT: [[drawing/res-%(dot/org-get :headline).png]]
;;   :END:"
;; 	 :empty-lines 1)))


;; ;;;; Org Clock

;; (defun dot/org-clock-get-tr-for-ivl (buffer tstart-str tend-str &optional limit)
;;   "Return clocking information touching a given time interval."
;;   (cl-assert (and buffer (get-buffer buffer)) nil "Error: :buffer must be defined")
;;   (with-current-buffer buffer
;;     (save-excursion
;;       (let ((re (concat "^\\(\\*+[ \t]*.*\\)\\|^[ \t]*"
;;                         org-clock-string
;;                         "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
;;             (counter 0)
;;             (tmphd "BEFORE FIRST HEADING")
;;             (tstart (org-time-string-to-seconds tstart-str))
;;             (tend (org-time-string-to-seconds tend-str))
;;             (limit (or limit (point-max)))
;;             headings timelst
;;             lvl title result ts te)
;;         (goto-char (point-min))
;;         (cl-block myblock
;;           (while (re-search-forward re nil t)
;;             (cond
;;              ;; found a org heading
;;              ((match-end 1)
;;               (if (> (length timelst) 0)
;;                   (setq result (nconc result (list (list
;;                                                     (copy-sequence headings)
;;                                                     timelst)))))
;;               (setq tmphd (org-heading-components)
;;                     lvl (car tmphd)
;;                     title (nth 4 tmphd)
;;                     timelst nil)
;;               ;; maintain a list of the current heading hierarchy
;;               (cond
;;                ((> lvl (length headings))
;;                 (setq headings  (nconc headings `(,title))))
;;                ((= lvl (length headings))
;;                 (setf (nth (1- lvl) headings) title))
;;                ((< lvl (length headings))
;;                 (setq headings (cl-subseq headings 0 lvl))
;;                 (setf (nth (1- lvl) headings) title))))
;;              ;; found a clock line with 2 timestamps
;;              ((match-end 3)
;;               (setq ts (save-match-data (org-time-string-to-seconds
;;                                          (match-string-no-properties 2)))
;;                     te (save-match-data (org-time-string-to-seconds
;;                                          (match-string-no-properties 3))))
;;               ;; the clock lines progress from newest to oldest. This
;;               ;; enables skipping the rest if this condition is true
;;               (if (> tstart te)
;;                   (if (re-search-forward "^\\(\\*+[ \t]*.*\\)" nil t)
;;                       (beginning-of-line)
;;                     (goto-char (point-max)))
;;                 (when (> tend ts)
;;                   (setq timelst (nconc timelst (list
;;                                                 (list (match-string-no-properties 2)
;;                                                       (match-string-no-properties 3)))))))))
;;             (when (>= (point) limit)
;;               (cl-return-from myblock))))
;;         (if (> (length timelst) 0)
;;             (setq result (nconc result (list (list (copy-sequence headings)
;;                                                    timelst)))))
;;         result))))

;; (defun dot/org-slice-tr (tstart-str tend-str cutstart-str cutend-str)
;;   "Return time slice of a time range in minutes."
;;   (let ((tstart (org-time-string-to-seconds tstart-str))
;;         (tend (org-time-string-to-seconds tend-str))
;;         (cutstart (if (stringp cutstart-str)
;;                       (org-time-string-to-seconds cutstart-str)
;;                     cutstart-str))
;;         (cutend (if (stringp cutend-str)
;;                     (org-time-string-to-seconds cutend-str)
;;                   cutend-str))
;;         result)
;;     (setq result (max 0
;;                       (/  (- (min tend cutend) (max tstart cutstart))
;;                           60)))))

;; (defun dot/org-clock-hourly-report (struct tstart-str tend-str)
;;   "Return a structure containing a per hour report within an interval."
;;   (let* ((tstart (org-time-string-to-seconds tstart-str))
;;          (tend (org-time-string-to-seconds tend-str))
;;          (delta 3600)
;;          (intvls (cl-loop for tm from tstart to (- tend delta) by delta
;;                           collect `(,tm ,(+ tm delta))))
;;          result)
;;     ;; iterate over the intervals for the final table
;;     (cl-loop for iv in intvls
;;              collect (list
;;                       iv
;;                       (let* ((cutstart (car iv))
;;                              (cutend (cadr iv))
;;                              (tmsum 0.0)
;;                              headings trlst)
;;                         ;; iterate over the task structure
;;                         (cl-loop
;;                          for item in struct
;;                          do (progn
;;                               (setq headings (car item)
;;                                     trlst (cadr item)
;;                                     ;; sum up the parts of the time
;;                                     ;; ranges falling into this
;;                                     ;; interval
;;                                     tmsum (apply
;;                                            #'+
;;                                            (mapcar
;;                                             (lambda (tr)
;;                                               (dot/org-slice-tr (car tr)
;;                                                                    (cadr tr)
;;                                                                    cutstart
;;                                                                    cutend))
;;                                             trlst))))
;;                          if (> tmsum 0) collect `(,headings ,tmsum) into lst
;;                          finally return lst))))))

;; (defun org-dblock-write:nagora-report (params)
;;   "Fill in a dynamic timesheet reporting block."
;;   (let* ((buffer (plist-get params :buffer))
;;          (day (symbol-name (plist-get params :day)))
;;          (tstart (if (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" day)
;;                      day
;;                    (error "Error: day format must be in YYYY-mm-dd format")))
;;          (tend (concat day " 23:59"))
;;          (table (dot/org-clock-hourly-report
;;                  (dot/org-clock-get-tr-for-ivl buffer tstart tend)
;;                  tstart tend)))
;;     (insert (format "#+CAPTION: timesheet for day %s\n" day))
;;     (insert "|Time|Customer| Task |Minutes|\n|------\n")
;;     (cl-loop
;;      for item in table
;;      do (let ((ivl (car item))
;;               (entries (cadr item)))
;;           (cl-loop for e in entries
;;                    do (let ((headings (car e))
;;                             (minutes (cadr e)))
;;                         (insert (concat
;;                                  "|"
;;                                  (format-time-string "%H:%M" (seconds-to-time
;;                                                               (car ivl)))
;;                                  "-"
;;                                  (format-time-string "%H:%M" (seconds-to-time
;;                                                               (cadr ivl)))
;;                                  "|" (nth 1 headings)
;;                                  "|" (car (last headings))
;;                                  "|" (format "%d" minutes)
;;                                  "|\n"))))))
;;     (insert "|----\n|TOTAL||||\n#+TBLFM: @>$>=vsum(@I..@II)")
;;     (search-backward "Time")
;;     (org-table-align)
;;     (org-table-recalculate '(16))))

(provide 'dot-org)
