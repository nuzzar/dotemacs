(require 'xml)

;;; Org

;;;; Org Export

(defun dot-org-inline-css (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
	   (path (concat dir "style.css"))
	   (custom-style (or (null dir) (null (file-exists-p path))))
	   (final (if custom-style "~/.emacs.d/org-style.css" path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
			   "<style type=\"text/css\">\n"
			   "<!--/*--><![CDATA[/*><!--*/\n"
			   (with-temp-buffer
			     (insert-file-contents final)
			     (buffer-string))
			   "/*]]>*/-->\n"
			   "</style>\n")))))

(defun dot-org-inline-css (exporter)
  "Insert custom inline css"
  (if (and (eq exporter 'html) dot-org-css-style
           (file-exists-p dot-org-css-style))
      (progn
        (setq org-html-head-include-default-style nil)
        (setq org-html-head (concat
			     "<style type=\"text/css\">\n"
			     "<!--/*--><![CDATA[/*><!--*/\n"
			     (with-temp-buffer
			       (insert-file-contents dot-org-css-style)
			       (buffer-string))
			     "/*]]>*/-->\n"
			     "</style>\n")))
    (setq org-html-head-include-default-style t)
    (setq org-html-head "")))
       
(add-hook 'org-export-before-processing-hook 'dot-org-inline-css)

(defvar dot-org-css-style nil)

(defun dot-org-set-css-style (style)
  "Set org export css style."
  (interactive
   (let ((style (read-file-name "Style: ")))
     (list style)))
  (if (string= style "")
      (setq dot-org-css-style nil)
    (setq dot-org-css-style style)))

;;;; Org Capture

(defvar dot-org nil
  "Symbol used to save data.")

(defvar dot-org-study-completion-parts '("HEAD" "EYES" "NOSE" "MOUTH"
				      "EARS" "ARMS" "HANDS" "LEG" "FOOT"
				      "TORSO" "FULLBODY"))

(setq org-agenda-files "~/.emacs.d/agenda-files")

(setq org-todo-keyword-faces
      '(("SCULPTING" . "red") ("BREAK" . "brightred")
	("FINISHED" . "green")))

(defun dot-org-study-read-part ()
  "Read study part. Function for `org-capture-templates'."
  (let ((part (completing-read
		  "Part: " dot-org-study-completion-parts)))
    part))

(defun dot-org-study-read-timeframe ()
  "Return study timeframe. Function for `org-capture-templates'."
  (let* ((timeframe  (completing-read
		      "Timeframe: " '() nil nil nil nil "30s")))
    timeframe))

(defun dot-org-study-read (prefix)
  (let ((time (format-time-string "%y%m%d-%H%M"))
	(part (dot-org-study-read-part))
	(timeframe (dot-org-study-read-timeframe)))

    (dot-org-put :part part)
    (dot-org-put :timeframe timeframe)
    (dot-org-put :headline
	 (format "%s%s-%s-%s" prefix time part timeframe))
    nil))

(defun dot-org-put (key val)
  (put 'dot-org key val))

(defun dot-org-get (key)
  (get 'dot-org key))

(defun dot-org-work-upwork-read ()
  (let ((id (read-string "Contract ID: ")))
    (dot-org-put :headline id)
    (dot-org-put :title (read-string "Title: "))
    (dot-org-put :contract (format "[[https://www.upwork.com/jobs/%s][%s]]" id id))
    (dot-org-put :type (read-string "Type: "))
    (dot-org-put :budget (read-string "Budget: "))
    (dot-org-put :level (read-string "Level: ")))
  nil)

(defun dot-org-play-bell ()
  "Play bell with `paplay', a workaround in wsl."
  (call-process
   "paplay" "/usr/share/sounds/ubuntu/stereo/bell.ogg"))

(add-hook 'org-timer-done-hook 'dot-org-play-bell)

(setq org-capture-before-finalize-hook 'org-align-all-tags)

(setq org-capture-templates
      '(
	;; Capture templates for Work
	("w" "Work")

	;; Upwork
	("wu" "Upwork" entry (file "/mnt/d/project/threed/upwork/proposal-journaldot-org")
	 "
%(dot-org-work-upwork-read)
* %(dot-org-get :headline)%? :upwork:
  :PROPERTIES:
  :TITLE: %(dot-org-get :title)
  :CONTRACT: %(dot-org-get :contract)
  :TYPE: %(dot-org-get :type)
  :BUDGET: %(dot-org-get :budget)
  :LEVEL: %(dot-org-get :level)
  :END:"
	 :empty-lines 1)

	;; Capture templates for Studies
	("s" "Studies")

	;; Sculpting Studies
	("ss" "Sculpting" entry (file "/mnt/d/studies/sculpting-journaldot-org")
	 "
%(dot-org-study-read \"S\")
* %(dot-org-get :headline)%? :sculpting:
  :PROPERTIES:
  :PART: %(dot-org-get :part)
  :TIMEFRAME: %(dot-org-get :timeframe)
  :ZPROJECT: [[zproject/%(dot-org-get :headline).zpr]]
  :ZTOOL: [[ztool/%(dot-org-get :headline).ztl]]
  :REFERENCE: [[reference/%(dot-org-get :headline).png]]
  :RESULT: [[result/%(dot-org-get :headline).png]]
  :END:"
	 :empty-lines 1)

	;; Drawing Studies
	("sd" "Drawing" entry (file "/mnt/d/studies/drawing-journaldot-org")

	 "
%(dot-org-study-read \"S\")
* %(dot-org-get :headline)%? :drawing:
  :PROPERTIES:
  :PART: %(dot-org-get :part)
  :TIMEFRAME: %(dot-org-get :timeframe)
  :REFERENCE: [[reference/%(dot-org-get :headline).png]]
  :RESULT: [[result/%(dot-org-get :headline).png]]
  :END:"
	 :empty-lines 1)))

(provide 'dot-org)
