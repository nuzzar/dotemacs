;;; init.el --- basic lisp subroutines for Emacs -*- lexical-binding:t -*-

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(package-initialize)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      ring-bell-function nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq package-enable-at-startup nil)

(setq package-archives
      (list (cons "melpa" "https://melpa.org/packages/")
	    (cons "gnu" "http://elpa.gnu.org/packages/")))

;; This is only needed once, near the top of the file
(eval-when-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp")
                '("~/.emacs.d/dot")
                '("~/.emacs.d/lisp/use-package")))
  (require 'use-package))


;;; ENV

(setenv "scripts" "/c/coralian/_threed/lib/_scripts")
(setenv "project" "/c/coralian/_threed/_project")
(setenv "artwork" "/c/coralian/_threed/_project/artwork")
(setenv "mcmhouse" "/c/coralian/_threed/_project/artwork")
(setenv "wildcat" "/c/coralian/_threed/_project/wildcat")

;;; Package

(use-package compat
  :ensure t)

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))
  
(use-package js
  :config
  (setq js-indent-level 2))

(use-package simpleclip
  :ensure t)

(use-package better-defaults
  :ensure t)

(use-package magit
  :ensure t)

(use-package helm
  :ensure t
  :init
  (helm-mode)
  :config
  (require 'helm-config)
  :bind
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files)
  ("C-x b"   . helm-mini)
  ("C-x C-r" . helm-recentf)
  ("C-c i"   . helm-imenu)
  ("M-y"     . helm-show-kill-ring))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle 0.5
        which-key-idle-delay 50)
  (which-key-setup-minibuffer))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 4
        company-selection-wrap-around t))

(use-package flycheck
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package org
  :ensure t
  :custom (org-adapt-indentation t)
  ;; (my/org-roam-refresh-agenda-list)
  (org-clock-sound "/usr/share/sounds/freedesktop/stereo/bell.oga")
  :bind
  ("C-c a" . org-agenda))

(use-package dot-org)

(use-package dot-kmacro)

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  ;; This is usually the default, but keep in mind it must be nil
  (setq org-hide-leading-stars nil)
  ;; This line is necessary.
  (setq org-superstar-leading-bullet ?\s)
  ;; If you use Org Indent you also need to add this, otherwise the
  ;; above has no effect while Indent is enabled.
  (setq org-indent-mode-turns-on-hiding-stars nil))


;; (use-package org-bullet
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n b" . my/org-roam-capture-inbox)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

;;; Org-Roam

(defvar my/org-roam-project nil)

(defvar my/org-roam-project-path "/c/coralian/_threed/_project")

(defvar my/org-roam-project-list '(("Artwork" . "_artwork")
                                   ("Wildcat" . "wildcat")
                                   ("Creators3D" . "creators3d")))

(defvar my/org-roam-workflow-list '("PM" "LOW" "EGG" "SCAN"))

(defun my/org-roam-get-uid-list (main project)
  (let ((path (file-name-concat my/org-roam-project-path
                      (cdr (assoc project my/org-roam-project-list))
                      "ref" main)))
    (directory-files path nil "[^.]+")))

(defun my/org-roam-on-project-file ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         "^#\\+filetags\\: \\(.*\\)$" nil t)
        (match-string 1)
      nil)))

;; FIXME: Doesn't work.
(defun my/org-roam-workflow-is-set ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         "^#\\+category\\: \\(.*\\)$" nil t)
        (match-string 1)
      nil)))

(defun my/org-roam-get-project ()
  (completing-read "Project: " my/org-roam-project-list))

(defun my/org-roam-get-workflow ()
  (unless (my/org-roam-workflow-is-set)
    (completing-read "Workflow: " my/org-roam-workflow-list)))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Artwork"))
  (setcdr (last org-agenda-files) (my/org-roam-list-notes-by-tag "Wildcat"))
  (setcdr (last org-agenda-files) (my/org-roam-list-notes-by-tag "Creators3d"))
  org-agenda-files)

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-find-project ()
  (interactive)

  ;; Set the project
  (setq my/org-roam-project (my/org-roam-get-project))

  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook
            #'my/org-roam-project-finalize-hook)

  (let* ((node (org-roam-node-read nil (my/org-roam-filter-by-tag my/org-roam-project)))
         (title (org-roam-node-title node))
         (uid-list (my/org-roam-get-uid-list title my/org-roam-project))
         (uid (completing-read "UID: " uid-list))
         (lv1 "* Tasks")
         (lv2 (format "** %s" uid))
         (lv3 (format "*** TODO %s" uid))

         (template `(("p" "project" plain ,(format "%s\n\n%s\n\n%s%%?" lv1 lv2 lv3)
                      :if-new (file+head
                               "%<%Y%m%d%H%M%S>-${slug}.org"
                               ,(format "#+title: ${title}\n#+category: %s\n#+filetags: %s"
                                        (my/org-roam-get-workflow) my/org-roam-project))
                      :unnarrowed t
                      :empty-lines 1
                      :prepend t))))
         

    (if (org-roam-node-file node)
        (org-roam-node-visit node nil)
      (org-roam-capture-
       :node node
       :templates template
       :props '(:finalize find-file)))))

(defun my/org-roam-capture-task ()
  (interactive)

  ;; Set the project
  (let ((project (my/org-roam-on-project-file)))
    (if project
        (setq my/org-roam-project project)
      (setq my/org-roam-project (my/org-roam-get-project))))

  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook
            #'my/org-roam-project-finalize-hook)

  (let* ((node (org-roam-node-read nil (my/org-roam-filter-by-tag my/org-roam-project)))
         (title (org-roam-node-title node))
         (uid-list (my/org-roam-get-uid-list title my/org-roam-project))
         (uid (completing-read "UID: " uid-list))
         ; remove round
         ; (round (completing-read "Round: " '()))
         (template `(("p" "project" plain ,(format "*** TODO %s" uid)
                      :if-new (file+head+olp
                               "%<%Y%m%d%H%M%S>-${slug}.org"
                               ,(format "#+title: ${title}\n#+category: %s\n#+filetags: %s\n\n* Tasks"
                                        (my/org-roam-get-workflow) my/org-roam-project)
                               ("Tasks" ,uid))
                      :unnarrowed t
                      :empty-lines 1
                      :prepend t))))

    ;; capture the new task, creating the project file if necessary
    (org-roam-capture-
     :node node
     :templates template)))


(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-buffer)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    (if (member (car (org-get-tags)) (mapcar 'car my/org-roam-project-list))
    ;; Only refile if the target file is different than the current file
        (unless (equal (file-truename today-file)
                       (file-truename (buffer-file-name)))
          (org-refile nil nil (list "Tasks" today-file nil pos)))
      nil)))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (cond ((equal org-state "DONE")
                      (my/org-roam-copy-todo-to-today))
                     ((equal org-state "CANCEL")
                      (my/org-roam-copy-todo-to-today))
                     ((equal org-state "PROG")
                      (org-clock-in))
                     ((equal org-state "WAIT")
                      (org-clock-out)))))


;; TODO keywords.
(setq org-todo-keywords
  (list '(sequence "TODO(t)" "PROG(p)" "WAIT(w)" "QA(q@)" "|" "DONE(d!)" "CANCEL(c@)")))

;; Show the daily agenda by default.
(setq org-agenda-span 14)

;; Hide tasks that are scheduled in the future.
(setq org-agenda-todo-ignore-scheduled 'future)

;; Use "second" instead of "day" for time comparison.
;; It hides tasks with a scheduled time like "<2020-11-15 Sun 11:30>"
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

;; Hide the deadline prewarning prior to scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;; Customized view for the daily workflow. (Command: "C-c a n")
(setq org-agenda-custom-commands
  '(("n" "Agenda / TODO / PROG / WAIT / QA"
     ((agenda "" nil)
      (todo "TODO" nil)
      (todo "PROG" nil)
      (todo "WAIT" nil)
      (todo "QA" nil))
     nil)))
  
;;; Custom Customization

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


;;; Finalization

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

;; init.el ends here
