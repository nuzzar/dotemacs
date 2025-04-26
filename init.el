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

(setenv "scripts" "/mnt/e/_threed/lib/_scripts")
(setenv "project" "/mnt/e/_threed/_project")
(setenv "artwork" "/mnt/e/_threed/_project/artwork")
(setenv "orcavia" "/mnt/e/_threed/_project/orcavia")
(setenv "mcmhouse" "/mnt/e/_threed/_project/mcmhouse")
(setenv "wildcat" "/mnt/e/_threed/_project/wildcat")

; (desktop-save-mode 1)
; (add-hook 'server-after-make-frame-hook (lambda () (desktop-save-mode 1)))
; (add-hook 'server-after-make-frame-hook 'desktop-read)

;;; Package

;; (use-package desktop
;;   :init
;;   (setq desktop-save t)
;;   (desktop-save-mode 1))
  ;; (add-hook 'server-after-make-frame-hook (lambda () (desktop-save-mode 1)))
  ;; (add-hook 'server-after-make-frame-hook 'desktop-read))

(use-package recentf
  :ensure t
  :init
  (setq recentf-save-file "~/.emacs.d/var/recentf-save.el")
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200)
  ;; Save after 10s idle when opening a file
  (add-hook 'find-file-hook
            (lambda () 
              (run-with-idle-timer 33 nil #'recentf-save-list)))
  ;; Save immediately when closing a client frame
  (add-hook 'server-done-hook #'recentf-save-list)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t #'recentf-save-list)))

(use-package no-littering
  :ensure t
  :requires recentf
  :init
  (setq recentf-save-file "~/.emacs.d/var/recentf-save.el")

  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package compat
  :ensure t)
  
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2)) ; Adjust indentation

(use-package typescript-mode
  :ensure t) ; For TypeScript support

;; (use-package prettier-js
;;   :ensure t
;;   :hook (js2-mode . prettier-js-mode)) ; Auto-format on save

(use-package lsp-mode
  :ensure t
  :hook (js2-mode . lsp)
  :config
  (setq lsp-clients-typescript-prefer-pnpm t) ; Prefer pnpm for LSP
  (setq lsp-clients-typescript-tls-path "typescript-language-server")
  (add-to-list 'lsp-language-id-configuration '(js2-mode . "javascript")))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-node)
  (dap-node-setup)) ; Configures Node.js debugging

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(add-hook 'js2-mode-hook #'tree-sitter-mode)

(use-package jest-test-mode
  :ensure t
  :hook (js2-mode . jest-test-mode))

(use-package skewer-mode
  :ensure t
  :hook (js2-mode . skewer-mode))

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
        company-selection-wrap-around t)
  :hook (js2-mode . company-mode))

(use-package flycheck
  :ensure t
  :hook (js2-mode . flycheck-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pyenv-mode
  :ensure t
  :demand f
  :preface
  (defvar pyenv-current-version nil nil)

  (defun set-pyenv-version-path ()
    "Automatically activates pyenv version if .python-version file exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((pyenv-version-path (f-expand ".python-version" path)))
         (if (f-exists? pyenv-version-path)
	     (progn
               (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))) t))))))
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  (add-hook 'find-file-hook 'set-pyenv-version-path)
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package emacsql-sqlite-builtin
  :ensure t)

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

;;(setq org-roam-database-connector 'sqlite-builtin)


(use-package pomm
  :ensure t
  :commands (pomm pomm-third-time)
  :init
  (add-hook 'pomm-mode-line-mode-hook 'pomm-update-mode-line-string)
  (add-hook 'pomm-on-tick-hook 'pomm-update-mode-line-string)
  (add-hook 'pomm-on-status-changed-hook 'pomm-update-mode-line-string)
  :config
  (setq pomm-audio-enabled t)
  (setq pomm-audio-player-executable "paplay")
  (setq pomm-ask-before-work t)
  (setq pomm-audio-files
        '((work . "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg")
          (tick . "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg")
          (short-break . "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg")
          (break . "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg")
          (long-break . "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg")
          (stop . "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg"))))



(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  ;;
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

(defvar my/org-roam-project-path "/mnt/e/_threed/_project")

(defvar my/org-roam-project-list '(("Artwork" . "_artwork")
                                   ("Wildcat" . "wildcat")
                                   ("Orcavia" . "orcavia")
                                   ("DEV" . "_dev")
                                   ("C3D" . "c3d")))

(defvar my/org-roam-workflow-list '("PM" "LOW" "EGG" "SCAN" "CODING"))

(defun my/org-roam-get-uid-path (main project)
  (let ((project-path (cdr (assoc project my/org-roam-project-list))))
    (if (string-equal project "DEV")
        (file-name-concat my/org-roam-project-path project-path "blender" main)
      (file-name-concat my/org-roam-project-path project-path "ref" main))))

(defun my/org-roam-get-uid-list (main project)
  (let* ((uid-parent-path (my/org-roam-get-uid-path main project))
         (uid-path (mapcar (lambda (path)
                           (if (file-directory-p path)
                               path
                             nil))
                           (directory-files uid-parent-path t "[^.]")))
         (uid-list (mapcar (lambda (path)
                             (string-remove-prefix (concat uid-parent-path "/") path))
                           uid-path)))
    (or (remove nil uid-list)
        (list main))))

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
  (setcdr (last org-agenda-files) (my/org-roam-list-notes-by-tag "Orcavia"))
  (setcdr (last org-agenda-files) (my/org-roam-list-notes-by-tag "Dev"))
  (setcdr (last org-agenda-files) (my/org-roam-list-notes-by-tag "C3D"))
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
                     ((equal org-state "TODO")
                      (org-clock-out))
                     ((equal org-state "PROG")
                      (org-clock-in))
                     ((equal org-state "WAIT")
                      (org-clock-out))
                     ((equal org-state "RFU")
                      (org-clock-out))
                     ((equal org-state "RFM")
                      (org-clock-out))
                     ((equal org-state "QA")
                      (org-clock-out)))))


;; TODO keywords.
(setq org-todo-keywords
  (list '(sequence "TODO(t)" "PROG(p)" "WAIT(w)" "RFU(r)" "RFM(m)" "|" "DONE(d!)" "QA(q@)" "CANCEL(c@)")))

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
      (todo "RFU" nil)
      (todo "RFM" nil)
      (todo "QA" nil))
     nil)))
  
;;; Custom Customization

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;; MODELINE

(setq-default mode-line-format
      '("%e" mode-line-front-sopace
        (:propertize
         ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
         display
         (min-width
          (5.0)))
        mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
        (vc-mode vc-mode)
        "  " (:eval pomm-current-mode-line-string)
        "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(run-with-timer 0 1 #'(lambda () (force-mode-line-update t)))

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
