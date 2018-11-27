;;; init.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
(setq package-archives (list (cons "melpa" (concat "http" "://melpa.org/packages/"))
			     (cons "melpa-stable" (concat "http" "://stable.melpa.org/packages/"))
			     (cons "gnu" "http://elpa.gnu.org/packages/")))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/lisp/use-package")
  (add-to-list 'load-path "~/.emacs.d/dot")
  (require 'use-package))

(use-package simpleclip
  :ensure t)

(use-package better-defaults
  :ensure t)

(use-package magit
  :ensure t)

(use-package helm
  :ensure t
  :defer 10
  :config
  (setq helm-completing-read-handlers-alist
        '((describe-function . helm-completing-read-symbols)
          (describe-variable . helm-completing-read-symbols)
          (describe-symbol . helm-completing-read-symbols)
          (debug-on-entry . helm-completing-read-symbols)
          (find-function . helm-completing-read-symbols)
          (disassemble . helm-completing-read-symbols)
          (trace-function . helm-completing-read-symbols)
          (trace-function-foreground . helm-completing-read-symbols)
          (trace-function-background . helm-completing-read-symbols)
          (find-tag . helm-completing-read-default-find-tag)
          (org-capture . helm-org-completing-read-tags)
          (org-set-tags . helm-org-completing-read-tags)
          (ffap-alternate-file)
          (tmm-menubar)
          (find-file . helm-completing-read-sync-default-handler)
          (find-file-at-point . helm-completing-read-sync-default-handler)
          (ffap . helm-completing-read-sync-default-handler)
          (execute-extended-command)
          (switch-to-buffer . helm-completing-read-sync-default-handler)))
  (ac-config-default)
  (require 'helm-config)
  (helm-mode 1))

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
  
(use-package dot-org)

;;; ORG CUSTOMIZATION

;; (setq org-agenda-files "~/.emacs.d/agenda-files")

;; (setq org-todo-keyword-faces
;;       '(("SCULPTING" . "red") ("BREAK" . "brightred")
;; 	("FINISHED" . "green")))

;; (defun .play-bell ()
;;   (call-process
;;    "paplay" "/usr/share/sounds/ubuntu/stereo/bell.ogg"))

;; (add-hook 'org-timer-done-hook '.play-bell)

;; (require 'dot-org)

	
;;; CUSTOM CUSTOMIZATION

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; init.el ends here
