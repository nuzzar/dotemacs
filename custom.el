;;; ...  -*- lexical-binding: t; -*-

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(custom-group-tag ((t (:height 1.2 :weight bold :foreground "gray" :inherit variable-pitch))))
 '(custom-variable-tag ((t (:foreground "gray" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "brightwhite"))))
 '(font-lock-variable-name-face ((t (:foreground "Gray80"))))
 '(fringe ((t (:background "gray8"))))
 '(helm-selection ((t (:background "color-240" :distant-foreground "black"))))
 '(magit-section-highlight ((t (:extend t :background "gray20"))))
 '(match ((t (:background "gray40"))))
 '(org-agenda-done ((t (:foreground "gray40"))))
 '(org-agenda-structure ((t (:foreground "gray60"))))
 '(org-column ((t (:strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((t (:underline t :weight bold))))
 '(org-document-title ((t (:foreground "brightblack" :weight bold))))
 '(org-drawer ((t (:foreground "brightblack"))))
 '(org-headline-done ((t (:foreground "gray60"))))
 '(org-imminent-deadline ((t (:foreground "gray80"))))
 '(org-superstar-leading ((t (:inherit default :foreground "unspecified-fg"))))
 '(org-table ((t (:foreground "#99968b"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(wombat))
 '(elpy-folding-fringe-indicators nil)
 '(elpy-rpc-virtualenv-path 'current)
 '(org-adapt-indentation t)
 '(org-clock-sound "/usr/share/sounds/freedesktop/stereo/bell.oga")
 '(org-roam-completion-everywhere t)
 '(org-roam-directory "~/org/roam")
 '(package-selected-packages
   '(better-defaults dap-mode elpy emacsql-sqlite-builtin flycheck
                     git-commit helm jest-test-mode magit no-littering
                     org-pomodoro org-roam org-superstar pkg-info
                     pomidor pomm popup prettier-js projectile
                     pyenv-mode rainbow-delimiters simpleclip
                     skewer-mode tree-sitter-langs typescript-mode
                     which-key))
 '(safe-local-variable-values
   '((eval setq eslintd-fix-args '("--rule=indent: [error, 4]"))
     (eval setq prettier-js-args '("--tab-width=4"))
     (eval setq lsp-enable-format-on-save nil) (js2-basic-offset . 4)))
 '(warning-suppress-types '((comp))))

 ;; '(org-roam-directory "~/roam-notes" t)
 ;; '(package-selected-packages
 ;;   (quote
 ;;    (simpleclip py-autopep8 material-theme magit jedi helm flycheck elpy ein better-defaults))))

