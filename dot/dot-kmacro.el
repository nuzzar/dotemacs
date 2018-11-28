;;; Macron C-x C-k M
;;;; Macron A
(fset 'insert-Ā-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("Ā" 0 "%d")) arg)))
(global-set-key [24 11 77 65] 'insert-Ā-kmacro)

;;;; Macron a
(fset 'insert-ā-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ā" 0 "%d")) arg)))
(global-set-key [24 11 77 97] 'insert-ā-kmacro)

;;;; Macron I
(fset 'insert-Ī-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("Ī" 0 "%d")) arg)))
(global-set-key [24 11 77 73] 'insert-Ī-kmacro)

;;;; Macron i
(fset 'insert-ī-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ī" 0 "%d")) arg)))
(global-set-key [24 11 77 105] 'insert-ī-kmacro)

;;;; Macron E
(fset 'insert-Ē-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("Ē" 0 "%d")) arg)))
(global-set-key [24 11 77 69] 'insert-Ē-kmacro)

;;;; Macron e
(fset 'insert-ē-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ē" 0 "%d")) arg)))
(global-set-key [24 11 77 101] 'insert-ē-kmacro)

;;;; Macron U
(fset 'insert-Ū-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("Ū" 0 "%d")) arg)))
(global-set-key [24 11 77 85] 'insert-Ū-kmacro)

;;;; Macron u
(fset 'insert-ū-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ū" 0 "%d")) arg)))
(global-set-key [24 11 77 117] 'insert-ū-kmacro)

;;;; Macron O
(fset 'insert-Ō-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("Ō" 0 "%d")) arg)))
(global-set-key [24 11 77 79] 'insert-Ō-kmacro)

;;;; Macron o
(fset 'insert-ō-kmacro
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ō" 0 "%d")) arg)))
(global-set-key [24 11 77 111] 'insert-ō-kmacro)

(provide 'dot-kmacro)
