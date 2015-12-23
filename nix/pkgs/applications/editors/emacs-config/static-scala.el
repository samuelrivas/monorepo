(defun my-scala-mode-hook ()
  (setq whitespace-line-column 120))
(add-hook 'scala-mode-hook 'my-scala-mode-hook)
