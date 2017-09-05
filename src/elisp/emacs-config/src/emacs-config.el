;; Disable packages (we use nix for dependencies)
(defvar package-archives)
(setq package-archives nil)
(package-initialize)

;; Disable customize (to avoid conflicts with this configuration)
(setq custom-file "/dev/null")

;; Useful functions that are not there by default
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed open files."))

;; Enable the solarised theme, with dark background We assume that
;; color-theme-solarized is in the path and use that info to set
;; custom-theme-load-path
(add-to-list
 'custom-theme-load-path
 (file-name-directory
  (locate-file "color-theme-solarized" load-path '(".el" ".elc"))))


(setq frame-background-mode 'dark)
(load-theme 'solarized t)
(enable-theme 'solarized)

;; Minor modes
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(blink-cursor-mode -1)
(auto-fill-mode t)

;; Key bindings
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)
(global-set-key "\M-g" 'goto-line)

;; Global variables
(setq-default fill-column 80)

(setq inhibit-splash-screen t)

(setq-default indent-tabs-mode nil)

(defvar browse-url-generic-program)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(setq delete-old-versions t)
(setq kept-new-versions 10)
(setq kept-old-versions 0)
(setq version-control t)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; Whitespace mode
(global-whitespace-mode t)
(defvar whitespace-line-column)
(defvar whitespace-style)
(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing empty tabs lines-tail))

;; LaTeX mode
(defun my-latex-mode-hook ()
  "LaTeX mode hook"
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\C-c\C-u" 'uncomment-region)
  (flyspell-mode t)
  (ispell-change-dictionary "british")
  (auto-fill-mode t))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

;; C mode
(defun my-c-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil)
  (setq comment-start "//")
  (setq comment-end "")
  (local-set-key "\C-c\C-u" 'uncomment-region))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Javascript mode
(defun my-javascript-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\C-c\C-u" 'uncomment-region))

(add-hook 'js-mode-hook 'my-javascript-mode-hook)

;; Text mode
(defun my-text-mode-hook ()
  (ispell-change-dictionary "british")
  (auto-fill-mode t)
  (flyspell-mode t))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; ORG
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(defvar org-log-into-drawer)
(defvar org-catch-invisible-edits)
(defvar sams-org-config)
(defvar org-capture-templates)
(defvar org-agenda-files)
(defvar org-log-reschedule)
(defvar org-log-done)

(setq org-log-into-drawer t)
(setq org-catch-invisible-edits 'error)
(setq org-agenda-files (plist-get sams-org-config :agenda-file))
(setq org-log-reschedule 'note)
(setq org-log-done 'time)

(defun sams-template (file)
  (format "%s/%s" (plist-get sams-org-config :template-dir) file))

(setq
 org-capture-templates
 `(("t" "Todo" entry
    (file+headline ,(plist-get sams-org-config :todo-file) "Inkorg")
    (file ,(sams-template "todo-capturing.org")))

   ("m" "Meeting" entry
    (file+datetree+prompt ,(plist-get sams-org-config :meeting-file))
    (file ,(sams-template "meeting-capturing.org")))

   ("i" "Interview" entry
    (file+datetree+prompt ,(plist-get sams-org-config :interview-file))
    (file ,(sams-template "interview-capturing.org"))
    :jump-to-captured t)))


;; Erlang mode
(autoload 'erlang-mode "erlang-start" "erlang-mode" t)
(add-to-list 'auto-mode-alist '("\\.[he]rl\\'" . erlang-mode))

(defun my-erlang-mode-hook ()
  "Erlang mode hook"
  (setq indent-tabs-mode nil)
  (auto-fill-mode)
  (flyspell-prog-mode)
  (ispell-change-dictionary "british"))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)


;; Haskell mode
(defvar haskell-mode-map)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-stylish-on-save t)
 )

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (flycheck-mode)
  (define-key haskell-mode-map (kbd "C-c C-c") 'comment-region))

;; Ocaml mode
;; TODO: this will fail if ocaml is blacklisted but I don't want to complicate
;; it moving it to a dynamicly generated file
(load-library "tuareg-site-file")

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'my-tuareg-mode-hook)

(defvar utop-command)
(defvar merlin-mode-map)
(defvar merlin-use-auto-complete-mode)
(defun my-tuareg-mode-hook ()
  (require 'merlin)
  (require 'ocp-indent)
  (setq utop-command "utop -emacs")
  (utop-setup-ocaml-buffer)
  (merlin-mode)
  (flyspell-prog-mode)

  (setq merlin-use-auto-complete-mode t)

  (define-key merlin-mode-map
    (kbd "C-c C-n") 'merlin-error-next)
  (define-key merlin-mode-map
    (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map
    (kbd "C-c <down>") 'merlin-type-enclosing-go-down))

;; Nix mode
(autoload 'nix-mode "nix-mode" "nix-mode" t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Scala mode
(defvar ensime-auto-generate-config)
(defun my-scala-mode-hook ()
  (setq ensime-auto-generate-config t)
  (setq whitespace-line-column 120)
  (flyspell-prog-mode)
  (ensime))
(add-hook 'scala-mode-hook 'my-scala-mode-hook)

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Groovy mode
(defun my-groovy-mode-hook ()
  (auto-fill-mode)
  (flyspell-prog-mode))

(add-hook 'groovy-mode-hook 'my-groovy-mode-hook)
