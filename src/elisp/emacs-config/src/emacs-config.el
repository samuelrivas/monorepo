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

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

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

(setq require-final-newline 'ask)
(setq mode-require-final-newline t)

;; Assorted modes
(add-to-list 'auto-mode-alist '("Makefile\\..*\\'" . makefile-mode))

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

;; C/C++ mode
(defun my-c-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil)
  (setq comment-start "//")
  (setq comment-end ""))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

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

(defvar sams-org-config)

(setq-default org-log-into-drawer t)
(setq-default org-catch-invisible-edits 'error)
(setq-default org-agenda-files (plist-get sams-org-config :agenda-file))
(setq-default org-log-reschedule 'note)
(setq-default org-log-done 'time)
(setq-default org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq-default org-refile-use-outline-path 'file)
(setq-default org-refile-allow-creating-parent-nodes 'confirm)
(setq-default org-clock-display-default-range 'untilnow)

(defun sams-template (file)
  (format "%s/%s" (plist-get sams-org-config :template-dir) file))

(setq-default
 org-capture-templates
 `(("t" "Todo" entry
    (file+headline ,(plist-get sams-org-config :todo-file) "Inkorg")
    (file ,(sams-template "todo-capturing.org")))

   ("m" "Meeting" entry
    (file+datetree+prompt ,(plist-get sams-org-config :meeting-file))
    (file ,(sams-template "meeting-capturing.org"))
    :jump-to-captured t)

   ("i" "Interview" entry
    (file+datetree+prompt ,(plist-get sams-org-config :interview-file))
    (file ,(sams-template "interview-capturing.org"))
    :jump-to-captured t)

   ("r" "Reflection" entry
    (file+datetree+prompt ,(plist-get sams-org-config :reflection-file))
    (file ,(sams-template "reflection-capturing.org"))
    :jump-to-captured t))
)


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
;; (eval-after-load 'flycheck
;;   (setq-local flycheck-ghc-args ("-i")))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-stylish-on-save nil)
 '(flycheck-ghc-args '("-Wall" "-Werror" "-Wredundant-constraints" "-v"))
 '(flycheck-ghc-search-path '("/home/samuel/src/monorepos/iltxn/lib/aeson-extensions/src" "/home/samuel/src/monorepos/iltxn/lib/bank-connections-engine/app" "/home/samuel/src/monorepos/iltxn/lib/bank-connections-engine/src" "/home/samuel/src/monorepos/iltxn/lib/bank-connections-engine/systest" "/home/samuel/src/monorepos/iltxn/lib/bank-connections-engine/test" "/home/samuel/src/monorepos/iltxn/lib/basic-types/src" "/home/samuel/src/monorepos/iltxn/lib/basic-types/test" "/home/samuel/src/monorepos/iltxn/lib/bookkeeping-service/app" "/home/samuel/src/monorepos/iltxn/lib/bookkeeping-service/src" "/home/samuel/src/monorepos/iltxn/lib/cheap-repl/src" "/home/samuel/src/monorepos/iltxn/lib/direct-debit-us/app" "/home/samuel/src/monorepos/iltxn/lib/direct-debit-us/refund-hack" "/home/samuel/src/monorepos/iltxn/lib/direct-debit-us/src" "/home/samuel/src/monorepos/iltxn/lib/direct-debit-us/systest" "/home/samuel/src/monorepos/iltxn/lib/direct-debit-us/test" "/home/samuel/src/monorepos/iltxn/lib/hedgehog-extras/src" "/home/samuel/src/monorepos/iltxn/lib/hedgehog-json-schema/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-docs/app" "/home/samuel/src/monorepos/iltxn/lib/iltxn-docs/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-engine/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-prelude/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-protocol-v2/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-protocol-v2/test" "/home/samuel/src/monorepos/iltxn/lib/iltxn-protocol/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-protocol/test" "/home/samuel/src/monorepos/iltxn/lib/iltxn-service/app" "/home/samuel/src/monorepos/iltxn/lib/iltxn-service/src" "/home/samuel/src/monorepos/iltxn/lib/iltxn-service/test" "/home/samuel/src/monorepos/iltxn/lib/kafka-extras/src" "/home/samuel/src/monorepos/iltxn/lib/monad-log/src" "/home/samuel/src/monorepos/iltxn/lib/monad-metrics/src" "/home/samuel/src/monorepos/iltxn/lib/postgres-extras/src" "/home/samuel/src/monorepos/iltxn/lib/servant-extras/src" "/home/samuel/src/monorepos/iltxn/lib/servicing-engine/app" "/home/samuel/src/monorepos/iltxn/lib/servicing-engine/src" "/home/samuel/src/monorepos/iltxn/lib/servicing-engine/systest" "/home/samuel/src/monorepos/iltxn/lib/servicing-engine/test" "/home/samuel/src/monorepos/iltxn/lib/test-common/src" "/home/samuel/src/monorepos/iltxn/lib/txn-backend/app" "/home/samuel/src/monorepos/iltxn/lib/txn-backend/src" "/home/samuel/src/monorepos/iltxn/lib/txn-backend/systest" "/home/samuel/src/monorepos/iltxn/lib/txn-backend/test"))
 '(flycheck-ghc-language-extensions '("MultiParamTypeClasses" "ScopedTypeVariables" "FlexibleContexts" "FlexibleInstances" "KindSignatures" "TemplateHaskell" "NamedFieldPuns" "GeneralizedNewtypeDeriving" "StandaloneDeriving" "TypeOperators" "RecordWildCards" "OverloadedStrings" "DeriveGeneric" "ViewPatterns" "TupleSections" "MultiWayIf" "LambdaCase" "DeriveFunctor" "DataKinds" "DuplicateRecordFields" "TypeApplications" "OverloadedLabels" "NoImplicitPrelude" "DerivingVia"))
 )

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (flycheck-mode)
  (flyspell-prog-mode)
  (define-key haskell-mode-map (kbd "C-c C-c") 'comment-region))

;; Ocaml mode
;; TODO: this will fail if ocaml is blacklisted but I don't want to complicate
;; it moving it to a dynamicly generated file
(with-demoted-errors
  "Couldn't load tuareg, but this is only a problem if you want OCaml support: %S"
  (load-library "tuareg-site-file"))

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

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Groovy mode
(defun my-groovy-mode-hook ()
  (auto-fill-mode)
  (flyspell-prog-mode))

(add-hook 'groovy-mode-hook 'my-groovy-mode-hook)
