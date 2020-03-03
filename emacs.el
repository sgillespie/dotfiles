;;; Start the emacs server
(add-hook 'after-init-hook 'server-start)

;;; Custom variables
;; This must be first(ish)!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-mode nil)
 '(backup-directory-alist (quote (("." . "~/.emacs_backups"))))
 '(company-idle-delay nil)
 '(compilation-environment (quote ("TERM=xterm-256color")))
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(ensime-startup-notification nil)
 '(fill-column 90)
 '(flycheck-add-next-checker (quote haskell) t)
 '(flycheck-haskell-runghc-command
   (quote
    ("/run/current-system/sw/bin/runghc" "--" "-i" "-packageCabal" "-packagebase" "-packagebytestring" "-packagecontainers" "-packageprocess" "-packagedirectory" "-packagefilepath")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save t)
 '(helm-completion-style (quote emacs))
 '(helm-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-allow-member-expr-as-function-name t)
 '(js2-basic-offset 2)
 '(js2-missing-semi-one-line-override t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-var-hides-function-arg-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(make-backup-files nil)
 '(neo-window-width 35)
 '(nxml-child-indent 2)
 '(package-archives (quote (("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (flycheck-haskell company helm helm-ghc nix-mode format-all rjsx-mode thrift ensime smex restclient neotree magit ido-completing-read+ find-file-in-repository exec-path-from-shell yaml-mode web-mode json-mode js2-mode intero idris-mode groovy-mode ghc flycheck emacsql-mysql emacsql dockerfile-mode color-theme-sanityinc-tomorrow color-theme use-package)))
 '(truncate-lines t)
 '(web-mode-indent-offset 2))

;;; UI
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)

;; Make shell mode nicer
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Pro:pixelsize=15:weight=normal"))

;;; Key Bindings
(defvar personal-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "personal-keys-minor-mode keymap.")

(define-minor-mode personal-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " personal-keys")

(personal-keys-minor-mode 1)

;; Disable my keys in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (personal-keys-minor-mode 0)))

(defun prev-window ()
  (interactive)
  (other-window -1))

(define-key personal-keys-minor-mode-map (kbd "C-c n") 'other-window)
(define-key personal-keys-minor-mode-map (kbd "C-c p") 'prev-window)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c C-l") 'visual-line-mode)

;; Override disabled keys
(put 'downcase-region 'disabled nil)

;;; Packages
(require 'package)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; Themes
(use-package color-theme
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (color-theme-sanityinc-tomorrow-night))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (("M-/" . 'company-complete-common)))

;; Languages
(use-package dockerfile-mode
  :ensure t
  :hook
  (dockerfile-mode . (lambda ()
                       (setq tab-width 4))))

(use-package emacsql
  :ensure t)

(use-package emacsql-mysql
  :ensure t)

(use-package ensime
  :ensure t)

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config (flycheck-add-mode 'javascript-eslint 'js-mode)
          (flycheck-add-mode 'javascript-eslint 'js2-mode)
          (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers '(javascript-jshint))))

(use-package flycheck-haskell
  :ensure t
  :after (haskell-mode)
  :hook ((haskell-mode . flycheck-haskell-setup)))

(use-package format-all
  :ensure t)

(use-package ghc
  :ensure t)

(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode))
  :interpreter "groovy"
  :hook (groovy-mode . java-like-mode-hook))

(use-package haskell-mode
  :ensure t
  :config (setq-default flycheck-disabled-checkers
            (append flycheck-disabled-checkers '(haskell-stack-ghc)))
  :bind (:map haskell-mode-map
          ("C-c `" . haskell-interactive-bring))
  :custom (haskell-stylish-on-save t)
          (flycheck-add-next-checker
             'haskell
             '(warning . haskell-hlint))
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode)))

(use-package helm
  :ensure t
  :config (helm-mode 1))

(use-package helm-company
  :ensure t
  :after helm
  :defer nil
  :bind (:map company-mode-map
         ("C-;" . helm-company)
         :map company-active-map
         ("C-;" . helm-company))
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package idris-mode
  :ensure t)

(use-package intero
  :ensure t
  :after (fly-check)
  :config (intero-global-mode nil)
          (flycheck-add-next-checker
           'intero
           '(warning . haskell-hlint)))

(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map
          ("RET" . newline-and-indent))
  :config (set (make-local-variable 'electric-indent-functions)
               (list (lambda (one two)
                       'no-indent))))

(use-package json-mode
  :ensure t
  :mode (("package\\.json" . json-mode)
         ("\\.babelrc$" . json-mode)))

(use-package ruby-mode
  :mode "capfile")

(use-package nix-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :mode "\\.js$")

(use-package sbt-mode
  :ensure t)

(use-package scala-mode
  :ensure t
  :bind (:map ensime-mode-map
              ("C-c C-b RET" . sbt-command)))

(use-package thrift
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html$")

(use-package yaml-mode
  :ensure t)

;; Others
(use-package ansi-color
  :ensure t
  :config
  (setq compilation-finish-functions
        'colorize-compilation))
  
(use-package exec-path-from-shell
  :if window-system
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package find-file-in-repository
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package neotree
  :ensure t
  :bind (("C-c f" . neotree-show)
         ("C-c C-f" . neotree-toggle)))

(use-package restclient
  :ensure t
  :mode ("\\.rest$" . restclient-mode))

(use-package use-package
  :ensure t)

;; Java-esque indenting
(defun java-like-mode-hook ()
  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil))

(add-hook 'java-mode-hook 'java-like-mode-hook)

;; ansi-colorize after a test so terminal colors come through.
(define-derived-mode ansi-compilation-mode compilation-mode "ansi compilation"
  "Compilation mode that understands ansi colors."
  (read-only-mode 0)
  (ansi-color-apply-on-region (point-min) (point-max)))
  
(defun colorize-compilation ()
  "Ansi colorize the compilation buffer."
  (ansi-compilation-mode))

;;; Custom variables

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
