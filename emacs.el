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
 '(backup-directory-alist '(("." . "~/.emacs_backups")))
 '(company-idle-delay nil)
 '(compilation-environment '("TERM=xterm-256color"))
 '(compilation-scroll-output 'first-error)
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(fill-column 90)
 '(flycheck-haskell-runghc-command
   '("/run/current-system/sw/bin/runghc" "--" "-i" "-packageCabal" "-packagebase" "-packagebytestring" "-packagecontainers" "-packageprocess" "-packagedirectory" "-packagefilepath"))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(indent-tabs-mode nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
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
 '(neo-window-width 40)
 '(nxml-child-indent 2)
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/")))
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

;; Ivy keybindings
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h u") 'counsel-unicode-char)
(global-set-key (kbd "C-h j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

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

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config (flycheck-add-mode 'javascript-eslint 'js-mode)
          (flycheck-add-mode 'javascript-eslint 'js2-mode)
          ; (flycheck-kotlin-setup)
          (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers '(javascript-jshint))))

; Haskell
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((haskell-mode . lsp))
  :commands lsp)

(use-package lsp-treemacs :ensure t)
(use-package lsp-ivy :ensure t)
(use-package lsp-ui :ensure t)
(use-package lsp-haskell :ensure t)

(use-package format-all
  :ensure t)

(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode))
  :interpreter "groovy"
  :hook (groovy-mode . java-like-mode-hook))

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

(use-package kotlin-mode
  :ensure t)

(use-package fsharp-mode
  :defer t
  :ensure t)

(use-package flycheck-kotlin
  :ensure t
  :after (flycheck)
  :init (flycheck-kotlin-setup))

(use-package glsl-mode
  :ensure t)

(use-package ruby-mode
  :mode "capfile")

(use-package nix-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :mode "\\.js$")

(use-package web-mode
  :ensure t
  :mode "\\.html$")

(use-package yaml-mode
  :ensure t)

;; Others
(use-package counsel
  :ensure t
  :config (ivy-mode 1))

(use-package exec-path-from-shell
  :if window-system
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package neotree
  :ensure t
  :bind (:map personal-keys-minor-mode-map
         ("C-c f" . neotree-show)
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

;;; Custom variables
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
