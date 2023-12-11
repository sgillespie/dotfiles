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
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
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
 '(lsp-haskell-plugin-stan-global-on nil)
 '(lsp-haskell-server-path "haskell-language-server")
 '(lsp-lens-enable nil)
 '(make-backup-files nil)
 '(neo-window-width 40)
 '(nxml-child-indent 2)
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(ligature gh-md direnv solarized-theme restclient neotree magit exec-path-from-shell counsel yaml-mode web-mode rjsx-mode nix-mode glsl-mode flycheck-kotlin fsharp-mode kotlin-mode json-mode js2-mode groovy-mode format-all lsp-haskell lsp-ui lsp-ivy lsp-treemacs lsp-mode flycheck emacsql-mysql emacsql dockerfile-mode company color-theme-sanityinc-tomorrow use-package))
 '(safe-local-variable-values
   '((format-all-formatters
      ("Haskell" fourmolu))
     (format-all-formatters
      ("Haskell" stylish-haskell))
     (lsp-haskell-server-path "/home/sgillespie/dev/haskell/haskell-language-server/dist-newstyle/build/x86_64-linux/ghc-9.2.8/haskell-language-server-2.2.0.0/x/haskell-language-server/build/haskell-language-server/haskell-language-server")
     (eglot-server-programs
      (haskell-mode "/home/sgillespie/.cabal/bin/haskell-language-server" "--lsp"))
     (eglot-server-programs
      (haskell-mode "sean" "--lsp"))
     (eglot-server-programs
      (haskell-mode "haskell-language-server" "--lsp"))
     (lsp-haskell-server-path . "/home/sgillespie/dev/haskell/haskell-language-server/dist-newstyle/build/x86_64-linux/ghc-9.2.8/haskell-language-server-2.2.0.0/x/haskell-language-server/build/haskell-language-server/haskell-language-server")
     (lsp-haskell-server-path . "/path/to/your/hacked/haskell-language-server")))
 '(truncate-lines t)
 '(warning-suppress-types '((direnv) (comp)))
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
(add-to-list 'default-frame-alist '(font . "0xProto:pixelsize=15"))

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

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Packages
(require 'package)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; Themes
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (("M-/" . 'company-complete-common)))

(use-package gh-md
  :ensure t)

;; Languages
(use-package dockerfile-mode
  :ensure t
  :hook
  (dockerfile-mode . (lambda ()
                       (setq tab-width 4))))

(use-package emacsql
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
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package eglot
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false)))) ; disable stan
                  (haskell
                   (formattingProvider . "fourmolu"))))
  :config
  (add-to-list
   'eglot-server-programs
   '(haskell-mode . ("haskell-language-server" "--lsp")))
  :custom
  (flycheck-global-modes '(not haskell-mode)) ; Disable flycheck
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil))  ;; allow edits without confirmation

(use-package lsp-mode
  :ensure f)

(use-package lsp-treemacs :ensure t)
(use-package lsp-ivy :ensure t)
(use-package lsp-ui :ensure t)
(use-package lsp-haskell :ensure t)

(use-package format-all
  :ensure t
  :hook
  ((haskell-mode . format-all-mode)))

(use-package json-mode
  :ensure t
  :mode (("package\\.json" . json-mode)
         ("\\.babelrc$" . json-mode)))

(use-package nix-mode
  :ensure t)

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

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '("->" "<-" "=>" "=>>" ">=>"
     "=>=" "=<<" "=<=" "<=<" "<=>"
     ">>" ">>>" "<<" "<<<" "<>"
     "<|>" "==" "===" ".=" ":="
     "#=" "!=" "!==" "=!=" "=:="
     "::" ":::" ":<:" ":>:" "||"
     "|>" "||>" "|||>" "<|" "<||"
     "<|||" "**" " ***" "<*" "<*>"
     "*>" "<+" "<+>" "+>" "<$"
     "<$>" "$>" "&&" "??" "%%"
     "|]" "[|" "//" "///"))
  (global-ligature-mode))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package neotree
  :ensure t
  :bind (:map personal-keys-minor-mode-map
         ("C-c f" . neotree-show)
         ("C-c C-f" . neotree-toggle)))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package restclient
  :ensure t
  :mode ("\\.rest$" . restclient-mode))

(use-package use-package
  :ensure t)

;;; Custom variables
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
