;;; Start the emacs server
(add-hook 'after-init-hook 'server-start)

;;; Global Settings
(require 'compile)
(require 'tramp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "red" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])
 '(auto-save-default nil)
 '(auto-save-mode nil)
 '(backup-directory-alist (quote (("." . "~/.emacs_backups"))))
 '(compilation-environment (quote ("TERM=xterm-256color")))
 '(compilation-scroll-output 'first-error)
 '(compile-command "./gradle.sh build")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(fill-column 90)
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
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("elpa" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (idris-mode intero magit magit-filenotify magit-find-file emacsql emacsql-mysql smex neotree ido-ubiquitous find-file-in-repository exec-path-from-shell nix-mode web-mode json-mode js2-mode flycheck groovy-mode ghc haskell-mode color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme sr-speedbar)))
 '(speedbar-frame-parameters
   (quote
    ((width . 50)
     (minibuffer)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(sr-speedbar-auto-refresh nil)
 '(truncate-lines t)
 '(web-mode-indent-offset 2))

(add-to-list 'load-path "~/.emacs.d/elisp")

;;; Packages
(require 'package)
(package-initialize)

(defvar install-packages '(; Themes
			   color-theme
                           color-theme-sanityinc-tomorrow
			   color-theme-sanityinc-solarized

                           ; languages
                           dockerfile-mode
                           haskell-mode
                           idris-mode
                           intero
			   ghc
                           groovy-mode
                           yaml-mode

                           ; javascript
                           flycheck
                           js2-mode
                           json-mode
                           web-mode

                           ; nix
                           nix-mode

                           ; sql
                           emacsql
                           emacsql-mysql

                           ; Misc
                           exec-path-from-shell
                           find-file-in-repository
                           ido-completing-read+
                           magit
                           neotree
                           restclient
                           smex))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p install-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Mode-specific Settings
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

;; Haskell mode
(intero-global-mode 1)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook
          (lambda ()
            (setq compile-command "stack build --fast --test")))
(flycheck-add-next-checker
 'intero
 '(warning . haskell-hlint))

;; Make shell mode nicer
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; This stuff is to ansi-colorize the compilation buffer after a rails test so the terminal colors come through.
(define-derived-mode ansi-compilation-mode compilation-mode "ansi compilation"
  "Compilation mode that understands ansi colors."
  (require 'ansi-color)
  (read-only-mode 0)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun colorize-compilation (one two)
  "Ansi colorize the compilation buffer."
  (ansi-compilation-mode))

(setq compilation-finish-function
      'colorize-compilation)

; What happened here?
; (bury-successful-compilation t)

;; Override disabled keys
(put 'downcase-region 'disabled nil)

;;; Load environment from shell
(when window-system
  (exec-path-from-shell-initialize))

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

; Disable my keys in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (personal-keys-minor-mode 0)))

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

(setq site-el "/usr/share/emacs/site-lisp/")
(setq home-el "~/.emacs.d/")

;; Mutt support.
(add-to-list 'auto-mode-alist '("/tmp/mutt.*" . mail-mode))

;;; Colors
(require 'color-theme)
(color-theme-sanityinc-tomorrow-night)

;;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(javascript-jshint)))

;;; Compilation
(add-hook 'compilation-mode-hook
          (lambda () (font-lock-mode -1)))

;;; Groovy
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("package\\.json" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc" . json-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg)
                         'no-indent)))))

(defun java-like-mode-hook ()
  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil))

(add-hook 'java-mode-hook 'java-like-mode-hook)
(add-hook 'groovy-mode-hook 'java-like-mode-hook)

;;; UI
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
; (desktop-save-mode 1)

;;; magit
(define-key personal-keys-minor-mode-map
  (kbd "C-c g") 'magit-status)

;;; neotree
(require 'neotree)

(define-key personal-keys-minor-mode-map
  (kbd "C-c f") 'neotree-show)
(define-key personal-keys-minor-mode-map
  (kbd "C-c C-f") 'neotree-toggle)

;;; fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro:pixelsize=16:weight=normal"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
