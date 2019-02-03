;; init.el --- Emacs configuration

;; MacOS key bindings
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
;(set-default-font "Menlo 12")

;(setq
; hscroll-step 1
; scroll-conservatively 1000)

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

;;; Code:
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    auto-package-update
    ;; Auto completion
    company
    ;; Git
    magit
    ;; Very large file
    vlf
    ;; Python
    ;elpy
    ;; Clojure
    use-package
    cider
    clj-refactor
    paredit
    highlight-symbol
    highlight-parentheses
    ;; Which-key show key bindings
    which-key
    ;; JS
    web-mode
    js2-mode
    json-mode
    exec-path-from-shell
    ;; Multilang
    flycheck
    ;; File Tree sidebar
    treemacs
    ;; Color themes
    spacemacs-theme
    ;solarized-theme
    spaceline))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
      	    (package-install package)))
      myPackages)


;; Auto package update

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Load Theme

(defconst current-theme 'spacemacs-dark)

(load-theme current-theme t)
(spaceline-emacs-theme)

;; MAGIT
(require 'magit)
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(global-set-key (kbd "C-x g") 'magit-status)

;; Very large files
(require 'vlf-setup)

;; Disable bell
(setq visible-bell 'top-bottom)
(setq bell-volume 0)

;; Auto Completions
(add-hook 'after-init-hook 'global-company-mode)

;; Clojure
(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect 'display-only)

(require 'clj-refactor)
(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "C-S-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-S-<right>") 'paredit-forward-slurp-sexp)))

;; Highlight parentheses
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook
    (lambda ()
      (highlight-parentheses-mode t)))

;; Highlight symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(setq highlight-symbol-idle-delay 0.6)
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (highlight-symbol-mode)))

;; Search under cursor
(global-set-key (kbd "C-S") 'isearch-forward-symbol-at-point)

;; Set window settings
(setq frame-title-format "%b")

;; Turn off scroll bar
(scroll-bar-mode -1)

;; JS configuration


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Use the project's eslint instead of global one
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


;; PYTHON LANGUAGE CONFIG

;(elpy-enable)
;(when (require 'flycheck nil t)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; SETTING UP TREEMACS
(global-set-key [(control ?p)] 'treemacs)
(with-eval-after-load 'treemacs
  (treemacs-toggle-show-dotfiles)
  ;; HACK to load the theme again when treemacs loads to adopt right BG color.
  (add-hook 'treemacs-mode-hook (lambda () (load-theme current-theme)))
  (defun custom-treemacs-file-open
      (p)
    "Custom function to handle RET keys in treemacs on files forwarding parameter P."
    (treemacs-visit-node-no-split p)
    (treemacs))
  (treemacs-define-RET-action 'file-node-open #'custom-treemacs-file-open)
  (treemacs-define-RET-action 'file-node-closed #'custom-treemacs-file-open))

;; SET BACKUP DIR

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message

(setq display-line-numbers (quote relative))
(global-display-line-numbers-mode t)
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))) ;; disable for treemacs

;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(highlight-symbol-foreground-color "yellow")
 '(package-selected-packages
   (quote
    (sublimity xah-find which-key web-mode use-package treemacs spacemacs-theme spaceline solarized-theme soft-stone-theme soft-morning-theme parinfer paredit organic-green-theme monokai-theme monokai-alt-theme moe-theme material-theme magit json-mode js2-mode highlight-symbol highlight-parentheses hamburg-theme github-modern-theme flycheck flatui-theme exec-path-from-shell elpy cider better-defaults auto-package-update alect-themes)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; EMACS WINDOW SIZE SAVE

(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )

    (when (not (number-or-marker-p framegeometry-left))
      (setq framegeometry-left 0))
    (when (not (number-or-marker-p framegeometry-top))
      (setq framegeometry-top 0))
    (when (not (number-or-marker-p framegeometry-width))
      (setq framegeometry-width 0))
    (when (not (number-or-marker-p framegeometry-height))
      (setq framegeometry-height 0))

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )

(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
  )

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-framegeometry)
      (add-hook 'kill-emacs-hook 'save-framegeometry))
  )

