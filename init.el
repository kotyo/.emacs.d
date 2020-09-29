;;; package -- Minimal settings by kotyo
;;; Commentary:
;;; Code:
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Deal with trailing whitespaces on save
(defun nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'nuke-trailing-whitespace)

(straight-use-package 'use-package)
(use-package magit :straight t)
(use-package color-theme-sanityinc-tomorrow :straight t)
(use-package theme-changer :straight t :hook ((prog-mode . hl-line-mode)
					      (text-mode . hl-line-mode)
					      (after-init . cua-mode)))
(use-package treemacs :straight t)
(use-package which-key :straight t :hook (after-init . which-key-mode))
(use-package vlf :straight t)
(use-package centered-cursor-mode :straight t :hook (prog-mode . centered-cursor-mode))
(use-package clojure-mode :straight t)
(use-package cider :straight t)
(use-package clj-refactor :straight t :hook (clojure-mode . clj-refactor-mode))
(use-package paredit :straight t :hook ((clojure-mode . paredit-mode)
					(clojurescript-mode . paredit-mode)))
(use-package flycheck :straight t :hook ((after-init . global-flycheck-mode)
					 (text-mode . flyspell-mode)
					 (prog-mode . flyspell-prog-mode)))
(use-package flycheck-clj-kondo :straight t)
(use-package company :straight t :hook (after-init . global-company-mode))
;;(use-package rainbow-delimiters :straight t :hook (clojure-mode . rainbow-delimiters-mode))
;;(use-package focus :straight t :hook ((prog-mode . focus-mode)
;;				      (text-mode . focus-mode)))
(use-package highlight-symbol :straight t :hook (prog-mode . highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.4))
(use-package highlight-parentheses :straight t :hook (prog-mode . highlight-parentheses-mode))

;; SETTING UP TREEMACS
(global-set-key [(control ?p)] 'treemacs)
(with-eval-after-load 'treemacs
  (treemacs-toggle-show-dotfiles)
  ;; HACK to load the theme again when treemacs loads to adopt right BG color.
  ;;(add-hook 'treemacs-mode-hook (lambda () (load-theme current-theme)))
  (defun custom-treemacs-file-open
      (p)
    "Custom function to handle RET keys in treemacs on files forwarding parameter P."
    (treemacs-visit-node-no-split p)
    (treemacs))
  (treemacs-define-RET-action 'file-node-open #'custom-treemacs-file-open)
  (treemacs-define-RET-action 'file-node-closed #'custom-treemacs-file-open))


(menu-bar-mode -1)
(kill-buffer "*scratch*")
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Setting up theme changer
(setq calendar-location-name "Budapest")
(setq calendar-latitude 47.49)
(setq calendar-longitude 19.04)
(require 'theme-changer)
(change-theme 'sanityinc-tomorrow-bright 'sanityinc-tomorrow-night)

;; Magit settings
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))

;; Cider settings
(setq cider-repl-pop-to-buffer-on-connect 'display-only)
(setq cider-repl-display-in-current-window t)
(setq nrepl-force-ssh-for-remote-hosts t)

;; Clj refactor
(defun clj-refactor-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Cut and Paste settings
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

;; Desktop save mode
;(desktop-save-mode 1)
;(setq desktop-path '("."))

;; Search under cursor
;(global-unset-key (kbd "C-S-s"))
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s") 'isearch-forward)

;; Autoupdate packages
(use-package auto-package-update
   :straight t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 6)
   (auto-package-update-maybe))

;; Long line support
(setq-default bidi-inhibit-bpa t)
(setq-default bidi-display-reordering nil)
(setq-default truncate-lines t)

;; SET BACKUP DIR - backup in one place. flat, no tree structure
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/emacs-backup"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-file-name-transforms
    `((".*" "~/.emacs.d/emacs-backup/")))       ; use versioned backups

(provide 'init)

;;; init.el ends here
