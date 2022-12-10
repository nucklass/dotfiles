;; nucklass's sick emacs init file
;; supress gc on startup

(setq gc-cons-threshold 5000000) ;; can increase this threshold to reduce startup time
(setq max-specpdl-size 13000)

(add-hook 'emacs-startup-hook 'my/set-gc-threshold)
(defun my/set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

;;set up straight.el

(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; install straight packages
;;allow custom package configuration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; the rest
(mapc 'straight-use-package
      '(cargo
	counsel
	crux
	ess
	avy
	all-the-icons
	dashboard
	multiple-cursors
	dockerfile-mode
	arduino-mode
	forth-mode
	;;swiper
	ob-rust
	drawille
	csv-mode
	scala-mode
	yaml-mode
	fsharp-mode
	isend-mode
	emojify
	kaomoji
	lolcat
	slime
	;;poly-org
	roguel-ike
	rust-mode
	scad-preview
	scad-mode
	sqlite3
	;;ob-julia-vterm
	julia-mode
	ess
	python-mode
	;;vterm-toggle
	multi-vterm
	typescript-mode
	;;posframe
	stan-mode
	;;jupyter
	))

;;idk whats up w/ org mode but i have to set configs outside of use-package :/
(setq org-startup-indented t)
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python . t) (julia . t)))

(use-package whitespace
  :hook python-mode)

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  )

(use-package ace-popup-menu
  :config (ace-popup-menu-mode 1))

(use-package vterm
  :config (setq vterm-shell "nu"))


(use-package dirvish
  :init
  (with-eval-after-load 'winum
  (define-key winum-keymap (kbd "M-0") #'dirvish-side))

  :config
  (dirvish-override-dired-mode)
  ;;(dirvish-side-follow-mode)
    :bind
  (nil ; Bind `dirvish|dirvish-dired|dirvish-side|dirvish-dwim' as you see fit
   :map dired-mode-map
   ("M-0" . dirvish-side)
   )
  )

(use-package polymode
  :ensure t
  :defer t
  ;; :mode ("\.jl$" . poly-jl-stan-mod)
  :config
  (setq polymode-prefix-key (kbd "C-c n"))
  (define-hostmode poly-julia-hostmode :mode 'julia-mode)
  (define-innermode poly-stan-expr-julia-innermode
  :mode 'stan-mode
  :head-matcher (rx (= 3 (char "\"'")) "//stan"  (* (any space)))
  :tail-matcher (rx (= 3 (char "\"'")))
  :head-mode 'host
  :tail-mode 'host
  )
    (define-polymode poly-julia-stan-mode
    :hostmode 'poly-julia-hostmode
    :innermodes '(poly-stan-expr-julia-innermode)
   ;; (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
    ;; (define-key poly-python-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk)
    )
    )

(use-package cobol-mode
  :defer t
  :config
  (setq cobol-source-format 'free)
 )

(use-package go-translate
  :defer t
  :config
  (setq gts-translate-list '(("ja" "en")))

  (setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)
       :engines (list (gts-bing-engine) (gts-google-engine))
       :render (gts-buffer-render)))
)

(use-package erc
  :defer t
  :init
  (global-set-key (kbd "C-c e b")
		  (lambda () (interactive)
		    (erc :server "localhost" :port "6667" :nick "nucklass")))
)

 (use-package org-bullets
    :defer t
   :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; (use-package org
;;    :defer t
;;    :config
;;    (setq org-startup-indented t)
;;    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python . t) (julia-ob-vterm))
;;    ;;(add-to-list 'org-babel-load-languages '(julia-ob-vterm . t))
;; ;;   ;;(add-to-list 'org-babel-load-languages '(R . t))
;; ;;   ;;(orgp-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;; ;;   ;;(defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;;    ))

(use-package vterm
  :defer t
  :config (setq cursor-type 'bar))

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))

;; (use-package julia-vterm
;;   :defer t
;;   :init
;;   (add-hook 'julia-mode-hook #'julia-vterm-mode)
;;   :config
;;   ;; overwrite default submit kebinding to work on terminal mode
;;   (define-key julia-vterm-mode-map (kbd "C-c RET") 'julia-vterm-send-region-or-current-line)
;;   ()
;;   )


;;themes
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-atom" for a more minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;;(doom-themes-org-config)
 )

;;solaire-mode 
(use-package solaire-mode
  :init
  (add-hook 'after-init-hook #'solaire-global-mode))

;;enable rainbow-delimeters in most programming modes
(use-package rainbow-delimiters
  :defer t
  :init   
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
 )

;;enable company auto-completion in most programming modes
(use-package company
  :defer t
  :init
  (add-hook 'prog-mode-hook #'company-mode)
)

;; isend mode customization
;; (use-package isend-mode
;;   :defer t
;;   :config
;;   (define-key isend-mode-map (kbd "C-c RET") 'isend-send)
;;   )

;; eval in repl for ielm stuff
(use-package eval-in-repl
  :defer t
  :config
  (setq eir-repl-placement 'right)
  ;;; ielm support (for emacs lisp)
  (require 'eval-in-repl-ielm)
  ;; Evaluate expression in the current buffer.
  (setq eir-ielm-eval-in-current-buffer t)
  ;; for .el files
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for *scratch*
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-irc t)
  )

(use-package nyan-mode
  :hook (after-init . nyan-mode))

(use-package winum
  :init (setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-²") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
  :hook (after-init . winum-mode)
  )

;;startup and dashboard options
;;(use-package dashboard-ls)
(use-package dashboard
  ;;:after (dashboard-ls)
  :config
    (setq dashboard-startup-banner "~/.config/emacs/title.txt")
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

  (setq dashboard-set-navigator t)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  ;;(setq dashboard-set-navigator t)
  (setq dashboard-week-agenda t)
  ;;(add-to-list 'dashboard-items '(ls-directories . 5) t)
  ;;(add-to-list 'dashboard-items '(ls-files . 5) t)
  ;;(add-to-list 'dashboard-items '(agenda) t
  (dashboard-setup-startup-hook)
  )

;; stole this from online lol https://github.com/Atman50/emacs-config#org1fb3f72
(use-package ivy
  :diminish ""
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word) ;; make work like isearch
              ("C-r" . ivy-previous-line))
   :config
  (ivy-mode 1)
   (setq ivy-initial-inputs-alist nil) ;; no regexp by default
   (setq ivy-re-builders-alist         ;; allow input not in order
         '((t . ivy--regex-ignore-order)))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-mode t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t))
 (use-package swiper
   :bind (("C-S-s" . isearch-forward)  ;; Keep isearch-forward on Shift-Ctrl-s
          ("C-s" . swiper)             ;; Use swiper for search and reverse search
         ("C-S-r" . isearch-backward) ;; Keep isearch-backward on Shift-Ctrl-r
         ("C-r" . swiper)))
(use-package avy
  :bind (("C-:" . avy-goto-char)))
 

;; global set keys and set some variables outside of use-package's

;; have math symbols print pretty
(global-prettify-symbols-mode +1)

;;set eww as the default browser to open links in emacs
;;(I want to see if I can make this w3m)
(setq browse-url-browser-function 'eww)

;;enable vterm toggle stuff
(global-set-key (kbd "C-c v") 'multi-vterm-dedicated-toggle)
(global-set-key (kbd "C-c t") 'multi-vterm)

;; bind M-n and M-p to paragraph moveme
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;;change yes/no to y/nx
(fset 'yes-or-no-p 'y-or-n-p)

;;lol dont worry about it
;;(setq gamegrid-glyph-height-mm 2.5)

;;override the split  windows functions to switch automatically
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; vterm setup
(setq vterm-eval-cmds '(("find-file" find-file-other-window)
			("message" message)
			("vterm-clear-scrollback" vterm-clear-scrollback)))

(setq inferior-lisp-program "sbcl")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq shift-select-mode nil)
(setq-default cursor-type 'box) 


(add-hook 'prog-mode-hook  #'display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
