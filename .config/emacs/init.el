;; nucklass's sick emacs init file
;;set up straight.el
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
	crux
	avy
	all-the-icons
	w3m
	dashboard
	multiple-cursors
	spotify
	dockerfile-mode
	arduino-mode
	forth-mode
	swiper
	ob-rust
	drawille
	csv-mode
	scala-mode
	yaml-mode
	fsharp-mode
	cobol-mode
	flappymacs
	isend-mode
	emojify
	kaomoji
	lolcat
	pacmacs
	poly-org
	roguel-ike
	rustic
	rust-mode
	scad-preview
	scad-mode
	sqlite3
	python-mode
	ob-julia-vterm
	julia-mode
	ess
	vterm-toggle
	typescript-mode
	posframe
	stan-mode
	))

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

(use-package org
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(juliaOB-vterm . t))
  ;;(add-to-list 'org-babel-load-languages '(python . t))
  ;;(add-to-list 'org-babel-load-languages '(R . t))
  ;;(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  )

(use-package julia-vterm
  :defer t
  :init
  (add-hook 'julia-mode-hook #'julia-vterm-mode)
  :config
  ;; overwrite default submit kebinding to work on terminal mode
  (define-key julia-vterm-mode-map (kbd "C-c RET") 'julia-vterm-send-region-or-current-line)
  ()2
  )

(use-package vterm
  :defer t
  :config (setq cursor-type 'bar))

(use-package treemacs
  :defer t
  :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window))
)

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
;; I should try with use pkg init next, but this works for now
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
(use-package isend-mode
  :defer t
  :config
  (define-key isend-mode-map (kbd "C-c RET") 'isend-send)
  )

;;load custom 
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
    (setq dashboard-startup-banner '4)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

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

;; global set keys and set some variables outside of use-package's

;;set eww as the default browser to open links in emacs
;;(I want to see if I can make this w3m)
(setq browse-url-browser-function 'w3m)

;;enable avy goto stuff
(global-set-key (kbd "M-;") 'avy-goto-char)
(global-set-key (kbd "M-\"") 'avy-goto-char-2)

;;enable vterm toggle stuff
(global-set-key (kbd "C-c v") 'vterm-toggle)

;; bind M-n and M-p to paragraph moveme
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;;change yes/no to y/nx
(fset 'yes-or-no-p 'y-or-n-p)

;;make swiper default "find in page" thing
(global-set-key "\C-s" 'swiper)

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

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq shift-select-mode nil)
(setq-default cursor-type 'bar) 

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
