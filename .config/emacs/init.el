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

;;the rest
(mapc 'straight-use-package '(cargo treemacs crux avy doom-modeline all-the-icons w3m dashboard multiple-cursors spotify dockerfile-mode arduino-mode forth-mode swiper ob-rust drawille csv-mode dracula-theme scala-mode yaml-mode fsharp-mode cobol-mode flappymacs isend-mode kaomoji lolcat mines moe-theme nyan-mode pacmacs poly-org roguel-ike rustic rust-mode scad-preview scad-mode sqlite3 python-mode ob-julia-vterm  julia-mode ess vterm-toggle))
;; switch up dired to dirvish
(use-package dirvish
  :defer t
  :init
(dirvish-override-dired-jump)
  )
;; make julia-vterm work w/ org mode
(use-package org
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(julia-vterm . t))
  (add-to-list 'org-babel-load-languages '(python . t))
  (add-to-list 'org-babel-load-languages '(R . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )

(use-package julia-vterm
  :defer t
  :init
  (add-hook 'julia-mode-hook #'julia-vterm-mode)
  :config
  ;; overwrite default submit kebinding to work on terminal mode
  (define-key julia-vterm-mode-map (kbd "C-c RET") 'julia-vterm-send-region-or-current-line)
  )

;;enable rainbow-delimeters in most programming modes
;; this isn't working with the following use-package statement, so doing it raw for now,
;; I should try with use pkg init next, but this works for now
(use-package rainbow-delimiters
:init   
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
)
;;enable company auto-completion in most programming modes
(use-package company
  :init
  (add-hook 'prog-mode-hook #'company-mode)
)

;; isend mode customization
(use-package isend-mode
  :defer t
  :config
  (define-key isend-mode-map (kbd "C-c RET") 'isend-send)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b4ba3e1bba2e303265eb3e9753215408e75e031f7c894786ad04cabef46ff94c" "ba099d9f4f87bc9c2d5ae497782e05b88dfbe746e3c96df6a8a19668f8ad7518" "02fefdfc9a0c7256a10c8794a4985c9c70c5fbf674873b66807e8143e02c81a7" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" "0feb7052df6cfc1733c1087d3876c26c66410e5f1337b039be44cb406b6187c6" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "97fbd952a3b01fbace2aa49b3b07692cacc3009883c7219b86e41669c2b65683" "f703efe04a108fcd4ad104e045b391c706035bce0314a30d72fbf0840b355c2c" "23b564cfb74d784c73167d7de1b9a067bcca00719f81e46d09ee71a12ef7ee82" "8f0a782ba26728fa692d35e82367235ec607d0c836e06bc39eb750ecc8e08258" default))
 '(org-agenda-files '("~/.agenda/main_agenda.org"))
 '(send-mail-function 'smtpmail-send-it)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JuliaMono" :foundry "UKWN" :slant normal :weight semi-bold :height 136 :width normal)))))

;;themes
(load-theme 'dracula t)

;;load custom 
(add-hook 'after-init-hook #'doom-modeline-mode)
(add-hook 'after-init-hook #'nyan-mode)

;;change yes/no to y/nx
(fset 'yes-or-no-p 'y-or-n-p)

;;make swiper default "find in page" thing
(global-set-key "\C-s" 'swiper)

;;lol dont worry about it
(setq gamegrid-glyph-height-mm 2.5)

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

;; turning off default windmove bindings for now
;;switch windows with shift-arrow, no more need to C-x o
;;(when (fboundp 'windmove-default-keybindings)
;;  (windmove-default-keybindings))

;;set eww as the default browser to open links in emacs
;;(I want to see if I can make this w3m)
(setq browse-url-browser-function 'eww-browse-url)

;;enable avy goto stuff
(global-set-key (kbd "M-;") 'avy-goto-char)
(global-set-key (kbd "M-\"") 'avy-goto-char-2)

;;enable treemacs stuff
(global-set-key (kbd "M-q") 'treemacs)

;;enable vterm toggle stuff
(global-set-key (kbd "C-c v") 'vterm-toggle)

;; vterm setup
(setq vterm-eval-cmds '(("find-file" find-file-other-window)
			("message" message)
			("vterm-clear-scrollback" vterm-clear-scrollback)))

;;  EDIT DEC15, disable to try ace-window only for window movement
;; bind shifted motion keys to wind move, edit for vterm
(eval-after-load 'vterm '(define-key vterm-mode-map  (kbd "M-B") nil))
(eval-after-load 'vterm '(define-key vterm-mode-map  (kbd "M-F") nil))
(eval-after-load 'vterm '(define-key vterm-mode-map  (kbd "M-P") nil))
(eval-after-load 'vterm '(define-key vterm-mode-map  (kbd "M-N") nil))
(global-set-key (kbd "M-B")  #'windmove-left)
(global-set-key (kbd "M-F") #'windmove-right)
(global-set-key (kbd "M-P")    #'windmove-up)
(global-set-key (kbd "M-N")  #'windmove-down)

;; bind M-n and M-p to paragraph moveme
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;;startup and dashboard options
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq dashboard-startup-banner '3)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-set-navigator t)
  (setq dashboard-week-agenda t)
  ;;(add-to-list 'dashboard-items '(agenda) t)
)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq shift-select-mode nil)
;; (split-window-horizontally)
;; ;;(deer)
;; (other-window 1)
;; (vterm)
;; (rename-buffer "local")
;; (other-window 1)

;; terminal specific stuff
