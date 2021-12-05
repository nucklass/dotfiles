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
;(straight-use-package 'org-mode)
(mapc 'straight-use-package '(cargo treemacs dired-ranger dired-rainbow dired-filter crux avy minimap aggressive-indent doom-modeline all-the-icons w3m company dashboard multiple-cursors theme-magic spotify dockerfile-mode arduino-mode forth-mode rainbow-delimiters swiper ob-rust drawille csv-mode bluetooth gruvbox-theme dracula-theme scala-mode yaml-mode yaml fsharp-mode 2048-game cobol-mode dark-souls flappymacs hacker-typer isend-mode kaomoji lolcat mbo70s-theme mines moe-theme nyan-mode pacmacs poly-org rainbow-mode rubik roguel-ike rustic rust-mode scad-preview scad-mode sexy-monochrome-theme spacemacs-theme sqlite3 vscode-dark-plus-theme vscode-icon python-mode powerline ob-julia-vterm material-theme julia-mode ess vterm-toggle))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b4ba3e1bba2e303265eb3e9753215408e75e031f7c894786ad04cabef46ff94c" "ba099d9f4f87bc9c2d5ae497782e05b88dfbe746e3c96df6a8a19668f8ad7518" "02fefdfc9a0c7256a10c8794a4985c9c70c5fbf674873b66807e8143e02c81a7" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" "0feb7052df6cfc1733c1087d3876c26c66410e5f1337b039be44cb406b6187c6" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "97fbd952a3b01fbace2aa49b3b07692cacc3009883c7219b86e41669c2b65683" "f703efe04a108fcd4ad104e045b391c706035bce0314a30d72fbf0840b355c2c" "23b564cfb74d784c73167d7de1b9a067bcca00719f81e46d09ee71a12ef7ee82" "8f0a782ba26728fa692d35e82367235ec607d0c836e06bc39eb750ecc8e08258" default))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(warning-suppress-types '((comp))))

;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (package-install-selected-packages)

; make julia-vterm work w/ org mode
(require 'org)
(add-to-list 'org-babel-load-languages '(julia-vterm . t))
(add-to-list 'org-babel-load-languages '(python . t))
(add-to-list 'org-babel-load-languages '(R . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JuliaMono" :foundry "UKWN" :slant normal :weight semi-bold :height 136 :width normal)))))

; overwrite default submit kebinding to work on terminal mode
(define-key julia-vterm-mode-map (kbd "C-c RET") 'julia-vterm-send-region-or-current-line)

; do the same for isend
(eval-after-load "isend-mode"
 '(define-key isend-mode-map (kbd "C-c RET") 'isend-send))

;activate julia-vterm with julia-mode
(add-hook 'julia-mode-hook #'julia-vterm-mode)

;themes
;(setq themes '(
;	       moe-dark
;	       mbo70s
;	       spacemacs-dark
;	       vscode-dark-plus
;	       )
;     )
;(defun random-choice (items)
;  (let* ((size (length items))
;         (index (random size)))
;    (nth index items)))
;(load-theme (random-choice themes) t)
;(load-theme 'vscode-dark-plus t)
;(load-theme 'spacemacs-dark t)
;(load-theme 'mbo70s t)
(load-theme 'dracula t)

;load custom modeline
;(require 'powerline)
;(powerline-default-theme)
(add-hook 'after-init-hook #'doom-modeline-mode)
(add-hook 'after-init-hook #'nyan-mode)
;change yes/no to y/nx
(fset 'yes-or-no-p 'y-or-n-p)

 
;make swiper default "find in page" thing
(global-set-key "\C-s" 'swiper)

;lol dont worry about it
(setq gamegrid-glyph-height-mm 2.5)

;override the split  windows functions to switch automatically
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

;switch windows with shift-arrow, no more need to C-x o
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;enable rainbow-delimeters in most programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;enable company auto-completion in most programming modes
(add-hook 'prog-mode-hook #'company-mode)

;enable extra features in dired
(add-hook 'dired-mode-hook #'dired-filter-mode)

;set eww as the default browser to open links in emacs 
;(I want to see if I can make this w3m)
(setq browse-url-browser-function 'eww-browse-url)
					
;enable aggressive indentation
;(global-aggressive-indent-mode 1)
;enable avy goto stuff
(global-set-key (kbd "M-;") 'avy-goto-char)
(global-set-key (kbd "M-\"") 'avy-goto-char-2)

;enable treemacs stuff
(global-set-key (kbd "M-q") 'treemacs)

;enable vterm toggle stuff
(global-set-key (kbd "C-x v") 'vterm-toggle)

; vterm setup
(setq vterm-eval-cmds '(("find-file" find-file-other-window)
                             ("message" message)
                             ("vterm-clear-scrollback" vterm-clear-scrollback)))

;startup options
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
;; (split-window-horizontally)
;; ;(deer)
;; (other-window 1)
;; (vterm)
;; (rename-buffer "local")
;; (other-window 1)

; terminal specific stuff

