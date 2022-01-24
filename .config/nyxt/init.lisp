(in-package #:nyxt-user)

;; Import Files
(dolist (file (list (nyxt-init-file "statusline.lisp")
                    (nyxt-init-file "stylesheet.lisp")))
  (load file))

(DEFINE-CONFIGURATION BUFFER
  ((DEFAULT-MODES (APPEND '(NYXT::EMACS-MODE) %SLOT-DEFAULT%))))
(SETF (UIOP/OS:GETENV "WEBKIT_DISABLE_COMPOSITING_MODE") "1")

