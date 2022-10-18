;; features
;; [X] send region to terminal
;; [X] major mode which branches off of shell
;; [ ] minor mode which is used in shell buffer
;; [X] link a buffer to terminal: default buffer
;; [ ] link a buffer to terminal: multiple buffers
;; [ ] reference shell buffer name in scratch mode lile
;; support bash, zsh, etc. (and set correctly (autodetect?))
;; configure windows for nice use (e.g. vertical stacking)
(require 'a)
(require 'dash)

;; (defvar scratsh-shell-configurations
;;   '(:term (:insert-function #'term-send-raw-string
;;		   :return-function term-send-input)
;;	:vterm (:insert-function vterm-insert)))

;; (term-send-raw-string)

;; (scratsh-shell-configurations)

;; (defvar-local scratsh--shell-type nil)

;; (defvar-local scratsh-local-shell-configuration nil)

(defvar-local scratsh-linked-shell-buffer nil
  "The shell buffer linked to a scratsh buffer.")

(defvar-local scratsh-primary-scratch-buffer nil
  "The primary scratch buffer linked to a shell buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text insertion to a shell buffer

(defun scratsh--send-text (text)
  (save-excursion
	(with-current-buffer scratsh-linked-shell-buffer
	  (vterm-insert text))))

(defun scratsh--send-return ()
  (with-current-buffer "*vterm*"
	(vterm-send-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch buffer setup

(defun scratsh--create-scratch-buffer ()
  (let ((scratch-buffer (generate-new-buffer "*scratsh*")))
	(with-current-buffer scratch-buffer
	  (sh-mode)
	  (sh-set-shell "bash" t nil))
	scratch-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-level api

(defun scratsh-send-region (start end)
  (interactive "r")
  (scratsh--send-text (buffer-substring start end))
  (scratsh--send-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major mode

(defun scratsh-connect (buffer-name)
  (interactive "bSelect shell buffer: ")
  (setq scratsh-linked-shell-buffer buffer-name)
  (message "Connected to buffer: %s" scratsh-linked-shell-buffer))

;; TODO: check if already connected in
(defun scratsh-jack-in ()
  (interactive)
  (if scratsh-primary-scratch-buffer
	  (error "bad")
	(let ((shell-buffer (current-buffer))
		  (scratch-buffer (scratsh--create-scratch-buffer)))
	  (setq scratsh-primary-scratch-buffer (buffer-name scratch-buffer))
	  (switch-to-buffer-other-window scratch-buffer)
	  (setq scratsh-linked-shell-buffer (buffer-name shell-buffer))
	  (message "Connected buffers: %s <--> %s " shell-buffer scratch-buffer))))

(define-derived-mode scratsh-mode sh-mode "Scratsh"
  "Major mode for using a shell with a scratch buffer."
  (call-interactively 'scratsh-connect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor mode

;; (defun scratsh--enable-minor-mode ()
;;   (setq scratsh-primary-scratch-buffer nil))

;; (defvar-local scratsh-minor-mode nil)

;; (define-minor-mode scratsh-minor-mode
;;   "Minor mode for a shell with a primary scratch buffer."
;;   (if scratsh-minor-mode
;;	  (progn
;;		(setq-local scratsh-primary-scratch-buffer nil))
;;	;;(scratsh--enable-minor-mode)
;;	))

(provide 'scratsh)
