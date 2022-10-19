;; features
;; [X] send region to terminal
;; [X] major mode which branches off of shell
;; [ ] minor mode which is used in shell buffer
;; [X] link a buffer to terminal: default buffer
;; [ ] link a buffer to terminal: multiple buffers
;; [ ] reference shell buffer name in scratch mode lile
;; [ ] eliminate newlines in shell output
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

(defconst scratsh-logo
  "###########################################################
#   _________                        __          .__      #
#  /   _____/  ____ _______ _____  _/  |_  ______|  |__   #
#  \\_____  \\ _/ ___\\\\_  __ \\\\__  \\ \\   __\\/  ___/|  |  \\  #
#  /        \\\\  \\___ |  | \\/ / __ \\_|  |  \\___ \\ |   Y  \\ #
# /_______  / \\___  >|__|   (____  /|__| /____  >|___|  / #
#         \\/      \\/             \\/           \\/      \\/  #
###########################################################
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch buffer operations

(defconst scratsh-comment-separator "#")
(defconst scratsh-comment-start-regexp (concat "^" scratsh-comment-separator))
(defconst scratsh-comment-not-regexp (concat "^[^" scratsh-comment-separator "]"))
(defconst scratsh-empty-line-regexp "^\\s-*$")

(defun scratsh--current-min ()
  (save-excursion
	(beginning-of-line)
	(if (looking-at scratsh-comment-start-regexp)
		(if (re-search-forward scratsh-comment-not-regexp (point-max) t)
			(point-at-bol) (point-max))
	  (if (re-search-backward scratsh-comment-start-regexp (point-min) t)
		  (point-at-bol 2)
		(point-min)))))

(defun scratsh--current-max ()
  (save-excursion
	(if (re-search-forward scratsh-comment-start-regexp (point-max) t)
		(max (- (point-at-bol) 1) 1)
	  (progn (goto-char (point-max))
			 (if (looking-at "^$") (- (point) 1) (point))))))

(defun scratsh--create-scratch-buffer ()
  (let ((scratch-buffer (generate-new-buffer "*scratsh*")))
	(with-current-buffer scratch-buffer
	  (sh-mode)
	  (scratsh-shell-minor-mode 1)
	  (sh-set-shell "bash" t nil)
	  (insert scratsh-logo))
	scratch-buffer))

(defun scratsh--check-linked-shell
	(when (scratsh-linked-shell-buffer)
	  (with-current-buffer scratsh-linked-shell-buffer
		(vterm-insert text))
	  (if (y-or-n-p "No linked shell buffer. Connect to a shell? ")
		  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell buffer operations

(defun scratsh--clean-text (text)
  (replace-regexp-in-string "\n[\n]+" "" text))

(defun scratsh--send-text (text)
  (save-excursion
	(if (not scratsh-linked-shell-buffer)
		(error "Scratsh error: No attached shell. Connect with scratsh-connect")
	  (with-current-buffer scratsh-linked-shell-buffer
		(vterm-insert text)))))

(defun scratsh--send-return ()
  (with-current-buffer "*vterm*"
	(vterm-send-return)))

(defun scratsh--connect-shell-to-new-scratch-buffer (shell-buffer)
  (let ((scratch-buffer (scratsh--create-scratch-buffer)))
	(setq scratsh-primary-scratch-buffer (buffer-name scratch-buffer))
	(switch-to-buffer-other-window scratch-buffer)
	(setq scratsh-linked-shell-buffer (buffer-name shell-buffer))
	(message "Connected buffers: %s <--> %s " shell-buffer scratch-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-level api

(defun scratsh-new ()
  (interactive)
  (switch-to-buffer (scratsh--create-scratch-buffer)))

(defun scratsh-send ()
  (interactive)
  (-> (buffer-substring (scratsh--current-min) (scratsh--current-max))
	  (scratsh--clean-text)
	  (scratsh--send-text))
  (scratsh--send-return))

(defun scratsh-send-region (start end)
  (interactive "r")
  (scratsh--send-text (buffer-substring start end))
  (scratsh--send-return))

(defun scratsh-send-buffer ()
  (interactive)
  (scratsh--send-text (buffer-string))
  (scratsh--send-return))

(defun scratsh-connect (buffer-name)
  (interactive "bSelect shell buffer: ")
  (setq scratsh-linked-shell-buffer buffer-name)
  (message "Connected to buffer: %s" scratsh-linked-shell-buffer))

(defun scratsh-jack-in ()
  (interactive)
  (let ((shell-buffer (current-buffer)))
	(if scratsh-primary-scratch-buffer
		(when (y-or-n-p
			   (format "This shell is already attached to %s. Make a new scratch buffer primary?"
					   scratsh-primary-scratch-buffer))
		  (scratsh--connect-shell-to-new-scratch-buffer shell-buffer))
	  (scratsh--connect-shell-to-new-scratch-buffer shell-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratsh scratch minor mode

(defvar-local scratsh-scratch-minor-mode nil)

(define-minor-mode scratsh-shell-minor-mode
  "Minor mode for a scratch buffer connected to a shell."
  :keymap (let ((keymap (make-sparse-keymap)))
			(define-key keymap (kbd "C-c C-c") 'scratsh-send)
			(define-key keymap (kbd "C-c C-r") 'scratsh-send-region)
			(define-key keymap (kbd "C-c C-b") 'scratsh-send-buffer)
			keymap))

(provide 'scratsh)
