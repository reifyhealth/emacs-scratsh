;; Scratsh version 0.1
;;
;; features
;; [X] send region to terminal
;; [X] major mode which branches off of shell
;; [X] minor mode which is used in scratch buffer
;; [X] link a buffer to terminal: default buffer
;; [X] link a buffer to terminal: multiple buffers
;; [X] reference shell buffer name in scratch mode lile
;; [X] eliminate newlines in shell output
;; [X] docstrings on public functions
;; [ ] support both vterm and ansi-term
;; [ ] first-class support for scripting languages other than bash
;; [ ] detect scripting language based on shell
;; [X] customize inserting of logo on startup
;; [X] readme
;; [ ] configure windows for nice use (e.g. vertical stacking)
;; [ ] git repository
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user options

(defcustom scratsh-show-logo t
  "If non-nil, then show the scratsh logo on startup. Otherwise, do not show the logo."
  :group 'scratsh)

;; (defvar scratsh-shell-configurations
;;   '(:term (:insert-function #'term-send-raw-string
;;		   :return-function term-send-input)
;;	:vterm (:insert-function vterm-insert)))

;; (term-send-raw-string)

;; (scratsh-shell-configurations)

;; (defvar-local scratsh--shell-type nil)

;; (defvar-local scratsh-local-shell-configuration nil)

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

(defvar-local scratsh-linked-shell-buffer nil
  "The shell buffer linked to a scratsh buffer.")

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

(defun scratsh--maybe-insert-logo () (when scratsh-show-logo (insert scratsh-logo)))

(defun scratsh--create-scratch-buffer ()
  (let ((scratch-buffer (generate-new-buffer "*scratsh*")))
	(with-current-buffer scratch-buffer
	  (sh-mode)
	  (scratsh-shell-minor-mode 1)
	  (sh-set-shell "bash" t nil)
	  (scratsh--maybe-insert-logo))
	scratch-buffer))

(defun scratsh--check-linked-shell
	(when (scratsh-linked-shell-buffer)
	  (with-current-buffer scratsh-linked-shell-buffer
		(vterm-insert text))
	  (if (y-or-n-p "No linked shell buffer. Connect to a shell? ")
		  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell buffer operations

(defvar-local scratsh-primary-scratch-buffer nil
  "The primary scratch buffer linked to a shell buffer.")

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
;; minor mode for scratch buffer

(defvar-local scratsh-scratch-minor-mode nil)

(define-minor-mode scratsh-shell-minor-mode
  "Minor mode for a scratch buffer connected to a shell."
  :keymap (let ((keymap (make-sparse-keymap)))
			(define-key keymap (kbd "C-c C-c") 'scratsh-send)
			(define-key keymap (kbd "C-c C-r") 'scratsh-send-region)
			(define-key keymap (kbd "C-c C-b") 'scratsh-send-buffer)
			keymap)
  :lighter (:eval (concat " Scratsh["
						  (if scratsh-linked-shell-buffer
							  scratsh-linked-shell-buffer
							(propertize "!disconnected!" 'face 'font-lock-warning-face))
						  "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public api

(defun scratsh-send ()
  "Send the region between two comment lines to the connected shell. If there is no comment
line before (resp. after) the current point, then the region extends to the beginning
(resp. end) of the buffer.

If the current buffer is not connected to a shell, an error will be thrown. To connect,
run the command `scratsh-connect'.

See also: `scratsh-send-buffer' `scratsh-send-region'"
  (interactive)
  (-> (buffer-substring (scratsh--current-min) (scratsh--current-max))
	  (scratsh--clean-text)
	  (scratsh--send-text))
  (scratsh--send-return)) 

(defun scratsh-send-region (start end)
  "Send the contents of the current region to the connected shell.

If the current buffer is not connected to a shell, an error will be thrown. To connect,
run the command `scratsh-connect'.

See also: `scratsh-send' `scratsh-send-buffer'"
  (interactive "r")
  (scratsh--send-text (buffer-substring start end))
  (scratsh--send-return))

(defun scratsh-send-buffer ()
  "Send the contents of the current buffer to the connected shell.

If the current buffer is not connected to a shell, an error will be thrown. To connect,
run the command `scratsh-connect'.

See also: `scratsh-send' `scratsh-send-region'"
  (interactive)
  (scratsh--send-text (buffer-string))
  (scratsh--send-return))

(defun scratsh-new ()
  "Create a new scratch buffer. If one or more scratch buffers already exist, then the created
buffer will be named according to the behavior of `generate-new-buffer-name'.

The newly created scratch buffer is not connected to any shell. To connect, run the command
`scratsh-connect'.

See also: `scratsh-connect' `scratsh-jack-in'"
  (interactive)
  (switch-to-buffer (scratsh--create-scratch-buffer)))

(defun scratsh-connect (buffer-name)
  "When called in a scratch buffer, connect the scratch buffer to a shell buffer. Behavior outside
of scratch buffers is undefined.

When called interactively, the user selects the target shell buffer in a menu.

See also: `scratsh-jack-in' `scratsh-new'"
  (interactive "bSelect shell buffer: ")
  (setq scratsh-linked-shell-buffer buffer-name)
  (message "Connected to buffer: %s" scratsh-linked-shell-buffer))

(defun scratsh-jack-in ()
  "When called in a shell buffer H, creates a scratch buffer C, connects C to H, and sets C
as the primary scratch buffer of H. Behavior outside of shell buffers is undefined.

If the shell buffer already has a primary scratch buffer B, then the user will be prompted
to screate a new scratch buffer and set the new buffer as primary. Regardless of the choice, B
will remain connected to the shell.

See also: `scratsh-connect' `scratsh-new'"
  (interactive)
  (let ((shell-buffer (current-buffer)))
	(if scratsh-primary-scratch-buffer
		(when (y-or-n-p
			   (format "This shell is already attached to %s. Make a new scratch buffer primary?"
					   scratsh-primary-scratch-buffer))
		  (scratsh--connect-shell-to-new-scratch-buffer shell-buffer))
	  (scratsh--connect-shell-to-new-scratch-buffer shell-buffer))))

(defun scratsh-switch ()
  "When called in a shell buffer H with a primary scratch buffer C, opens C using
`switch-to-buffer-other-window'. If H has no primary scratch buffer, do nothing.

See also: `scratsh-jack-in'."
  (interactive)
  (when scratsh-primary-scratch-buffer
	(switch-to-buffer-other-window scratsh-primary-scratch-buffer)))

(provide 'scratsh)
