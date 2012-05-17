; Run testdrb against the current file or an arbitrary filename via ansi-term
; Relies on spork being already up and running, but will start a new
; ansi-term buffer for you automatically if there is not one already
; (configured by the spork-test-buffer variable)

; TODO: Guess which test to run for a given model/controller/view
;       Run all Unit/Functional/Integration tests
;       Run all tests, via some kind of directory traversing up and back down

; The name of the buffer the tests will run in.
(defvar spork-test-buffer "spork-tests")

; This sw-* stuff is from
;   http://curiousprogrammer.wordpress.com/2009/03/19/emacs-terminal-emulator/
; It takes care of getting ansi-term up and running, and returning to the same
; buffer for subsequent tests.

; sw-basic-shell is modified to open the buffer in another window, so that your
; tests pop up alongside the code.

(defun sw-shell-get-process (buffer-name)
  (let ((buffer (get-buffer (concat "*" buffer-name "*"))))
    (and (buffer-live-p buffer) (get-buffer-process buffer))))

(defun sw-get-process-if-live (buffer-name)
  (let ((proc (sw-shell-get-process buffer-name)))
    (and (processp proc)
         (equal (process-status proc) 'run)
         proc)))

(defun sw-kill-buffer-if-no-process (buffer-name)
  (let* ((buffer (get-buffer (concat "*" buffer-name "*")))
         (proc (sw-get-process-if-live buffer-name)))
    (when (and (not proc) (buffer-live-p buffer)) (kill-buffer buffer))))

(defalias 'sw-shell-exists-p 'sw-get-process-if-live)

(defun sw-basic-shell (buffer-name)
  (sw-kill-buffer-if-no-process buffer-name)
  ;; If there is a process running, leave it, otherwise
  ;; create the new buffer
  (if (sw-shell-exists-p buffer-name)
      (message "Buffer already exists")
    (ansi-term "bash" buffer-name))
  (switch-to-buffer-other-window (concat "*" buffer-name "*")))

(defun sw-shell/commands (buffer-name &rest commands)
  (sw-basic-shell buffer-name)
  (let ((proc (sw-shell-get-process buffer-name)))
    (dolist (cmd commands)
      (term-simple-send proc cmd))))

(defun test-file (filename)
  (interactive "FFile:")
  (sw-shell/commands spork-test-buffer (concat "testdrb " filename) ))

(defun test-current-file ()
  (interactive)
  (sw-shell/commands spork-test-buffer (concat "testdrb " buffer-file-name)))

(provide 'emacs-spork)
