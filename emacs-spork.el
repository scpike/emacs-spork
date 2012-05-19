; Run testdrb against the current file or an arbitrary filename via ansi-term
; Relies on spork being already up and running, but will start a new
; ansi-term buffer for you automatically if there is not one already
; (configured by the spork-test-buffer variable)

; TODO: Guess which test to run for a given model/controller/view
;       Run all Unit/Functional/Integration tests
;       Run all tests, via some kind of directory traversing up and back down

; The name of the buffer the tests will run in.
(defvar spork-test-buffer "spork-tests")
(defvar es-last-command nil)


;; https://github.com/jimm/elisp/blob/master/emacs.el
(defun es-singularize (str)
  "Singularize STR, which is assumed to be a single word. This is
a simple algorithm that may grow over time if needed."
  (interactive "s")
  (let ((len (length str)))
    (cond ((equal "ies" (substring str (- len 3))) (concat (substring str 0 (- len 3)) "y"))
          ((equal "i" (substring str (- len 1))) (concat (substring str 0 (- len 1)) "us"))
          ((equal "s" (substring str (- len 1))) (substring str 0 (- len 1)))
          (t str))))
(defun es-pluralize (str)
  "Pluralize STR, which is assumed to be a single word. This is a
simple algorithm that may grow over time if needed."
  (interactive "s")
  (let ((len (length str)))
    (cond ((equal "y" (substring str (- len 1))) (concat (substring str 0 (- len 1)) "ies"))
          ((equal "us" (substring str (- len 2))) (concat (substring str 0 (- len 2)) "i"))
          (t (concat str "s")))))


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
      (term-simple-send proc cmd)
      (setq es-last-command cmd))))

(defun es-test-file (file-name)
  (interactive "FFile:")
  (let ((cmd (concat "testdrb " file-name)))
    (sw-shell/commands spork-test-buffer cmd)))

(defun es-test-files (filenames)
  (test-file (mapconcat 'identity filenames " ")))

(mapconcat 'identity (list "sdf" "asdf") " ")

(identity "asdf")
(es-test-files (list "asdf" "sdfsdF"))

(defun es-run-current-file ()
  (interactive)
  (es-test-file buffer-file-name))

(defun es-redo-last-test ()
  (interactive)
  (sw-shell/commands spork-test-buffer es-last-command))

(defun es-run-unit-tests ()
  (interactive)
  (let ((default-directory (es-project-directory)))
    (es-test-files (file-expand-wildcards "*test/unit/*rb" t))))
;; (es-run-unit-tests)

(defun es-run-functional-tests ()
  (interactive)
  (let ((default-directory (es-project-directory)))
    (es-test-files (file-expand-wildcards "*test/functional/*rb" t))))
;; (es-run-functional-tests)

(defun es-run-data-file-tests ()
  (interactive)
  (let ((default-directory (es-project-directory)))
    (es-test-files (file-expand-wildcards "*test/data_file_parsers/*rb" t))))
;; (es-run-data-file-tests)

(defun es-project-directory ()
  "search up from the current directory, looking for a project folder. project
folder being a directory with a folder called test in it."
  (locate-dominating-file default-directory "test"))

(defun es-run-tests-for-current-file ()
  "Run the tests that correspond to the current file.
 Support:
   - Models: app/models/model_name.rb
     Unit,functional,parsers
   - Views: app/views/pluralized_name/thing.erb
   - Controller app/contollers/pluralized_name.rb"
  (interactive)
  (let ((model (es-model-for-file buffer-file-name)))
    (let ((tests (es-tests-for-model model)))
      (es-test-files tests)
      (message (concat "Running tests for " model
                       ": " (mapconcat 'identity tests ", "))))))

(defun es-run-unit-test-for-current-file ()
  "Run the unit tests that corresponds to the current file.
 Support:
   - Models: app/models/model_name.rb
     Unit,functional,parsers
   - Views: app/views/pluralized_name/thing.erb
   - Controller app/contollers/pluralized_name.rb"
  (interactive)
  (let ((model (es-model-for-file buffer-file-name)))
    (let ((tests (es-unit-test-for-model model)))
      (es-test-files tests)
      (message (concat "Running unit test for " model
                       ": " (mapconcat 'identity tests ", "))))))

(defun es-run-functional-test-for-current-file ()
  "Run the functional test that corresponds to the current file.
 Support:
   - Models: app/models/model_name.rb
     Unit,functional,parsers
   - Views: app/views/pluralized_name/thing.erb
   - Controller app/contollers/pluralized_name.rb"
  (interactive)
  (let ((model (es-model-for-file buffer-file-name)))
    (let ((tests (es-functional-test-for-model model)))
      (es-test-files tests)
      (message (concat "Running functional test for " model
                       ": " (mapconcat 'identity tests ", "))))))

(defun es-unit-test-for-model (model)
  (let ((default-directory (es-project-directory)))
    (append
     (file-expand-wildcards (concat "test/*/"
                                    (es-singularize model) "_test.rb") t))))

(defun es-functional-test-for-model (model)
  (let ((default-directory (es-project-directory)))
    (append
     (file-expand-wildcards (concat "test/functional/"
                                    model "_controller_test.rb") t)
     (file-expand-wildcards (concat "test/functional/"
                                    (es-pluralize model)
                                    "_controller_test.rb") t))))

(defun es-tests-for-model (model)
  "Return both the unit and functional tests associated with this model"
  (append (es-unit-test-for-model model)
    (es-functional-test-for-model model))
  )
;; (es-functional-test-for-model "sample_list")
;; (es-unit-test-for-model "sample_list")
;; (es-tests-for-model "sample_list")
;; (es-tests-for-model "bh_data_file")
;; (es-tests-for-model "data_file")
;; (es-tests-for-model "dashboard")

(defun es-model-for-file (filename)
  "Returns an underscored model name from a path name using rails' conventions"
  (cond ((string-match ".*views/\\([^/]*\\)" filename)
         (es-singularize (match-string 1 filename)))
        ((string-match ".*models.*/\\(.*\\).rb" filename)
         (match-string 1 filename))
        ((string-match ".*controllers/\\(.*\\)_controller.rb" filename)
         (es-singularize (match-string 1 filename)))))
;; (es-model-for-file "app/models/foo.rb")
;; (es-model-for-file "app/models/anomalies/foo.rb")
;; (es-model-for-file "app/views/foos/index.html.erb")
;; (es-model-for-file "app/controllers/foos_controller.rb")

(provide 'emacs-spork)
