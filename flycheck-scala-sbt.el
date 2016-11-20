;;; flycheck-scala-sbt.el --- sbt-mode checker for Scala -*- lexical-binding: t -*-

;; Version: 0.1
;; Url: https://www.github.com/rjmac/flycheck-scala-sbt
;; Package-requires: ((emacs "25.1") (flycheck "30") (sbt-mode "0.2"))

;;; Commentary:

;; Provides an sbt-mode based checker for scala files.  Call
;; flycheck-scala-sbt-init from your scala-mode-hook to set it up.

;;; Code:

(require 'flycheck)
(require 'sbt-mode)
(require 'cl-lib)

(defgroup flycheck-scala-sbt nil
  "Scala support for Flycheck via sbt-mode."
  :prefix "flycheck-scala-sbt-"
  :tag "Scala-SBT"
  :group 'flycheck)

(defcustom flycheck-scala-sbt-collect-from-other-files-p
  t
  "Whether to collect errors and warnings from other files.

If set, errors detected in other files are shown at the start of
the current buffer."
  :type 'boolean
  :tag "Collect from other files"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-auto-reload-p
  t
  "Whether to automatically reload the project when the build changes.

If set, changes to .sbt files and .scala files living in
directories named \"project\" trigger an SBT reload before
compiling."
  :type 'boolean
  :tag "Reload SBT automatically"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-auto-exit-console-p
  t
  "Whether to automatically exit from a console session.

If set, when SBT is in a console session flycheck will exit it to
issue a compile command."
  :type 'boolean
  :tag "Exit REPL automatically"
  :group 'flycheck-scala-sbt)

(defcustom flycheck-scala-sbt-enable-in-java-mode-p
  nil
  "Whether to register as a Java-mode checker.

If set, the checker will register itself as a `java-mode' checker
as well.  Changing this does not take immediate effect; Emacs must
be restarted, as it affects the way the checker is defined."
  :type 'boolean
  :tag "Register as a Java-mode checker"
  :group 'flycheck-scala-sbt)

(defun flycheck-scala-sbt--build-error-p (line)
  "Test whether LINE indicates that building the project itself failed."
  (string= "Project loading failed: (r)etry, (q)uit, (l)ast, or (i)gnore? " line))

(defun flycheck-scala-sbt--prompt-detection (line)
  "Test whether LINE represents an SBT input prompt."
  (cond
   ((flycheck-scala-sbt--build-error-p line) :build-error)
   ((string-match sbt:sbt-prompt-regexp line) :normal)
   ((string-match sbt:console-prompt-regexp line) :console)
   (t nil)))

(defun flycheck-scala-sbt--wait-for-prompt-then-call (sbt-buffer f)
  "Wait for the SBT prompt in SBT-BUFFER, then call F.

F is called with `:normal' if a normal SBT, prompt was detected,
`:console' if a Scala console prompt was, or `:build-error' if a
build-script error prompt was found.

The function is called with same current buffer as was originally
active when the original call was made."
  (let ((original-buffer (current-buffer)))
    (let* ((last-line (with-current-buffer sbt-buffer
                        (save-excursion
                          (goto-char (point-max))
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
           (prompt-type (flycheck-scala-sbt--prompt-detection last-line)))
      (if prompt-type
          (funcall f prompt-type)
        (run-at-time 0.1 nil (lambda ()
                               (with-current-buffer original-buffer
                                 (flycheck-scala-sbt--wait-for-prompt-then-call sbt-buffer f))))))))

(defun flycheck-scala-sbt--issue-retry (sbt-buffer)
  "Issue a retry command in SBT-BUFFER after a build script failure."
  (with-current-buffer sbt-buffer
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (comint-send-string (get-buffer-process sbt-buffer) "r\n"))
    (sbt:clear sbt-buffer)))

(cl-defmacro flycheck-scala-sbt--wait-for-prompt-then (sbt-buffer status-callback (&key prompt-type) &body body)
  "Wait for the SBT prompt in SBT-BUFFER, using STATUS-CALLBACK for failure, then evaluate BODY.

STATUS-CALLBACK is used for reporting errors if the project build
fails or if BODY throws an error."
  (let ((sbt-buffer-var (cl-gensym))
        (status-callback-var (cl-gensym))
        (err (cl-gensym))
        (result (or prompt-type (cl-gensym))))
    `(let ((,sbt-buffer-var ,sbt-buffer)
           (,status-callback-var ,status-callback))
       (flycheck-scala-sbt--wait-for-prompt-then-call
        ,sbt-buffer-var
        (lambda (,result)
          (condition-case ,err
              (progn ,@body)
            (error (funcall ,status-callback-var 'errored (error-message-string ,err)))))))))

(defun flycheck-scala-sbt--build-script-p (file)
  "Check whether FILE is likely a part of the build script.

FILE can be either a buffer or a filename.

Returns true if the name ends in \".sbt\" or if the last
directory in the path is \"project\"."
  (let ((filename (if (bufferp file) (buffer-file-name file) file)))
    (and filename (or (string-suffix-p ".sbt" filename)
                      (string-match "/project/[^/]+$" filename)))))

(defun flycheck-scala-sbt--start (checker status-callback)
  "The main entry point for the CHECKER.  Don't call this.  STATUS-CALLBACK."
  (let ((sbt-buffer (save-window-excursion (sbt:run-sbt))))
    ;; ok, this is sort of complicated.
    (cl-labels ((finish ()
                  (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer status-callback ()
                    (funcall status-callback 'finished (flycheck-scala-sbt--errors-list sbt-buffer checker))))
                (finish-post-reload ()
                  ;; We've just issued a reload.  Now we need to wait
                  ;; for that to finish.
                  (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer status-callback (:prompt-type prompt-type)
                    (cl-ecase prompt-type
                      (:normal
                       ;; Good -- it finished normally.  Now send a
                       ;; compile command without clearing the buffer,
                       ;; so we can collect diagnostics from both the
                       ;; reload and the compile.
                       (save-window-excursion
                         ;; First, delete our current prompt so that
                         ;; we don't immediately say "hey there's a
                         ;; prompt, we must be done".
                         (with-current-buffer sbt-buffer
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (delete-region (line-beginning-position) (line-end-position))))
                         (let ((sbt:save-some-buffers nil)
                               (sbt:clear-buffer-before-command nil))
                           (sbt:command "test:compile" nil))))
                      (:console
                       (error "Didn't expect to end up in console mode here!"))
                      (:build-error
                       ;; The reload didn't succeed.  Ok then, we'll
                       ;; just collect what diagnostics we have.
                       t))
                    (finish))))
      (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer status-callback (:prompt-type prompt-type)
        (save-window-excursion
          (cl-ecase prompt-type
            (:normal
             ;; ok, everything's good with the build script as far as
             ;; we know.  Let's try building stuff!
             (let ((sbt:save-some-buffers nil))
               (if (and flycheck-scala-sbt-auto-reload-p
                        (flycheck-scala-sbt--build-script-p (current-buffer)))
                   (progn
                     ;; we're in a build script (probably) so issue a
                     ;; reload to pick up changes to it.
                     (sbt:command "reload" nil)
                     (finish-post-reload))
                 ;; Normal source file, just recompile.
                 (sbt:command "test:compile" nil)
                 (finish))))
            (:console
             (if flycheck-scala-sbt-auto-exit-console-p
                 (progn
                   (sbt:clear sbt-buffer)
                   (comint-send-string sbt-buffer ":quit\n")
                   (flycheck-scala-sbt--start checker status-callback))
               (funcall status-callback 'errored "SBT is in a console session.  Exit it to reenable flycheck.")))
            (:build-error
             ;; nope, we left off in a bad place.  Issue a retry and
             ;; see if that fixes things.
             (if flycheck-scala-sbt-auto-reload-p
                 (progn
                   (flycheck-scala-sbt--issue-retry sbt-buffer)
                   (finish-post-reload))
               (funcall status-callback 'errored "The project failed to load.")))))))))

(defun flycheck-scala-sbt--errors-list (sbt-buffer checker)
  "Find the current list of errors in SBT-BUFFER and convert them into errors for CHECKER."
  (let ((errors
         (with-current-buffer sbt-buffer
           (save-excursion
             (goto-char (point-min))
             (beginning-of-line)
             (let ((acc '()))
               ;; The horror, the horror
               ;;
               ;; Ok so.
               ;;
               ;; The way this actually works is by finding the _carets_ that
               ;; indicate the column, and then working backward two lines to
               ;; find the relevant filename, row, and message.
               (while (re-search-forward "^\\(\\[\\(error\\|warn\\)][[:space:]]\\)?[[:space:]]*^$" (point-max) t)
                 (push (flycheck-scala-sbt--extract-error-info) acc))
               (reverse acc))))))
    (cl-mapcan (lambda (error) (flycheck-scala-sbt--convert-error-info checker error)) errors)))

(defun flycheck-scala-sbt--extract-error-info ()
  "Extract the error at point.

The point should be placed on the last line of the error message,
where scala prints the caret indicating the column in which the
error occurred."
  (save-excursion
    (let ((final-line (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
      (if (string-prefix-p "[" final-line)
          (flycheck-scala-sbt--extract-main-error-info)
        (flycheck-scala-sbt--extract-buildscript-error-info)))))

(defun flycheck-scala-sbt--extract-main-error-info ()
  "Extract the non-build-script error or warning at point."
  (let ((line-regexp "^\\[\\(error\\|warn\\)][[:space:]]+\\(.*?\\):\\([0-9]+\\):[[:space:]]+\\(?:\\(?:error\\|warning\\):[[:space:]]*\\)?\\(.*\\)$")
        (else-regexp "^\\[\\(error\\|warn\\)][[:space:]]\\(.*\\)$")
        (subsequent-lines '())
        (current-line nil)
        (error-column (current-column)))
    (forward-line -2) ;; skip caret and actual erroring line
    (while (not (string-match line-regexp (setq current-line
                                                (buffer-substring-no-properties (line-beginning-position)
                                                                                (line-end-position)))))
      (unless (string-match else-regexp current-line)
        (error "Expected to find an error continuation line"))
      (push (match-string 2 current-line) subsequent-lines)
      (forward-line -1))
    (let ((type (match-string 1 current-line))
          (file (match-string 2 current-line))
          (row (string-to-number (match-string 3 current-line)))
          (message (mapconcat 'identity (cons (match-string 4 current-line) subsequent-lines) "\n")))
      (list file
            row
            (- error-column (+ 3 (length type)))
            (if (string= type "warn") 'warning 'error)
            message))))

(defun flycheck-scala-sbt--extract-buildscript-error-info ()
  "Extract the build-script error or warning at point.

This is a little less safe than extracting from the main build,
because the lines aren't marked by \"error\" or \"warning\" at
the start of the line."
  (let ((line-regexp "^\\(/.*?\\):\\([0-9]+\\):[[:space:]]\\(error\\|warning\\):[[:space:]]\\(.*\\)$")
        (subsequent-lines '())
        (current-line nil)
        (error-column (current-column)))
    (forward-line -2) ;; skip caret and actual erroring line
    (while (not (string-match line-regexp (setq current-line
                                                (buffer-substring-no-properties (line-beginning-position)
                                                                                (line-end-position)))))
      (push current-line subsequent-lines)
      (forward-line -1))
    (let ((file (match-string 1 current-line))
          (row (string-to-number (match-string 2 current-line)))
          (type (match-string 3 current-line))
          (message (mapconcat 'identity (cons (match-string 4 current-line) subsequent-lines) "\n")))
      (list file
            row
            error-column
            (if (string= type "warning") 'warning 'error)
            message))))

(defun flycheck-scala-sbt--extract-build-error-info ()
  "Extract the error at point.

The point should be placed on the last line of the error message,
where scala prints the caret indicating the column in which the
error occurred."
  (save-excursion
    (let ((line-regexp "^\\[\\(error\\|warn\\)][[:space:]]+\\(.*?\\):\\([0-9]+\\):[[:space:]]+\\(.*\\)$")
          (else-regexp "^\\[\\(error\\|warn\\)][[:space:]]\\(.*\\)$")
          (subsequent-lines '())
          (current-line nil)
          (error-column (current-column)))
      (forward-line -2) ;; skip caret and actual erroring line
      (while (not (string-match line-regexp (setq current-line
                                                  (buffer-substring-no-properties (line-beginning-position)
                                                                                  (line-end-position)))))
        (unless (string-match else-regexp current-line)
          (error "Expected to find an error continuation line"))
        (push (match-string 2 current-line) subsequent-lines)
        (forward-line -1))
      (let ((type (match-string 1 current-line))
            (file (match-string 2 current-line))
            (row (string-to-number (match-string 3 current-line)))
            (message (mapconcat 'identity (cons (match-string 4 current-line) subsequent-lines) "\n")))
        (list file
              row
              (- error-column (+ 3 (length type)))
              (if (string= type "warn") 'warning 'error)
              message)))))

(defun flycheck-scala-sbt--convert-error-info (checker error)
  "Provide CHECKER an ERROR converted into a flycheck-error.

ERROR should come from `flycheck-scala-sbt--extract-error-info'."
  (let* ((file (nth 0 error))
         (row (nth 1 error))
         (col (nth 2 error))
         (level (nth 3 error))
         (message (nth 4 error))
         (buffer (find-buffer-visiting file)))
    ;; it sure seems like you should be able to return errors in other
    ;; buffers, but it seems flycheck tries to highlight all errors in
    ;; the CURRENT buffer no matter what buffer they were sourced from.
    (cond
     ((eql buffer (current-buffer))
      (list (flycheck-error-new :buffer buffer :message message :checker checker :filename file :line row :column col :level level)))
     (flycheck-scala-sbt-collect-from-other-files-p
      (list (flycheck-error-new :buffer (current-buffer)
                                :message (concat "from " file ":" (prin1-to-string row) ":" (prin1-to-string col) ":\n  " (replace-regexp-in-string "\n" "\n  " message))
                                :checker checker
                                :filename (buffer-file-name)
                                :line 1
                                :column 1
                                :level level)))
     (t nil))))

(flycheck-define-generic-checker
    'scala-sbt
  "Check scala buffers using sbt-mode"
  :modes (append '(scala-mode) (if flycheck-scala-sbt-enable-in-java-mode-p '(java-mode)))
  :predicate 'sbt:find-root
  :start 'flycheck-scala-sbt--start
  :next-checkers '((warning . scala-scalastyle)))

(cl-pushnew 'scala-sbt flycheck-checkers)

;;;###autoload
(defun flycheck-scala-sbt-init ()
  "Call this from your scala-mode-hook to set up flycheck-sbt."
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-mode t))

(provide 'flycheck-scala-sbt)

;;; flycheck-scala-sbt.el ends here
