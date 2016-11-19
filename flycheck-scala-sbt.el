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

(defun flycheck-scala-sbt--build-error-p (line)
  "Test whether LINE indicates that building the project itself failed."
  (string= "Project loading failed: (r)etry, (q)uit, (l)ast, or (i)gnore? " line))

(defun flycheck-scala-sbt--default-prompt-detection (line)
  "Test whether LINE represents an SBT input prompt."
  (cond
   ((flycheck-scala-sbt--build-error-p line) :build-error)
   ((string= line "> ") :normal)
   (t nil)))

(defun flycheck-scala-sbt--wait-for-prompt-then-call (sbt-buffer f)
  "Wait for the SBT prompt in SBT-BUFFER, then call F.

F is called with `:normal' if a normal prompt was detected or
`:build-error' if a build-script error prompt was found.

The function is called with same current buffer as was originally
active when the original call was made."
  (let ((original-buffer (current-buffer)))
    (let* ((last-line (with-current-buffer sbt-buffer
                        (save-excursion
                          (goto-char (point-max))
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
           (prompt-type (flycheck-scala-sbt--default-prompt-detection last-line)))
      (if prompt-type
          (funcall f prompt-type)
        (run-at-time 0.1 nil (lambda ()
                               (with-current-buffer original-buffer
                                 (flycheck-scala-sbt--wait-for-prompt-then-call sbt-buffer f))))))))

(defun flycheck-scala-sbt--issue-retry (sbt-buffer)
  "Issue a retry command in SBT-BUFFER after a build script failure."
  (with-current-buffer sbt-buffer
    (goto-char (point-max))
    (comint-send-string (get-buffer-process sbt-buffer) "r\n")
    (sbt:clear sbt-buffer)
    ;; this additional clear is required because otherwise the buffer is left in read-only mode
    (comint-clear-buffer)))

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
    (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer status-callback (:prompt-type prompt-type)
      (save-window-excursion
        (cl-ecase prompt-type
          (:normal
           (let ((sbt:save-some-buffers nil))
             (if (flycheck-scala-sbt--build-script-p (current-buffer))
                 (sbt:command ";reload;compile" nil)
               (sbt:command "compile" nil))))
          (:build-error
           (flycheck-scala-sbt--issue-retry sbt-buffer))))
      (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer status-callback ()
        (funcall status-callback 'finished (flycheck-scala-sbt--errors-list sbt-buffer checker))))))

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
    (mapcar (lambda (error) (flycheck-scala-sbt--convert-error-info checker error)) errors)))

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
    (if (eql buffer (current-buffer))
        (flycheck-error-new :buffer buffer :message message :checker checker :filename file :line row :column col :level level)
      (flycheck-error-new :buffer (current-buffer)
                          :message (concat "from " file ":" (prin1-to-string row) ":" (prin1-to-string col) ":\n  " (replace-regexp-in-string "\n" "\n  " message))
                          :checker checker
                          :filename (buffer-file-name)
                          :line 1
                          :column 1
                          :level level))))

(flycheck-define-generic-checker
    'scala-sbt
  "Check scala buffers using sbt-mode"
  :modes 'scala-mode
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
