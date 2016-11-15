;;; flycheck-scala-sbt.el --- sbt-mode checker for Scala

;; Version: 0.1
;; Url: https://www.github.com/rjmac/flycheck-scala-sbt
;; Package-requires: ((emacs "25.1") (sbt-mode "20161110.1221"))

;;; Commentary:

;; Provides an sbt-mode based checker for scala files.  Call
;; flycheck-scala-sbt-init from your scala-mode-hook to set it up.

;;; Code:

(require 'flycheck)
(require 'sbt-mode)

(defun flycheck-scala-sbt--working-directory (checker)
  (sbt:find-root))

(defun flycheck-scala-sbt--find-sbt ()
  (let ((current-frame (selected-frame)))
    (and current-frame (get-buffer-window (sbt:run-sbt) current-frame))))

(defun flycheck-scala-sbt--hide-sbt ()
  (let ((w (flycheck-scala-sbt--find-sbt)))
    (when w (delete-window w))))

(defun flycheck-scala-sbt--wait-for-prompt-then-call (sbt-buffer f)
  (lexical-let ((current-buffer (current-buffer))
                (sbt-buffer sbt-buffer)
                (f f))
    (let ((end (with-current-buffer sbt-buffer
                 (buffer-substring-no-properties (max (point-min) (- (point-max) 2)) (point-max)))))
      (if (string= end "> ")
          (funcall f)
        (run-at-time 0.1 nil (lambda () (with-current-buffer current-buffer (flycheck-scala-sbt--wait-for-prompt-then-call sbt-buffer f))))))))

;; Waits for the sbt session in SBT-BUFFER to end with a prompt, then
;; evaluates the block with the current buffer set to what it was when
;; the macro was first evaluated.
(defmacro flycheck-scala-sbt--wait-for-prompt-then (sbt-buffer &rest body)
  `(flycheck-scala-sbt--wait-for-prompt-then-call ,sbt-buffer (lambda () ,@body)))

(defun flycheck-scala-sbt--start (checker status-callback)
  (lexical-let ((checker checker)
                (status-callback status-callback)
                (sbt-buffer (save-window-excursion (sbt:run-sbt))))
    (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer
      (save-window-excursion
        (let ((sbt:save-some-buffers nil))
          (sbt:command "compile" nil)))
      (flycheck-scala-sbt--wait-for-prompt-then sbt-buffer
        (condition-case err
            (funcall status-callback 'finished (flycheck-scala-sbt--errors-list sbt-buffer checker))
          (error
           (funcall status-callback 'errored (error-message-string err))))))))

(defun flycheck-scala-sbt--errors-list (buffer checker)
  (let ((errors
         (with-current-buffer buffer
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
               (while (re-search-forward "^\\[\\(error\\|warn\\)][[:space:]]+^$" (point-max) t)
                 (setf acc (cons (flycheck-scala-sbt--extract-error-info) acc)))
               (reverse acc))))))
    (cl-mapcan (lambda (error) (flycheck-scala-sbt--convert-error-info checker error)) errors)))

(defun flycheck-scala-sbt--extract-error-info ()
  (save-excursion
    (let ((line-regexp "^\\[\\(error\\|warn\\)][[:space:]]+\\(.*?\\):\\([0-9]+\\):[[:space:]]+\\(.*\\)$")
          (else-regexp "^\\[\\(error\\|warn\\)][[:space:]]\\(.*\\)$")
          (subsequent-lines '())
          (current-line nil)
          (error-column (current-column)))
      (forward-line -2) ;; skip caret and actual erroring line
      (while (not (string-match line-regexp (setq current-line
                                                  (buffer-substring-no-properties (progn (beginning-of-line) (point))
                                                                                  (progn (end-of-line) (point))))))
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

(defun flycheck-scala-sbt--convert-error-info (checker error-stuff)
  (let* ((file (nth 0 error-stuff))
         (row (nth 1 error-stuff))
         (col (nth 2 error-stuff))
         (level (nth 3 error-stuff))
         (message (nth 4 error-stuff))
         (buffer (find-buffer-visiting file)))
    ;; it sure seems like you should be able to return errors in other
    ;; buffers, but it seems flycheck tries to highlight all errors in
    ;; the CURRENT buffer no matter what buffer they were sourced from.
    (when (eql buffer (current-buffer))
      (list (flycheck-error-new :buffer buffer :message message :checker checker :filename file :line row :column col :level level)))))

(flycheck-define-generic-checker
    'scala-sbt
  "Check SBT processes"
  :modes 'scala-mode
  :predicate 'sbt:find-root
  :start 'flycheck-scala-sbt--start
  :working-directory 'flycheck-scala-sbt--working-directory)

(pushnew 'scala-sbt flycheck-checkers)

(flycheck-add-next-checker 'scala-sbt 'scala)

(defun flycheck-scala-sbt-init ()
  "Call this from your scala-mode-hook to set up flycheck-sbt."
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-mode t))

(provide 'flycheck-scala-sbt)

;;; flycheck-scala-sbt.el ends here
