;; FILE: util.el
;; DESC: Utility unctions of a general nature.

(defun ni-util-display-current-buffer-path()
  "Prints path of current buffer in a message."
  (interactive)
  (message (buffer-file-name)))

;; Test
;; (ni-print-file-path)

(defmacro with-file (file-path &rest body)
  "Executes BODY in a buffer visiting the file at FILE-PATH.  If
file at FILE-PATH is already open, use that buffer temporarily.
If not visiting the file already, the file will be visited
temporarily until the end of this macro.  The result of its last
embedded expression is returned."
  `(save-excursion
     (let ((file-buffer (find-buffer-visiting ,file-path))
           (buffer-created nil)
           (result-of-last-body-expr nil))
       (if file-buffer
           (set-buffer file-buffer)
         (find-file ,file-path)
         (setq buffer-created t))
       ,@(butlast body)
       (setq result-of-last-body-expr ,@(last body))
       (when buffer-created
         (kill-buffer file-buffer))
       result-of-last-body-expr)))

;; Test (print first 10 lines of .emacs to Messages)
;; (with-file "~/.emacs" (message "%s" (buffer-substring (point-min) (point-max))))

(defun chomp (string)
  "Like perl's chomp."
  (replace-regexp-in-string "[\r\n]+$" "" string))
;; Test
;; (equal (chomp "this is a test") "this is a test")

(defun ni-util-prompt-user-for-word(prompt)
  "Prompts for input from the user, using the word at point as default value."
  (let ((default (thing-at-point 'word)))
    (read-string (concat prompt " (" default "): ") nil nil default)))

;; Test
;; (ni-util-prompt-user-for-word "test")

(defun ni-util-string-match-regexp (string regex &optional start end &rest match-nums)
  "Search STRING for REGEX.  START and END are optional bounds
for the search.  Any further args should be numbers of match
strings requested.  If no MATCH-NUMS are given, this function
just returns t or nil.  If a single match number is given, this
function returns a single string.  If multiple match numbers are
given, this function returns a list of match strings."
  (with-temp-buffer
    ;; Insert string into temp buffer
    (insert string)
    (goto-char (or start (point-min)))
    (if (re-search-forward regex
                           (if end
                               (- end start)
                             nil)
                           t)
        (cond
         ;; No specific matches wanted
         ((null match-nums) t)
         ;; Only 1 match requested
         ((null (cdr match-nums)) (match-string (car match-nums)))
         ;; Multiple matches requested
         (t (mapcar '(lambda (num)
                       (match-string num))
                    match-nums)))
      nil)))

;; Tests
;; (equal (ni-util-string-match-regexp "ababcdabqd" "ab") t)
;; (equal (ni-util-string-match-regexp "an unlucky number is the number 13" "\\([0-9][0-9]\\)" nil nil 1) "13")
;; (null (ni-util-string-match-regexp "13 is an unlucky number" "\\([0-9][0-9]\\)" 3 nil 1))
;; (equal (ni-util-string-match-regexp "you can find multiple matches here" "\\w+ *\\(\\w+\\) *\\(\\w+\\)" nil nil 1 2) '("can" "find"))

(defun ni-util-list-with-string-appended (inputList inputString)
  "Return a list with INPUTSTRING appended to the end of each item in INPUTLIST."
  (mapcar '(lambda (x) (concat x inputString)) inputList))

;; Tests
;; (equal (ni-util-list-with-string-appended '("p:/sa/ss/rpc/export/4.0/4.0.0d3" "p:/sa/ss/orb/export/1.9/1.9.0a0") "/includes") '("p:/sa/ss/rpc/export/4.0/4.0.0d3/includes" "p:/sa/ss/orb/export/1.9/1.9.0a0/includes"))

(defun ni-util-join (input-list separator-string)
  "Like perl's join."
  (reduce '(lambda (x y) (concat x separator-string y)) input-list))

;; Tests
;; (equal (ni-util-join '("one" "two" "three") ", ") "one, two, three")
;; (equal (ni-util-join '("one" "two" "three") " ") "one two three")

(defun ni-util-toggle-show-trailing-ws()
  (interactive)
  (setq show-trailing-whitespace (if show-trailing-whitespace nil t))
  (redraw-frame))

;; Tests
;; (ni-util-toggle-show-trailing-ws)

(defun ni-util-directory-files-recursive (directory)
  "List files in DIRECTORY and in its sub-directories."
  (let* ((files-list '())
         (current-directory-list (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           (file-regular-p f)
           (file-readable-p f))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1))))
          ;; recurse only if necessary
          (setq files-list (append files-list (ni-util-directory-files-recursive f))))
         (t)))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

(defun ni-util-set-file-writable ()
  "Makes a file writable."
  (interactive)
  (set-file-modes buffer-file-name 0666)
  (revert-buffer :ignore-auto :noconfirm))

(provide 'ni-emacs/util)
