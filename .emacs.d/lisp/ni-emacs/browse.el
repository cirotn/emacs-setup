;; FILE: browse.el
;; DESC: Functions related to browser.

(defun ni-browse-google (searchTerm)
  "Googles a search term."
  (interactive "MSearch Term: ")
  (browse-url (concat
               "http://www.google.com/search?hl=en&rls=ig&q="
               (replace-regexp-in-string " " "+" searchTerm)
               "&btnG=Search")))

(defun ni-browse-cplusplus ()
  "Looks up a given function/keyword on cplusplus.com"
  (interactive)
  (browse-url (concat "http://www.cplusplus.com/"
                      (ni-browse--create-query))))

(defun ni-browse-msdn()
  "Looks up a given function/keyword on msdn"
  (interactive)
  (browse-url (concat "http://social.msdn.microsoft.com/Search/en-us/?Query="
                      (ni-browse--create-query))))

(defun ni-browse--create-query ()
  "Infrastructure function that other documentation commands use."
  (let ((wordToLookup (read-string (concat "Keyword to lookup"
                                           (if (not (string-equal (thing-at-point 'word)
                                                                  ""))
                                               (concat " (" (thing-at-point 'word) "): ")
                                             ": ")))))
    (if (string-equal wordToLookup
                      "")
        (setq wordToLookup (thing-at-point 'word)))
    wordToLookup))

(provide 'ni-emacs/browse)
