(provide 'org-source-block-helpers)

(defun osbh:insert-org-src-code-block ()
  "Insert an org source code block. Prompt the user for the org babel language and properties."
  (interactive)
  (org-insert-structure-template "src")
  (forward-line)
  (re-search-backward "#\\+BEGIN_SRC\\|#\\+begin_src")
  (call-interactively 'osbh:add-additional-org-src-block-properties))

(defun osbh:enclose-region-in-src-code-block (beg end)
  "Enclose the highlighted region in an org source code block.
Prompt the user for the org babel language and properties."
  (interactive "r")
  (kill-region beg end)
  (org-insert-structure-template "src")
  (forward-line)
  (yank)
  (re-search-backward "#\\+BEGIN_SRC\\|#\\+begin_src")
  ;; (move-beginning-of-line nil)
  (call-interactively 'osbh:add-additional-org-src-block-properties))

(defun osbh:add-additional-org-src-block-properties ()
  "When point is at beginning of org source code block, interact
with user to generate org source block properties and their values."
  (interactive)
  (when (not (looking-at "#\\+BEGIN_SRC\\|#\\+begin_src"))
    (error "Point not at beginning of org source code block"))
  (move-end-of-line nil)
  (cl-labels ((completing-org-block-property-read
               (property-key property-string)
               "Given a property-key, e.g., 'tangle, prompt the
user for their choice from all available defined values for that property."
               (completing-read (format  "Value for %s property (note: if available, choose :any if you want/need to supply value yourself): "  (capitalize property-string))
                                (mapcar '(lambda (symb) (format "%s" symb))
                                        (flatten (cdr (eval `(assq ,property-key org-babel-common-header-args-w-values)))))
                                nil t))
              (prompt-for-org-src-block-property
               ()
               "Prompt user for property they'd like to add and then the value for that property."
               (let* ((property-string (completing-read "Choose the property you'd like to add a value for: "
                                                    (mapcar #'car org-babel-common-header-args-w-values) nil t))
                      (property-key (read (concat "'" property-string)))
                      (property-value (completing-org-block-property-read property-key property-string))
                      (property-value (if (equal property-value ":any")
                                          (read-string (format  "Set %s to: " property-string))
                                        property-value)))
                 (insert (format ":%s %s " property-string property-value)))))
             (let ((lang (completing-read "Choose the language: "
                                          (mapcar '(lambda (pair) (format "%s" (car pair))) org-babel-load-languages))))
               (insert (format " %s " lang))
               (when (and (and (or (equal lang "C")(equal lang "C++")))
                          (y-or-n-p "Add includes property? "))
                 (insert (format ":includes %s " "'(<iostream>)")))
               (when (and (and (or (equal lang "C")(equal lang "C++")))
                          (y-or-n-p "Add main property? "))
                 (insert (format ":main %s "
                                 (completing-read
                                  ":main value (choosing no means you can supply a main, yes is the default): "
                                  '("yes" "no")))))
               (while (y-or-n-p "Add another property? ")
                 (prompt-for-org-src-block-property)))))
