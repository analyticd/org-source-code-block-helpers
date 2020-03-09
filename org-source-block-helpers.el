;;; org-source-block-helpers.el --- -*- lexical-binding: t; -*-

;;; Author: github.com/analyticd

;;; Commentary:

;; Various helpers to efficiently work with org-mode source
;; blocks.
;;
;; User API:
;; M-x osbh:insert-org-src-code-block to create a brand new source block.
;; M-x osbh:enclose-region-in-src-code-block to enclose a region in a source
;; code block.
;; M-x osbh:add-additional-org-src-block-properties to add source code block
;; babel language if missing and other properties.
;;
;; NOTE Each command above requires the point (cursor) to be anywhere on the
;; same line as the #+BEGIN_SRC or #+begin_src directive when the command is
;; executed.
;;
;; See README for additional details.

;;; Code:

;;;;###autoload
(defun osbh:insert-org-src-code-block ()
  "Insert an org source code block. Prompt the user for the org
babel language and other properties."
  (interactive)
  (org-insert-structure-template "src")
  (forward-line)
  (re-search-backward "#\\+BEGIN_SRC\\|#\\+begin_src")
  (call-interactively 'osbh:add-additional-org-src-block-properties))

;;;;###autoload
(defun osbh:enclose-region-in-src-code-block ()
  "Enclose the highlighted region in an org source code block.
Prompt the user for the org babel language and other properties."
  (interactive)
  (org-insert-structure-template "src")
  (re-search-backward "#\\+BEGIN_SRC\\|#\\+begin_src")
  (call-interactively 'osbh:add-additional-org-src-block-properties))

;;;;###autoload
(defun osbh:add-additional-org-src-block-properties ()
  "Prompt the user to generate Org babel language when missing and
additional org source block properties and their values."
  (interactive)
  (osbh:move-point-to-start-of-src-block)
  (let ((lang (osbh:get-or-insert-org-babel-language)))
    (move-end-of-line nil)
    (while (and (and (or (equal lang "C")(equal lang "C++")))
                (y-or-n-p "Add C or C++ specific property? "))
      (let ((c-property
             (completing-read "Choose the C or C++ property you'd like to add a value for: "
                              (mapcar #'car org-babel-header-args:C))))
        (insert (format ":%s %s "
                        c-property
                        (read-string (format "Value for %s property: " c-property))))))
    (while (y-or-n-p "Add another property? ")
      (prompt-for-org-src-block-property))))

(defun osbh:get-or-insert-org-babel-language ()
  "If the org source block has a babel language defined already, then return
that, otherwise prompt the user for the babel language to use, insert that
chosen value, and then return it."
  (osbh:move-point-to-start-of-src-block)
  (let ((lang (osbh:org-src-block-lang-or-nil)))
    (when (null lang)
      (re-search-forward "#\\+BEGIN_SRC\\|#\\+begin_src")
      (insert " ")
      (setf lang
            (completing-read
             "Choose the language: "
             (mapcar #'(lambda (pair) (format "%s" (car pair)))
                     org-babel-load-languages)))
      (insert (format "%s " (osbh:transform-lang-name lang))))
    lang))

(defun osbh:move-point-to-start-of-src-block ()
  "Move point to first character of `#+begin_src' or `#+BEGIN_SRC' directive
on the current line. Throw an error if this is not the end result.

Necessary precondition: point on same line as `#+begin_src' or `#+BEGIN_SRC'
directive."
  (move-beginning-of-line nil)
  (re-search-forward
   "#\\+BEGIN_SRC\\|#\\+begin_src"
   (line-end-position) t)
  (re-search-backward
   "#\\+BEGIN_SRC\\|#\\+begin_src"
   (line-beginning-position) t)
  (when (not (looking-at "#\\+BEGIN_SRC\\|#\\+begin_src"))
    (error "Point not at on same line as start of org source code block.")))

(defun osbh:org-src-block-lang-or-nil ()
  "If the org src block on current line where point is located exists then return it, otherwise return nil."
  (save-excursion
    (re-search-forward "#\\+BEGIN_SRC +[^ ]\\|#\\+begin_src +[^ ]" (line-end-position) t)
    (forward-char -1)
    (if (and (looking-at "[^ ]")
             (not (looking-at ":"))
             (not (looking-at "\n")))
        (progn
          (setf lang (substring-no-properties (thing-at-point 'sexp)))
          lang)
      nil)))

(defun osbh:transform-lang-name (lang)
  "Return the `LANG' transformed to a custom value. This allows
to deal with situations like:

1. j-mode not loading when you go to an org src edit buffer since the
org-babel language provided was spelled capital J.
2. C++ not being offered as a value in org babel languages."
  (cond ((equal lang "J") (downcase lang))
        ((equal lang "C")
         (when (y-or-n-p "Do you actually want C++ instead of C? ")
           (setf lang "C++")))
        (t lang)))


(defun completing-org-block-property-read (property-key property-string)
  "Given a `property-key', e.g., :tangle, then prompt the user for their
choice from all available defined values for that property."
  (completing-read
   (format
    (concat "Value for %s property (note: if available, choose :any if you want"
            " to supply value yourself): ")
    (capitalize property-string))
   (mapcar #'(lambda (symb) (format "%s" symb))
           (flatten
            (cdr
             (eval
              `(assq ,property-key org-babel-common-header-args-w-values)))))
   nil t))

(defun prompt-for-org-src-block-property ()
  "Prompt user for property to add and then the value for that property."
  (let* ((property-string
          (completing-read
           "Choose the property you'd like to add a value for: "
           (mapcar #'car org-babel-common-header-args-w-values) nil t))
         (property-key (read (concat "'" property-string)))
         (property-value
          (completing-org-block-property-read property-key property-string))
         (property-value
          (if (equal property-value ":any")
              (read-string (format  "Set %s to: " property-string))
            property-value)))
    (insert (format " :%s %s " property-string property-value))))

(provide 'org-source-block-helpers)

;;; org-source-block-helpers.el ends here
