;;;; utils.lisp --- utility functions

;;;; Code:

(in-package :commonmark)

(defmacro letf (bindings &body body)
  "Like LET, but accept arbitrary SETF-able places as the targets of BINDINGS.
The original value of each place will be saved and restored on exit."
  (let ((original-value-symbols
         (loop for binding in bindings
            collect (gensym))))
    `(let ,(loop for (place nil) in bindings
              for original-value in original-value-symbols
              collect (list original-value place))
       (unwind-protect
            (progn
              ,@(loop for (place value) in bindings
                   collect `(setf ,place ,value))
              ,@body)
         ,@(loop for (place nil) in bindings
              for original-value in original-value-symbols
              collect `(setf ,place ,original-value))))))

;;; Object creation

(defun make-text (text)
  "Convert TEXT to the format expected by text nodes.
If TEXT is a string, return an adjustable vector with TEXT as its only
element. If TEXT is another type of vector, ensure it is
adjustable (creating a copy if necessary)."
  (etypecase text
    (string (make-array 1
                        :initial-contents text
                        :element-type '(or string inline-node)
                        :fill-pointer 1
                        :adjustable t))
    (vector (if (and (adjustable-array-p text)
                     (array-has-fill-pointer-p text))
                text
                (make-array (length text)
                            :initial-contents text
                            :element-type '(or string inline-node)
                            :fill-pointer (length text)
                            :adjustable t)))))

;;; Input

(defun read-markdown-line (stream)
  "Read a line from STREAM.
Lines may be terminated by a carriage return, a newline, a carriage
return followed by a newline or end of file. Return nil if at end of
file."
  (when (peek-char nil stream nil)
    (with-output-to-string (line)
      (loop for char = (read-char stream nil)
         do (case char
              ((nil #\Newline) (return))
              (#\Return (when (eql #\Newline (peek-char nil stream nil))
                          ;; Ignore newline after carriage return
                          (read-char stream))
                        (return))
              (t (write-char char line)))))))

;;; Regex helpers

(ppcre:define-parse-tree-synonym blank-line
    (:sequence :start-anchor optional-whitespace :end-anchor))

(ppcre:define-parse-tree-synonym single-whitespace
    (:char-class #\Space #\Tab))

(ppcre:define-parse-tree-synonym optional-whitespace
    (:greedy-repetition 0 nil single-whitespace))

(ppcre:define-parse-tree-synonym required-whitespace
    (:greedy-repetition 1 nil single-whitespace))

(ppcre:define-parse-tree-synonym single-non-whitespace
    (:inverted-char-class #\Space #\Tab))

(ppcre:define-parse-tree-synonym tab-stop (:alternation
                                           (:sequence
                                            (:greedy-repetition 0 3 #\Space)
                                            #\Tab)
                                           (:greedy-repetition 4 4 #\Space)))

(defun line (&rest parse-trees)
  "Return a parse tree matching a full line containing PARSE-TREES."
  `(:sequence :start-anchor ,@parse-trees :end-anchor))

(defun line-register (&rest parse-trees)
  "Like LINE, but establish a register for the contents of the line."
  (line `(:register (:group ,@parse-trees))))

(defun indentation (min &optional (max min))
  "Return a parse tree matching between MIN and MAX (inclusive) spaces of indentation."
  (assert (not (minusp min)) (min))
  (assert (>= max min) (max))
  (let ((required-tabs (floor (/ min 4)))
        (required-spaces (rem min 4))
        (optional-tabs (floor (/ (- max min) 4)))
        (optional-spaces (rem (- max min) 4)))
    `(:sequence
      (:greedy-repetition ,required-tabs ,required-tabs tab-stop)
      (:greedy-repetition ,required-spaces ,required-spaces #\Space)
      (:greedy-repetition 0 ,optional-tabs tab-stop)
      (:greedy-repetition 0 ,optional-spaces #\Space))))

(defun setext-underline (char)
  "Return a parse tree matching a setext underline consisting of CHAR."
  (line
    (indentation 0 3)
    'optional-whitespace
    `(:greedy-repetition 1 nil ,char)
    'optional-whitespace))

(ppcre:define-parse-tree-synonym type-1-html-block-tag
    (:group
     :case-insensitive-p
     (:alternation "script" "pre" "style")))

(ppcre:define-parse-tree-synonym type-6-html-block-tag
    (:group
     :case-insensitive-p
     (:alternation "address" "article" "aside" "base" "basefont" "blockquote" "body" "caption" "center" "col" "colgroup" "dd" "details" "dialog" "dir" "div" "dl" "dt" "fieldset" "figcaption" "figure" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hr" "html" "iframe" "legend" "li" "link" "main" "menu" "menuitem" "nav" "noframes" "ol" "optgroup" "option" "p" "param" "section" "source" "summary" "table" "tbody" "td" "tfoot" "th" "thead" "title" "tr" "track" "ul")))

;; HTML tags

(ppcre:define-parse-tree-synonym open-tag
    (:group
     #\<
     tag-name
     (:greedy-repetition 0 nil attribute)
     optional-whitespace
     (:greedy-repetition 0 1 #\/)
     #\>))

(ppcre:define-parse-tree-synonym closing-tag
    (:group "</" tag-name optional-whitespace #\>))

(ppcre:define-parse-tree-synonym tag-name
    (:group
     (:char-class (:range #\A #\Z) (:range #\a #\z))
     (:greedy-repetition 0 nil
                         (:char-class (:range #\A #\Z)
                                      (:range #\a #\z)
                                      (:range #\0 #\9)
                                      #\-))))

(ppcre:define-parse-tree-synonym attribute
    (:group
     required-whitespace
     attribute-name
     (:greedy-repetition 0 1 (:group
                              optional-whitespace
                              #\=
                              optional-whitespace
                              attribute-value))))

(ppcre:define-parse-tree-synonym attribute-name
    (:group
     (:char-class (:range #\A #\Z) (:range #\a #\z) #\_ #\:)
     (:greedy-repetition 0 nil
                         (:char-class (:range #\A #\Z)
                                      (:range #\a #\z)
                                      (:range #\0 #\9)
                                      #\_ #\. #\: #\-))))

(ppcre:define-parse-tree-synonym attribute-value
    (:alternation unquoted-attribute-value
                  single-quoted-attribute-value
                  double-quoted-attribute-value))

(ppcre:define-parse-tree-synonym unquoted-attribute-value
    (:greedy-repetition 1 nil
                        (:inverted-char-class #\Space #\Tab #\"
                                              #\' #\= #\< #\> #\`)))

(ppcre:define-parse-tree-synonym single-quoted-attribute-value
    (:group
     #\'
     (:greedy-repetition 0 nil (:inverted-char-class #\'))
     #\'))

(ppcre:define-parse-tree-synonym double-quoted-attribute-value
    (:group
     #\"
     (:greedy-repetition 0 nil (:inverted-char-class #\"))
     #\"))

;;; Text manipulation

(defparameter *blank-line-scanner*
  (ppcre:create-scanner 'blank-line)
  "A scanner matching blank lines.")

(defparameter *whitespace-chars*
  (vector #\Space #\Tab #\Newline (code-char #xB) (code-char #xC) #\Return)
  "A vector containing all the characters considered to be whitespace.")

(defun blankp (line)
  "Return non-nil if LINE is blank."
  (ppcre:scan *blank-line-scanner* line))

(defun add-raw-text (new-text text)
  "Add NEW-TEXT to the end of TEXT.
TEXT is a vector of raw text strings and inline nodes."
  ;; Ensure we have a text string to append to
  (when (or (emptyp text) (not (stringp (last-elt text))))
    (vector-push-extend "" text))
  (setf (last-elt text)
        (concatenate 'string (last-elt text) new-text)))

(defun delete-whitespace (text)
  "Trim leading and trailing whitespace from TEXT.
TEXT is a vector of raw text strings and inline nodes."
  (delete-surrounding-characters text *whitespace-chars*))

(defun delete-surrounding-characters (text characters)
  "Trim leading and trailing characters in CHARACTERS from TEXT.
TEXT is a vector of raw text strings and inline nodes."
  ;; TODO: for now, I don't bother handling the case that the text
  ;; starts or ends with an inline node
  (unless (zerop (length text))
    (let ((start (first-elt text))
          (end (last-elt text)))
      (when (stringp start)
        (setf (first-elt text)
              (string-left-trim characters start)))
      (when (stringp end)
        (setf (last-elt text)
              (string-right-trim characters end))))))

(defun delete-surrounding-blank-lines (text)
  "Trim leading and trailing blank lines in TEXT.
TEXT is a vector of raw text strings and inline nodes."
  (unless (zerop (length text))
    (let ((start (first-elt text))
          (end (last-elt text)))
      (when (stringp start)
        (setf (first-elt text) (remove-leading-blank-lines start)))
      (when (stringp end)
        (setf (last-elt text) (remove-trailing-blank-lines start))))))

(defun delete-surrounding-empty-lines (text)
  "Trim leading and trailing empty lines in TEXT.
TEXT is a vector of raw text strings and inline nodes."
  (delete-surrounding-characters text #(#\Newline)))

(defun remove-leading-blank-lines (text)
  "Remove leading blank lines from TEXT, returning the result."
  (let ((start 0))
    (loop for i from 0 below (length text)
       for char across text
       when (eql #\Newline char)
       do (setf start (1+ i))
       unless (or (eql #\Space char) (eql #\Tab char))
       do (return))
    (subseq text start)))

(defun remove-trailing-blank-lines (text)
  "Remove trailing blank lines from TEXT, returning the result."
  (let ((end (length text)))
    (loop for i from (1- (length text)) downto 0
       when (eql #\Newline (aref text i))
       do (setf end i)
       unless (or (eql #\Space (aref text i)) (eql #\Tab (aref text i)))
       do (return))
    (subseq text 0 end)))

(defun strip-indentation (line &optional spaces)
  "Strip up to SPACES of indentation from LINE or all leading indentation if SPACES is nil.
Return LINE with the indentation removed and the number of spaces that
were actually removed."
  (let* ((spaces-stripped 0)
         (stripped
          (with-output-to-string (stripped)
            (let* ((spaces-remaining spaces)
                   (start
                    (loop for i from 0
                       when (>= i (length line)) return (length line)
                       unless (or (null spaces-remaining)
                                  (plusp spaces-remaining))
                       return i
                       if (eql (aref line i) #\Space)
                       do (when spaces-remaining (decf spaces-remaining))
                         (incf spaces-stripped)
                       else if (eql (aref line i) #\Tab)
                       do (let* ((next-stop (next-tab-stop spaces-stripped))
                                 (stop-diff (- next-stop spaces-stripped)))
                            (when spaces-remaining
                              (decf spaces-remaining stop-diff))
                            (incf spaces-stripped stop-diff))
                       else
                       return i)))
              ;; Fill in extra indentation from expanded tabs
              (when spaces-remaining
                (loop repeat (- spaces-remaining)
                   do (write-char #\Space stripped)
                     (decf spaces-stripped)))
              ;; TODO: there may be a more efficient way to do this
              (loop for i from start below (length line)
                 do (write-char (aref line i) stripped))))))
    (values stripped spaces-stripped)))

(defun next-tab-stop (position)
  "Return the position of the next tab stop (4 spaces wide) starting at POSITION."
  (* 4 (floor (/ (+ position 4) 4))))

;;;; utils.lisp ends here
