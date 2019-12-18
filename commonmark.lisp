;;;; commonmark.lisp --- core definitions

;;;; Code:

(in-package :commonmark)

;;; Document structure (nodes) and context

(defclass context ()
  ((block-starts
    :initarg :block-starts
    :initform ()
    :type list
    :accessor block-starts
    :documentation "A list of patterns identifying blocks by their starting lines.

Each element of the list is a list of the form (SCANNER FUNCTION
CAN-INTERRUPT).

When MAKE-BLOCK is invoked, the list is traversed in order. If SCANNER
matches, FUNCTION is called with the context as its first argument
followed by the capturing groups of the regex. The function should
return the block corresponding to the matched line. If no applicable
scanners match a line, MAKE-BLOCK will return NIL.

MAKE-BLOCK is also invoked for each line being added to a paragraph,
but with different behavior: only block starts where CAN-INTERRUPT is
non-nil will be considered."))
  (:documentation "Context for parsing a document."))

(defun make-context (block-starts)
  "Return a new CONTEXT using BLOCK-STARTS.
BLOCK-STARTS is a list as described in the documentation of CONTEXT,
but the keys will be converted to efficient scanners."
  (let ((block-starts
         (loop for (regex function can-interrupt) in block-starts
            collect (list (ppcre:create-scanner regex)
                          function
                          can-interrupt))))
    (make-instance 'context :block-starts block-starts)))

(defun make-standard-context ()
  "Return a new CONTEXT using the standard CommonMark block starts."
  (make-context
   `(("^(?: {0,3}\\t| {4})(.*[^ \\t].*)$"
      ,(lambda (context text)
         (declare (ignore context))
         (make-instance 'indented-code-block :text text))
      nil)
     ("^(.*[^ \\t].*)$"
      ,(lambda (context text)
         (declare (ignore context))
         (make-instance 'raw-paragraph :text (strip-indentation text)))
      nil))))

(defgeneric make-block (line context &optional in-paragraph)
  (:documentation "Make a new block from LINE using CONTEXT. If LINE
represents several nested blocks (for example, a paragraph within a
block quote), return the top-level block containing the children.
Return nil if no block can be created (e.g. if LINE is empty)."))

(defmethod make-block (line (context context) &optional in-paragraph)
  (loop for (scanner function can-interrupt) in (block-starts context)
     when (or (not in-paragraph) can-interrupt)
     do (multiple-value-bind (match registers)
            (ppcre:scan-to-strings scanner line)
          (when match
            (let ((reg-list (map 'list #'identity registers)))
              (return (apply function context reg-list)))))))

(defclass node ()
  ()
  (:documentation "A node in the AST of a Markdown document."))

(defclass block-node (node)
  ((closed
    :initarg :closed
    :initform nil
    :type boolean
    :accessor closedp
    :documentation "Whether the block is closed."))
  (:documentation "A structural element of a document."))

(defclass text-block-node (block-node)
  ((text
    :initarg :text
    :initform ""
    :type string
    :accessor text
    :documentation "The text of the paragraph."))
  (:documentation "A block containing only textual content."))

(defmethod initialize-instance :after ((block text-block-node) &key)
  "Ensure the text block's text is an adjustable array."
  (with-slots (text) block
    (setf text (make-array (length text)
                           :element-type 'character
                           :adjustable t
                           :fill-pointer (length text)
                           :initial-contents text))))

(defmethod print-object ((object text-block-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((text text) (closed closedp)) object
      (format stream "~s :CLOSED ~s" text closed))))

(defclass raw-paragraph (text-block-node)
  ()
  (:documentation "A raw paragraph containing only un-processed text."))

(defclass code-block (text-block-node)
  ((info-string
    :initarg :info-string
    :initform nil
    :type string
    :accessor info-string
    :documentation "The code block's info string, usually specifying the language of the block."))
  (:documentation "A block of literal code."))

(defclass indented-code-block (code-block)
  ()
  (:documentation "An indented code block (block of code where each line is preceded by a tab or four spaces)."))

(defclass container-block-node (block-node)
  ((children
    :initarg :children
    :initform #()
    :type (vector block-node)
    :accessor children
    :documentation "The children of the block."))
  (:documentation "A block node that can contain other blocks as children."))

(defmethod initialize-instance :after ((node container-block-node) &key)
  "Ensure the container's children array is adjustable."
  (with-slots (children) node
    (setf children (make-array (length children)
                               :element-type 'block-node
                               :adjustable t
                               :fill-pointer (length children)
                               :initial-contents children))))

(defmethod print-object ((object container-block-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((children children) (closed closedp)) object
      (format stream "~s :CLOSED ~s" children closed))))

(defclass document (container-block-node)
  ()
  (:documentation "The top-level node in the document AST."))

;;; Line handlers

(defgeneric accept-line (line block context)
  (:documentation "Ask BLOCK to accept LINE as additional contents.
Return two values, ACTION and OBJECT. ACTION is a request for the
parent of this block to perform some action with OBJECT. Valid actions
are as follows:

nil - do nothing with OBJECT.

:ACCEPT-CHILD - accept OBJECT as a new child node, if possible."))

(defgeneric close-block (block)
  (:documentation "Close BLOCK, indicating that it can no longer accept additional contents."))

(defun handle-new-block (line context)
  "Return the values expected for ACCEPT-LINE given that LINE needs to be made into a new block.
In other words, return (VALUES :ACCEPT-CHILD (MAKE-BLOCK LINE
CONTEXT)) if MAKE-BLOCK returns non-nil or (VALUES NIL NIL)
otherwise."
  (let ((new-block (make-block line context)))
    (values (when new-block :accept-child) new-block)))

(defmethod accept-line :around (line (block block-node) context)
  "Prevent BLOCK from accepting new lines if it is closed."
  (if (not (closedp block))
      (call-next-method)
      (handle-new-block line context)))

(defmethod close-block ((block block-node))
  (setf (closedp block) t))

(defmethod accept-line (line (block text-block-node) context)
  (unless (zerop (length (text block)))
    (vector-push-extend #\Newline (text block)))
  ;; TODO: there's probably a more efficient way to do this
  (loop for char across line
     do (vector-push-extend char (text block)))
  (values nil nil))

(defmethod accept-line (line (block raw-paragraph) context)
  (when (blankp line)
    (close-block block)
    (return-from accept-line (values nil nil)))
  ;; Check for blocks that can interrupt paragraphs
  (let ((interrupting-block (make-block line context t)))
    (when interrupting-block
      (close-block block)
      (return-from accept-line (values :accept-child interrupting-block))))
  ;; Otherwise, we have an ordinary text line
  (call-next-method (strip-indentation line) block context))

(defmethod accept-line (line (block indented-code-block) context)
  (multiple-value-bind (stripped-line indentation)
      (strip-indentation line 4)
    (if (or (>= indentation 4) (blankp line))
        (call-next-method stripped-line block context)
        (progn
          (close-block block)
          (handle-new-block line context)))))

(defmethod close-block :after ((block indented-code-block))
  "Remove leading and trailing blank lines from BLOCK."
  ;; For some reason, the Commonmark reference implementation adds a
  ;; newline to the end of every code block:
  ;; https://github.com/commonmark/commonmark-spec/issues/501
  (setf (text block) (concatenate 'string
                                  (remove-surrounding-blank-lines (text block))
                                  "
")))

(defmethod accept-line (line (block container-block-node) context)
  "Ask the last child of BLOCK to accept LINE or create a new child from LINE if there are none."
  (if (zerop (length (children block)))
      (let ((new-block (make-block line context)))
        (when new-block
          (vector-push-extend new-block (children block))))
      (let ((last-child (aref (children block) (1- (length (children block))))))
        (multiple-value-bind (action object)
            (accept-line line last-child context)
          (ecase action
            (:accept-child (vector-push-extend object (children block)))
            ((nil))))))
  (values nil nil))

(defmethod close-block :after ((block container-block-node))
  "Close all unclosed children of BLOCK."
  (loop for child across (children block)
     unless (closedp child)
     do (close-block child)))

;;; Helper functions

(defparameter *blank-line-scanner*
  (ppcre:create-scanner "^[ \\t]*$")
  "A scanner matching blank lines.")

(defun whitespacep (char)
  "Return non-nil if CHAR is a whitespace character."
  (case char
    ((#\Newline #\Space #\Tab) t)
    (t nil)))

(defun blankp (line)
  "Return non-nil if LINE is blank."
  (ppcre:scan *blank-line-scanner* line))

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

(defun remove-surrounding-blank-lines (text)
  "Remove leading and trailing blank lines from TEXT, returning the result."
  (let ((start 0)
        (end (length text)))
    (loop for i from 0 below (length text)
       for char across text
       when (eql #\Newline char)
       do (setf start (1+ i))
       unless (whitespacep char)
       do (return))
    (loop for i from (1- (length text)) downto 0
       when (eql #\Newline (aref text i))
       do (setf end i)
       unless (whitespacep (aref text i))
       do (return))
    (when (> start end)
      ;; No non-whitespace characters in string
      (setf start 0
            end 0))
    (subseq text start end)))

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
                       do (let* ((next-stop (next-tab-stop spaces-stripped 4))
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

(defun next-tab-stop (position tab-width)
  "Return the position of the next tab stop of width TAB-WIDTH starting at POSITION."
  (* tab-width (floor (/ (+ position tab-width) tab-width))))

;;; Main entry points

(defgeneric parse-block-structure (input &optional context)
  (:documentation "Parse the block structure of the document read from INPUT.
This does not do any processing of inlinep elements; it is only the
first pass of the full parsing process."))

(defmethod parse-block-structure ((input stream)
                                  &optional (context (make-standard-context)))
  (loop with document = (make-instance 'document)
     for line = (read-markdown-line input)
     while line
     do (accept-line line document context)
     finally (close-block document)
       (return document)))

(defmethod parse-block-structure ((input string)
                                  &optional (context (make-standard-context)))
  (with-input-from-string (stream input)
    (parse-block-structure stream context)))

;;;; commonmark.lisp ends here
