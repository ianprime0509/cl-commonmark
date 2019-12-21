;;;; commonmark.lisp --- core definitions

;;;; Code:

(in-package :commonmark)

;;; Parsing context

;;; The context object is used to provide contextual information and
;;; operations for implementations of ACCEPT-LINE. For example, the
;;; MAKE-BLOCK method allows any node to start a new block with a line
;;; without hardcoding assumptions about the block starts into each
;;; ACCEPT-LINE method.

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
non-nil will be considered.")
   (setext-underlines
    :initarg :setext-underlines
    :initform ()
    :type list
    :accessor setext-underlines
    :documentation "A list of patterns matching setext heading underlines.
Each element of the list is a list of the form (SCANNER LEVEL).

When a new line is added to a paragraph, if it is possible to
interpret the line as a setext heading underline (that is, if the line
is not a lazy continuation line), the list is traversed in order
similarly to the processing of BLOCK-STARTS. If SCANNER matches, the
line is interpreted as a setext heading underline for a heading of
level LEVEL.

This behavior is controlled by the PARSE-SETEXT-UNDERLINE function.")
   (lazy-continuation
    :initarg :lazy-continuation
    :initform nil
    :type boolean
    :accessor lazy-continuation-p
    :documentation "Whether we're currently looking for a lazy continuation line.
This affects whether certain block structures can be recognized in a
paragraph, such as a setext heading line."))
  (:documentation "Context for parsing a document."))

(defun make-context (&key block-starts setext-underlines)
  "Return a new CONTEXT using the provided components.
BLOCK-STARTS and SETEXT-UNDERLINES are lists as described in the
documentation of CONTEXT, but the keys will be converted to efficient
scanners."
  (let ((block-starts
         (loop for (regex function can-interrupt) in block-starts
            collect (list (ppcre:create-scanner regex)
                          function
                          can-interrupt)))
        (setext-underlines
         (loop for (regex level) in setext-underlines
            collect (list (ppcre:create-scanner regex) level))))
    (make-instance 'context
                   :block-starts block-starts
                   :setext-underlines setext-underlines)))

(defun make-standard-context ()
  "Return a new CONTEXT using the standard CommonMark block starts."
  (make-context
   :block-starts
   `(
     ;; Thematic breaks
     ("^ {0,3}([*_-])(?:[ \\t]*\\1[ \\t]*){2,}$"
      ,(lambda (context break-string)
         (declare (ignore context break-string))
         (make-instance 'thematic-break))
      t)
     ;; ATX headings
     ("^ {0,3}(#{1,6})[ \\t]+(.*?)(?:[ \\t]#*[ \\t]*)?$"
      ,(lambda (context opening text)
         (declare (ignore context))
         (make-instance 'heading
                        :level (length opening)
                        :text (vector text)))
      t)
     ;; Fenced code blocks
     ("^( {0,3})(`{3,})[ \\t]*([^`]*[^ \\t`])?[ \\t]*$"
      ,(lambda (context indent fence info-string)
         (declare (ignore context))
         (make-instance 'fenced-code-block
                        :info-string info-string
                        :opening-fence-indentation (length indent)
                        :opening-fence-length (length fence)
                        :tilde-fence nil))
      t)
     ("^( {0,3})(~{3,})[ \\t]*(.*[^ \\t])?[ \\t]*$"
      ,(lambda (context indent fence info-string)
         (declare (ignore context))
         (make-instance 'fenced-code-block
                        :info-string info-string
                        :opening-fence-indentation (length indent)
                        :opening-fence-length (length fence)
                        :tilde-fence t))
      t)
     ;; Indented code blocks
     ("^(?: {0,3}\\t| {4})(.*[^ \\t].*)$"
      ,(lambda (context text)
         (declare (ignore context))
         (make-instance 'indented-code-block
                        :text (vector text)))
      nil)
     ;; Paragraphs
     ("^[ \\t]*(.*[^ \\t].*)$"
      ,(lambda (context text)
         (declare (ignore context))
         (make-instance 'paragraph
                        :text (vector text)))
      nil))
   :setext-underlines
   '(("^ {0,3}[ \\t]*=+[ \\t]*" 1)
     ("^ {0,3}[ \\t]*-+[ \\t]*" 2))))

(defun make-block (line context &optional in-paragraph)
  "Make a new block from LINE using CONTEXT.
If LINE represents several nested blocks (for example, a paragraph
within a block quote), return the top-level block containing the
children. Return nil if no block can be created (e.g. if LINE is
empty)."
  (loop for (scanner function can-interrupt) in (block-starts context)
     when (or (not in-paragraph) can-interrupt)
     do (multiple-value-bind (match registers)
            (ppcre:scan-to-strings scanner line)
          (when match
            (let ((reg-list (map 'list #'identity registers)))
              (return (apply function context reg-list)))))))

(defun parse-setext-underline (line context)
  "Attempt to parse LINE as a setext heading underline using CONTEXT.
Return the heading level (between 1 and 6, inclusive) or nil if LINE
is not a setext heading underline."
  (loop for (scanner level) in (setext-underlines context)
     when (ppcre:scan scanner line)
     return level))

;;; Generic node methods

(defgeneric accept-line (line block context)
  (:documentation "Ask BLOCK to accept LINE as additional contents.
Return two values, ACTION and OBJECT. ACTION is a request for the
parent of this block to perform some action with OBJECT. Valid actions
are as follopws:

nil - do nothing with OBJECT.

:ACCEPT - accept OBJECT as a new child node, if possible.

:REPLACE-SELF - replace this block with OBJECT.

:DELETE-SELF - do nothing with OBJECT and delete this block."))

(defun handle-new-block (line context)
  "Return the values expected for ACCEPT-LINE given that LINE needs to be made into a new block.
In other words, return (VALUES :ACCEPT (MAKE-BLOCK LINE
CONTEXT)) if MAKE-BLOCK returns non-nil or (VALUES NIL NIL) otherwise.
This is a helper function for ACCEPT-LINE implementations."
  (let ((new-block (make-block line context)))
    (values (when new-block :accept) new-block)))

(defgeneric close-block (block context)
  (:documentation "Close BLOCK, indicating that it can no longer accept additional contents.
Like ACCEPT-LINE, CONTEXT is supplied so that nodes can perform
contextually sensitive operations (such as adding link reference
definitions) when closing themselves."))

;;; Node definitions

(defclass node ()
  ()
  (:documentation "A node in the AST of a Markdown document."))

;;; Block nodes

(defclass block-node (node)
  ((closed
    :initarg :closed
    :initform nil
    :type boolean
    :reader closedp
    :documentation "Whether the block is closed.
To close a block, use CLOSE-BLOCK rather than setting this slot
directly."))
  (:documentation "A structural element of a document."))

(defmethod print-object ((block block-node) stream)
  (print-unreadable-object (block stream :type t)
    (format stream ":CLOSED ~s" (closedp block))))

(defmethod accept-line :around (line (block block-node) context)
  "Prevent BLOCK from accepting new lines if it is closed."
  (if (not (closedp block))
      (call-next-method)
      (handle-new-block line context)))

(defmethod close-block ((block block-node) context)
  (declare (ignore context))
  (setf (slot-value block 'closed) t))

(defclass atomic-block-node (block-node)
  ()
  (:documentation "A block node that does not accept any additional contents after being created, such as a thematic break."))

(defmethod initialize-instance :after ((block atomic-block-node) &key)
  "Ensure atomic blocks are always closed."
  (setf (slot-value block 'closed) t))

(defmethod accept-line (line (block atomic-block-node) context)
  "A stub implementation of ACCEPT-LINE.
This method will never be called because of the around method defined
for BLOCK-NODE."
  (values nil nil))

(defclass thematic-break (atomic-block-node)
  ()
  (:documentation "A thematic break (horizontal rule) separating parts of a document."))

(defclass heading (atomic-block-node)
  ((level
    :initarg :level
    :initform (error "Must provide heading level")
    :type (integer 1 6)
    :accessor level
    :documentation "The level of the heading, between 1 and 6 (inclusive).")
   (text
    :initarg :text
    :initform (error "Must provide heading text")
    :type (vector (or string inline-node))
    :accessor text
    :documentation "The text of the heading."))
  (:documentation "A heading of any type (ATX or setext) containing inline content."))

(defmethod print-object ((block heading) stream)
  (print-unreadable-object (block stream :type t)
    (with-accessors ((level level) (text text) (closed closedp)) block
      (format stream "~s ~s :CLOSED ~s" level text closed))))

(defmethod close-block :after ((block heading) context)
  "Strip whitespace surrounding the heading text."
  (declare (ignore context))
  (delete-whitespace (text block)))

(defclass text-block-node (block-node)
  ((text
    :initarg :text
    :initform (make-array 0
                          :element-type '(or string inline-node)
                          :adjustable t
                          :fill-pointer 0)
    :type (vector (or string inline-node))
    :accessor text
    :documentation "The text of the paragraph."))
  (:documentation "A block containing inline content (including text)."))

(defmethod initialize-object :after ((block text-block-node) &key)
  "Ensure the block's text vector is adjustable."
  (setf (text block)
        (make-adjustable (text block) '(or string inline-node))))

(defmethod print-object ((object text-block-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((text text) (closed closedp)) object
      (format stream "~s :CLOSED ~s" text closed))))

(defmethod accept-line (line (block text-block-node) context)
  (with-accessors ((text text)) block
    (add-raw-text (concatenate 'string
                               (unless (emptyp text) (string #\Newline))
                               line)
                  text))
  (values nil nil))

(defclass paragraph (text-block-node)
  ()
  (:documentation "A paragraph."))

(defmethod accept-line (line (block paragraph) context)
  (when (blankp line)
    (close-block block context)
    (return-from accept-line (values nil nil)))
  ;; Check for setext heading underlines if we're not processing a
  ;; lazy continuation line and if we already have some content in the
  ;; paragraph
  (unless (or (lazy-continuation-p context)
              (zerop (length (text block))))
    (let ((level (parse-setext-underline line context)))
      (when level
        (close-block block context)
        (return-from accept-line
          (values :replace-self (make-instance 'heading
                                               :text (text block)
                                               :level level))))))
  ;; Check for blocks that can interrupt paragraphs
  (let ((interrupting-block (make-block line context t)))
    (when interrupting-block
      (close-block block context)
      (return-from accept-line (values :accept interrupting-block))))
  ;; Otherwise, we have an ordinary text line
  (call-next-method (strip-indentation line) block context))

(defmethod close-block :after ((block paragraph) context)
  "Strip whitespace surrounding paragraph text."
  (declare (ignore context))
  (delete-whitespace (text block)))

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

(defmethod accept-line (line (block indented-code-block) context)
  (multiple-value-bind (stripped-line indentation)
      (strip-indentation line 4)
    (if (or (>= indentation 4) (blankp line))
        (call-next-method stripped-line block context)
        (progn
          (close-block block context)
          (handle-new-block line context)))))

(defmethod close-block :after ((block indented-code-block) context)
  "Remove leading and trailing blank lines from BLOCK."
  (declare (ignore context))
  (with-accessors ((text text)) block
    (delete-surrounding-blank-lines text)
    ;; For some reason, the Commonmark reference implementation adds a
    ;; newline to the end of every code block:
    ;; https://github.com/commonmark/commonmark-spec/issues/501
    (add-raw-text (string #\Newline) text)))

(defclass fenced-code-block (code-block)
  ((opening-fence-indentation
    :initarg :opening-fence-indentation
    :initform 0
    :type (integer 0 3)
    :reader opening-fence-indentation
    :documentation "The number of spaces of indentation of the opening code fence.")
   (opening-fence-length
    :initarg :opening-fence-length
    :initform (error "Must provide opening fence length")
    :type (integer 3)
    :reader opening-fence-length
    :documentation "The length of the opening code fence.")
   (tilde-fence
    :initarg :tilde-fence
    :initform (error "Must specify if this is a tilde code fence")
    :type boolean
    :reader tilde-fence-p
    :documentation "Whether this block is fenced by tildes (if nil, backtick is the fence character)."))
  (:documentation "A fenced code block (block of code surrounded by backtick or tilde fences)."))

(defmethod print-object ((block fenced-code-block) stream)
  (with-accessors ((closed closedp) (text text) (info info-string)
                   (indent opening-fence-indentation)
                   (length opening-fence-length)
                   (tilde tilde-fence-p))
      block
    (print-unreadable-object (block stream :type t)
      (format stream "~s :INFO-STRING ~s :OPENING-FENCE-INDENTATION ~s ~
:OPENING-FENCE-LENGTH ~s :TILDE-FENCE ~s :CLOSED ~s"
              text info indent length tilde closed))))

(defparameter *backtick-closing-fence-pattern*
  (ppcre:create-scanner "^ {0,3}(`+)[ \\t]*$")
  "A scanner for closing backtick code fences.")

(defparameter *tilde-closing-fence-pattern*
  (ppcre:create-scanner "^ {0,3}(~+)[ \\t]*$")
  "A scanner for closing tilde code fences.")

(defmethod accept-line (line (block fenced-code-block) context)
  (let ((closing-scanner (if (tilde-fence-p block)
                             *tilde-closing-fence-pattern*
                             *backtick-closing-fence-pattern*)))
    (ppcre:register-groups-bind (fence) (closing-scanner line)
      ;; See if we have a closing fence
      (when (>= (length fence) (opening-fence-length block))
        (close-block block context)
        (return-from accept-line (values nil nil)))))
  (call-next-method (strip-indentation line (opening-fence-indentation block))
                    block context))

(defmethod close-block :after ((block fenced-code-block) context)
  "Remove leading and trailing empty lines from BLOCK."
  (declare (ignore context))
  (with-accessors ((text text)) block
    (delete-surrounding-empty-lines text)
    ;; Again, for some reason we need to add a newline
    (add-raw-text (string #\Newline) text)))

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

(defmethod accept-line (line (block container-block-node) context)
  "Ask the last child of BLOCK to accept LINE or create a new child from LINE if there are none."
  (with-accessors ((children children)) block
    (if (emptyp children)
        (let ((new-block (make-block line context)))
          (when new-block
            (vector-push-extend new-block children)))
        (multiple-value-bind (action object)
            (accept-line line (last-elt children) context)
          (ecase action
            (:accept (vector-push-extend object children))
            (:replace-self (setf (last-elt children) object))
            (:delete-self (vector-pop children))
            ((nil))))))
  (values nil nil))

(defmethod close-block :after ((block container-block-node) context)
  "Close all unclosed children of BLOCK."
  (loop for child across (children block)
     unless (closedp child)
     do (close-block child context)))

(defclass document (container-block-node)
  ()
  (:documentation "The top-level node in the document AST."))

;;; Inline nodes

(defclass inline-node (node)
  ()
  (:documentation "An inline node."))

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
     finally (close-block document context)
       (return document)))

(defmethod parse-block-structure ((input string)
                                  &optional (context (make-standard-context)))
  (with-input-from-string (stream input)
    (parse-block-structure stream context)))

;;;; commonmark.lisp ends here
