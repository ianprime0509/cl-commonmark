;;;; commonmark.lisp --- core definitions

;;;; TODO

;;; - Tab stops are not handled correctly: for example, a block quote
;;;   marker (>) followed by a tab is currently interpreted as an
;;;   indented code block within a block quote, since the column
;;;   position after the marker is not considered.

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

This behavior is controlled by the PARSE-SETEXT-UNDERLINE function."))
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
     (,(line
         (indentation 0 3)
         '(:register (:char-class #\* #\_ #\-))
         'optional-whitespace
         '(:greedy-repetition 2 nil
           (:sequence (:back-reference 1) optional-whitespace)))
       ,(lambda (context break-string)
          (declare (ignore break-string))
          (make-thematic-break context))
       t)
     ;; ATX headings
     (,(line
         (indentation 0 3)
         '(:register (:greedy-repetition 1 6 #\#))
         'required-whitespace
         '(:register (:non-greedy-repetition 0 nil :everything))
         '(:greedy-repetition 0 1
           (:sequence
            required-whitespace
            (:greedy-repetition 1 nil #\#)))
         'optional-whitespace)
       ,(lambda (context opening text)
          (make-heading (length opening) (vector text) context))
       t)
     ;; Fenced code blocks
     (,(line
         `(:register ,(indentation 0 3))
         '(:register (:greedy-repetition 3 nil #\`))
         'optional-whitespace
         '(:greedy-repetition 0 1
           (:register
            (:non-greedy-repetition 1 nil
             (:inverted-char-class #\`))))
         'optional-whitespace)
       ,(lambda (context indent fence info-string)
          (make-fenced-code-block info-string nil (length indent)
                                  (length fence) nil context))
       t)
     (,(line
         `(:register ,(indentation 0 3))
         '(:register (:greedy-repetition 3 nil #\~))
         'optional-whitespace
         '(:greedy-repetition 0 1
           (:register
            (:non-greedy-repetition 1 nil :everything)))
         'optional-whitespace)
       ,(lambda (context indent fence info-string)
          (make-fenced-code-block info-string nil (length indent)
                                  (length fence) t context))
       t)
     ;; Indented code blocks
     (,(line-register
        (indentation 4)
        '(:non-greedy-repetition 0 nil :everything)
        'single-non-whitespace
        '(:non-greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-indented-code-block text context))
       nil)
     ;; HTML blocks
     (,(line-register
        (indentation 0 3)
        "<"
        'type-1-html-block-tag
        '(:alternation single-whitespace #\> :end-anchor)
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-html-block text '(:sequence "</" type-1-html-block-tag ">")
                           t context))
       t)
     (,(line-register
        (indentation 0 3)
        "<!--"
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-html-block text "-->" t context))
       t)
     (,(line-register
        (indentation 0 3)
        "<?"
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-html-block text "\\?>" t context))
       t)
     (,(line-register
        (indentation 0 3)
        "<!"
        '(:char-class (:range #\A #\Z))
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-html-block text ">" t context))
       t)
     (,(line-register
        (indentation 0 3)
        "<![CDATA["
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-html-block text "\\]\\]>" t context))
       t)
     (,(line-register
        (indentation 0 3)
        '(:alternation "<" "</")
        'type-6-html-block-tag
        '(:alternation single-whitespace #\> "/>" :end-anchor)
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-html-block text 'blank-line nil context))
       t)
     (,(line-register
        (indentation 0 3)
        '(:alternation open-tag closing-tag)
        'optional-whitespace)
       ,(lambda (context text)
          (make-html-block text 'blank-line nil context))
       ;; HTML blocks of type 7 cannot interrupt a paragraph, unlike
       ;; all other types
       nil)
     ;; Block quotes
     (,(line-register
        (indentation 0 3)
        #\>
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-block-quote text context))
       t)
     ;; Paragraphs
     (,(line-register
        '(:greedy-repetition 0 nil :everything)
        'single-non-whitespace
        '(:greedy-repetition 0 nil :everything))
       ,(lambda (context text)
          (make-paragraph text context))
       nil))
   :setext-underlines
   `((,(setext-underline #\=) 1)
     (,(setext-underline #\-) 2))))

(defun make-block (line context)
  "Make a new block from LINE using CONTEXT.
If LINE represents several nested blocks (for example, a paragraph
within a block quote), return the top-level block containing the
children. Return nil if no block can be created (e.g. if LINE is
empty)."
  (loop for (scanner function nil) in (block-starts context)
     do (multiple-value-bind (match registers)
            (ppcre:scan-to-strings scanner line)
          (when match
            (let ((reg-list (map 'list #'identity registers)))
              (return (apply function context reg-list)))))))

(defun paragraph-interruption-p (line context)
  "Return non-nil if LINE can interrupt a paragraph."
  (loop for (scanner nil can-interrupt) in (block-starts context)
     thereis (and can-interrupt (ppcre:scan scanner line))))

(defun parse-setext-underline (line context)
  "Attempt to parse LINE as a setext heading underline using CONTEXT.
Return the heading level (between 1 and 6, inclusive) or nil if LINE
is not a setext heading underline."
  (loop for (scanner level) in (setext-underlines context)
     when (ppcre:scan scanner line)
     return level))

;;; Generic node methods

(defgeneric accept-line (line block context &optional continuation-p)
  (:documentation "Ask BLOCK to accept LINE as additional contents.
If CONTINUATION-P is non-nil, process LINE as a lazy continuation
line.

Return two values, ACCEPTED-P and ACTIONS. If ACCEPTED-P is non-nil,
LINE was accepted by BLOCK. ACTIONS is a list of post-processing
actions for the parent to take. Valid actions are as follows:

(:REPLACE NEW-BLOCK) - replace BLOCK with NEW-BLOCK"))

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
  ((can-accept-continuation-p
    :initform nil
    :type boolean
    :reader can-accept-continuation-p
    :allocation :class
    :documentation "Whether this node can accept lazy continuation lines.")
   (closed
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

(defmethod accept-line :around (line (block block-node) context
                                &optional continuation-p)
  "Handle a new line for a closed block or a block that cannot handle lazy continuation lines."
  ;; If LINE is a lazy continuation line and BLOCK cannot handle such
  ;; lines, BLOCK should be closed
  (when (and continuation-p (not (can-accept-continuation-p block)))
    (close-block block context))
  (unless (closedp block)
    (call-next-method line block context continuation-p)))

(defmethod close-block ((block block-node) context)
  (declare (ignore context))
  (setf (slot-value block 'closed) t))

(defclass atomic-block-node (block-node)
  ((closed
    ;; Atomic blocks are always closed
    :initform t))
  (:documentation "A block node that does not accept any additional contents after being created, such as a thematic break."))

(defmethod accept-line (line (block atomic-block-node) context
                        &optional continuation-p)
  "A stub implementation of ACCEPT-LINE.
This method will never be called because of the around method defined
for BLOCK-NODE; atomic blocks are always closed."
  (declare (ignore line block context continuation-p))
  (error "Attempted to accept a line for an atomic block node"))

(defclass thematic-break (atomic-block-node)
  ()
  (:documentation "A thematic break (horizontal rule) separating parts of a document."))

(defun make-thematic-break (context)
  "Return a new thematic break."
  (declare (ignore context))
  (make-instance 'thematic-break))

(defclass heading (atomic-block-node)
  ((level
    :initarg :level
    :initform (error "Must provide heading level")
    :type (integer 1 6)
    :reader level
    :documentation "The level of the heading, between 1 and 6 (inclusive).")
   (text
    :initarg :text
    :initform (error "Must provide heading text")
    :type (vector (or string inline-node))
    :reader text
    :documentation "The text of the heading."))
  (:documentation "A heading of any type (ATX or setext) containing inline content."))

(defun make-heading (level text context)
  "Return a new heading at level LEVEL with TEXT."
  (declare (ignore context))
  (make-instance 'heading :level level :text (make-text text)))

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
    :reader text
    :documentation "The text of the paragraph.
This vector must be adjustable."))
  (:documentation "A block containing inline content (including text)."))

(defmethod print-object ((object text-block-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((text text) (closed closedp)) object
      (format stream "~s :CLOSED ~s" text closed))))

(defmethod accept-line (line (block text-block-node) context
                        &optional continuation-p)
  (declare (ignore context continuation-p))
  (with-accessors ((text text)) block
    (add-raw-text (concatenate 'string
                               (unless (emptyp text) (string #\Newline))
                               line)
                  text))
  t)

(defclass paragraph (text-block-node)
  ((can-accept-continuation-p
    :initform t))
  (:documentation "A paragraph."))

(defun make-paragraph (first-line context)
  "Return a new paragraph with FIRST-LINE as its first line.
This is equivalent to creating an empty paragraph and then calling
ACCEPT-LINE with FIRST-LINE."
  (let ((block (make-instance 'paragraph)))
    (when first-line
      (accept-line first-line block context))
    block))

(defmethod accept-line (line (block paragraph) context
                        &optional continuation-p)
  (when (blankp line)
    (close-block block context)
    (return-from accept-line nil))
  ;; Check for setext heading underlines if we're not processing a
  ;; lazy continuation line and if we already have some content in the
  ;; paragraph
  (unless (or continuation-p (emptyp (text block)))
    (when-let ((level (parse-setext-underline line context)))
      (close-block block context)
      (return-from accept-line
        (values t `((:replace ,(make-heading level (text block) context)))))))
  ;; Check for paragraph interruptions
  (when (paragraph-interruption-p line context)
    (close-block block context)
    (return-from accept-line nil))
  ;; Otherwise, we have an ordinary text line
  (call-next-method (strip-indentation line) block context continuation-p))

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

(defun make-indented-code-block (first-line context)
  "Return a new indented code block with FIRST-LINE as its first line."
  (let ((block (make-instance 'indented-code-block)))
    (when first-line
      (accept-line first-line block context))
    block))

(defmethod accept-line (line (block indented-code-block) context
                        &optional continuation-p)
  (multiple-value-bind (stripped-line indentation)
      (strip-indentation line 4)
    (if (or (>= indentation 4) (blankp line))
        (call-next-method stripped-line block context continuation-p)
        (prog1 nil (close-block block context)))))

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
   (tilde-fence-p
    :initarg :tilde-fence-p
    :initform (error "Must specify if this is a tilde code fence")
    :type boolean
    :reader tilde-fence-p
    :documentation "Whether this block is fenced by tildes (if nil, backtick is the fence character)."))
  (:documentation "A fenced code block (block of code surrounded by backtick or tilde fences)."))

(defun make-fenced-code-block (info-string first-line opening-fence-indentation
                               opening-fence-length tilde-fence-p context)
  "Return a new fenced code block with FIRST-LINE as its first line."
  (let ((block (make-instance 'fenced-code-block
                              :info-string info-string
                              :opening-fence-indentation opening-fence-indentation
                              :opening-fence-length opening-fence-length
                              :tilde-fence-p tilde-fence-p)))
    (when first-line
      (accept-line first-line block context))
    block))

(defmethod print-object ((block fenced-code-block) stream)
  (with-accessors ((closed closedp) (text text) (info info-string)
                   (indent opening-fence-indentation)
                   (length opening-fence-length)
                   (tilde tilde-fence-p))
      block
    (print-unreadable-object (block stream :type t)
      (format stream "~s :INFO-STRING ~s :OPENING-FENCE-INDENTATION ~s ~
:OPENING-FENCE-LENGTH ~s :TILDE-FENCE-P ~s :CLOSED ~s"
              text info indent length tilde closed))))

(defparameter *backtick-closing-fence-pattern*
  (ppcre:create-scanner "^ {0,3}(`+)[ \\t]*$")
  "A scanner for closing backtick code fences.")

(defparameter *tilde-closing-fence-pattern*
  (ppcre:create-scanner "^ {0,3}(~+)[ \\t]*$")
  "A scanner for closing tilde code fences.")

(defmethod accept-line (line (block fenced-code-block) context
                        &optional continuation-p)
  (let ((closing-scanner (if (tilde-fence-p block)
                             *tilde-closing-fence-pattern*
                             *backtick-closing-fence-pattern*)))
    (ppcre:register-groups-bind (fence) (closing-scanner line)
      ;; See if we have a closing fence
      (when (>= (length fence) (opening-fence-length block))
        (close-block block context)
        (return-from accept-line t))))
  (call-next-method (strip-indentation line (opening-fence-indentation block))
                    block context continuation-p))

(defmethod close-block :after ((block fenced-code-block) context)
  "Remove leading and trailing empty lines from BLOCK."
  (declare (ignore context))
  (with-accessors ((text text)) block
    (delete-surrounding-empty-lines text)
    ;; Again, for some reason we need to add a newline
    (add-raw-text (string #\Newline) text)))

(defclass html-block (text-block-node)
  ((end-line-scanner
    :initarg :end-line-scanner
    :initform (ppcre:create-scanner "^[ \\t]*$")
    :reader end-line-scanner
    :documentation "A scanner matching the ending line of this block.")
   (include-end-line-p
    :initarg :include-end-line-p
    :initform nil
    :type boolean
    :reader include-end-line-p
    :documentation "Whether to include the ending line in this block's text."))
  (:documentation "A block of raw HTML."))

(defun make-html-block (first-line end-line-scanner include-end-line-p context)
  "Return a new HTML block with FIRST-LINE as its first line."
  (let ((block (make-instance 'html-block
                              :end-line-scanner
                              (ppcre:create-scanner end-line-scanner)
                              :include-end-line-p include-end-line-p)))
    (when first-line
      (accept-line first-line block context))
    block))

(defmethod accept-line (line (block html-block) context
                        &optional continuation-p)
  (when (ppcre:scan (end-line-scanner block) line)
    (when (or (emptyp (text block))
              (and (stringp (first-elt (text block)))
                   (emptyp (first-elt (text block))))
              (include-end-line-p block))
      (call-next-method line block context continuation-p))
    (close-block block context)
    (return-from accept-line (include-end-line-p block)))
  (call-next-method line block context continuation-p))

(defclass container-block-node (block-node)
  ((can-accept-continuation-p
    :initform t)
   (children
    :initarg :children
    :initform (make-array 0
                          :element-type 'block-node
                          :fill-pointer 0
                          :adjustable t)
    :type (vector block-node)
    :accessor children
    :documentation "The children of the block."))
  (:documentation "A block node that can contain other blocks as children."))

(defmethod print-object ((object container-block-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((children children) (closed closedp)) object
      (format stream "~s :CLOSED ~s" children closed))))

(defmethod accept-line (line (block container-block-node) context
                        &optional continuation-p)
  "Ask the last child of BLOCK to accept LINE or create a new child from LINE if there are none."
  (with-accessors ((children children)) block
    (if (emptyp children)
        (when-let ((new-block (make-block line context)))
          (vector-push-extend new-block children)
          t)
        (multiple-value-bind (accepted-p actions)
            (accept-line line (last-elt children) context continuation-p)
          ;; Regardless of whether the line was accepted, we need to
          ;; perform any post-processing actions on the child
          (loop for action in actions
             do (destructuring-case action
                  ((:replace new-block)
                   (setf (last-elt children) new-block))))
          (if accepted-p
              t
              ;; If the line was not accepted by the child, we either
              ;; need to create a new child (if this block can remain
              ;; open) or close this block and tell the parent to
              ;; re-parse (if we're looking for a continuation line)
              (if continuation-p
                  (prog1 nil (close-block block context))
                  (when-let ((new-block (make-block line context)))
                    (vector-push-extend new-block children)
                    t)))))))

(defmethod close-block :after ((block container-block-node) context)
  "Close all unclosed children of BLOCK."
  (loop for child across (children block)
     unless (closedp child)
     do (close-block child context)))

(defclass block-quote (container-block-node)
  ()
  (:documentation "A quoted block of Markdown contents."))

(defun make-block-quote (first-line context)
  "Return a new block quote with FIRST-LINE as its first line."
  (let ((block (make-instance 'block-quote)))
    (when first-line
      (accept-line first-line block context))
    block))

(defparameter *block-quote-scanner*
  (ppcre:create-scanner (line
                          (indentation 0 3)
                          #\>
                          '(:greedy-repetition 0 1 #\Space)
                          '(:register
                            (:group
                             (:greedy-repetition 0 nil :everything))))))

(defmethod accept-line (line (block block-quote) context
                        &optional continuation-p)
  (multiple-value-bind (match registers)
      (ppcre:scan-to-strings *block-quote-scanner* line)
    (let ((line (if match (aref registers 0) line))
          (continuation-p (or continuation-p (not match))))
      (call-next-method line block context continuation-p))))

(defclass list-item (container-block-node)
  ((continuation-indent
    :initarg :continuation-indent
    :initform (error "Must provide continuation indent")
    :type (integer 0)
    :reader continuation-indent
    :documentation "The number of spaces of indentation required for each following line (not the first) of the list item.")
   (loose
    :initarg :loose
    :initform t
    :type boolean
    :accessor loose
    :documentation "Whether this list item is considered 'loose' (directly contains block elements separated by blank lines).
The spec has no notion of list items being 'loose' or 'tight', only
lists themselves. We use this information only to aid in deciding
whether a list is loose or tight."))
  (:documentation "A list item.
This is a superclass of ordered and unordered list items and should
not be used directly."))

(defclass unordered-list-item (list-item)
  ((list-marker
    :initarg :list-marker
    :initform (error "Must provide list marker")
    :type string
    :reader list-marker
    :documentation "The list marker (e.g. * or -)."))
  (:documentation "An unordered list item."))

(defclass ordered-list-item (list-item)
  ((item-number
    :initarg :item-number
    :initform (error "Must provide item number")
    :type (integer 0)
    :reader item-number
    :documentation "The number of this item in the list, as specified in the source document.
When an ordered list is rendered to HTML, according to the spec, all
item numbers except the first are ignored. However, they are retained
in the AST.")
   (marker-suffix
    :initarg :marker-suffix
    :initform (error "Must provide list marker suffix")
    :type string
    :reader marker-suffix
    :documentation "The suffix following the list item number (e.g. . or ))."))
  (:documentation "An ordered list item."))

(defclass document (container-block-node)
  ()
  (:documentation "The top-level node in the document AST."))

(defmethod accept-line (line (block document) context
                        &optional continuation-p)
  "Accept LINE unconditionally.
This just ensures that ACCEPT-LINE always returns t for (unclosed)
documents."
  (call-next-method line block context continuation-p)
  t)

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
