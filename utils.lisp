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

(defun make-adjustable (vector
                        &optional (element-type (array-element-type vector)))
  "Ensure VECTOR is adjustable with a fill pointer and the given element type.
Return VECTOR if it is already adjustable with a fill pointer or
return a new vector with the same contents."
  (if (and (adjustable-array-p vector)
           (array-has-fill-pointer-p vector))
      vector
      (make-array (length vector)
                  :element-type element-type
                  :adjustable t
                  :fill-pointer (length vector)
                  :initial-contents vector)))

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

;;; Whitespace manipulation

(defparameter *blank-line-scanner*
  (ppcre:create-scanner "^[ \\t]*$")
  "A scanner matching blank lines.")

(defparameter *whitespace-chars*
  (vector #\Space #\Tab #\Newline (code-char #xB) (code-char #xC) #\Return)
  "A vector containing all the characters considered to be whitespace.")

(defun blankp (line)
  "Return non-nil if LINE is blank."
  (ppcre:scan *blank-line-scanner* line))

(defun delete-whitespace (text)
  "Trim leading and trailing whitespace from TEXT, which is a vector of raw text strings and inline nodes."
  ;; I don't bother handling the case that the paragraph starts or
  ;; ends with an inline node at this point in processing
  (unless (zerop (length text))
    (let ((start (first-elt text))
          (end (last-elt text)))
      (when (stringp start)
        (setf (first-elt text)
              (string-left-trim *whitespace-chars* start)))
      (when (stringp end)
        (setf (last-elt text)
              (string-right-trim *whitespace-chars* end))))))

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

;;;; utils.lisp ends here