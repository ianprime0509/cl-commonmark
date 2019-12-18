;;;; block-parse-test.lisp --- tests for block-level parsing

;;;; Code:

(in-package :commonmark-test)

(defun print= (s1 s2)
  "Return non-nil if S1 and S2 have the same printed representation."
  (equal (format nil "~s" s1)
         (format nil "~s" s2)))

(defmacro make-document (closed &body children)
  `(make-instance 'document
                  :closed ,closed
                  :children
                  (flet ((raw-paragraph (text &rest rest)
                           (apply #'make-instance 'raw-paragraph
                                  :text text rest))
                         (indented-code-block (text &rest rest)
                           (apply #'make-instance 'indented-code-block
                                  :text text rest)))
                    (vector ,@children))))

(def-suite block-parsing)

(in-suite block-parsing)

(test single-paragraph
  (let ((document
         (parse-block-structure "line one
line two
line three")))
    (is (print= (make-document t
                  (raw-paragraph "line one
line two
line three" :closed t))
                document))))

(test multiple-paragraphs
  (let ((document
         (parse-block-structure "line one
line two

line three

line four")))
    (is (print= (make-document t
                  (raw-paragraph "line one
line two" :closed t)
                  (raw-paragraph "line three" :closed t)
                  (raw-paragraph "line four" :closed t))
                document))))

(test paragraphs-with-indentation
  (let ((document
         (parse-block-structure "  line one
    line two
            line three
         line four
line five

   line six")))
    (is (print= (make-document t
                  (raw-paragraph "line one
line two
line three
line four
line five" :closed t)
                  (raw-paragraph "line six" :closed t))
                document))))

(test indented-code-blocks
  (let ((document
         (parse-block-structure "    this is code
        this is indented code
    end indentation

this is a paragraph
    this is more paragraph

	this is code with a tab
    	this is code with spaces and a tab

    blank lines don't split code blocks
	    four spaces is a tab when parsing
  	weird mixture of spaces and a tab gets handled correctly
  
    those spaces above are not part of the block  
    but these trailing spaces are:  ")))
    (is (print= (make-document t
                  (indented-code-block "this is code
    this is indented code
end indentation
" :closed t)
                  (raw-paragraph "this is a paragraph
this is more paragraph" :closed t)
                  (indented-code-block "this is code with a tab
	this is code with spaces and a tab

blank lines don't split code blocks
    four spaces is a tab when parsing
weird mixture of spaces and a tab gets handled correctly

those spaces above are not part of the block  
but these trailing spaces are:  
" :closed t))
                document))))

;;;; block-parse-test.lisp ends here
