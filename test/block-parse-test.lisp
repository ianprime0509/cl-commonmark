;;;; block-parse-test.lisp --- tests for block-level parsing

;;;; Code:

(in-package :commonmark-test)

(defun print= (s1 s2)
  "Return non-nil if S1 and S2 have the same printed representation."
  (equal (format nil "~s" s1)
         (format nil "~s" s2)))

(defmacro make-document (&body children)
  `(make-instance 'document
                  :closed t
                  :children
                  (flet ((paragraph (text &rest rest)
                           (apply #'make-instance 'paragraph
                                  :closed t :text (vector text) rest))
                         (indented-code-block (text &rest rest)
                           (apply #'make-instance 'indented-code-block
                                  :closed t
                                  :text (vector text) rest))
                         (thematic-break (&rest rest)
                           (apply #'make-instance 'thematic-break
                                  :closed t rest))
                         (heading (level text &rest rest)
                           (apply #'make-instance 'heading
                                  :level level :text (vector text) rest)))
                    (vector ,@children))))

(def-suite block-parsing)

(in-suite block-parsing)

(test single-paragraph
  (let ((document
         (parse-block-structure "line one
line two
line three")))
    (is (print= (make-document
                  (paragraph "line one
line two
line three"))
                document))))

(test multiple-paragraphs
  (let ((document
         (parse-block-structure "line one
line two

line three

line four")))
    (is (print= (make-document
                  (paragraph "line one
line two")
                  (paragraph "line three")
                  (paragraph "line four"))
                document))))

(test paragraphs-with-indentation
  (let ((document
         (parse-block-structure "  line one
    line two
            line three
         line four
line five

   line six")))
    (is (print= (make-document
                  (paragraph "line one
line two
line three
line four
line five")
                  (paragraph "line six"))
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
    (is (print= (make-document
                  (indented-code-block "this is code
    this is indented code
end indentation
")
                  (paragraph "this is a paragraph
this is more paragraph")
                  (indented-code-block "this is code with a tab
	this is code with spaces and a tab

blank lines don't split code blocks
    four spaces is a tab when parsing
weird mixture of spaces and a tab gets handled correctly

those spaces above are not part of the block  
but these trailing spaces are:  
"))
                document))))

(test thematic-breaks
  (let ((document
         (parse-block-structure "***
paragraph text
 ***
  ***
   ***
    ***
more paragraph

_

__

___
---
* * * *	* ")))
    (is (print= (make-document
                  (thematic-break)
                  (paragraph "paragraph text")
                  (thematic-break)
                  (thematic-break)
                  (thematic-break)
                  (indented-code-block "***
")
                  (paragraph "more paragraph")
                  (paragraph "_")
                  (paragraph "__")
                  (thematic-break)
                  (thematic-break)
                  (thematic-break))
                document))))

(test atx-headings
  (let ((document
         (parse-block-structure "# heading 1
 ## heading 2   
  ### heading 3	
   ####     heading 4
    ##### not a heading
##### heading 5
###### heading 6 ######

paragraph text
# heading # 
## heading 2 ########	
### heading 3 #
#### heading 4 \\#
#not a heading
#\\# also not a heading")))
    (is (print= (make-document
                  (heading 1 "heading 1")
                  (heading 2 "heading 2")
                  (heading 3 "heading 3")
                  (heading 4 "heading 4")
                  (indented-code-block "##### not a heading
")
                  (heading 5 "heading 5")
                  (heading 6 "heading 6")
                  (paragraph "paragraph text")
                  (heading 1 "heading")
                  (heading 2 "heading 2")
                  (heading 3 "heading 3")
                  (heading 4 "heading 4 \\#")
                  (paragraph "#not a heading
#\\# also not a heading"))
                document))))

(test setext-headings
  (let ((document
         (parse-block-structure "heading 1
===
heading 2
   --------------
another heading
 -

how about
a multi-line heading
=

this isn't a heading

---")))
    (is (print= (make-document
                  (heading 1 "heading 1")
                  (heading 2 "heading 2")
                  (heading 2 "another heading")
                  (heading 1 "how about
a multi-line heading")
                  (paragraph "this isn't a heading")
                  (thematic-break))
                document))))

;;;; block-parse-test.lisp ends here
