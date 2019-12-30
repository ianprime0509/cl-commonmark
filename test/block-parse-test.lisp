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
                  (flet ((paragraph (text)
                           (make-instance 'paragraph
                                          :closed t :text (vector text)))
                         (indented-code-block (text)
                           (make-instance 'indented-code-block
                                          :closed t :text (vector text)))
                         (thematic-break ()
                           (make-instance 'thematic-break :closed t))
                         (heading (level text)
                           (make-instance 'heading
                                          :level level :text (vector text)))
                         (fenced-code-block (info-string fence-length tilde
                                                         text
                                                         &key (indent 0))
                           (make-instance 'fenced-code-block
                                          :info-string info-string
                                          :opening-fence-length fence-length
                                          :tilde-fence tilde
                                          :text (vector text)
                                          :closed t
                                          :opening-fence-indentation indent))
                         (html-block (text)
                           (make-instance 'html-block
                                          :closed t :text (vector text)))
                         (block-quote (&rest children)
                           (make-instance 'block-quote
                                          :children
                                          (make-array (length children)
                                                      :initial-contents children)
                                          :closed t)))
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

(test backtick-fenced-code-blocks
  (let ((document
         (parse-block-structure "```
this is a simple code block
```
this is a paragraph
 ``````info string goes here
 this is another code block
  it's indented!
	of course tabs are treated as four spaces
 ```
 ~~~~~~
still going...
``````
````another one!
another one  

  

   ````
```not a fenced code block`
no")))
    (is (print= (make-document
                  (fenced-code-block nil 3 nil
                                     "this is a simple code block
")
                  (paragraph "this is a paragraph")
                  (fenced-code-block "info string goes here" 6 nil
                                     "this is another code block
 it's indented!
   of course tabs are treated as four spaces
```
~~~~~~
still going...
"
                                     :indent 1)
                  (fenced-code-block "another one!" 4 nil
                                     "another one  

  
")
                  (paragraph "```not a fenced code block`
no"))
                document))))

(test tilde-fenced-code-blocks
  (let ((document
         (parse-block-structure "opening paragraph
  ~~~info
this is a tilde code block
 fancy!
  some indentation here too
   pretty cool, huh?
         ~~~~~~~~
   ~~~~~~~~~~   	  
another paragraph
~~~~~    
tildes:
~~~
`````````
~~~~~
~~~``
hi
~~~")))
    (is (print= (make-document
                  (paragraph "opening paragraph")
                  (fenced-code-block "info" 3 t
                                     "this is a tilde code block
fancy!
some indentation here too
 pretty cool, huh?
       ~~~~~~~~
"
                                     :indent 2)
                  (paragraph "another paragraph")
                  (fenced-code-block nil 5 t
                                     "tildes:
~~~
`````````
")
                  (fenced-code-block "``" 3 t
                                     "hi
"))
                document))))

(test type-1-html-blocks
  (let ((document
         (parse-block-structure "<script>
script contents

more script contents
</script>

    <pre>this is a code block</pre>

  <pre>leading indentation
    is fine

  </script>the end tag doesn't even have to be the same
here's a paragraph
<pre>starting and ending on same line</pre>
<style>
body { color: red; }")))
    (is (print= (make-document
                  (html-block "<script>
script contents

more script contents
</script>")
                  (indented-code-block "<pre>this is a code block</pre>
")
                  (html-block "  <pre>leading indentation
    is fine

  </script>the end tag doesn't even have to be the same")
                  (paragraph "here's a paragraph")
                  (html-block "<pre>starting and ending on same line</pre>")
                  (html-block "<style>
body { color: red; }"))
                document))))

(test types-2-through-5-html-blocks
  (let ((document
         (parse-block-structure "<!-- HTML comment -->
<!-- Longer
HTML comment -->

<? HTML block
continues here ?>

<!DOCTYPE html>

<![CDATA[This is some cdata
text.]]>")))
    (is (print= (make-document
                  (html-block "<!-- HTML comment -->")
                  (html-block "<!-- Longer
HTML comment -->")
                  (html-block "<? HTML block
continues here ?>")
                  (html-block "<!DOCTYPE html>")
                  (html-block "<![CDATA[This is some cdata
text.]]>"))
                document))))

(test type-6-html-blocks
  (let ((document
         (parse-block-structure "<address invalid!!invalid
element contents

<div/>
still part of the block



paragraph starts here
<p>
but is interrupted!
</p>")))
    (is (print= (make-document
                  (html-block "<address invalid!!invalid
element contents")
                  (html-block "<div/>
still part of the block")
                  (paragraph "paragraph starts here")
                  (html-block "<p>
but is interrupted!
</p>"))
                document))))

(test type-7-html-blocks
  (let ((document
         (parse-block-structure
          "<open-tag>
    contents of the block

this is just a paragraph
<cannot-interrupt>

<empty-tag/>

<not-a-complete-tag
so this is a paragraph")))
    (is (print= (make-document
                  (html-block "<open-tag>
    contents of the block")
                  (paragraph "this is just a paragraph
<cannot-interrupt>")
                  (html-block "<empty-tag/>")
                  (paragraph "<not-a-complete-tag
so this is a paragraph"))
                document))))

(test block-quotes
  (let ((document
         (parse-block-structure
          "> this is a block quote
>>> these are some nested block quotes
> > > spaces are fine too
> this is a lazy continuation line

> another block quote
lazy

this is a paragraph
>this is a block quote

> > > nested block quotes
> ```
> this is a code block
```
this is a separate code block")))
    (is (print= (make-document
                  (block-quote
                   (paragraph "this is a block quote")
                   (block-quote
                    (block-quote
                     (paragraph "these are some nested block quotes
spaces are fine too
this is a lazy continuation line"))))
                  (block-quote
                   (paragraph "another block quote
lazy"))
                  (paragraph "this is a paragraph")
                  (block-quote
                   (paragraph "this is a block quote"))
                  (block-quote
                   (block-quote
                    (block-quote
                     (paragraph "nested block quotes")))
                   (fenced-code-block nil 3 nil "this is a code block
"))
                  (fenced-code-block nil 3 nil "this is a separate code block
"))
                document))))

;;;; block-parse-test.lisp ends here
