;;;; packages.lisp --- package definition

;;;; Code:

(in-package :cl-user)

(defpackage :commonmark
  (:use :cl)
  (:export

   :context
   :make-context
   :make-standard-context

   :node

   :block-node
   :closedp

   :thematic-break
   :raw-heading

   :text-block-node
   :text

   :raw-paragraph

   :code-block
   :info-string

   :indented-code-block

   :container-block-node
   :children

   :document

   :accept-line

   :read-markdown-line
   :strip-indentation

   :parse-block-structure))

;;;; packages.lisp ends here
