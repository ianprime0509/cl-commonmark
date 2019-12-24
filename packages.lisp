;;;; packages.lisp --- package definition

;;;; Code:

(in-package :cl-user)

(defpackage :commonmark
  (:use :cl)
  (:import-from :alexandria :emptyp :first-elt :last-elt :when-let)
  (:export

   :context
   :make-context
   :make-standard-context

   :node

   :block-node
   :closedp

   :thematic-break
   :heading

   :text-block-node
   :text

   :paragraph

   :code-block
   :info-string

   :indented-code-block
   :fenced-code-block
   :html-block

   :container-block-node
   :children

   :block-quote
   :document

   :accept-line

   :read-markdown-line
   :strip-indentation

   :parse-block-structure))

;;;; packages.lisp ends here
