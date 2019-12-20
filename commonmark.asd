;;;; commonmark.asd --- ASDF system definition

;;;; Code:

(defsystem "commonmark"
  :description "A Common Lisp implementation of the Commonmark standard for Markdown."
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("alexandria" "cl-ppcre" "uiop")
  :components ((:file "packages")
               (:file "commonmark" :depends-on ("packages"))))

(defsystem "commonmark/test"
  :depends-on ("commonmark" "fiveam")
  :pathname "test"
  :components ((:file "packages")
               (:file "block-parse-test" :depends-on ("packages"))))

;;;; commonmark.asd ends here
