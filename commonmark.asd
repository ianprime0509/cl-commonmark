;;;; commonmark.asd --- ASDF system definition

;;;; Code:

(defsystem "commonmark"
  :description "A Common Lisp implementation of the Commonmark standard for Markdown."
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("alexandria" "cl-ppcre" "uiop")
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "commonmark" :depends-on ("packages" "utils")))
  :in-order-to ((test-op (test-op "commonmark/tests"))))

(defsystem "commonmark/tests"
  :depends-on ("commonmark" "fiveam")
  :pathname "tests"
  :components ((:file "packages")
               (:file "block-parse-test" :depends-on ("packages")))
  :perform (test-op (o c)
                    (unless (symbol-call :fiveam :run!
                                         (find-symbol* :commonmark
                                                       :commonmark-tests))
                      (error "Test suite failed"))))

;;;; commonmark.asd ends here
