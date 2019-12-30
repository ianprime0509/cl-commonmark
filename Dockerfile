FROM parentheticalenterprises/sbcl-quicklisp-base

WORKDIR /root/common-lisp/cl-commonmark/
COPY . ./
RUN sbcl --non-interactive \
    --eval '(ql:quickload "commonmark")' \
    --eval '(ql:quickload "commonmark/tests")'
