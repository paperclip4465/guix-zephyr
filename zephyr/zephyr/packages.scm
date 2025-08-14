(define-module (zephyr packages)
  #:use-module ((gnu packages) :prefix gnu:)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)

  #:export (search-patch
            search-patches
            %patch-path))

(define (search-patch file-name)
  (or (search-path (%patch-path) file-name)
      (error (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  (list (search-patch file-name) ...))

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/zephyr/packages.scm")))
        %load-path))

(define %patch-path
  (make-parameter
   (cons (string-append %channel-root "/patches")
         (gnu:%patch-path))))
