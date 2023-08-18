(define-module (guest scripts)
  #:use-module (guest i18n)
  #:use-module (guest utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (define-command
	    command-synopsis
	    parse-command-line
	    synopsis))

(define synopsis 'command-synopsis)

(define-syntax define-command
  (syntax-rules (synopsis)
    "Define the given command as a procedure along with its synopsis.
The synopsis becomes the docstring of the
procedure, the synopsis is also meant to be read (parsed) by
'guest help'."
    ;; The (synopsis ...) form is here so that xgettext sees those strings as
    ;; translatable.
    ((_ (name . args)
	(synopsis doc) body ...)
     (define (name . args)
       doc
       body ...))
    ((_ (name . args)
	(category cat) (synopsis doc)
	body ...)
     (begin
       (assert-valid-command-category cat)
       (define (name . args)
	 doc
	 body ...)))))

(define-syntax-rule (leave args ...)
  "Emit an error message and exit."
  (begin
    (exit 1)))

(define (args-fold* args options unrecognized-option-proc operand-proc . seeds)
  "A wrapper on top of `args-fold' that does proper user-facing error
reporting."
  (catch 'misc-error
    (lambda ()
      (apply args-fold args options unrecognized-option-proc
	     operand-proc seeds))
    (lambda (key proc msg args . rest)
      ;; XXX: MSG is not i18n'd.
      (leave (G_ "invalid argument: ~a~%")
	     (apply format #f msg args)))))

(define (environment-build-options)
  "Return additional build options passed as environment variables."
  (arguments-from-environment-variable "GUEST_BUILD_OPTIONS"))

(define %default-argument-handler
  ;; The default handler for non-option command-line arguments.
  (lambda (arg result)
    (alist-cons 'argument arg result)))

(define* (parse-command-line args options seeds
			     #:key
			     (build-options? #t)
			     (argument-handler %default-argument-handler))
  "Parse the command-line arguments ARGS according to OPTIONS (a list of
SRFI-37 options) and return the result, seeded by SEEDS.  When BUILD-OPTIONS?
is true, also pass arguments passed via the 'GUIX_BUILD_OPTIONS' environment
variable.  Command-line options take precedence those passed via
'GUIX_BUILD_OPTIONS'.

ARGUMENT-HANDLER is called for non-option arguments, like the 'operand-proc'
parameter of 'args-fold'."
  (define (parse-options-from args seeds)
    ;; Actual parsing takes place here.
    (apply args-fold* args options
	   (lambda (opt name arg . rest)
	     (display (G_ (format #f "Bad option ~a = ~a\n" name arg)))
	     (exit 1))
	   argument-handler
	   seeds))

  (call-with-values
      (lambda ()
	(if build-options?
	    (parse-options-from (environment-build-options) seeds)
	    (apply values seeds)))
    (lambda seeds
      ;; ARGS take precedence over what the environment variable specifies.
      (parse-options-from args seeds))))
