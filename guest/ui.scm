(define-module (guest ui)
  #:use-module (guest config)
  #:use-module (guest i18n)
  #:use-module (guest utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (texinfo)
  #:use-module (texinfo plain-text)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:export (guest-main))

;; Shamelessly "borrowed" from guix/ui.scm
(define %text-width (fluid->parameter *line-width*))

(define (show-bug-report-information)
  (display (G_ (format #f "Email bugs to ~a\n" %guest-bug-report-address))))

(define (texi->plain-text str)
  "Return a plain-text representation of texinfo fragment STR."
  ;; 'texi-fragment->stexi' uses a string port so make sure it's a
  ;; Unicode-capable one (see <http://bugs.gnu.org/11197>.)
  (with-fluids ((%default-port-encoding "UTF-8"))
    (stexi->plain-text (texi-fragment->stexi str))))

(define (texinfo-quote str)
  "Quote at signs and braces in STR to obtain its Texinfo represention."
  (list->string
   (string-fold-right (lambda (chr result)
			(if (memq chr '(#\@ #\{ #\}))
			    (cons* #\@ chr result)
			    (cons chr result)))
		      '()
		      str)))

(define-syntax-rule (leave-on-EPIPE exp ...)
  "Run EXP... in a context where EPIPE errors are caught and lead to 'exit'
with successful exit code.  This is useful when writing to the standard output
may lead to EPIPE, because the standard output is piped through 'head' or
similar."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      ;; We really have to exit this brutally, otherwise Guile eventually
      ;; attempts to flush all the ports, leading to an uncaught EPIPE down
      ;; the path.
      (if (= EPIPE (system-error-errno args))
	  (primitive-_exit 0)
	  (apply throw args)))))

(define* (display-hint message
		       #:key (port (current-error-port))
		       #:rest arguments)
  "Display MESSAGE, a l10n message possibly containing Texinfo markup and
'format' escape, to PORT.  ARGUMENTS is a (possibly empty) list of strings or
other objects that must match the 'format' escapes in MESSAGE."
  (display
   (texi->plain-text (match arguments
		       (() (format #f message))
		       (_  (apply format #f message
				  (map (match-lambda
					 ((? string? str)
					  (texinfo-quote str))
					 (obj
					  (texinfo-quote
					   (object->string obj))))
				       arguments)))))
   port))

(define (install-locale)
  "Install the current locale settings."
  (catch 'system-error
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      (display-hint (G_ "Consider installing the @code{glibc-locales} package
and defining @code{GUIX_LOCPATH}, along these lines:

@example
guix install glibc-locales
export GUIX_LOCPATH=\"$HOME/.guix-profile/lib/locale\"
@end example

See the \"Application Setup\" section in the manual, for more info.\n"))
      ;; We're now running in the "C" locale.  Try to install a UTF-8 locale
      ;; instead.  This one is guaranteed to be available in 'guix' from 'guix
      ;; pull'.
      (false-if-exception (setlocale LC_ALL "en_US.utf8")))))

(define (initialize-guest)
  "Perform the usual initialization for stand-alone Guest commands."
  ;; By default don't annoy users with deprecation warnings.  In practice,
  ;; 'define-deprecated' in (ice-9 deprecated) arranges so that those warnings
  ;; are emitted at expansion-time only, but there are cases where they could
  ;; slip through, for instance when interpreting code.
  (unless (getenv "GUILE_WARN_DEPRECATED")
    (debug-disable 'warn-deprecated))

  (install-locale)
  (textdomain %gettext-domain)

  ;; Ignore SIGPIPE.  If the daemon closes the connection, we prefer to be
  ;; notified via an EPIPE later.
  (sigaction SIGPIPE SIG_IGN)

  (setvbuf (current-output-port) 'line)
  (setvbuf (current-error-port) 'line))

(define* (show-version-and-exit #:optional (command (car (command-line))))
  "Display version information for COMMAND and `(exit 0)'."
  (simple-format #t "~a (~a) ~a~%"
		 command %guest-package-name %guest-version)
  (format #t "Copyright ~a 2023 ~a"
	  ;; TRANSLATORS: Translate "(C)" to the copyright symbol
	  ;; (C-in-a-circle), if this symbol is available in the user's
	  ;; locale.  Otherwise, do not translate "(C)"; leave it as-is.  */
	  (G_ "(C)")
	  (G_ "the Guest authors\n"))
  (display (G_"\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
"))
  (exit 0))



;; Representation of a 'guest' command.
(define-immutable-record-type <command>
  (command name synopsis)
  command?
  (name     command-name)
  (synopsis command-synopsis))

(define (source-file-command file)
  "Read FILE, a Scheme source file, and return either a <command> object based
on the 'define-command' top-level form found therein, or #f if FILE does not
contain a 'define-command' form."
  (define command-name
    (match (filter (negate string-null?)
		   (string-split file #\/))
      ((_ ... "guest" "scripts" name)
       (list (file-sans-extension name)))
      ((_ ... "guest" "scripts" first second)
       (list first (file-sans-extension second)))))

  ;; The strategy here is to parse FILE.  This is much cheaper than a
  ;; technique based on run-time introspection where we'd load FILE and all
  ;; the modules it depends on.
  (call-with-input-file file
    (lambda (port)
      (let loop ()
	(match (read port)
	  (('define-command _ ('synopsis synopsis)
	     _ ...)
	   (command command-name synopsis))
	  ((? eof-object?)
	   #f)
	  (_
	   (loop)))))))

(define* (command-files #:optional directory)
  "Return the list of source files that define Guest sub-commands."
  (define directory*
    (or directory
	(and=> (search-path %load-path "guest.scm")
	       (compose (cut string-append <> "/guest/scripts")
			dirname))))

  (define dot-scm?
    (cut string-suffix? ".scm" <>))

  (if directory*
      (map (cut string-append directory* "/" <>)
	   (scandir directory* dot-scm?))
      '()))

(define (extension-directories)
  "Return the list of directories containing Guix extensions."
  (filter file-exists?
	  (parse-path
	   (getenv "GUIX_EXTENSIONS_PATH"))))

(define (commands)
  "Return the list of commands, alphabetically sorted."
  (filter-map source-file-command
	      (append (command-files)
		      (append-map command-files
				  (extension-directories)))))

(define (show-guest-help)
  (define (display-commands commands)
    (let* ((names     (map (lambda (command)
			     (string-join (command-name command)))
			   commands))
	   (max-width (reduce max 0 (map string-length names))))
      (for-each (lambda (name command)
		  (format #t "    ~a  ~a~%"
			  (string-pad-right name max-width)
			  (G_ (command-synopsis command))))
		names
		commands)))

  (display (G_ "Usage: guest OPTION | COMMAND ARGS...
Run COMMAND with ARGS, if given.\n"))

  (display (G_ "
  -h, --help             display this helpful text again and exit"))
  (display (G_ "
  -V, --version          display version and copyright information and exit"))
  (newline)

  (newline)
  (show-bug-report-information))

(define (run-guest-command command . args)
  "Run COMMAND with the given ARGS.  Report an error when COMMAND is not
found."
  (define (command-hint guess commands)
    (define command-names
      (map (lambda (command)
	     (match (command-name command)
	       ((head tail ...) head)))
	   commands))
    (string-closest (symbol->string guess) command-names #:threshold 3))

  (define module
    (catch 'misc-error
      (lambda ()
	(resolve-interface `(guest scripts ,command)))
      (lambda _
	(let ((hint (command-hint command (commands))))
	  (format (current-error-port)
		  (G_ "guest: ~a: command not found~%") command)))))

  (let ((command-main (module-ref module
				  (symbol-append 'guest- command))))
    (dynamic-wind
      (const #f)
      (lambda ()
	(apply command-main args))
      (lambda ()
	;; Abuse 'exit-hook' (which is normally meant to be used by the
	;; REPL) to run things like profiling hooks upon completion.
	(run-hook exit-hook)))))

(define (run-guest . args)
  "Run the 'guest' command defined by command line ARGS.
Unlike 'guest-main', this procedure assumes that locale, i18n support,
and signal handling have already been set up."
  (define option? (cut string-prefix? "-" <>))

  ;; The default %LOAD-EXTENSIONS includes the empty string, which doubles the
  ;; number of 'stat' calls per entry in %LOAD-PATH.  Shamelessly remove it.
  (set! %load-extensions '(".scm"))

  ;; Disable canonicalization so we don't don't stat unreasonably.
  (with-fluids ((%file-port-name-canonicalization #f))
    (match args
      (()
       (format (current-error-port)
	       (G_ "guest: missing command name~%"))
       (show-guest-help))
      ((or "-V" "--version")
       (show-version-and-exit))
      ((or "-h" "--help")
       (show-guest-help))
      (((? option? o) args ...)
       (format (current-error-port)
	       (G_ "guest: unrecognized option '~a'~%") o)
       (show-guest-help))
      (("help" command)
       (apply run-guest-command (string->symbol command)
	      '("--help")))
      (("help" args ...)
       (leave-on-EPIPE (show-guest-help)))
      ((command args ...)
       (apply run-guest-command
	      (string->symbol command)
	      args)))))

(define (guest-main arg0 . args)
  (initialize-guest)
  (apply run-guest args))
