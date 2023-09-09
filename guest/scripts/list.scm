(define-module (guest scripts list)
  #:use-module (guest scripts)
  #:use-module (guest i18n)
  #:use-module (ice-9 match)
  #:use-module (zephyr build utils)

  #:export (guest-list))

(define (zephyr-modules-env-paths)
  "Return a list of paths retrieved from the environment"
  (string-split (getenv "ZEPHYR_MODULE_PATH") #\:))

(define-command (guest-list . args)
  (synopsis "print $ZEPHYR_MODULES for cmake")
  (match args
    (("--help")
     (display (G_ "guest list search-path"))
     (newline)
     (display (G_ "
Print the ZEPHYR_MODULES cmake variable for modules found in SEARCH-PATH\n")))
    ((directories ...)
     (display (zephyr-modules-cmake-argument
	       (find-zephyr-modules (append directories
					    (zephyr-modules-env-paths))))))
    (()
     (display (zephyr-modules-cmake-argument
	       (find-zephyr-modules (zephyr-modules-env-paths)))))))
