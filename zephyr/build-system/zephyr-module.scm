(define-module (zephyr build-system zephyr-module)
  #:use-module (guix packages)
  #:use-module (guix discovery)
  #:use-module (guix memoization)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module ((guix build-system copy) #:prefix copy:)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export (%zephyr-module-build-system-modules
	    %zephyr-workspace-name
	    zephyr-module-build-system))

;; Commentary:
;;
;; Standard build procedure for Zephyr modules. This is implemented as an
;; extension of `copy-build-system'.
;;
;; Code:

(define %zephyr-module-build-system-modules copy:%copy-build-system-modules)

(define %zephyr-workspace-name "zephyr-workspace")


(define (default-glibc)
  "Return the default glibc package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages base))))
    (module-ref module 'glibc)))


(define* (lower name
		#:key (workspace-path (string-append "/modules/" name))
		#:allow-other-keys
		#:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:workspace-path))
  (apply (@@ (guix build-system copy) lower)
	 name
	 (append `(#:install-plan '(("." ,(string-append %zephyr-workspace-name workspace-path))))
		 (strip-keyword-arguments private-keywords arguments))))


(define zephyr-module-build-system
  (build-system
    (name 'zephyr-module)
    (description "The standard zephyr module build system")
    (lower lower)))
