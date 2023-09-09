(define-module (zephyr build-system zephyr)
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export (%zephyr-build-system-modules
	    zephyr-build
	    zephyr-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using Zephyr. This is implemented as an
;; extension of `cmake-build-system'.
;;
;; Code:

(define %zephyr-build-system-modules
  `((zephyr build zephyr-build-system)
    (zephyr build utils)
    ,@%cmake-build-system-modules))

(define default-zephyr-base
  (module-ref (resolve-interface '(zephyr packages zephyr))
	      'zephyr-3.1))

(define default-zephyr-sdk
  (module-ref (resolve-interface '(zephyr packages zephyr))
	      'arm-zephyr-eabi-sdk))

(define default-ninja
  (module-ref (resolve-interface '(gnu packages ninja))
	      'ninja))

(define default-cmake
  (module-ref (resolve-interface '(gnu packages cmake))
	      'cmake-minimal))


(define* (lower name
		#:key source inputs native-inputs system target
		(outputs '("out" "debug"))
		(zephyr default-zephyr-base)
		(sdk default-zephyr-sdk)
		(ninja default-ninja)
		(cmake default-cmake)
		(bin-name "zephyr")
		#:allow-other-keys
		#:rest arguments)
  "Return a bag for NAME."
  (define private-keywords `(#:zephyr #:inputs #:native-inputs #:target
			     #:sdk #:ninja #:cmake))
  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source `(("source" ,source)) '())
		    ,@`(("cmake" ,cmake))
		    ,@`(("zephyr-sdk" ,sdk))
		    ,@`(("zephyr" ,zephyr))
		    ,@`(("ninja" ,ninja))
		    ,@native-inputs
		    ,@(standard-packages)))
    ;; Inputs need to be available at build time
    ;; since everything is statically linked.
    (host-inputs inputs)
    (outputs (if (member "debug" outputs)
		 outputs
		 (cons* "debug" outputs)))
    (build zephyr-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (zephyr-build name inputs
		       #:key guile source
		       board
		       bin-name
		       (outputs '("out" "debug")) (configure-flags ''())
		       (search-paths '())
		       (make-flags ''())
		       (out-of-source? #t)
		       (tests? #f)
		       (test-target "test")
		       (parallel-build? #t) (parallel-tests? #t)
		       (validate-runpath? #f)
		       (patch-shebangs? #t)
		       (phases '%standard-phases)
		       (system (%current-system))
		       (substitutable? #t)
		       (imported-modules %zephyr-build-system-modules)
		       (modules '((zephyr build zephyr-build-system)
				  (zephyr build utils)
				  (guix build utils))))
  "Build SOURCE using CMAKE, and with INPUTS. This assumes that SOURCE
provides a 'CMakeLists.txt' file as its build system."
  (define build
    (with-imported-modules imported-modules
      #~(begin
	  (use-modules #$@(sexp->gexp modules))
	  #$(with-build-variables inputs outputs
	      #~(zephyr-build #:source #+source
			      #:name #$name
			      #:system #$system
			      #:outputs %outputs
			      #:inputs %build-inputs
			      #:board #$board
			      #:bin-name #$bin-name
			      #:search-paths '#$(sexp->gexp
						 (map search-path-specification->sexp
						      search-paths))
			      #:phases #$(if (pair? phases)
					     (sexp->gexp phases)
					     phases)
			      #:configure-flags #$(if (pair? configure-flags)
						      (sexp->gexp configure-flags)
						      configure-flags)
			      #:make-flags #$make-flags
			      #:out-of-source? #$out-of-source?
			      #:tests? #$tests?
			      #:test-target #$test-target
			      #:parallel-build? #$parallel-build?
			      #:parallel-tests? #$parallel-tests?
			      #:validate-runpath? #$validate-runpath?
			      #:patch-shebangs? #$patch-shebangs?
			      #:strip-binaries? #f)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
						  system #:graft? #f)))
    (gexp->derivation name build
		      #:system system
		      #:target #f
		      #:graft? #f
		      #:substitutable? substitutable?
		      #:guile-for-build guile)))

(define zephyr-build-system
  (build-system
    (name 'zephyr)
    (description "The standard Zephyr build system")
    (lower lower)))
