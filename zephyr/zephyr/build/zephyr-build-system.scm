(define-module (zephyr build zephyr-build-system)
  #:use-module ((guix build cmake-build-system) #:prefix cmake:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export (%standard-phases
	    find-zephyr-modules
	    zephyr-build
	    zephyr-modules-cmake-argument))

;; Commentary:
;;
;; Builder-side code of the standard zephyr build procedure
;;
;; Code:

(define* (find-zephyr-modules directories)
  "Return the list of directories containing zephyr/module.yml found
under DIRECTORY, recursively. Return the empty list if DIRECTORY is
not accessible."
  (define (module-directory file)
    (dirname (dirname file)))

  (define (enter? name stat result)
    ;; Skip version control directories.
    (not (member (basename name) '(".git" ".svn" "CVS"))))

  (define (leaf name stat result)
    ;; Add module root directory to results
    (if (and (string= "module.yml" (basename name))
	     (string= "zephyr" (basename (dirname name))))
	(cons (module-directory name) result)
	result))

  (define (down name stat result) result)
  (define (up name stat result) result)
  (define (skip name stat result) result)

  (define (find-modules directory)
    (file-system-fold enter? leaf down up skip error
		      '() (canonicalize-path directory)
		      stat))

  (append-map find-modules directories))

(define (zephyr-modules-cmake-argument modules)
  (format #f "-DZEPHYR_MODULES='~{~a~^;~}'" modules))

(define* (configure #:key (configure-flags '())
		    board
		    inputs (out-of-source? #t)
		    build-type
		    #:allow-other-keys)
  "Configure the given package."
  (let* ((abs-srcdir (getcwd))
	 (srcdir     (if out-of-source?
			 (string-append "../" (basename abs-srcdir))
			 ".")))
    (format #t "source directory: ~s (relative from build: ~s)~%"
	    abs-srcdir srcdir)
    (when out-of-source?
      (mkdir "../build")
      (chdir "../build"))
    (format #t "build directory: ~s~%" (getcwd))
    (setenv "XDG_CACHE_HOME" (getcwd))

    (let ((args `(,srcdir
		  ,@(if build-type
			(list (string-append "-DCMAKE_BUILD_TYPE="
					     build-type))
			'())
		  ;; enable verbose output from builds
		  "-DCMAKE_VERBOSE_MAKEFILE=ON"
		  ,@(if board
			(list (string-append "-DBOARD=" board))
			'())
		  ,(zephyr-modules-cmake-argument
		    (find-zephyr-modules (map cdr inputs)))
		  ,@configure-flags)))
      (format #t "running 'cmake' with arguments ~s~%" args)
      (apply invoke "cmake" args))))

(define* (install #:key bin-name outputs inputs #:allow-other-keys)
  (let* ((out (string-append (assoc-ref outputs "out") "/firmware"))
	 (dbg (string-append (assoc-ref outputs "debug") "/share/zephyr")))
    (mkdir-p out)
    (mkdir-p dbg)
    (copy-file "zephyr/.config" (string-append dbg "/config"))
    (copy-file "zephyr/zephyr.map" (string-append dbg "/" bin-name ".map"))
    (copy-file "zephyr/zephyr.elf" (string-append out "/" bin-name ".elf"))
    (copy-file "zephyr/zephyr.bin" (string-append out "/" bin-name ".bin"))))

(define %standard-phases
  (modify-phases cmake:%standard-phases
    (replace 'configure configure)
    (replace 'install install)))

(define* (zephyr-build #:key inputs (phases %standard-phases)
		       #:allow-other-keys #:rest args)
  (apply cmake:cmake-build
	 #:inputs inputs #:phases phases args))
