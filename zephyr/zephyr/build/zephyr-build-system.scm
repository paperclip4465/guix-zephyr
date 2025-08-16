(define-module (zephyr build zephyr-build-system)
  #:use-module ((guix build cmake-build-system) #:prefix cmake:)
  #:use-module (guix build utils)
  #:use-module (zephyr build utils)
  #:export (%standard-phases
	    zephyr-build))

;; Commentary:
;;
;; Builder-side code of the standard zephyr build procedure
;;
;; Code:

(define* (configure #:key (configure-flags '())
		    board
		    (build-location "../build")
		    build-type
		    inputs (out-of-source? #t)
		    #:allow-other-keys)
  "Configure the given package."
  (let* ((abs-srcdir (getcwd))
	 (srcdir     abs-srcdir))
    (format #t "source directory: ~s (relative from build: ~s)~%"
	    abs-srcdir srcdir)
    (when out-of-source?
      (mkdir build-location)
      (chdir build-location))
    (format #t "build directory: ~s~%" (getcwd))
    (setenv "XDG_CACHE_HOME" (getcwd))

    (let ((args (configure-args #:configure-flags configure-flags
                                #:board board
                                #:build-location build-location
                                #:source-location srcdir
                                #:build-type build-type
                                #:inputs inputs)))
      (format #t "running 'cmake' with arguments ~s~%" args)
      (apply invoke "cmake" args))))


(define* (install #:key bin-name outputs inputs
                  output-extensions
                  debug-extensions
                  #:allow-other-keys)
  (let* ((out (string-append (assoc-ref outputs "out") "/firmware"))
         (dbg (string-append (assoc-ref outputs "debug") "/share/zephyr")))
    (mkdir-p out)
    (mkdir-p dbg)
    (map (lambda (ext)
           (let ((orig (string-append "zephyr/zmk." ext)))
             (if (file-exists? orig)
                 (copy-file orig (string-append out "/" bin-name "." ext))
                 (format #t "Skipping ~a. File does not exist!\n" orig))))
         output-extensions)
    (map (lambda (ext)
           (let ((orig (string-append "zephyr/zmk." ext)))
             (if (file-exists? orig)
                 (copy-file orig (string-append dbg "/" bin-name "." ext))
                 (format #t "Skipping ~a. File does not exist!\n" orig))))
         debug-extensions)
    (copy-file "zephyr/.config" (string-append dbg "/config"))))

(define %standard-phases
  (modify-phases cmake:%standard-phases
    (replace 'configure configure)
    (replace 'install install)))


(define* (zephyr-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  (apply cmake:cmake-build
         #:inputs inputs #:phases phases args))
