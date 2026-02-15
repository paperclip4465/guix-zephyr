;;; Guix Zephyr --- Functional package management for ZephyrRTOS
;;; Copyright © 2023,2025 Mitchell Schmeisser <mitchellschmeisser@librem.one>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (zephyr build zephyr-build-system)
  #:use-module ((guix build cmake-build-system) #:prefix cmake:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-26)
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
  (define (zephyr-copy dest ext)
    (let ((orig (string-append "zephyr/zephyr." ext)))
      (if (file-exists? orig)
          (copy-file orig (string-append dest "/" bin-name "." ext))
          (format #t "Skipping ~a. File does not exist!\n" orig))))

  (let* ((out (string-append (assoc-ref outputs "out") "/firmware"))
         (dbg (string-append (assoc-ref outputs "debug") "/share/zephyr")))
    (map mkdir-p (list out dbg))
    (map (cut zephyr-copy out <>) output-extensions)
    (map (cut zephyr-copy dbg <>) debug-extensions)
    (copy-file "zephyr/.config" (string-append dbg "/config"))))

(define %standard-phases
  (modify-phases cmake:%standard-phases
    (replace 'configure configure)
    (replace 'install install)))


(define* (zephyr-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  (apply cmake:cmake-build
         #:inputs inputs #:phases phases args))
