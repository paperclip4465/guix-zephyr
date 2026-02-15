;;; Guix Zephyr --- Functional package management for ZephyrRTOS
;;; Copyright © 2023 Mitchell Schmeisser <mitchellschmeisser@librem.one>
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
