;;; Guix Zephyr --- Functional package management for ZephyrRTOS
;;; Copyright © 2025 Mitchell Schmeisser <mitchellschmeisser@librem.one>
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
