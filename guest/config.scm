;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;;
;;; This file is part of GNU Guix.
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

(define-module (guest config)
  #:export (%guest-package-name
	    %guest-version
	    %guest-bug-report-address
	    %guest-home-page-url))

;;; Commentary:
;;;
;;; Compile-time configuration of Guest.
;;;
;;; Code:

(define %guest-package-name
  "Guest")

(define %guest-version
  "0.0.1")

(define %guest-bug-report-address
  "mitchellschmeisser@librem.one")

(define %guest-home-page-url
  "https://github.com/paperclip4465/guix-zephyr")

;;; config.scm ends here
