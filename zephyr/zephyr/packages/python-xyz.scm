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

(define-module (zephyr packages python-xyz)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages check))

(define-public python-setuptools-scm-7.0.5
  (package
    (name "python-setuptools-scm")
    (version "7.0.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32
                "0i28zghzdzzkm9w8rrjwphggkfs58nh6xnqsjhmqjvqxfypi67h3"))))
    (build-system python-build-system)
    (propagated-inputs (list python-importlib-metadata python-packaging
                             python-setuptools python-tomli
                             python-typing-extensions))
    (native-inputs (list python-pytest python-virtualenv))
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "the blessed package to manage your versions by scm tags")
    (description "the blessed package to manage your versions by scm tags")
    (license license:expat)))
