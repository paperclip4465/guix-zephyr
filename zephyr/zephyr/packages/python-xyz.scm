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

(define-public python-cbor2
  (package
    (name "python-cbor2")
    (version "5.4.3")
    (source (origin
	      (method url-fetch)
	      (uri (pypi-uri "cbor2" version))
	      (sha256
	       (base32
		"1hj6wllgwdsj2jc88fsxk585adsg930z6j79mwr41vbcxv2n7f32"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-wheel))
    (native-inputs (list python-setuptools-scm-7.0.5
			 python-pytest
			 python-pytest-cov))
    (home-page "")
    (synopsis "CBOR (de)serializer with extensive tag support")
    (description "CBOR (de)serializer with extensive tag support")
    (license license:expat)))
