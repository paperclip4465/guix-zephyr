(define-module (zephyr packages guile-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages texinfo))

(define-public guile-mcumgr
  (let ((commit "a1a821eeb816d579fa7b3ce940e194f514856b7d"))
    (package
      (name "guile-mcumgr")
      (version (git-version "0.0.1" "0" commit))
      (home-page "https://github.com/paperclip4465/guile-mcumgr")
      (source (origin (method git-fetch)
		      (uri (git-reference
			    (url "https://github.com/paperclip4465/guile-mcumgr")
			    (commit commit)))
		      (file-name (git-file-name name version))
		      (sha256
		       (base32 "0d96lffcbg4dr13mj9dzn2ir03g82riwv74mj3midx385h3p419r"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules
	 ((ice-9 match)
	  (ice-9 ftw)
	  ,@%gnu-build-system-modules)
	 #:phases
	 (modify-phases %standard-phases
	   (add-after 'install 'hall-wrap-binaries
	     (lambda* (#:key inputs outputs #:allow-other-keys)
	       (let* ((compiled-dir
		       (lambda (out version)
			 (string-append
			  out
			  "/lib/guile/"
			  version
			  "/site-ccache")))
		      (uncompiled-dir
		       (lambda (out version)
			 (string-append
			  out
			  "/share/guile/site"
			  (if (string-null? version) "" "/")
			  version)))
		      (dep-path
		       (lambda (env modules path)
			 (list env
			       ":"
			       'prefix
			       (cons modules
				     (map (lambda (input)
					    (string-append
					     (assoc-ref inputs input)
					     path))
					  ,''("guile-cbor" "guile-gcrypt"))))))
		      (out (assoc-ref outputs "out"))
		      (bin (string-append out "/bin/"))
		      (site (uncompiled-dir out "")))
		 (match (scandir site)
		   (("." ".." version)
		    (for-each
		     (lambda (file)
		       (wrap-program
			   (string-append bin file)
			 (dep-path
			  "GUILE_LOAD_PATH"
			  (uncompiled-dir out version)
			  (uncompiled-dir "" version))
			 (dep-path
			  "GUILE_LOAD_COMPILED_PATH"
			  (compiled-dir out version)
			  (compiled-dir "" version))))
		     ,''("mcumgr-cli"))
		    #t))))))))
      (native-inputs
       (list autoconf
	     automake
	     pkg-config
	     texinfo
	     guile-3.0
	     guile-config
	     guile-cbor
	     guile-gcrypt
	     guile-hall))
      (inputs `(("guile" ,guile-3.0)))
      (propagated-inputs
       (list guile-config
	     guile-cbor
	     guile-gcrypt))
      (synopsis
       "The mcumgr smp protocol is used to manage microcontrollers.")
      (description
       "guile implementation of the mcumgr protocol.")
      (license license:gpl3+))))
