(define-module (zephyr packages guest)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo))

(define-public guest
  (let ((commit "98c2e428753c99ee419a1d4c955da13f7030a9a1"))
    (package
      (name "guest")
      (version "0.0.2")
      (home-page "https://github.com/paperclip4465/guest-zephyr")
      (source (origin (method git-fetch)
		      (uri (git-reference
			    (url "https://github.com/paperclip4465/guest")
			    (commit commit)))
		      (file-name (git-file-name name version))
		      (sha256
		       (base32 "1qxvbjkvy5gyjl4hyag3pbs51lxw7h06wfjm7c9m9015gv1bxblr"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules
	 ((ice-9 match)
	  (ice-9 ftw)
	  ,@%gnu-build-system-modules)
	 #:phases
	 (modify-phases
	     %standard-phases
	   (add-after
	       'install
	       'hall-wrap-binaries
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
					  ,''("guile-libyaml"))))))
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
		     ,''("guest"))
		    #t))))))))
      (inputs
       (list guile-3.0
	     guile-libyaml))
      (native-inputs
       (append
	(list autoconf
	      automake
	      pkg-config
	      texinfo)
	(if (%current-target-system)
	    ;; add guile/libs for cross compilation support
	    (list guile-3.0
		  guile-libyaml)
	    '())))
      (native-search-paths
       (list (search-path-specification
	      (variable "ZEPHYR_MODULE_PATH")
	      (files '("zephyr-workspace/modules")))))
      (synopsis "Project meta-tool for Zephyr RTOS")
      (description "Guest is a West replacement implemented in GNU Guile.")
      (license license:gpl3+))))

(define guest-local
  (package
    (inherit guest)
    (source (local-file "../../../guest-0.0.1.tar.gz"))))

guest-local
