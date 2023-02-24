(define-module (zephyr packages zephyr)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages check)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages bootloaders)
  #:use-module (zephyr packages python-xyz))

(define-public arm-zephyr-eabi-binutils
  (let ((xbinutils (cross-binutils "arm-zephyr-eabi")))
    (package
      (inherit xbinutils)
      (name "arm-zephyr-eabi-binutils")
      (version "2.38")
      (source
       (origin (method git-fetch)
	       (uri (git-reference
		     (url "https://github.com/zephyrproject-rtos/binutils-gdb")
		     (commit "6a1be1a6a571957fea8b130e4ca2dcc65e753469")))
	       (file-name (git-file-name name version))
	       (sha256 (base32 "0ylnl48jj5jk3jrmvfx5zf8byvwg7g7my7jwwyqw3a95qcyh0isr"))))
      (arguments
       `(#:tests? #f
	 ,@(substitute-keyword-arguments (package-arguments xbinutils)
	     ((#:configure-flags flags)
	      `(cons "--program-prefix=arm-zephyr-eabi-" ,flags)))))
      (native-inputs
       (append
	(list texinfo
	      bison
	      flex
	      gmp
	      dejagnu)
	(package-native-inputs xbinutils)))
      (home-page "https://zephyrproject.org")
      (synopsis "binutils for zephyr RTOS"))))

(define-public isl-0.15
  (package
    (inherit isl)
    (version "0.15")
    (source (origin
	      (method url-fetch)
	      (uri (list (string-append "mirror://sourceforge/libisl/isl-"
					version ".tar.gz")))
	      (sha256
	       (base32
		"11vrpznpdh7w8jp4wm4i8zqhzq2h7nix71xfdddp8xnzhz26gyq2"))))))


(define-public gcc-arm-zephyr-eabi-12
  (let ((xgcc (cross-gcc "arm-zephyr-eabi"
			 #:xgcc gcc-12
			 #:xbinutils arm-zephyr-eabi-binutils)))
    (package
      (inherit xgcc)
      (version "12.1.0")
      (source (origin (method git-fetch)
		      (uri (git-reference
			    (url "https://github.com/zephyrproject-rtos/gcc")
			    (commit "0218469df050c33479a1d5be3e5239ac0eb351bf")))
		      (file-name (git-file-name (package-name xgcc) version))
		      (sha256
		       (base32 "1s409qmidlvzaw1ns6jaanigh3azcxisjplzwn7j2n3s33b76zjk"))
		      (patches
		       (search-patches "gcc-12-cross-environment-variables.patch"
				       "gcc-cross-gxx-include-dir.patch"))))
      (native-inputs
       (modify-inputs (package-native-inputs xgcc)
	 (delete "isl")
	 (prepend flex
		  isl-0.15)))
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
	 ((#:phases phases)
	  `(modify-phases ,phases
	     (add-after 'unpack 'fix-genmultilib
	       (lambda _
		 (substitute* "gcc/genmultilib"
		   (("#!/bin/sh") (string-append "#!" (which "sh"))))
		 #t))
	     (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
	       (lambda* (#:key inputs #:allow-other-keys)
		 (let ((gcc (assoc-ref inputs  "gcc")))
		   ;; Remove the default compiler from CPLUS_INCLUDE_PATH to
		   ;; prevent header conflict with the GCC from native-inputs.
		   (setenv "CPLUS_INCLUDE_PATH"
			   (string-join
			    (delete (string-append gcc "/include/c++")
				    (string-split (getenv "CPLUS_INCLUDE_PATH")
						  #\:))
			    ":"))
		   (format #t
			   "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
			   (getenv "CPLUS_INCLUDE_PATH"))
		   #t)))))
	 ((#:configure-flags flags)
	  `(append (list "--enable-multilib"
			 "--with-newlib"
			 "--with-multilib-list=rmprofile"
			 "--with-host-libstdcxx=-static-libgcc -Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
			 "--enable-plugins"
			 "--disable-decimal-float"
			 "--disable-libffi"
			 "--disable-libgomp"
			 "--disable-libmudflap"
			 "--disable-libquadmath"
			 "--disable-libssp"
			 "--disable-libstdcxx-pch"
			 "--disable-nls"
			 "--disable-shared"
			 "--disable-threads"
			 "--disable-tls"
			 "--with-gnu-ld"
			 "--with-gnu-as"
			 "--enable-initfini-array")
		   (delete "--disable-multilib" ,flags)))))
      (native-search-paths
       (list (search-path-specification
	      (variable "CROSS_C_INCLUDE_PATH")
	      (files '("arm-zephyr-eabi/include")))
	     (search-path-specification
	      (variable "CROSS_CPLUS_INCLUDE_PATH")
	      (files '("arm-zephyr-eabi/include"
		       "arm-zephyr-eabi/c++"
		       "arm-zephyr-eabi/c++/arm-zephyr-eabi")))
	     (search-path-specification
	      (variable "CROSS_LIBRARY_PATH")
	      (files '("arm-zephyr-eabi/lib")))))
      (home-page "https://zephyrproject.org")
      (synopsis "GCC for zephyr RTOS"))))

(define-public arm-zephyr-eabi-newlib
  (package
    (name "newlib")
    (version "3.3")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/zephyrproject-rtos/newlib-cygwin")
		    (commit "4e150303bcc1e44f4d90f3489a4417433980d5ff")))
	      (sha256
	       (base32 "08qwjpj5jhpc3p7a5mbl7n6z7rav5yqlydqanm6nny42qpa8kxij"))))
    (build-system gnu-build-system)
    (arguments
     `(#:out-of-source? #t
       #:configure-flags '("--target=arm-zephyr-eabi"
			   "--enable-headers"
			   "--enable-newlib-io-long-long"
			   "--enable-newlib-io-float"
			   "--enable-newlib-io-c99-formats"
			   "--enable-newlib-retargetable-locking"
			   "--enable-newlib-lite-exit"
			   "--enable-newlib-multithread"
			   "--enable-newlib-register-fini"
			   "--enable-newlib-extra-sections"
			   "--disable-newlib-nano-malloc"
			   "--disable-newlib-nano-formatted-io"
			   "--disable-newlib-wide-orient"
			   "--disable-newlib-fseek-optimization"
			   "--disable-newlib-supplied-syscalls"
			   "--disable-newlib-target-optspace"
			   "--disable-nls")
       #:make-flags '("CFLAGS_FOR_TARGET=\"-O2\"")
       #:phases
       (modify-phases %standard-phases
	 (add-after 'unpack 'fix-references-to-/bin/sh
	   (lambda _
	     (substitute* '("libgloss/arm/cpu-init/Makefile.in"
			    "libgloss/arm/Makefile.in"
			    "libgloss/libnosys/Makefile.in"
			    "libgloss/Makefile.in")
	       (("/bin/sh") (which "sh")))
	     #t)))))
    (native-inputs
     `(("xbinutils" ,arm-zephyr-eabi-binutils)
       ("xgcc" ,gcc-arm-zephyr-eabi-12)
       ("texinfo" ,texinfo)))
    (home-page "https://www.sourceware.org/newlib/")
    (synopsis "C library for use on embedded systems")
    (description "Newlib is a C library intended for use on embedded
systems.  It is a conglomeration of several library parts that are easily
usable on embedded products.")
    (license (license:non-copyleft
	      "https://www.sourceware.org/newlib/COPYING.NEWLIB"))))

(define-public arm-zephyr-eabi-newlib-nano
  (package (inherit arm-zephyr-eabi-newlib)
    (name "newlib-nano")
    (arguments
     (substitute-keyword-arguments (package-arguments arm-zephyr-eabi-newlib)
       ((#:configure-flags flags)
	''("--target=arm-zephyr-eabi"
	   "--enable-multilib"
	   "--enable-newlib-reent-small"
	   "--enable-lite-exit"
	   "--enable-newlib-global-atexit"
	   "--enable-newlib-gcc-libstdcxx"
	   "--enable-newlib-install-in-target"
	   "--enable-newlib-io-float"
	   "--enable-newlib-fvwrite-in-streamio"
	   "--enable-newlib-atexit-dynamic-alloc"
	   "--enable-newlib-global-atexit"
	   "--enable-newlib-multithread"
	   "--enable-newlib-retargetable-locking"
	   "--enable-newlib-extra-sections"
	   "--enable-newlib-target-optspace"
	   "--enable-newlib-nano-malloc"
	   "--enable-newlib-nano-formatted-io"
	   "--enable-newlib-io-c99-formats"
	   "--disable-newlib-io-long-long"
	   "--disable-newlib-supplied-syscalls"
	   "--disable-newlib-fseek-optimization"
	   "--disable-newlib-wide-orient"
	   "--disable-newlib-unbuf-stream-opt"
	   "--disable-lto"
	   "--disable-nls"))
       ((#:make-flags flags)
	''("CFLAGS_FOR_TARGET=\"-mthumb-interwork\""))
       ((#:phases phases)
	`(modify-phases ,phases
	   (add-after 'install 'hardlink-newlib
	     (lambda* (#:key outputs #:allow-other-keys)
	       (let ((out (assoc-ref outputs "out")))
		 ;; The nano.specs file says that newlib-nano files should end
		 ;; in "_nano.a" instead of just ".a".  Note that this applies
		 ;; to all the multilib folders too.
		 (for-each
		  (lambda (file)
		    (link file
			  (string-append
			   ;; Strip ".a" off the end
			   (substring file 0 (- (string-length file) 2))
			   ;; Add "_nano.a" onto the end
			   "_nano.a")))
		  (find-files
		   out
		   "^(libc.a|libg.a|librdimon.a|libstdc\\+\\+.a|libsupc\\+\\+.a)$"))

		 ;; newlib.h is usually in this location instead so both
		 ;; newlib and newlib-nano can be in the toolchain at the same
		 ;; time
		 (mkdir (string-append out "/arm-zephyr-eabi/include/newlib-nano"))
		 (symlink
		  "../newlib.h"
		  (string-append out "/arm-zephyr-eabi/include/newlib-nano/newlib.h"))
		 #t)))))))
    (synopsis "Newlib variant for small systems with limited memory")))

(define (make-libstdc++-arm-zephyr-eabi xgcc newlib)
  (let ((libstdc++ (make-libstdc++ xgcc)))
    (package (inherit libstdc++)
      (name "libstdc++-arm-zephyr-eabi")
      (arguments
       (substitute-keyword-arguments (package-arguments libstdc++)
	 ((#:configure-flags flags)
	  `(append '("--target=arm-zephyr-eabi"
		     "--host=arm-zephyr-eabi"
		     "--disable-libstdcxx-pch"
		     "--enable-multilib"
		     "--with-multilib-list=armv7-m,armv7e-m"
		     "--disable-shared"
		     "--disable-tls"
		     "--disable-plugin"
		     "--with-newlib")
		   ,flags))))
      (native-inputs
       `(("newlib" ,newlib)
	 ("xgcc" ,xgcc)
	 ,@(package-native-inputs libstdc++))))))

(define (arm-zephyr-eabi-toolchain xgcc newlib)
  "Produce a cross-compiler zephyr toolchain package with the compiler XGCC and the C
library variant NEWLIB."
  (let ((newlib-with-xgcc (package (inherit newlib)
			    (native-inputs
			     (alist-replace "xgcc" (list xgcc)
					    (package-native-inputs newlib))))))
    (package
      (name (string-append "arm-zephyr-eabi"
			   (if (string=? (package-name newlib-with-xgcc)
					 "newlib-nano")
			       "-nano" "")
			   "-toolchain"))
      (version (package-version xgcc))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       '(#:modules ((guix build union)
		    (guix build utils))
	 #:builder
	 (begin
	   (use-modules (ice-9 match)
			(guix build union)
			(guix build utils))
	   (let ((out (assoc-ref %outputs "out")))
	     (mkdir-p out)
	     (match %build-inputs
	       (((names . directories) ...)
		(union-build (string-append out "/arm-zephyr-eabi")
			     directories)
		#t))))))
      (propagated-inputs
       `(("binutils" ,arm-zephyr-eabi-binutils)
	 ;; XXX: The zephyr toolchain also includes libc++ but I
	 ;; cannot get it to build. The arm-none-eabi-toolchain also includes
	 ;; libc++ but copy and pasting that code has failed  us this time :(.
	 ;; No C++ is "okay" for now but we need it to run tensor flow.
	 ;; ("libstdc++" ,(make-libstdc++-arm-zephyr-eabi xgcc newlib-with-xgcc))
	 ("gcc" ,xgcc)
	 ("newlib" ,newlib-with-xgcc)))
      (synopsis "Complete GCC tool chain for ARM zephyrRTOS development")
      (description "This package provides a complete GCC tool chain for ARM
bare metal development with zephyr rtos.  This includes the GCC arm-zephyr-eabi cross compiler
and newlib (or newlib-nano) as the C library.  The supported programming
language is C.")
      (home-page (package-home-page xgcc))
      (license (package-license xgcc)))))

(define-public arm-zephyr-eabi-toolchain-12
  (arm-zephyr-eabi-toolchain
   gcc-arm-zephyr-eabi-12
   arm-zephyr-eabi-newlib))

(define-public arm-zephyr-eabi-nano-toolchain-12
  (arm-zephyr-eabi-toolchain
   gcc-arm-zephyr-eabi-12
   arm-zephyr-eabi-newlib-nano))

(define-public gdb-arm-zephyr-eabi
  ;; XXX: Does not work... I do not know why.
  (package
    (inherit gdb-12)
    (name "gdb-arm-zephyr-eabi")
    (version "12.1")
    (source
     (origin (method git-fetch)
	     (uri (git-reference
		   (url "https://github.com/zephyrproject-rtos/binutils-gdb")
		   (commit "db8bd068edeaac983273727d9322fc1867702d15")))
	     (file-name (git-file-name name version))
	     (sha256 (base32 "082lh9p3jcqnmk31yr7zmd1yrpp4z49xljg06rrfipk8x0z2sv23"))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ,@(package-native-inputs gcc-12)))
    (arguments
     (substitute-keyword-arguments (package-arguments gdb-12)
       ((#:configure-flags flags)
	`(cons* "--target=arm-zephyr-eabi"
		"--enable-multilib"
		"--enable-interwork"
		"--enable-languages=c"
		"--disable-nls"
		(delete "--enable-languages=" ,flags)))))))

(define-public arm-zephyr-eabi-sdk
  (package
    (name "arm-zephyr-eabi-sdk")
    (version "0.15.0")
    (home-page "https://zephyrproject.org")
    (source (origin (method git-fetch)
		    (uri (git-reference
			  (url "https://github.com/zephyrproject-rtos/sdk-ng")
			  (commit "v0.15.0")))
		    (file-name (git-file-name name version))
		    (sha256 (base32 "04gsvh20y820dkv5lrwppbj7w3wdqvd8hcanm8hl4wi907lwlmwi"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build union)
		  (guix build utils))
       #:builder
       (begin
	 (use-modules (guix build union)
		      (ice-9 match)
		      (guix build utils))
	 (let* ((out (assoc-ref %outputs "out"))
		(cmake-scripts (string-append (assoc-ref %build-inputs "source")
					      "/cmake"))
		(sdk-out (string-append out "/zephyr-sdk-0.15.0")))
	   (mkdir-p out)

	   (match (assoc-remove! %build-inputs "source")
	     (((names . directories) ...)
	      (union-build sdk-out directories)))

	   (copy-recursively cmake-scripts
			     (string-append sdk-out "/cmake"))

	   (with-directory-excursion sdk-out
	     (call-with-output-file "sdk_version"
	       (lambda (p)
		 (format p "0.15.0")))
	     #t)))))
    (propagated-inputs
     (list
      arm-zephyr-eabi-nano-toolchain-12
      gdb-arm-zephyr-eabi
      dtc
      qemu))
    (native-search-paths
     (list (search-path-specification
	    (variable "ZEPHYR_SDK_INSTALL_DIR")
	    (files '("zephyr-sdk-0.15.0")))))
    (synopsis "SDK for zephyrRTOS")
    (description "zephyr-sdk contains bundles a complete gcc toolchain as well
as host tools like dtc, openocd, and qemu.")
    (license license:apsl2)))


(define-public zephyr-3.1
  (let ((version "3.1.0")
	(commit "zephyr-v3.1.0"))
    (package
      (name "zephyr")
      (version (git-version version "0" commit))
      (home-page "https://zephyrproject.org")
      (source (origin (method git-fetch)
		      (uri (git-reference
			    (url "https://github.com/zephyrproject-rtos/zephyr")
			    (commit commit)))
		      (file-name (git-file-name name version))
		      (sha256
		       (base32 "1yl5y9757xc3l037k3g1dynispv6j5zqfnzrhsqh9cx4qzd485lx"))
		      (patches
		       (search-patches "zephyr-3.1-linker-gen-abs-path.patch"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
	 '(("." "zephyr-workspace/zephyr"))
	 #:phases
	 (modify-phases %standard-phases
	   (add-after 'unpack 'patch-cmake-scripts
	     (lambda* _
	       (format #t "~a~&" (getcwd))
	       ;; Some cmake scripts assume the presence of a
	       ;; git repository in the source directory.
	       ;; We will just hard-code that information now
	       (substitute* "CMakeLists.txt"
		 (("if\\(DEFINED BUILD_VERSION\\)" all)
		  (format #f "set(BUILD_VERSION \"~a-~a\")~&~a"
			  ,version ,commit all))))))))
      (propagated-inputs
       (list python-3
	     python-pyelftools
	     python-pykwalify
	     python-pyyaml
	     python-packaging))
      (native-search-paths
       (list (search-path-specification
	      (variable "ZEPHYR_BASE")
	      (files '("zephyr-workspace/zephyr")))))
      (synopsis "Source code for zephyr rtos")
      (description "Zephyr rtos source code.")
      (license license:apsl2))))


(define-public zephyr-3.2.0-rc3
  (package (inherit zephyr-3.1)
    (version "3.2.0-rc3")
    (source (origin (method git-fetch)
		    (uri (git-reference
			  (url "https://github.com/zephyrproject-rtos/zephyr")
			  (commit "v3.2.0-rc3")))
		    (file-name (git-file-name "zephyr" version))
		    (sha256
		     (base32 "06ksd9zj4j19jq0zg3lms13jx0gxzjc41433zgb91cnd2cqmn5cb"))
		    (patches
		     (search-patches "zephyr-3.1-linker-gen-abs-path.patch"))))))

(define-public imgtool
  (package
    (name "imgtool")
    (version "1.9.0")
    (source (origin
	      (method url-fetch)
	      (uri (pypi-uri "imgtool" version))
	      (sha256
	       (base32
		"0hsa2gly17crxxyn1dy55yyhcqkbw5w7993yl3zvasghdfyzd9vz"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cbor2 python-click python-cryptography
			     python-intelhex))
    (home-page "http://github.com/mcu-tools/mcuboot")
    (synopsis "MCUboot's image signing and key management")
    (description "MCUboot's image signing and key management")
    (license license:apsl2)))
