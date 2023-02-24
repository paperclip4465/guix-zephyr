(define-module (zephyr packages zephyr-xyz)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (ice-9 format)
  #:use-module (zephyr packages zephyr)
  #:use-module (zephyr build-system zephyr)
  #:use-module (zephyr build-system zephyr-module)
  #:export (make-mcuboot))


(define-public hal-nxp
  (let ((module-path "/modules/hal/nxp")
	(commit "708c95825b0d5279620935a1356299fff5dfbc6e"))
    (package
      (name "hal-nxp")
      (version (git-version "3.1.0" "0" commit))
      (home-page "https://nxp.com")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/zephyrproject-rtos/hal_nxp")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32 "1yb7apbg9hpqz0lvca0r8wzr4zg3fdnzzsahkkx69d64j0vkwkcz"))))
      (build-system zephyr-module-build-system)
      (arguments `(#:workspace-path ,module-path))
      (native-search-paths
       (list (search-path-specification
	      (variable "ZEPHYR_MODULES")
	      (files `(,(string-append %zephyr-workspace-name module-path)))
	      (separator ";"))))
      (synopsis "Zephyr module for NXP Hardware Abstraction Layer")
      (description "Provides sources for NXP HAL zephyr module")
      (license license:bsd-3))))


(define-public hal-cmsis
  (let ((module-path "/modules/hal/cmsis")
	(commit "5f86244bad4ad5a590e084f0e72ba7a1416c2edf"))
    (package
      (name "hal-cmsis")
      (version (git-version "5.8.0" "3.1.0" commit))
      (home-page "https://developer.arm.com/tools-and-software/embedded/cmsis")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/zephyrproject-rtos/cmsis")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32 "0f7cipnwllna7iknsnz273jkvrly16yr6wm4y2018i6njpqh67wi"))))
      (build-system zephyr-module-build-system)
      (arguments `(#:workspace-path ,module-path))
      (native-search-paths
       (list (search-path-specification
	      (variable "ZEPHYR_MODULES")
	      (files `(,(string-append %zephyr-workspace-name module-path)))
	      (separator ";"))))
      (synopsis "Zephyr module providing the Common Microcontroller
Software Interface Standard")
      (description "Zephyr module providing the Common Microcontroller
Software Interface Standard")
      (license license:apsl2))))

(define-public hal-cmsis-3.2.0-rc3
  (let ((commit "093de61c2a7d12dc9253daf8692f61f793a9254a"))
    (package (inherit hal-cmsis)
      (version (git-version "5.8.0" "3.2.0-rc3" commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/zephyrproject-rtos/cmsis")
		      (commit commit)))
		(file-name (git-file-name "hal-cmsis" version))
		(sha256
		 (base32 "0354fz5lb2vg3zj0ciwjmz4slh3s5rnvkmixikn36m51wk8vcq1j")))))))

(define-public zcbor
  (let ((module-path "/modules/lib/zcbor")
	(commit "882c489a7d9fdfff31d27666914a78a9eb6976d7"))
    (package
      (name "zcbor")
      (version (git-version "0.3.0" "3.1.0" commit))
      (home-page "https://github.com/zephyrproject-rtos/zcbor")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/zephyrproject-rtos/zcbor")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32 "0z5j18h0m0pssmb8g6gr5w524qvjidm7xykk1kqmy1ipakl9bpvd"))))
      (build-system zephyr-module-build-system)
      (arguments `(#:workspace-path ,module-path))
      (native-search-paths
       (list (search-path-specification
	      (variable "ZEPHYR_MODULES")
	      (files `(,(string-append %zephyr-workspace-name module-path)))
	      (separator ";"))))
      (synopsis "Zephyr module providing a lightweight CBOR implementation")
      (description "zcbor is a low footprint CBOR library in the C language
that comes with a schema-driven script tool that can validate your data, or
even generate code for you. Aside from the script, the CBOR library is a
standalone library which is tailored for use in microcontrollers.")
      (license license:apsl2))))

(define-public zcbor-0.5.1
  (let ((commit "a0d6981f14d4001d6f0d608d1a427f9bc6bb6d02"))
    (package
      (inherit zcbor)
      (version (git-version "0.5.1" "3.2.0-rc3" commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/zephyrproject-rtos/zcbor")
		      (commit commit)))
		(file-name (git-file-name "zcbor" version))
		(sha256
		 (base32 "03xz79pi210kny55ks9cyr8i9m68f7kla3nv5zk2afg59ms0nwdc")))))))

(define-public zephyr-mcuboot
  (let ((module-path "/bootloader/mcuboot")
	(commit "e58ea98aec6e5539c5f872a98059e461d0155bbb"))
    (package
      (name "zephyr-mcuboot")
      (version (git-version "1.10.0" "3.1.0" commit))
      (home-page "https://mcuboot.com")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/zephyrproject-rtos/mcuboot")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32 "0mmv4wb3w9b66sljc3scckqb7bv54w95rd7dhqq8qmy4420n5vnd"))))
      (build-system zephyr-module-build-system)
      (arguments `(#:workspace-path ,module-path))
      (propagated-inputs
       (list zcbor))
      (native-search-paths
       (list (search-path-specification
	      (variable "ZEPHYR_MODULES")
	      (files `(,(string-append %zephyr-workspace-name module-path)))
	      (separator ";"))))
      (synopsis "Secure 32 bit bootloader built for ZephyrRTOS")
      (description "MCUboot is a secure bootloader for 32-bit
microcontrollers. It defines a common infrastructure for the
bootloader and the system flash layout on microcontroller systems, and
provides a secure bootloader that enables easy software upgrade.")
      (license license:apsl2))))


(define* (make-mcuboot board key #:key
		       (mcuboot zephyr-mcuboot)
		       (zephyr-base zephyr-3.1)
		       (extra-zephyr-modules '())
		       (extra-configure-flags '()))
  "Create a variant of MCUBOOT configured for BOARD and KEY.
KEY is the PEM signing key that will be expected to sign application
binaries. EXTRA-CONFIGURE-FLAGS can be given as a string, a list of strings,
or a file-like object to be used as an overlay."

  (define (parse-extra-flags)
    (cond
     ((file-like? extra-configure-flags)
      #~(string-append "-DOVERLAY_CONFIG=" #$extra-configure-flags))
     ((string? extra-configure-flags) (list extra-configure-flags))
     ((or (nil? extra-configure-flags)
	  (pair? extra-configure-flags)) extra-configure-flags)))

  (package (inherit mcuboot)
    (name (string-append (package-name mcuboot) "-" board))
    (build-system zephyr-build-system)
    (inputs extra-zephyr-modules)
    (native-inputs
     `(("imgtool" ,imgtool)
       ("signing-key" ,key)))
    (outputs '("out" "debug"))
    (arguments
     `(#:phases ,#~(modify-phases %standard-phases
		     (add-before 'configure 'cd-to-source
		       (lambda* _
			 (chdir "boot/zephyr")
			 #t)))
       #:configure-flags
       (append (list (string-append  "-DBOARD=" ,board)
		     (format #f "-DCONFIG_BOOT_SIGNATURE_KEY_FILE=~s"
			     (assoc-ref %build-inputs "signing-key")))
	       ',extra-configure-flags)))
    (description (format #f "~a~&This bootloader has been configured for ~a."
			 (package-description mcuboot) board))))


(define-public zephyr-hello-world-frdm-k64f
  (package
    (name "zephyr-hello-world-frdm-k64f")
    (version (package-version zephyr-3.1))
    (home-page "https://zephyrproject.org")
    (source (file-append (package-source zephyr-3.1)
			 "/samples/hello_world"))
    (build-system zephyr-build-system)
    (arguments
     '(#:configure-flags '("-DBOARD=frdm_k64f")))
    (outputs '("out" "debug"))
    (inputs
     (list hal-cmsis
	   hal-nxp))
    (synopsis "Hello world example from Zephyr Project")
    (description "Sample package for zephyr project")
    (license license:apsl2)))


(define-public zephyr-hello-world-newlib-frdm-k64f
  (package
    (inherit zephyr-hello-world-frdm-k64f)
    (name "zephyr-hello-world-newlib-frdm-k64f")
    (arguments
     (substitute-keyword-arguments (package-arguments zephyr-hello-world-frdm-k64f)
       ((#:configure-flags flags)
	`(append
	  '("-DCONFIG_MINIMAL_LIBC=n"
	    "-DCONFIG_NEWLIB_LIBC=y")
	  ,flags))))))

(define-public mcuboot-frdm-k64f
  (make-mcuboot "frdm_k64f"
		;; Use special dev key instead of production
		(local-file "../../ecdsap256-dev.pem")
		#:extra-zephyr-modules (list hal-cmsis hal-nxp)
		#:extra-configure-flags
		'( ;; k64 doesn't have fancy crypto hardware
		  ;; so we cannot use RSA keys.
		  "-DCONFIG_BOOT_SIGNATURE_TYPE_ECDSA_P256=y"
		  "-DCONFIG_BOOT_SIGNATURE_TYPE_RSA=n"
		  "-DCONFIG_BOOT_ECDSA_TINYCRYPT=y")))
