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
binaries. EXTRA-CONFIGURE-FLAGS can be given as a string or a list of strings."

  (package (inherit mcuboot)
	   (name (string-append (package-name mcuboot) "-" board))
	   (build-system zephyr-build-system)
	   (inputs extra-zephyr-modules)
	   (native-inputs
	    `(("imgtool" ,imgtool)
	      ("signing-key" ,key)))
	   (outputs '("out" "debug"))
	   (arguments
	    `(#:phases (modify-phases %standard-phases
			 (add-before 'configure 'cd-to-source
			   (lambda* _
			     (chdir "boot/zephyr")
			     #t)))
	      #:bin-name "mcuboot"
	      #:board ,board
	      #:zephyr ,zephyr-base
	      #:configure-flags
	      (append (list (format #f "-DCONFIG_BOOT_SIGNATURE_KEY_FILE=~s"
				    (assoc-ref %build-inputs "signing-key")))
		      ',(cond
			 ((string? extra-configure-flags) `(list ,extra-configure-flags))
			 ((pair? extra-configure-flags) extra-configure-flags)
			 (else (error "Bad extra configure flags for mcuboot ~a."
				      extra-configure-flags))))))
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
		'(;; k64 doesn't have fancy crypto hardware
		  ;; so we cannot use RSA keys.
		  "-DCONFIG_BOOT_SIGNATURE_TYPE_ECDSA_P256=y"
		  "-DCONFIG_BOOT_SIGNATURE_TYPE_RSA=n"
		  "-DCONFIG_BOOT_ECDSA_TINYCRYPT=y")))

(define-public canopennode
  (package
    (name "canopennode")
    (version "0.0-0.53d3415")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/canopennode")
	     (commit
	      "53d3415c14d60f8f4bfca54bfbc5d5a667d7e724")))
       (file-name "canopennode-53d3415c14-checkout")
       (sha256
	(base32
	 "1vqrx1zi2wbvnwza4z39nrd33j9dxpp9j9rnn12r1gdwxfjazbw0"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/canopennode"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public chre
  (package
    (name "chre")
    (version "0.0-0.ef76d34")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/chre")
	     (commit
	      "ef76d3456db07e4959df555047d6962279528c8d")))
       (file-name "chre-ef76d3456d-checkout")
       (sha256
	(base32
	 "00jnv58slzgp4srmsamd7x34yf0hj1c6agmws4csv7w28013bwfm"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/chre"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public cmsis
  (package
    (name "cmsis")
    (version "0.0-0.5f86244")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/cmsis")
	     (commit
	      "5f86244bad4ad5a590e084f0e72ba7a1416c2edf")))
       (file-name "cmsis-5f86244bad-checkout")
       (sha256
	(base32
	 "0f7cipnwllna7iknsnz273jkvrly16yr6wm4y2018i6njpqh67wi"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/cmsis"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public edtt
  (package
    (name "edtt")
    (version "0.0-0.1ea61a3")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/edtt")
	     (commit
	      "1ea61a390d2bfcf3b2ecdba8f8b0b98dfdffbd11")))
       (file-name "edtt-1ea61a390d-checkout")
       (sha256
	(base32
	 "10cpksv0imsd0jbyr2pag63agvfx9dg4028x3cxpvfvscizz4dpx"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "tools/edtt"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public fatfs
  (package
    (name "fatfs")
    (version "0.0-0.a30531a")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/fatfs")
	     (commit
	      "a30531af3a95a9a3ea7d771ea8a578ebfed45514")))
       (file-name "fatfs-a30531af3a-checkout")
       (sha256
	(base32
	 "04c33x1am766sdszmsmxd2rii6snyld68ca17qhg3ml2xiqy1z31"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/fs/fatfs"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public fff
  (package
    (name "fff")
    (version "0.0-0.6ce5ba2")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/fff")
	     (commit
	      "6ce5ba26486e93d5b7696a3e23f0585932c14b16")))
       (file-name "fff-6ce5ba2648-checkout")
       (sha256
	(base32
	 "1wwrsicibk5nrzj7arxd4a22qr596vnc1ygp9zi409ygqvmzayzy"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "modules/lib/fff"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_altera
  (package
    (name "hal_altera")
    (version "0.0-0.0d225dd")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_altera")
	     (commit
	      "0d225ddd314379b32355a00fb669eacf911e750d")))
       (file-name "hal_altera-0d225ddd31-checkout")
       (sha256
	(base32
	 "05f5a1sk8mp8vhbdsj31833n10kyd91f93hgk2zr62xc0kfbwq0z"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/altera"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_atmel
  (package
    (name "hal_atmel")
    (version "0.0-0.bb4e710")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_atmel")
	     (commit
	      "bb4e7104132bdc22c7cc7e20057434c2979e6706")))
       (file-name "hal_atmel-bb4e710413-checkout")
       (sha256
	(base32
	 "10sqdx1sasf3nnpp35jm4sj8i91izm2lb4kymy238jbafp1iggqg"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/atmel"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_espressif
  (package
    (name "hal_espressif")
    (version "0.0-0.22b4700")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_espressif")
	     (commit
	      "22b47009dc27bdd395c6ef1a66c347037a2fb585")))
       (file-name "hal_espressif-22b47009dc-checkout")
       (sha256
	(base32
	 "1c940bp4d07igi68y3fa61f0rchnhzk2mqqhmyvbj8ml31g85h35"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/espressif"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_gigadevice
  (package
    (name "hal_gigadevice")
    (version "0.0-0.7f15468")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_gigadevice")
	     (commit
	      "7f15468b07807ae6a913e0e9cbabfaa23a206bc6")))
       (file-name "hal_gigadevice-7f15468b07-checkout")
       (sha256
	(base32
	 "0br7j45w6s8zc3050nrgd6mvq08gdn9z8svmhbn1c26ipv62npzd"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/gigadevice"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_infineon
  (package
    (name "hal_infineon")
    (version "0.0-0.4af0696")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_infineon")
	     (commit
	      "4af06965f57ba1e7d170e6a97d24c33785543a8c")))
       (file-name "hal_infineon-4af06965f5-checkout")
       (sha256
	(base32
	 "1wgvdkib605j0hnyrlnwikb6zri0v3l8pfxdiazbaz07rqnc11vb"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/infineon"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_microchip
  (package
    (name "hal_microchip")
    (version "0.0-0.5d079f1")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_microchip")
	     (commit
	      "5d079f1683a00b801373bbbbf5d181d4e33b30d5")))
       (file-name "hal_microchip-5d079f1683-checkout")
       (sha256
	(base32
	 "11basljy0fsgcp3l7zvxklw6zqkmahm97pi156ndadrhr8g4v5q8"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/microchip"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_nordic
  (package
    (name "hal_nordic")
    (version "0.0-0.2e1c828")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_nordic")
	     (commit
	      "2e1c828cf4efb71679aeaec94128708a5353e031")))
       (file-name "hal_nordic-2e1c828cf4-checkout")
       (sha256
	(base32
	 "14ahxbz92dyqjv7pgsjsln2nrv4092645l25i2ix6gs2dqqgrwfi"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/nordic"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_nuvoton
  (package
    (name "hal_nuvoton")
    (version "0.0-0.b4d31f3")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_nuvoton")
	     (commit
	      "b4d31f33238713a568e23618845702fadd67386f")))
       (file-name "hal_nuvoton-b4d31f3323-checkout")
       (sha256
	(base32
	 "0942ainpvf64878vkwh9sx4bgwzmf98d40wqa125qmczlbb0d7my"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/nuvoton"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_nxp
  (package
    (name "hal_nxp")
    (version "0.0-0.9512229")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_nxp")
	     (commit
	      "9512229c35e230003c64fda41be578f2718b70f4")))
       (file-name "hal_nxp-9512229c35-checkout")
       (sha256
	(base32
	 "0ayqj41b3wj6kq060y8qkya1n669xwfwralx1kj8r4dc2x3v3wwm"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "modules/hal/nxp"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_openisa
  (package
    (name "hal_openisa")
    (version "0.0-0.40d049f")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_openisa")
	     (commit
	      "40d049f69c50b58ea20473bee14cf93f518bf262")))
       (file-name "hal_openisa-40d049f69c-checkout")
       (sha256
	(base32
	 "1kvb6pc28klw5pplfp3hbrznvi70gvzq0qjiz6ji256qbflj0437"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/openisa"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_quicklogic
  (package
    (name "hal_quicklogic")
    (version "0.0-0.b3a66fe")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_quicklogic")
	     (commit
	      "b3a66fe6d04d87fd1533a5c8de51d0599fcd08d0")))
       (file-name "hal_quicklogic-b3a66fe6d0-checkout")
       (sha256
	(base32
	 "0hk1x72kibaw3xkspy9822vh28ax3bk11b80qn8l4dwrm0wx34sy"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/quicklogic"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_rpi_pico
  (package
    (name "hal_rpi_pico")
    (version "0.0-0.191f5ba")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_rpi_pico")
	     (commit
	      "191f5ba46fda49523cdaaef27583d1c875ba2c36")))
       (file-name "hal_rpi_pico-191f5ba46f-checkout")
       (sha256
	(base32
	 "19wbqk27pfm37k1yj7wqb5cp5cx2aj6vp516n4f1pcp34mn9wbqg"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/rpi_pico"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_silabs
  (package
    (name "hal_silabs")
    (version "0.0-0.1ec8dd9")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_silabs")
	     (commit
	      "1ec8dd99aa4ac3e8632d2aa28a7438049bb27102")))
       (file-name "hal_silabs-1ec8dd99aa-checkout")
       (sha256
	(base32
	 "00cwkcg3v137md5hda9yv4s42isfrqnxd097i4kg6jj5sxm8f2bh"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/silabs"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_st
  (package
    (name "hal_st")
    (version "0.0-0.52a522c")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_st")
	     (commit
	      "52a522ca4a8a9ec1e9bb5bb514e1ab6f102863fe")))
       (file-name "hal_st-52a522ca4a-checkout")
       (sha256
	(base32
	 "12xpg8wvfr42kwnyiasip2axk923w6lpzfmrbgzl3zl4lc1wqa8c"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "modules/hal/st"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_stm32
  (package
    (name "hal_stm32")
    (version "0.0-0.51b373c")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_stm32")
	     (commit
	      "51b373cd3455b8c2b9babbf6ff41918116a442ac")))
       (file-name "hal_stm32-51b373cd34-checkout")
       (sha256
	(base32
	 "0hk6mvi12zyy8lclrkfhwwkxpz8vv5c9iksjrkc1izdgsprzrvfm"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/stm32"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_telink
  (package
    (name "hal_telink")
    (version "0.0-0.38573af")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_telink")
	     (commit
	      "38573af589173259801ae6c2b34b7d4c9e626746")))
       (file-name "hal_telink-38573af589-checkout")
       (sha256
	(base32
	 "1m5y6bhnhc6nnfd2pgxxhf30ny10vhiff4qaqililvg99b3wr0ca"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/telink"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_ti
  (package
    (name "hal_ti")
    (version "0.0-0.29af528")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_ti")
	     (commit
	      "29af528900d03af02a17b4b7f4fc93f19a81da2a")))
       (file-name "hal_ti-29af528900-checkout")
       (sha256
	(base32
	 "12a2waxajh10nkxyyshy7h2iqp72gw9vhnra8yaimdadsijqaxnq"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "modules/hal/ti"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public hal_xtensa
  (package
    (name "hal_xtensa")
    (version "0.0-0.63f6553")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_xtensa")
	     (commit
	      "63f655362423aa49507da7977a2d37142e8debeb")))
       (file-name "hal_xtensa-63f6553624-checkout")
       (sha256
	(base32
	 "0pjgmj1dmmcga15cwrqlz4iwscl0wddpw12v050xk6f4idql1h72"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/xtensa"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public libmetal
  (package
    (name "libmetal")
    (version "0.0-0.2f586b4")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/libmetal")
	     (commit
	      "2f586b4f1276fb075ee145421bdf6cbe5403aa41")))
       (file-name "libmetal-2f586b4f12-checkout")
       (sha256
	(base32
	 "1r0k3yxa50x0627fr61k6ahmfwyjg9zqiagfjv62a8jpwc134mw4"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/libmetal"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public liblc3codec
  (package
    (name "liblc3codec")
    (version "0.0-0.3951cf1")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/liblc3codec")
	     (commit
	      "3951cf1b71ff3be086c9b9b595e473e12301337c")))
       (file-name "liblc3codec-3951cf1b71-checkout")
       (sha256
	(base32
	 "0sapdsciazs92mdnik3j6g4y0v5x8rm6ga63szh436dw9jdd0dqi"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/liblc3codec"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public littlefs
  (package
    (name "littlefs")
    (version "0.0-0.652f2c5")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/littlefs")
	     (commit
	      "652f2c5646e79b881e6f3099686ad3b7af9e216c")))
       (file-name "littlefs-652f2c5646-checkout")
       (sha256
	(base32
	 "0an9ls0mbnxhjdnwa4mr150qxg7qg26jbzadvibhyflkqwibhlqc"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/fs/littlefs"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public loramac-node
  (package
    (name "loramac-node")
    (version "0.0-0.0257b50")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/loramac-node")
	     (commit
	      "0257b50905695192d095667b1c3abb80346db1a1")))
       (file-name "loramac-node-0257b50905-checkout")
       (sha256
	(base32
	 "0qrbjf09dp7cwazw5wl2z0zk1bn5yylrczpfv7hxzfc4mxgcm1ww"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/loramac-node"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public lvgl
  (package
    (name "lvgl")
    (version "0.0-0.487bcde")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/lvgl")
	     (commit
	      "487bcde705b6f453d053f28dbba4dd9f353d1ccb")))
       (file-name "lvgl-487bcde705-checkout")
       (sha256
	(base32
	 "07x3j6h2bg49vizg266z1ivv36hfq4r3pknzyc35vs7b82bzw64z"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/gui/lvgl"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public lz4
  (package
    (name "lz4")
    (version "0.0-0.8e303c2")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/lz4")
	     (commit
	      "8e303c264fc21c2116dc612658003a22e933124d")))
       (file-name "lz4-8e303c264f-checkout")
       (sha256
	(base32
	 "1kqs7gxg17gvws01rir8p6gmzp54y12s1898lflhsb418122v8nf"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "modules/lib/lz4"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public mbedtls
  (package
    (name "mbedtls")
    (version "0.0-0.7fed49c")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/mbedtls")
	     (commit
	      "7fed49c9b9f983ad6416986661ef637459723bcb")))
       (file-name "mbedtls-7fed49c9b9-checkout")
       (sha256
	(base32
	 "084m54xgqqqfixj7h9zigh98l06c2g5ybwbs6y7yji22x0v5dx7f"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/crypto/mbedtls"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public mcuboot
  (package
    (name "mcuboot")
    (version "0.0-0.1d44041")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/mcuboot")
	     (commit
	      "1d4404116a9a6b54d54ea9aa3dd2575286e666cd")))
       (file-name "mcuboot-1d4404116a-checkout")
       (sha256
	(base32
	 "1iv1cd4b8xk35sd4cp4s70ig7h4g7zl7b64s82yjf27hpr0ascv4"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "bootloader/mcuboot"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public mipi-sys-t
  (package
    (name "mipi-sys-t")
    (version "0.0-0.a5163c1")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/mipi-sys-t")
	     (commit
	      "a5163c1800a5243f8b05d84c942da008df4cb666")))
       (file-name "mipi-sys-t-a5163c1800-checkout")
       (sha256
	(base32
	 "1phg5i52k691my50n4821rdvlxyfr4dhs7146ayp0lf7nfw5asz4"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/debug/mipi-sys-t"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public nanopb
  (package
    (name "nanopb")
    (version "0.0-0.dc4deed")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/nanopb")
	     (commit
	      "dc4deed54fd4c7e1935e3b6387eedf21bb45dc38")))
       (file-name "nanopb-dc4deed54f-checkout")
       (sha256
	(base32
	 "0kmzh65hyhnl88w9x0rcdypj4nl1brbxrnzhy3flgxmhr564y05s"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/nanopb"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public net-tools
  (package
    (name "net-tools")
    (version "0.0-0.e0828aa")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/net-tools")
	     (commit
	      "e0828aa9629b533644dc96ff6d1295c939bd713c")))
       (file-name "net-tools-e0828aa962-checkout")
       (sha256
	(base32
	 "0m6ml684w8l5jjyzklf88crrcvniz4vw4g65py845hs4gk0qdiaw"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "tools/net-tools"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public nrf_hw_models
  (package
    (name "nrf_hw_models")
    (version "0.0-0.9340626")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/nrf_hw_models")
	     (commit
	      "93406267eca506003bcb86a86927777a32e729d9")))
       (file-name "nrf_hw_models-93406267ec-checkout")
       (sha256
	(base32
	 "185dq4x22mci7c85yr5d0vs8b5jf0c6f9y1z1s2x4x0hxdi1l2p1"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path
       "modules/bsim_hw_models/nrf_hw_models"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public open-amp
  (package
    (name "open-amp")
    (version "0.0-0.8d53544")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/open-amp")
	     (commit
	      "8d53544871e1f300c478224faca6be8384ab0d04")))
       (file-name "open-amp-8d53544871-checkout")
       (sha256
	(base32
	 "00lynjr3vj8a3gj3vh557gjwp7v7kzj7py0vxwvq8y349kh1ga50"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/open-amp"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public openthread
  (package
    (name "openthread")
    (version "0.0-0.b21e99b")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/openthread")
	     (commit
	      "b21e99b4b3d823f71c902b9174ff62b964c124f0")))
       (file-name "openthread-b21e99b4b3-checkout")
       (sha256
	(base32
	 "1kmmzvip0myjjrxbbassx5f2lj6z9z013l8mxb9w57cqhk2b6089"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/openthread"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public picolibc
  (package
    (name "picolibc")
    (version "0.0-0.e87b2fc")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/picolibc")
	     (commit
	      "e87b2fc37345a62361478f0a6efd140e14180ba5")))
       (file-name "picolibc-e87b2fc373-checkout")
       (sha256
	(base32
	 "1nml3c38zxl9d9fh9ssqski35xx8gfl5qqbln3w443iy0gqc39i7"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/picolibc"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public segger
  (package
    (name "segger")
    (version "0.0-0.d4e568a")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/segger")
	     (commit
	      "d4e568a920b4bd087886170a5624c167b2d0665e")))
       (file-name "segger-d4e568a920-checkout")
       (sha256
	(base32
	 "0a1rk0b2l1n2zkhlp1ia4cy2mijiaynk6qirw1gddnryj4qqr0nq"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/debug/segger"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public sof
  (package
    (name "sof")
    (version "0.0-0.fed466c")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/sof")
	     (commit
	      "fed466c264ad078c66f7bff9218ba1d3fa0eb201")))
       (file-name "sof-fed466c264-checkout")
       (sha256
	(base32
	 "18ndv22wg2ikpm63c89vgq1j4azgz3jrim4drxa4z8gw2i4fschq"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/audio/sof"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public tflite-micro
  (package
    (name "tflite-micro")
    (version "0.0-0.9156d05")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tflite-micro")
	     (commit
	      "9156d050927012da87079064db59d07f03b8baf6")))
       (file-name "tflite-micro-9156d05092-checkout")
       (sha256
	(base32
	 "1przq51rrhl032n831b2bl35f7kw22ypi9di2wxpfww5q4b6dbh1"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/tflite-micro"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public tinycbor
  (package
    (name "tinycbor")
    (version "0.0-0.9e1f34b")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tinycbor")
	     (commit
	      "9e1f34bc08123aaad7666d3652aaa839e8178b3b")))
       (file-name "tinycbor-9e1f34bc08-checkout")
       (sha256
	(base32
	 "0yqmbiqx4www3rfc1ym42j8gqwi1b8hkw27gjps25xz22zrh4a1p"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/tinycbor"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public tinycrypt
  (package
    (name "tinycrypt")
    (version "0.0-0.3e9a49d")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tinycrypt")
	     (commit
	      "3e9a49d2672ec01435ffbf0d788db6d95ef28de0")))
       (file-name "tinycrypt-3e9a49d267-checkout")
       (sha256
	(base32
	 "19d2q9y23yzz9i383q3cldjl3k5mryx9762cab23zy3ijdnmj2z6"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/crypto/tinycrypt"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public TraceRecorderSource
  (package
    (name "TraceRecorderSource")
    (version "0.0-0.9893bf1")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/TraceRecorderSource")
	     (commit
	      "9893bf1cf649a2c4ee2e27293f887994f3d0da5b")))
       (file-name
	"TraceRecorderSource-9893bf1cf6-checkout")
       (sha256
	(base32
	 "00lghqq3n7qln6kq2ikwgswap62z81fqw0k28j975nnc1z932flg"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/debug/TraceRecorder"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public trusted-firmware-m
  (package
    (name "trusted-firmware-m")
    (version "0.0-0.f13209f")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/trusted-firmware-m")
	     (commit
	      "f13209f1883232cbcb9f0c31fb4c63e7c242df0d")))
       (file-name
	"trusted-firmware-m-f13209f188-checkout")
       (sha256
	(base32
	 "173l0llv1ywvcx654gfgq6ysw228j06lfxyiny0si595qwyn2mmq"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path
       "modules/tee/tf-m/trusted-firmware-m"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public tf-m-tests
  (package
    (name "tf-m-tests")
    (version "0.0-0.c99a86b")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tf-m-tests")
	     (commit
	      "c99a86b295c4887520da9d8402566d7f225c974e")))
       (file-name "tf-m-tests-c99a86b295-checkout")
       (sha256
	(base32
	 "0cc4029i9qfzq4vply9l2yjigaf9rq1zc6a44b8dc779kx31qx36"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/tee/tf-m/tf-m-tests"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public psa-arch-tests
  (package
    (name "psa-arch-tests")
    (version "0.0-0.f4fc244")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/psa-arch-tests")
	     (commit
	      "f4fc2442b8e29e2a03d9899e46e5a3ea3df8c2c9")))
       (file-name "psa-arch-tests-f4fc2442b8-checkout")
       (sha256
	(base32
	 "015qan6qfqyfc1lwpqb29zk1wxx02b6ng5wc1nfpfvknvhiwifay"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path
       "modules/tee/tf-m/psa-arch-tests"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public zcbor
  (package
    (name "zcbor")
    (version "0.0-0.a0d6981")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/zcbor")
	     (commit
	      "a0d6981f14d4001d6f0d608d1a427f9bc6bb6d02")))
       (file-name "zcbor-a0d6981f14-checkout")
       (sha256
	(base32
	 "03xz79pi210kny55ks9cyr8i9m68f7kla3nv5zk2afg59ms0nwdc"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/zcbor"))
    (synopsis "")
    (description "")
    (license #f)))

(define-public zscilib
  (package
    (name "zscilib")
    (version "0.0-0.a54986a")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/zscilib")
	     (commit
	      "a54986aa98db4082ac56b582843bb5b5435208a6")))
       (file-name "zscilib-a54986aa98-checkout")
       (sha256
	(base32
	 "03rxpajcxs0rb5inlzk9l278nr4qrhs2clkmgz1raz6rcdcsmnm2"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/zscilib"))
    (synopsis "")
    (description "")
    (license #f)))
