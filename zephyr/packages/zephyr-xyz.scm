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

(define-public zephyr-canopennode
  (package
    (name "zephyr-canopennode")
    (version "0.0-0.53d3415")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/canopennode")
	     (commit
	      "53d3415c14d60f8f4bfca54bfbc5d5a667d7e724")))
       (file-name "zephyr-canopennode-53d3415c14-checkout")
       (sha256
	(base32
	 "1vqrx1zi2wbvnwza4z39nrd33j9dxpp9j9rnn12r1gdwxfjazbw0"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/canopennode"))
    (synopsis "CANopenNode is free and open source CANopen protocol stack.")
    (description "CANopen is the internationally standardized (EN
50325-4) (CiA301) higher-layer protocol for embedded control system
built on top of CAN. For more information on CANopen see
http://www.can-cia.org/")
    (license license:apsl2)))

(define-public zephyr-chre
  (package
    (name "zephyr-chre")
    (version "0.0-0.ef76d34")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/chre")
	     (commit
	      "ef76d3456db07e4959df555047d6962279528c8d")))
       (file-name "zephyr-chre-ef76d3456d-checkout")
       (sha256
	(base32
	 "00jnv58slzgp4srmsamd7x34yf0hj1c6agmws4csv7w28013bwfm"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/lib/chre"))
    (synopsis "AOSP reference implementation of the Context Hub Runtime Environment ")
    (description "This project contains the AOSP reference
implementation of the Context Hub Runtime Environment (CHRE), which is
Android’s platform for developing always-on applications, called
nanoapps. CHRE runs in a vendor-specific processor that is independent
of the main applications processor that runs Android. This enables
CHRE and its nanoapps to be more power-efficient for use cases that
require frequent background processing of contextual data, like sensor
inputs. Nanoapps are written to the CHRE API, which is standardized
across all platforms, enabling them to be code-compatible across
different devices.")
    (license license:apsl2)))

(define-public zephyr-cmsis
  (package
    (name "zephyr-cmsis")
    (version "0.0-0.5f86244")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/cmsis")
	     (commit
	      "5f86244bad4ad5a590e084f0e72ba7a1416c2edf")))
       (file-name "zephyr-cmsis-5f86244bad-checkout")
       (sha256
	(base32
	 "0f7cipnwllna7iknsnz273jkvrly16yr6wm4y2018i6njpqh67wi"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/cmsis"))
    (synopsis "Zephyr module providing the Common Microcontroller
Software Interface Standard")
    (description "Zephyr module providing the Common Microcontroller
Software Interface Standard.")
    (license license:apsl2)))

(define-public hal-cmsis zephyr-cmsis)

(define-public zephyr-edtt
  (package
    (name "zephyr-edtt")
    (version "0.0-0.1ea61a3")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/edtt")
	     (commit
	      "1ea61a390d2bfcf3b2ecdba8f8b0b98dfdffbd11")))
       (file-name "zephyr-edtt-1ea61a390d-checkout")
       (sha256
	(base32
	 "10cpksv0imsd0jbyr2pag63agvfx9dg4028x3cxpvfvscizz4dpx"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "tools/edtt"))
    (synopsis "The EDTT (Embedded Device Test Tool) is a collection of python scripts")
    (description "
The EDTT (Embedded Device Test Tool) is a collection of python scripts, which include:
    The PC side of a simple basic Remote Procedure Call (RPC)
    A transport mechanism, which is used by this RPC to transport data in an out of an embedded device
    A set of self checking tests of the BLE functionality (both host and controller)
    A basic tool which wraps all of this, and executes a set of tests as selected from command line.")
    (license license:apsl2)))

(define-public zephyr-fatfs
  (package
    (name "zephyr-fatfs")
    (version "0.0-0.a30531a")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/fatfs")
	     (commit
	      "a30531af3a95a9a3ea7d771ea8a578ebfed45514")))
       (file-name "zephyr-fatfs-a30531af3a-checkout")
       (sha256
	(base32
	 "04c33x1am766sdszmsmxd2rii6snyld68ca17qhg3ml2xiqy1z31"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/fs/fatfs"))
    (synopsis "Implementation of fatfs for embedded systems.")
    (description "This is based on FAT file system specification.  It
provides an interface to physical storage management through its disk
io API in diskio.c. The disk io interfaces are updated to interface
with Zephyr flash memory management.")
    (license (license:non-copyleft "http://elm-chan.org/fsw/ff/doc/appnote.html#license"))))

(define-public zephyr-fff
  (package
    (name "zephyr-fff")
    (version "0.0-0.6ce5ba2")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/fff")
	     (commit
	      "6ce5ba26486e93d5b7696a3e23f0585932c14b16")))
       (file-name "zephyr-fff-6ce5ba2648-checkout")
       (sha256
	(base32
	 "1wwrsicibk5nrzj7arxd4a22qr596vnc1ygp9zi409ygqvmzayzy"))))
    (build-system zephyr-module-build-system)
    (arguments '(#:workspace-path "modules/lib/fff"))
    (synopsis "Micro-framework for creating fake C functions for tests")
    (description "fff is a micro-framework for creating fake C
functions for tests. Because life is too short to spend time
hand-writing fake functions for testing.")
    (license license:x11)))

(define-public zephyr-hal-altera
  (package
    (name "zephyr-hal-altera")
    (version "0.0-0.0d225dd")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_altera")
	     (commit
	      "0d225ddd314379b32355a00fb669eacf911e750d")))
       (file-name "zephyr-hal_altera-0d225ddd31-checkout")
       (sha256
	(base32
	 "05f5a1sk8mp8vhbdsj31833n10kyd91f93hgk2zr62xc0kfbwq0z"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/altera"))
    (synopsis "Hardware Abstraction Layer for NIOSII based soft IP blocks")
    (description " Altera Hardware Abstraction Layer(or Altera HAL)
provides the low level driver support for NIOSII based soft IP blocks.

   Altera HAL primarily abstracts the low level hardware interactions
of different NIOSII based soft IP blocks like UART, I2C, SPI etc...
and provides the high level API's for the shim drivers.")
    (license license:x11)))

(define-public zephyr-hal-atmel
  (package
    (name "zephyr-hal-atmel")
    (version "0.0-0.bb4e710")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_atmel")
	     (commit
	      "bb4e7104132bdc22c7cc7e20057434c2979e6706")))
       (file-name "zephyr-hal_atmel-bb4e710413-checkout")
       (sha256
	(base32
	 "10sqdx1sasf3nnpp35jm4sj8i91izm2lb4kymy238jbafp1iggqg"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/atmel"))
    (synopsis "Register definitions for Atmel SAM/SAM0 SoC's")
    (description "register definitions for Atmel SAM/SAM0 SoC's.")
    (license license:x11)))

(define-public zephyr-hal-espressif
  (package
    (name "zephyr-hal-espressif")
    (version "0.0-0.22b4700")
    (home-page "")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_espressif")
	     (commit
	      "22b47009dc27bdd395c6ef1a66c347037a2fb585")))
       (file-name "zephyr-hal_espressif-22b47009dc-checkout")
       (sha256
	(base32
	 "1c940bp4d07igi68y3fa61f0rchnhzk2mqqhmyvbj8ml31g85h35"))))
    (build-system zephyr-module-build-system)
    (arguments
     '(#:workspace-path "modules/hal/espressif"))
    (synopsis "Hardware Abstraction Layer for Espressif SoCs")
    (description "ESP-IDF is the development framework for Espressif
SoCs supported on Windows, Linux and macOS.")
    (license license:apsl2)))

(define-public zephyr-hal-gigadevice
  (let ((commit "7f15468b07807ae6a913e0e9cbabfaa23a206bc6"))
    (package
     (name "zephyr-hal-gigadevice")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/hal_gigadevice")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_gigadevice")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0br7j45w6s8zc3050nrgd6mvq08gdn9z8svmhbn1c26ipv62npzd"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/gigadevice"))
     (synopsis "Standard firmware library and ARM CMSIS configurations
for GigaDevice MCUs")
     (description "Standard firmware library and ARM CMSIS configurations
for GigaDevice MCUs")
     (license license:apsl2))))

(define-public zephyr-hal-infineon
  (let ((commit "4af06965f57ba1e7d170e6a97d24c33785543a8c"))
    (package
     (name "zephyr-hal-infineon")
     ;; XXX Every component is versioned separately
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/hal_infineon")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_infineon")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1wgvdkib605j0hnyrlnwikb6zri0v3l8pfxdiazbaz07rqnc11vb"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/infineon"))
     (synopsis "This modules includes the Infineon Assets (PDL, HAL, etc)")
     (description "This modules includes the Infineon Assets (PDL,
HAL, etc), which are used in implementation of Zephyr drivers for
Infineon devices.")
     (license (license:non-copyleft
	       (file-append source "/License.txt"))))))

(define-public zephyr-hal-microchip
  (let ((commit "5d079f1683a00b801373bbbbf5d181d4e33b30d5"))
    (package
     (name "zephyr-hal-microchip")
     (version (git-version "1.2.1" "3.1" commit))
     (home-page  "https://github.com/zephyrproject-rtos/hal_microchip")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_microchip")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "11basljy0fsgcp3l7zvxklw6zqkmahm97pi156ndadrhr8g4v5q8"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/microchip"))
     (synopsis "Hardware Abstraction Layer for Microchip SoC's")
     (description "Hardware Abstraction Layer for Microchip SoC's")
     (license (list license:apsl2
		    (license:non-copyleft
		     "https://github.com/polarfire-soc/platform/blob/main/LICENSE.md"))))))

(define-public zephyr-hal-nordic
  (let ((commit "2e1c828cf4efb71679aeaec94128708a5353e031"))
    (package
     (name "zephyr-hal-nordic")
     (version (git-version "3.0.0" "3.1" commit))
     (home-page "https://github.com/NordicSemiconductor/nrfx")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_nordic")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "14ahxbz92dyqjv7pgsjsln2nrv4092645l25i2ix6gs2dqqgrwfi"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/nordic"))
     (synopsis "With added proper shims adapting it to Zephyr's APIs,
nrfx will provide peripheral drivers for Nordic SoCs")
     (description "nrfx is a standalone set of drivers for peripherals
present in Nordic Semiconductor's SoCs. It originated as an extract
from the nRF5 SDK.  The intention was to provide drivers that can be
used in various environments without the necessity to integrate other
parts of the SDK into them. For the user's convenience, the drivers
come with the MDK package. This package contains definitions of
register structures and bitfields for all supported SoCs, as well as
startup and initialization files for them.")
     (license license:bsd-3))))

(define-public zephyr-hal-nuvoton
  (let ((commit "b4d31f33238713a568e23618845702fadd67386f"))
    (package
     (name "zephyr-hal-nuvoton")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/hal_nuvoton")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_nuvoton")
	     (commit
	      "b4d31f33238713a568e23618845702fadd67386f")))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0942ainpvf64878vkwh9sx4bgwzmf98d40wqa125qmczlbb0d7my"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/nuvoton"))
     (synopsis "Hardware Abstraction Layer for Nuvoton ICs")
     (description "Hardware Abstraction Layer for Nuvoton ICs.")
     ;; XXX: I cannot find licensing information for this package.
     (license #f))))

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

(define-public zephyr-hal-openisa
  (let ((commit "40d049f69c50b58ea20473bee14cf93f518bf262"))
    (package
     (name "zephyr-hal_openisa")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_openisa")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1kvb6pc28klw5pplfp3hbrznvi70gvzq0qjiz6ji256qbflj0437"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/openisa"))
     (synopsis " Initial release of the RV32M1 SDK for the RISC-V cores")
     (description "This package is an extract from the upstream RV32M1
SDK that contains only the files needed for Zephyr.")
     (license license:bsd-3))))

(define-public zephyr-hal-quicklogic
  (let ((commit "b3a66fe6d04d87fd1533a5c8de51d0599fcd08d0"))
    (package
     (name "zephyr-hal-quicklogic")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_quicklogic")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0hk1x72kibaw3xkspy9822vh28ax3bk11b80qn8l4dwrm0wx34sy"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/quicklogic"))
     (synopsis "HAL for QuickLogic devices.")
     (description "This module provides the HAL layer needed to build
a Zephyr application running on the QuickLogic EOS S3 based platforms.

It is authored by the QuickLogic Corporation.")
     (license license:apsl2))))

(define-public zephyr-hal-rpi-pico
  (let ((commit "191f5ba46fda49523cdaaef27583d1c875ba2c36"))
    (package
     (name "zephyr-hal-rpi-pico")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_rpi_pico")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "19wbqk27pfm37k1yj7wqb5cp5cx2aj6vp516n4f1pcp34mn9wbqg"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/rpi_pico"))
     (synopsis "Raspberry Pi Pico HAL/SDK")
     (description "The Raspberry Pi Pico SDK provides the headers,
libraries and build system necessary to write programs for the
RP2040-based devices such as the Raspberry Pi Pico in C, C++ or
assembly language.")
     (license license:bsd-3))))

(define-public zephyr-hal-silabs
  (let ((commit "1ec8dd99aa4ac3e8632d2aa28a7438049bb27102"))
    (package
     (name "zephyr-hal-silabs")
     (version (git-version "3.1.2" "3.1" commit))
     (home-page "https://www.silabs.com")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_silabs")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "00cwkcg3v137md5hda9yv4s42isfrqnxd097i4kg6jj5sxm8f2bh"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/silabs"))
     (synopsis "Zephyr support for Silicon Labs EXX32 SoCs")
     (description "Zephyr support for Silicon Labs EXX32 SoCs.")
     (license license:zlib))))

(define-public zephyr-hal-st
  (let ((commit "52a522ca4a8a9ec1e9bb5bb514e1ab6f102863fe"))
    (package
     (name "zephyr-hal-st")
     (version (git-version "1.0.2" "3.1" commit))
     (home-page "https://www.st.com")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_st")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "12xpg8wvfr42kwnyiasip2axk923w6lpzfmrbgzl3zl4lc1wqa8c"))))
     (build-system zephyr-module-build-system)
     (arguments '(#:workspace-path "modules/hal/st"))
     (synopsis "Official libraries provided by STMicroelectronics")
     (description "Official libraries provided by STMicroelectronics.")
     (license license:bsd-3))))

(define-public zephyr-hal-stm32
  (let ((commit "51b373cd3455b8c2b9babbf6ff41918116a442ac"))
    (package
     (name "zephyr-hal-stm32")
     (version (git-version "1.17" "3.1" commit))
     (home-page "https://www.st.com")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_stm32")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0hk6mvi12zyy8lclrkfhwwkxpz8vv5c9iksjrkc1izdgsprzrvfm"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/stm32"))
     (synopsis "ST Microelectronics official MCU package for STM32 series.")
     (description " This package is an extract of official STM32Cube
package written by ST Microelectronics.  It is composed of STM32Cube
hardware abstraction layer (HAL) and low layer (LL) plus a set of
CMSIS headers files, one for each SoC in STM32 series.")
     (license license:bsd-3))))

(define-public zephyr-hal-telink
  (let ((commit "38573af589173259801ae6c2b34b7d4c9e626746"))
    (package
     (name "zephyr-hal-telink")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/hal_telink")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_telink")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1m5y6bhnhc6nnfd2pgxxhf30ny10vhiff4qaqililvg99b3wr0ca"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/telink"))
     (synopsis "Hardware abstraction layer for Telink devices")
     (description "Hardware abstraction layer for Telink devices.")
     (license #f))))

(define-public zephyr-hal-ti
  (let ((commit "29af528900d03af02a17b4b7f4fc93f19a81da2a"))
    (package
     (name "zephyr-hal-ti")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_ti")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "12a2waxajh10nkxyyshy7h2iqp72gw9vhnra8yaimdadsijqaxnq"))))
     (build-system zephyr-module-build-system)
     (arguments '(#:workspace-path "modules/hal/ti"))
     (synopsis "Hardware abstraction for Texas Instruments devices")
     (description "Hardware abstraction for Texas Instruments devices.")
     (license #f))))

(define-public zephyr-hal-xtensa
  (let ((commit "63f655362423aa49507da7977a2d37142e8debeb"))
    (package
     (name "zephyr-hal-xtensa")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/hal_xtensa")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0pjgmj1dmmcga15cwrqlz4iwscl0wddpw12v050xk6f4idql1h72"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/xtensa"))
     (synopsis "Hardware abstraction layer for Xtensa devices")
     (description "Hardware abstraction layer for Xtensa devices.")
     (license license:x11))))

(define-public zephyr-libmetal
  (let ((commit "2f586b4f1276fb075ee145421bdf6cbe5403aa41"))
    (package
     (name "zephyr-libmetal")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/libmetal")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1r0k3yxa50x0627fr61k6ahmfwyjg9zqiagfjv62a8jpwc134mw4"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/hal/libmetal"))
     (synopsis "HAL abstraction layer used by open-amp")
     (description "Libmetal provides common user APIs to access
devices, handle device interrupts and request memory across the
following operating environments:

   * Linux user space (based on UIO and VFIO support in the kernel)
   * RTOS (with and without virtual memory)
   * Bare-metal environments")
     (license license:bsd-3))))

(define-public zephyr-liblc3codec
  (let ((commit "3951cf1b71ff3be086c9b9b595e473e12301337c"))
    (package
     (name "zephyr-liblc3codec")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/liblc3codec")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0sapdsciazs92mdnik3j6g4y0v5x8rm6ga63szh436dw9jdd0dqi"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/liblc3codec"))
     (synopsis "LC3 codec implementation")
     (description "LC3 codec implementation.")
     (license license:apsl2))))

(define-public zephyr-littlefs
  (let ((commit "652f2c5646e79b881e6f3099686ad3b7af9e216c"))
    (package
     (name "zephyr-littlefs")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/littlefs")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/littlefs")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0an9ls0mbnxhjdnwa4mr150qxg7qg26jbzadvibhyflkqwibhlqc"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/fs/littlefs"))
     (synopsis "A little fail-safe filesystem designed for microcontrollers")
     (description "A little fail-safe filesystem designed for microcontrollers")
     (license license:bsd-3))))

(define-public zephyr-loramac-node
  (let ((commit "0257b50905695192d095667b1c3abb80346db1a1"))
    (package
     (name "zephyr-loramac-node")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/loramac-node")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/loramac-node")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0qrbjf09dp7cwazw5wl2z0zk1bn5yylrczpfv7hxzfc4mxgcm1ww"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/loramac-node"))
     (synopsis "LoRaWAN end-device stack implementation and example projects")
     (description "This project fully implements ClassA, ClassB and
ClassC end-device classes and it also provides SX1272/73,
SX1276/77/78/79, SX1261/2 and LR1110 radio drivers.")
     (license (license:non-copyleft (file-append source "/LICENSE"))))))

(define-public zephyr-lvgl
  (let ((commit "487bcde705b6f453d053f28dbba4dd9f353d1ccb"))
    (package
     (name "zephyr-lvgl")
     (version (git-version "8.3.7" "3.1" commit))
     (home-page "https://lvgl.io")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/lvgl")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "07x3j6h2bg49vizg266z1ivv36hfq4r3pknzyc35vs7b82bzw64z"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/gui/lvgl"))
     (synopsis "Light and Versatile Graphics Library")
     (description "LVGL provides everything you need to create an
embedded GUI with easy-to-use graphical elements, beautiful visual
effects and a low memory footprint.")
     (license license:x11))))

(define-public zephyr-lz4
  (let ((commit "8e303c264fc21c2116dc612658003a22e933124d"))
    (package
     (name "zephyr-lz4")
     (version (git-version "1.9.3" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/lz4")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1kqs7gxg17gvws01rir8p6gmzp54y12s1898lflhsb418122v8nf"))))
     (build-system zephyr-module-build-system)
     (arguments '(#:workspace-path "modules/lib/lz4"))
     (synopsis "Extremely fast lossless compression algorithm")
     (description "LZ4 is lossless compression algorithm, providing
compression speed > 500 MB/s per core, scalable with multi-cores
CPU. It features an extremely fast decoder, with speed in multiple
GB/s per core, typically reaching RAM speed limits on multi-core
systems.")
     (license (list license:bsd-2 ;file under /lib
		    license:gpl2 ;everything else
		    )))))

(define-public zephyr-mbedtls
  (let ((commit "7fed49c9b9f983ad6416986661ef637459723bcb"))
    (package
     (name "zephyr-mbedtls")
     (version (git-version "3.4.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/mbedtls")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "084m54xgqqqfixj7h9zigh98l06c2g5ybwbs6y7yji22x0v5dx7f"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/crypto/mbedtls"))
     (synopsis "C library that implements cryptographic primitives")
     (description "Mbed TLS is a C library that implements
cryptographic primitives, X.509 certificate manipulation and the
SSL/TLS and DTLS protocols. Its small code footprint makes it suitable
for embedded systems.")
     (license (list license:apsl2
		    license:gpl2+)))))

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

(define-public zephyr-mipi-sys-t
  (let ((commit "a5163c1800a5243f8b05d84c942da008df4cb666"))
    (package
     (name "zephyr-mipi-sys-t")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/mipi-sys-t")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1phg5i52k691my50n4821rdvlxyfr4dhs7146ayp0lf7nfw5asz4"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/debug/mipi-sys-t"))
     (synopsis "MIPI SyS-T instrumentation library")
     (description "The MIPI Alliance Specification for System
Software-Trace (MIPI SyS-T℠) defines a platform-independent, low
bandwidth trace data protocol and software API for generating trace
debug information from mobile or mobile influenced devices.

This project provides an example implementation of a MIPI SyS-T
instrumentation library as its main component. The library exposes the
MIPI SyS-T API which generates the MIPI SyS-T data protocol.")
     (license license:bsd-3))))

(define-public zephyr-nanopb
  (let ((commit "dc4deed54fd4c7e1935e3b6387eedf21bb45dc38"))
    (package
     (name "zephyr-nanopb")
     (version (git-version "0.4.7" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/nanopb")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0kmzh65hyhnl88w9x0rcdypj4nl1brbxrnzhy3flgxmhr564y05s"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/nanopb"))
     (synopsis "Protocol Buffers for Embedded Systems")
     (description "Nanopb is a small code-size Protocol Buffers
implementation in ansi C. It is especially suitable for use in
microcontrollers, but fits any memory restricted system.")
     (license license:zlib))))

(define-public zephyr-net-tools
  (let ((commit "e0828aa9629b533644dc96ff6d1295c939bd713c"))
    (package
     (name "zephyr-net-tools")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/net-tools")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/net-tools")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0m6ml684w8l5jjyzklf88crrcvniz4vw4g65py845hs4gk0qdiaw"))))
     (build-system zephyr-module-build-system)
     (arguments '(#:workspace-path "tools/net-tools"))
     (synopsis "Zephyr network tools for testing with Qemu")
     (description "These tools allow you to to communicate between
Zephyr that is running inside QEMU, and host device that is running
Linux.")
     ;; XXX: I could not find licensing information anywhere but
     ;; this is a zephyr project (not a shim for an external project)
     ;; so I'm assuming it is Apache2 like the rest of their stuff.
     (license license:apsl2))))

(define-public zephyr-nrf-hw-models
  (let ((commit "93406267eca506003bcb86a86927777a32e729d9"))
    (package
     (name "zephyr-nrf_hw_models")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/nrf_hw_models")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/nrf_hw_models")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "185dq4x22mci7c85yr5d0vs8b5jf0c6f9y1z1s2x4x0hxdi1l2p1"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path
	"modules/bsim_hw_models/nrf_hw_models"))
     (synopsis "Models of the nRF52xxx HW peripherals")
     (description "This repo contains models of the nRF52 HW
peripherals and some replacement nrfx HAL functions. When used in
combination with the real nrfx, these should enable code meant for the
nrfx to run without needing further changes. This includes Zephyr SW.

Where relevant differences exist, these models try to align with an
nRF52833.")
     (license license:bsd-3))))

(define-public zephyr-open-amp
  (let ((commit "8d53544871e1f300c478224faca6be8384ab0d04"))
    (package
     (name "zephyr-open-amp")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://github.com/OpenAMP/open-amp")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/open-amp")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "00lynjr3vj8a3gj3vh557gjwp7v7kzj7py0vxwvq8y349kh1ga50"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/open-amp"))
     (synopsis "IPC layer that implements rpmsg communication between cores")
     (description "The OpenAMP framework provides software components that
enable development of software applications for Asymmetric Multiprocessing
(AMP) systems. The framework provides the following key capabilities.

* Provides Life Cycle Management, and Inter Processor Communication capabilities
  for management of remote compute resources and their associated software
  contexts.
* Provides a stand alone library usable with RTOS and Baremetal software
  environments
* Compatibility with upstream Linux remoteproc and rpmsg components
* Following AMP configurations supported:
  a. Linux master/Generic(Baremetal) remote
  b. Generic(Baremetal) master/Linux remote
* Proxy infrastructure and supplied demos showcase ability of proxy on master
  to handle printf, scanf, open, close, read, write calls from Bare metal
  based remote contexts.")
     (license (list license:bsd-2
		    license:bsd-3)))))

(define-public zephyr-openthread
  (let ((commit "b21e99b4b3d823f71c902b9174ff62b964c124f0"))
    (package
     (name "zephyr-openthread")
     (version (git-version "14.0" "3.1" commit))
     (home-page "https://openthread.io")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/openthread")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1kmmzvip0myjjrxbbassx5f2lj6z9z013l8mxb9w57cqhk2b6089"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/openthread"))
     (synopsis "openthread module for Zephyr")
     (description "OpenThread is an open source implementation of the
Thread 1.3.0 Final Specification.  The Thread 1.3.0 Final
Specification is promulgated by the Thread Group.")
     (license license:bsd-3))))

(define-public zephyr-picolibc
  (let ((commit "e87b2fc37345a62361478f0a6efd140e14180ba5"))
    (package
     (name "zephyr-picolibc")
     (version (git-version "1.8.2" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/picolibc")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1nml3c38zxl9d9fh9ssqski35xx8gfl5qqbln3w443iy0gqc39i7"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/picolibc"))
     (synopsis "C library that targets small embedded systems with limited RAM")
     (description "Picolibc is library offering standard C library
APIs that targets small embedded systems with limited RAM. Picolibc
was formed by blending code from Newlib and AVR Libc.")
     (license (list license:gpl2
		    (license:non-copyleft
		     (file-append source "/COPYING.NEWLIB"))
		    (license:non-copyleft
		     (file-append source "/COPYING.picolibc")))))))

(define-public zephyr-segger
  (let ((commit "d4e568a920b4bd087886170a5624c167b2d0665e"))
    (package
     (name "zephyr-segger")
     (version (git-version "3.30" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/segger")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/segger")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0a1rk0b2l1n2zkhlp1ia4cy2mijiaynk6qirw1gddnryj4qqr0nq"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/debug/segger"))
     (synopsis "")
     (description "")
     (license #f))))

(define-public zephyr-sof
  (let ((commit "fed466c264ad078c66f7bff9218ba1d3fa0eb201"))
    (package
     (name "zephyr-sof")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://thesofproject.github.io/latest/index.html")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/sof")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "18ndv22wg2ikpm63c89vgq1j4azgz3jrim4drxa4z8gw2i4fschq"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/audio/sof"))
     (synopsis "Sound Open Firmware SDK")
     (description "Sound Open Firmware (SOF) is an open source audio
Digital Signal Processing (DSP) firmware infrastructure and SDK. SOF
provides infrastructure, real-time control pieces, and audio drivers
as a community project.")
     (license license:bsd-3))))

(define-public zephyr-tflite-micro
  (let ((commit "9156d050927012da87079064db59d07f03b8baf6"))
    (package
     (name "zephyr-tflite-micro")
     (version (git-version "0.0" "3.1" commit))
     (home-page "https://www.tensorflow.org")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tflite-micro")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "1przq51rrhl032n831b2bl35f7kw22ypi9di2wxpfww5q4b6dbh1"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/tflite-micro"))
     (synopsis "TensorFlow Lite for Microcontrollers")
     (description "TensorFlow Lite for Microcontrollers is a port of
TensorFlow Lite designed to run machine learning models on DSPs,
microcontrollers and other devices with limited memory.")
     (license license:apsl2))))

(define-public zephyr-tinycbor
  (let ((commit "9e1f34bc08123aaad7666d3652aaa839e8178b3b"))
    (package
     (name "zephyr-tinycbor")
     (version (git-version "0.5" "3.1" commit))
     (home-page "https://intel.github.io/tinycbor/current")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tinycbor")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0yqmbiqx4www3rfc1ym42j8gqwi1b8hkw27gjps25xz22zrh4a1p"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/tinycbor"))
     (synopsis "Concise Binary Object Representation (CBOR) Library")
     (description "Concise Binary Object Representation (CBOR) Library")
     (license license:x11))))

(define-public zephyr-tinycrypt
  (let ((commit "3e9a49d2672ec01435ffbf0d788db6d95ef28de0"))
    (package
     (name "zephyr-tinycrypt")
     (version (git-version "0.2.8" "3.1" commit))
     (home-page "https://github.com/intel/tinycrypt")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tinycrypt")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "19d2q9y23yzz9i383q3cldjl3k5mryx9762cab23zy3ijdnmj2z6"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/crypto/tinycrypt"))
     (synopsis "C library providing a minimal set of standard
cryptography primitives")
     (description "The TinyCrypt Library provides an implementation for
constrained devices of a minimal set of standard cryptography
primitives")
     (license
      (license:non-copyleft
       "https://github.com/intel/tinycrypt/blob/master/LICENSE")))))

(define-public zephyr-TraceRecorderSource
  (let ((commit "9893bf1cf649a2c4ee2e27293f887994f3d0da5b"))
    (package
     (name "zephyr-TraceRecorderSource")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/TraceRecorderSource")
	     (commit commit)))
       (file-name
	(git-file-name name version))
       (sha256
	(base32
	 "00lghqq3n7qln6kq2ikwgswap62z81fqw0k28j975nnc1z932flg"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/debug/TraceRecorder"))
     (synopsis "Trace Recorder source code for use with Percepio's Tracealyzer")
     (description "Trace Recorder source code for use with Percepio's Tracealyzer")
     (license license:apsl2))))

(define-public zephyr-trusted-firmware-m
  (let ((commit "f13209f1883232cbcb9f0c31fb4c63e7c242df0d"))
    (package
     (name "zephyr-trusted-firmware-m")
     (version (git-version "1.6.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/trusted-firmware-m")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/trusted-firmware-m")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "173l0llv1ywvcx654gfgq6ysw228j06lfxyiny0si595qwyn2mmq"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path
	"modules/tee/tf-m/trusted-firmware-m"))
     (synopsis  "The Trusted Firmware-M software implementation
contained in this project is designed to be a reference implementation
of the Platform Security Architecture (PSA) for ARMv7-M and Armv8-M.")
     (description "Trusted Firmware-M (TF-M) implements the Secure
Processing Environment (SPE) for Armv8-M, Armv8.1-M architectures
(e.g. the Cortex-M33, Cortex-M23, Cortex-M55, Cortex-M85 processors)
and dual-core platforms. It is the platform security architecture
reference implementation aligning with PSA Certified guidelines,
enabling chips, Real Time Operating Systems and devices to become PSA
Certified." )
     (license (license:non-copyleft (file-append source "/license.rst"))))))

(define-public zephyr-tf-m-tests
  (let ((commit "c99a86b295c4887520da9d8402566d7f225c974e"))
    (package
     (name "zephyr-tf-m-tests")
     (version (git-version "1.6.0" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/tf-m-tests")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/tf-m-tests")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "0cc4029i9qfzq4vply9l2yjigaf9rq1zc6a44b8dc779kx31qx36"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/tee/tf-m/tf-m-tests"))
     (synopsis "Various tests for the Trusted Firmware-M")
     (description "The Trusted Firmware-M(TF-M) Tests repo is meant to
hold various tests for the Trusted Firmware-M. The TF-M tests mainly
focus on functionalities of various TF-M componentes such as the TF-M
core and various secure partitions.")
     (license (license:non-copyleft (file-append source "/license.rst"))))))

(define-public zephyr-psa-arch-tests
  (let ((commit "f4fc2442b8e29e2a03d9899e46e5a3ea3df8c2c9"))
    (package
     (name "zephyr-psa-arch-tests")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/psa-arch-tests")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "015qan6qfqyfc1lwpqb29zk1wxx02b6ng5wc1nfpfvknvhiwifay"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path
	"modules/tee/tf-m/psa-arch-tests"))
     (synopsis "Tests for verifying implementations of TBSA-v8M and the PSA Developer APIs")
     (description "This test suite is one of a set of resources
provided by Arm that can help organizations develop products that meet
the security requirements of PSA Certified on Arm-based platforms. The
PSA Certified scheme provides a framework and methodology that helps
silicon manufacturers, system software providers and OEMs to develop
more secure products.")
     (license license:apsl2))))

(define-public zephyr-zcbor
  (let ((commit "a0d6981f14d4001d6f0d608d1a427f9bc6bb6d02"))
    (package
     (name "zephyr-zcbor")
     (version (git-version "0.5.1" "3.1" commit))
     (home-page "https://github.com/zephyrproject-rtos/zcbor")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/zcbor")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "03xz79pi210kny55ks9cyr8i9m68f7kla3nv5zk2afg59ms0nwdc"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/zcbor"))
     (synopsis "Zephyr module providing a lightweight CBOR implementation")
     (description "zcbor is a low footprint CBOR library in the C language
that comes with a schema-driven script tool that can validate your data, or
even generate code for you. Aside from the script, the CBOR library is a
standalone library which is tailored for use in microcontrollers.")
     (license license:apsl2))))

(define-public zcbor zephyr-zcbor)

(define-public zephyr-zscilib
  (let ((commit "a54986aa98db4082ac56b582843bb5b5435208a6"))
    (package
     (name "zephyr-zscilib")
     (version (git-version "0.0" "3.1" commit))
     (home-page "")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/zephyrproject-rtos/zscilib")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "03rxpajcxs0rb5inlzk9l278nr4qrhs2clkmgz1raz6rcdcsmnm2"))))
     (build-system zephyr-module-build-system)
     (arguments
      '(#:workspace-path "modules/lib/zscilib"))
     (synopsis "An open-source scientific computing library for
embedded systems running Zephyr OS or standalone.")
     (description " The Zephyr Scientific Library (zscilib) is an
attempt to provide a set of functions useful for scientific computing,
data analysis and data manipulation in the context of resource
constrained embedded hardware devices.

It is written entirely in C, and while the main development target for
the library is the Zephyr Project, it tries to be as portable as
possible, and a standalone reference project is included to use this
library in non-Zephyr-based projects.

This version of zscilib has been developed and tested against Zephyr
2.7.0.")
     (license license:apsl2))))
