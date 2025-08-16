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

(define-module (zephyr packages zmk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (zephyr build-system zephyr)
  #:use-module ((zephyr packages) #:prefix zephyr:)
  #:use-module (zephyr packages zephyr)
  #:use-module (zephyr packages zephyr-xyz)

  #:export (zmk-for-board))

(define (prettify-name name)
  (string-join (string-split name #\_) "-"))

(define-public zephyr-zmk-3.5
  (let ((commit "628a0d85e36938dddb6f0dfc6dc902de7359711c"))
    (package/inherit zephyr-3.5
      (name "zmk-zephyr")
      (source (origin (method git-fetch)
                      (uri (git-reference
                            (url "https://github.com/zmkfirmware/zephyr")
                            (commit commit)))
                      (file-name (git-file-name name (substring commit 0 5)))
                      (sha256
                       (base32 "0hmwi30ljh7a583zws40wqz38il7crplmvvsf62rg67yd64iksza"))
                      (patches
                       (zephyr:search-patches "zephyr-3.1-linker-gen-abs-path.patch")))))))

(define-public zmk-nanopb
  (let ((commit "8c60555d6277a0360c876bd85d491fc4fb0cd74a"))
    (package/inherit zephyr-nanopb
      (name "zmk-nanopb")
      (version (git-version "0.4.8" "" commit))
      (source (origin (method git-fetch)
                      (uri (git-reference
                            (url "https://github.com/zmkfirmware/nanopb")
                            (commit commit)))
                      (file-name (git-file-name name "0.4.8"))
                      (sha256
                       (base32 "1w77q47cvhg7xmfzbws4w2pn1zr74vh9lyzj0cf1p8gz0n2l3q1g")))))))

(define* (zmk-for-board board
                        #:key
                        config  shield
                        (extra-modules '())
                        (extra-config-flags '()))
  "Return a ZMK firmware package configured for board/shield.
If config is provided it should be a file-like object
pointing to a ZMK configuration repository which contains a \"config/\"
directory as described here: https://zmk.dev/docs/customization"
  (let ((commit "aa3e5dd70fdd1b364fa9ad26f14425be613d180c"))
    (package
      (name (prettify-name
             (string-append "zmk-" board
                            (if shield
                                (string-append "-" shield)
                                ""))))
      (version (git-version "0.3.0" "" commit))
      (source (origin (method git-fetch)
                      (uri (git-reference
                            (url "https://github.com/zmkfirmware/zmk")
                            (commit commit)))
                      (file-name (git-file-name name version))
                      (sha256
                       (base32 "0dq5i0rff3wb1g0s561v3ynqqdp77cwzryxhs0rnfyn7cfg7hs9h"))))
      (home-page "zmk.dev")
      (build-system zephyr-build-system)
      (arguments
       `(#:configure-flags
         ,#~(append
             (list (string-append "-DBOARD=" #$board)
                   "-DCONFIG_NEWLIB_LIBC=y"
                   "-DCONFIG_BUILD_OUTPUT_UF2=y")
             (if #$config
                 (list (string-append "-DZMK_CONFIG=" #$config "/config"))
                 '())
             (if #$shield
                 (list (string-append "-DSHIELD=" #$shield))
                 '())
             '#$extra-config-flags)
         #:bin-name ,name
         #:zephyr ,zephyr-zmk-3.5
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-to-app
             (lambda _
               (chdir "app")))
           (add-before 'install 'rename-outputs
             (lambda _
               (chdir "../build")
               (map (lambda (ext)
                      (let ((orig (string-append "zephyr/zmk." ext)))
                        (if (file-exists? orig)
                            (copy-file orig (string-append "zephyr/zephyr." ext))
                            (format #t "Skipping renaming ~a. File does not exist!\n" orig))))
                    ',(append %default-output-extensions %default-debug-extensions)))))))
      (native-inputs
       (append (list zephyr-cmsis
                     zephyr-lvgl
                     zmk-nanopb)
               (if config (list config) '())
               extra-modules))
      (synopsis "Zephyr™ Mechanical Keyboard firmware")
      (description
       (format #f "ZMK Firmware is keyboard firmware built on the Zephyr™ Project Real
Time Operating System. ZMK's goal is to provide a modern, wireless,
and powerful firmware free of licensing issues.
Configured for board/shield: ~a/~a." board shield))
      (license license:expat))))

(define-public zmk-planck-rev6
  (zmk-for-board "planck_rev6"
                 #:extra-modules (list zephyr-hal-stm32)))

(define-public zmk-bdn9-rev2
  (zmk-for-board "bdn9_rev2"
                 #:extra-modules (list zephyr-hal-stm32)))

(define-public zmk-bt60-v1-hs
  (zmk-for-board "bt60_v1_hs"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-bt60-v1
  (zmk-for-board "bt60_v1"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-bt60-v2
  (zmk-for-board "bt60_v2"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-bt65-v1
  (zmk-for-board "bt65_v1"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-corneish-zen-v1-left
  (zmk-for-board "corneish_zen_v1_left"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-corneish-zen-v1-right
  (zmk-for-board "corneish_zen_v1_right"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-corneish-zen-v2-left
  (zmk-for-board "corneish_zen_v2_left"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-corneish-zen-v2-right
  (zmk-for-board "corneish_zen_v2_right"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-ferris-rev02
  (zmk-for-board "ferris_rev02"
                 #:extra-modules (list zephyr-hal-stm32)))

(define-public zmk-nice-nano-v2-kyria-left
  (zmk-for-board "nice_nano_v2"
                 #:shield "kyria_left"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

(define-public zmk-nice-nano-v2-kyria-right
  (zmk-for-board "nice_nano_v2"
                 #:shield "kyria_right"
                 #:extra-modules (list zephyr-hal-nordic
                                       zephyr-tinycrypt)))

;;; zmk.scm ends here
