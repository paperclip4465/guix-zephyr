dnl -*- Autoconf -*-

AC_INIT([Guest], [0.0.1],
	[mitchellschmeisser@librem.one],
	[guest],
	[https://github.com/paperclip4465/guix-zephyr])
AC_SUBST(HVERSION, "0.0.1")
AC_SUBST(AUTHOR, "Mitchell Schmeisser")
AC_SUBST(COPYRIGHT, "'(2023)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(scripts/guest.in)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl guile-module dependencies
GUILE_MODULE_REQUIRED([yaml])

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_CONFIG_FILES([Makefile
		 guest/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/guest],[chmod +x scripts/guest])


AC_OUTPUT
