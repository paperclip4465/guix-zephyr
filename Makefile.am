bin_SCRIPTS = scripts/guest

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

scripts/guest: scripts/guest.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	$(AM_V_at)chmod a+x,a-w "$@-t" && mv -f "$@-t" "$@"

nodist_noinst_SCRIPTS = pre-inst-env


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)
nobase_nodist_guilemodule_DATA = guest/config.scm

# Handy way to remove the .go files without removing all the rest.
.PHONY: clean-go
clean-go:
	-$(RM) -f $(GOBJECTS)
	@find . -path ./test-tmp -prune -o -name '*.go' -print | \
	  if test -t 1; then \
	    xargs -r echo -e "\033[31mwarning:\033[0m stray .go files:"; \
	  else \
	    xargs -r echo "warning: stray .go files:"; \
	  fi

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_go_DATA
$(guile_install_go_files): install-nobase_mod_DATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = \
	guest/config.scm \
	guest/i18n.scm \
	guest/scripts.scm \
	guest/ui.scm \
	guest/utils.scm \
	guest/scripts/list.scm \
	zephyr/zephyr/build/utils.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

EXTRA_DIST += COPYING \
	      HACKING \
	      README \
	      Readme.org \
	      .gitignore

ACLOCAL_AMFLAGS = -I m4


CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
