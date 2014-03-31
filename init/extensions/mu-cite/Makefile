#
# Makefile for mu-cite.
#

PACKAGE = mu-cite
API	= 8
RELEASE = 1

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l MU-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc auto-autoloads.el custom-load.el \
	  *.cp *.cps *.ky *.kys *.fn *.fns *.vr *.vrs \
	  *.pg *.pgs *.tp *.tps *.toc *.aux *.log
FILES	= README.?? Makefile MU-MK MU-CFG MU-ELS *.el ChangeLog

VERSION	= $(API).$(RELEASE)
ARC_DIR = /pub/elisp/mu

elc:
	$(EMACS) $(FLAGS) -f compile-mu $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-mu $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


package:
	$(XEMACS) $(FLAGS) -f compile-mu-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-mu-package $(PACKAGEDIR)

clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) | tr . _`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		mu'
	cd /tmp; $(RM) $(PACKAGE)-$(VERSION)/ftp.in ; \
		$(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in | sed "s/API/$(API)/" \
		| sed "s/PACKAGE/$(PACKAGE)/" > ftp

invoice:
	sed "s/VERSION/$(VERSION)/" < ftp.in | sed "s/API/$(API)/" \
		| sed "s/PACKAGE/$(PACKAGE)/" > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
