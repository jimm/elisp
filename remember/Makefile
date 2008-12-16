.PHONY: all autoloads lisp doc clean realclean distclean fullclean install
.PHONY: test dist release
.PRECIOUS: %.elc

include Makefile.defs

EL  = $(filter-out $(PROJECT)-autoloads.el,$(wildcard *.el))
ELC = $(patsubst %.el,%.elc,$(EL))

all: autoloads lisp $(MANUAL).info $(MANUAL)-extra.info

lisp: $(ELC)

$(PROJECT)-build.elc: ./scripts/$(PROJECT)-build.el
	@echo $(PROJECT)-build.el is not byte-compiled

autoloads: $(PROJECT)-autoloads.el

$(PROJECT)-autoloads.el: $(EL)
	@$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/$(PROJECT)-build.el \
		-f $(PROJECT)-generate-autoloads .

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/$(PROJECT)-build.el \
		-f batch-byte-compile $< || :

%.info: %.texi
	makeinfo $<

%.html: %.texi
	makeinfo --html --no-split $<

doc: $(MANUAL).info $(MANUAL)-extra.info $(MANUAL).html $(MANUAL)-extra.html

clean:
	-rm -f *.elc *~

realclean fullclean: clean
	-rm -f $(MANUAL).info $(MANUAL)-extra.info \
	    $(MANUAL).html $(MANUAL)-extra.html \
	    $(PROJECT)-autoloads.el

install: autoloads lisp $(MANUAL).info $(MANUAL)-extra.info
	install -d $(ELISPDIR)
	install -m 0644 $(PROJECT)-autoloads.el $(EL) $(wildcard *.elc) \
	    $(ELISPDIR)
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 0644 $(MANUAL).info $(INFODIR)/$(MANUAL)
	install -m 0644 $(MANUAL)-extra.info $(INFODIR)/$(MANUAL)-extra
	$(INSTALLINFO) $(INFODIR)/$(MANUAL)
	$(INSTALLINFO) $(INFODIR)/$(MANUAL)-extra

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/$(PROJECT)-build.el \
		-f $(PROJECT)-elint-files $(EL)

distclean:
	-rm -f $(MANUAL).info $(MANUAL)-extra.info \
	    $(MANUAL).html $(MANUAL)-extra.html \
	    debian/dirs debian/files
	-rm -fr ../$(PROJECT)-$(VERSION)

dist: autoloads distclean
	git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ HEAD | \
	  (cd .. && tar xf -)
	rm -f ../$(PROJECT)-$(VERSION)/.gitignore
	cp $(PROJECT)-autoloads.el ../$(PROJECT)-$(VERSION)

release: dist
	(cd .. && tar -czf $(PROJECT)-$(VERSION).tar.gz \
	          $(PROJECT)-$(VERSION) && \
	  zip -r $(PROJECT)-$(VERSION).zip $(PROJECT)-$(VERSION) && \
	  gpg --detach $(PROJECT)-$(VERSION).tar.gz && \
	  gpg --detach $(PROJECT)-$(VERSION).zip)

upload: release
	(cd .. && scp $(PROJECT)-$(VERSION).zip* \
	    $(PROJECT)-$(VERSION).tar.gz* \
	    mwolson@download.gna.org:/upload/remember-el)
