PKGNAME := erda
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))
DISTNAME := $(PKGNAME)$(DISTSUFFIX)
DISTHOME := $(PWD)/dist

default : setup

-include local.mk

setup :
	raco setup $(PKGNAME)

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

test :
	raco test --direct --no-run-if-absent tests/run-*.rkt

api-doc :
	-rm -r doc
	mkdir -p doc/manual
	scribble ++xref-in setup/xref load-collections-xref --html --dest doc/manual --dest-name index.html manual-src/manual.scrbl

rm-dist :
	-rm -r $(DISTHOME)

pdf-manual :
	mkdir -p $(DISTHOME)
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --pdf --dest $(DISTHOME) --dest-name manual.pdf manual-src/manual.scrbl

html-manual :
	-rm -r $(DISTHOME)/manual
	mkdir -p $(DISTHOME)/manual
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest $(DISTHOME)/manual --dest-name index.html manual-src/manual.scrbl
	sed 's%http://docs.racket-lang.org/[a-z-]*/magnolisp/doc/manual/%http://magnolisp.github.io/%g' --in-place dist/manual/index.html

MIRROR_DIR := /tmp/raco-tmp/$(PKGNAME)

# this indirection ensures that we only get what we would have in a Git repo
# (using 'git archive' would be more straightforward, but for future proofing)
pkg :
	-mkdir $(DISTHOME)
	-rm -r $(MIRROR_DIR)
	mkdir -p $(MIRROR_DIR)
	cp -ai ./ $(MIRROR_DIR)/
	( cd $(MIRROR_DIR) && git clean -dxff && rm -rf $(MIRROR_DIR)/.git && raco pkg create --format tgz --dest $(DISTHOME) --from-dir $(MIRROR_DIR) )

website-local :

website : rm-dist html-manual pdf-manual pkg website-local
	chmod -R a+rX $(DISTHOME)
