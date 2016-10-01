PKGNAME := erda
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))
DISTNAME := $(PKGNAME)$(DISTSUFFIX)
DISTHOME := $(PWD)/dist

default : setup

-include local.mk

# if `make setup` fails, you may need to do `make install` first
install :
	raco pkg install --name $(PKGNAME)
	$(MAKE) setup

setup :
	-rm -r doc
	raco setup $(PKGNAME)

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

test :
	raco test --direct --no-run-if-absent tests/run-*.rkt

check-pkg-deps :
	raco setup --check-pkg-deps $(PKGNAME)

api-doc :
	-rm -r doc
	mkdir -p doc
	scribble ++main-xref-in --html --dest doc --dest-name index.html manual-src/erda.scrbl

rm-dist :
	-rm -r $(DISTHOME)

pdf-manual :
	mkdir -p $(DISTHOME)
	scribble ++main-xref-in --redirect-main http://docs.racket-lang.org/ --pdf --dest $(DISTHOME) --dest-name manual.pdf manual-src/erda.scrbl

html-manual :
	-rm -r $(DISTHOME)/manual
	mkdir -p $(DISTHOME)/manual
	scribble ++main-xref-in --redirect-main http://docs.racket-lang.org/ --html --dest $(DISTHOME)/manual --dest-name index.html manual-src/erda.scrbl
	sed -r 's%[^"]+/magnolisp/doc/magnolisp/%http://magnolisp.github.io/%g' --in-place $(DISTHOME)/manual/index.html

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

gh-homepage :
	( cd gh-pages && git clean -d -f && git rm --ignore-unmatch -rf . )
	scribble ++main-xref-in --redirect-main http://docs.racket-lang.org/ --html --dest gh-pages --dest-name index.html manual-src/erda.scrbl
	sed -r 's%href="[^"]+/magnolisp/doc/magnolisp/%href="http://magnolisp.github.io/%g' --in-place gh-pages/index.html
	( cd gh-pages && git add . && git status )

gh-upload :
	( cd gh-pages && git commit -m "update $$(date -u)" && git push github gh-pages:gh-pages )
