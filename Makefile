##########################################################################
#                                                                        #
#                               Headache                                 #
#                                                                        #
#          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           #
#                                                                        #
#  Copyright 2002                                                        #
#  Institut National de Recherche en Informatique et en Automatique.     #
#  All rights reserved.  This file is distributed under the terms of     #
#  the GNU Library General Public License.                               #
#                                                                        #
#  Vincent.Simonet@inria.fr           http://cristal.inria.fr/~simonet/  #
#                                                                        #
##########################################################################

OCAMLBUILD = ocamlbuild -package camomile -package unix -package str

.PHONY: headache mkconfig clean install bootstrap

headache: config_builtin.ml
	$(OCAMLBUILD) headache.native

mkconfig:
	$(OCAMLBUILD) mkconfig.native

clean::
	$(OCAMLBUILD) -clean
	rm -f config_builtin.ml

install:
ifndef INSTALLDIR
	$(error "Please define INSTALLDIR.")
else
	mkdir -p $(INSTALLDIR)
	cp -f _build/headache.native $(INSTALLDIR)/headache
endif

bootstrap: headache
	_build/headache.native -h example $(filter-out config_builtin.ml, $(wildcard *.ml*)) Makefile doc-src/Makefile doc-src/manual.tex

config_builtin.ml: config_builtin.txt mkconfig
	_build/mkconfig.native

# documentation
ifndef DOC_INSTALLDIR
DOC_SRC= doc-src
# default installation from ./doc-src to ./doc and update of ./README
DOC_INSTALLDIR= doc
.PHONY: install-doc
install-doc::
	cp -f doc-src/manual.txt ./README
else
# for installation from ./doc to $(DOC_INSTALLDIR)
DOC_SRC= doc
endif
sinclude doc-src/Makefile
