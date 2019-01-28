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

.PHONY: headache clean install test

headache:
	dune build

clean::
	dune clean

# install
install:
ifndef INSTALLDIR
	$(error "Please define INSTALLDIR.")
else
	mkdir -p $(INSTALLDIR)
	cp -f _build/install/default/bin/headache $(INSTALLDIR)/headache
endif

# test
bootstrap: headache
	dune exec -- headache -h example $(filter-out config_builtin.ml, $(wildcard *.ml*)) Makefile doc-src/Makefile doc-src/manual.tex

test: bootstrap
	dune exec -- headache -e Makefile > example.txt
	diff -q example example.txt
	rm -f example.txt

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
