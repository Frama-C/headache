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

clean:
	$(OCAMLBUILD) -clean
	rm -f config_builtin.ml

install: headache
ifndef INSTALLDIR
	@ echo "Please define INSTALLDIR."
else
	cp -f _build/headache.native $(INSTALLDIR)/headache
endif

bootstrap: headache
	_build/headache.native -h example *.ml* Makefile

config_builtin.ml: config_builtin mkconfig
	_build/mkconfig.native

