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

DOC_SRC?= .
MANUAL := $(DOC_SRC)/manual

DOC_FILES= $(addprefix $(MANUAL)., pdf ps.gz html txt)

.PHONY: doc clean-doc install-doc

doc: $(DOC_FILES)

clean-doc:
	rm -f $(addprefix $(MANUAL)., pdf ps ps.gz dvi aux log thm txt html haux htoc)

clean:: clean-doc

$(MANUAL).dvi: $(MANUAL).tex
	cd $(dir $@) && latex $(notdir $<)
	cd $(dir $@) && latex $(notdir $<)

$(MANUAL).ps: $(MANUAL).dvi
	dvips -t a4 -o $@ $<

$(MANUAL).ps.gz: $(MANUAL).ps
	gzip -c $< > $@

$(MANUAL).pdf: $(MANUAL).dvi
	dvipdfm -p a4 -o $@ $<

$(MANUAL).html: $(MANUAL).tex $(MANUAL).dvi
	hevea -o $@ $<

$(MANUAL).txt: $(MANUAL).tex $(MANUAL).dvi
	hevea -o $@ -text $<

install-doc::
ifndef DOC_INSTALLDIR
	$(error "Please define DOC_INSTALLDIR.")
else
	mkdir -p $(DOC_INSTALLDIR)/
	cp -f $(DOC_FILES) $(DOC_INSTALLDIR)/
endif
