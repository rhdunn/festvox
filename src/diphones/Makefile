###########################################################################
##                                                                       ##
##                   Carnegie Mellon University and                      ##
##                   Alan W Black and Kevin A. Lenzo                     ##
##                      Copyright (c) 1998-2000                          ##
##                        All Rights Reserved.                           ##
##                                                                       ##
##  Permission is hereby granted, free of charge, to use and distribute  ##
##  this software and its documentation without restriction, including   ##
##  without limitation the rights to use, copy, modify, merge, publish,  ##
##  distribute, sublicense, and/or sell copies of this work, and to      ##
##  permit persons to whom this work is furnished to do so, subject to   ##
##  the following conditions:                                            ##
##   1. The code must retain the above copyright notice, this list of    ##
##      conditions and the following disclaimer.                         ##
##   2. Any modifications must be clearly marked as such.                ##
##   3. Original authors' names are not deleted.                         ##
##   4. The authors' names are not used to endorse or promote products   ##
##      derived from this software without specific prior written        ##
##      permission.                                                      ##
##                                                                       ##
##  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         ##
##  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ##
##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ##
##  SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE      ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
##  THIS SOFTWARE.                                                       ##
##                                                                       ##
###########################################################################
##                                                                       ##
##  Makefile for diphone specific scripts                                ##
##                                                                       ##
###########################################################################
TOP=../..
DIRNAME=src/diphones
SRCS = phonealign_main.cc 
SCMFILES = diphlist.scm make_diphs_utts.scm  darpaasmrpa.scm \
           darpaaswb.scm 
SCHEMAFILES = us_schema.scm uk_schema.scm ja_schema.scm
SCRIPTS = play_them align_phones \
          make_diph_index.sh make_labs setup_diphone \
          prompt_them phonealign_test 
FILES = Makefile $(SCRIPTS) $(SRCS) $(SCMFILES) $(SCHEMAFILES)

ALL = phonealign make_diph_index

include $(TOP)/config/common_make_rules

# GCC and systems settings got from EST's config
EST=$(ESTDIR)
include $(ESTDIR)/config/config
include $(ESTDIR)/config/rules/defaults.mak
include $(ESTDIR)/config/rules/compile_options.mak

ESTINC=-I$(ESTDIR)/include
ESTLIBS=-L$(ESTDIR)/lib -lestools -lestbase -leststring $(COMPILERLIBS) -lm $(OS_LIBS)

CXXFLAGS += $(ESTINC) 

LOCAL_CLEAN = phonealign

phonealign: phonealign_main.o $(LIBDEPS)
	$(strip $(LINK_COMMAND)) -o phonealign phonealign_main.o $(ESTLIBS)
phonealign_main.o: phonealign_main.cc 
	$(strip $(CXX_COMMAND)) -c -o phonealign_main.o phonealign_main.cc

make_diph_index: make_diph_index.sh
	@echo making make_diph_index script
	@rm -f make_diph_index
	@echo "#!/bin/sh" >make_diph_index
	@echo "\"true\" ; exec "$(FESTDIR)/bin/festival --script '$$0 $$*' >>make_diph_index
	@cat make_diph_index.sh >> make_diph_index
	@chmod +x make_diph_index
