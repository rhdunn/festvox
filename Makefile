############################################################################
###                                                                       ##
###                    Alan W Black and Kevin Lenzo                       ##
###                         Copyright (c) 1998                            ##
###                        All Rights Reserved.                           ##
###                                                                       ##
###  Permission to use, copy, modify,  and licence this software and its  ##
###  documentation for any purpose, is hereby granted without fee,        ##
###  subject to the following conditions:                                 ##
###   1. The code must retain the above copyright notice, this list of    ##
###      conditions and the following disclaimer.                         ##
###   2. Any modifications must be clearly marked as such.                ##
###   3. Original authors' names are not deleted.                         ##
###                                                                       ##
###  THE AUTHORS OF THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO      ##
###  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ##
###  AND FITNESS, IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY         ##
###  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES            ##
###  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
###  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
###  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
###  THIS SOFTWARE.                                                       ##
###                                                                       ##
############################################################################
###                                                                       ##
###  "Building Voices in The Festival Speech Synthesis System"            ##
###     by Alan W Black and Kevin Lenzo                                   ##
###  Robotics Institute, Carnegie Mellon University, Fall 98              ##
###                                                                       ##
############################################################################
###                                                                       ##
###  Documention, examples scripts and scheme files                       ##
###                                                                       ##
############################################################################
TOP=.
DIRNAME=.

TEXT= festvox.texi refs.texi intro.texi copy.texi utt.texi \
      diphone.texi unitsel.texi \
      prosody.texi lexicons.texi text.texi resources.texi \
      eval.texi exam.texi
SRCDIPHONES = src/diphones/diphlist.scm src/diphones/phonealign_main.cc \
              src/diphones/darpaschema.scm \
              src/diphones/darpaasmrpa.scm \
              src/diphones/darpaaswb.scm \
              src/diphones/play_them \
              src/diphones/align_phones \
              src/diphones/make_diph_index.sh \
              src/diphones/make_diphs_utts.scm \
              src/diphones/make_labs \
	      src/diphones/Makefile src/diphones/phonealign_test 
SRCUNITSEL =  src/unitsel/Makefile \
              src/unitsel/make_mcep
SRCINTONE =   src/intonation/Makefile \
              src/intonation/tree_f0.scm
SRCGENERAL =  src/general/Makefile \
	      src/general/make_dirs \
	      src/general/make_pm \
	      src/general/make_pm_wave \
              src/general/make_lpc \
              src/general/find_unknowns.sh
SRCDB = src/db_example
SCRIPTS = 
OTHERS = texinfo.tex plan README stuff.ed
#PATCHFILES = fest-1.3.0.1.patch sptools-1.1.0.1.patch
FILES = Makefile $(OTHERS) $(TEXT) $(SRCDIPHONES) $(SRCUNITSEL) \
        $(SRCGENERAL) $(SRCDB) $(SRCINTONE)

doc: festvox.ps festvox.html festvox.info 

festvox.html: $(TEXT)
	@ if [ ! -d html ] ; \
          then mkdir -p html ; fi
	(cd html; texi2html -number -split_chapter ../festvox.texi)
	# give the html files background color of white
	@ for i in html/*.html ; \
	  do \
	    sed 's/<BODY>/<BODY bgcolor="#ffffff">/' $$i >ttt.html; \
	    mv ttt.html $$i ; \
	  done
	cat stuff.ed | ed html/festvox_toc.html
festvox.ps: festvox.dvi
	dvips -f festvox.dvi >festvox.ps
festvox.dvi: $(TEXT)
	tex festvox.texi
	texindex festvox.cp
	tex festvox.texi
festvox.info: $(TEXT)
	makeinfo festvox.texi
refs:
	texirefs $(TEXT)
clean:
	rm -rf *~ *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *.cps
backup:
	@ ls -d $(FILES) | sed 's/^/festvox\//' >.filelist
	@ (cd ..; tar zcvf festvox/festvox.tar.gz `cat festvox/.filelist`)
	@ ls -l festvox.tar.gz

