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
###                                                                       ##
###  The festvox document itself                                          ##
###                                                                       ##
############################################################################
TOP=..
DIRNAME=doc
BUILD_DIRS = 
ALL_DIRS = 
TEXT= festvox.texi refs.texi intro.texi copy.texi utt.texi \
      diphone.texi unitsel.texi general.texi \
      prosody.texi lexicons.texi text.texi resources.texi \
      eval.texi exam.texi ldom.texi phonetable.texi
IMAGESPNG = wave_clean.png wave_noisey.png \
            pm_raw_01.png pm_bad_01.png pm_raw_zoom01.png \
            pm_raw_zoom02.png pm_raw_zoom03.png
IMAGESPS = $(IMAGESPNG:.png=.eps)
IMAGES = $(IMAGESPNG) $(IMAGESPS) 
OTHERS = texinfo.tex stuff.ed speaker.licence
FILES = Makefile $(TEXT) $(OTHERS) $(IMAGES)
HTMLDIR = $(TOP)/html

ALL = 

include $(TOP)/config/common_make_rules

doc: festvox.html festvox.ps 

festvox.html: $(TEXT)
	@ if [ ! -d $(HTMLDIR) ] ; \
          then mkdir -p $(HTMLDIR) ; fi
	(cd $(HTMLDIR); texi2html -number -split_chapter $(TOP)/$(DIRNAME)/festvox.texi)
# give the html files background color of white
	@ for i in $(HTMLDIR)/*.html ; \
	  do \
	    sed 's/<BODY>/<BODY bgcolor="#ffffff">/' $$i >ttt.html; \
	    mv ttt.html $$i ; \
	  done
	@ cat stuff.ed | ed $(HTMLDIR)/festvox_toc.html
	@ (cd $(HTMLDIR); rm -f $(IMAGESPNG); )
	@ ln $(IMAGESPNG) $(HTMLDIR)
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

