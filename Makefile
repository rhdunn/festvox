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
##  Authors: Alan W Black (awb@cs.cmu.edu)                               ##
##           Kevin A. Lenzo (lenzo@cs.cmu.edu)                           ##
##  Version: festvox-1.2-beta July 2000                                  ##
##                                                                       ##
###########################################################################
##                                                                       ##
##  Documentation, tools and scripts to aid building of new synthetic    ##
##  voice for the Festival Speech Synthesis System.                      ##
##                                                                       ##
##  This project's home page is http://www.festvox.org                   ##
##                                                                       ##
##  This release corresponds to the Festival 1.4.1 release (which        ##
##  incorporates Edinburgh Speech Tools 1.2.1)                           ##
##                                                                       ##
###########################################################################
TOP=.
DIRNAME=.
BUILD_DIRS = src doc docbook
ALL_DIRS=config festvox.org course $(BUILD_DIRS)
OTHERS = README ACKNOWLEDGEMENTS ANNOUNCE-1.1 ANNOUNCE-1.2
FILES = Makefile $(OTHERS)

ALL = $(BUILD_DIRS)

# Try and say if config hasn't been created
config_dummy := $(shell test -f config/config || { echo '*** '; echo '*** Please Copy config/config-dist to config and edit to set options ***'; echo '*** '; }  >&2)

# force a check on the system file
system_dummy := $(shell $(MAKE) -C $(TOP)/config -f make_system.mak TOP=.. system.mak)

include $(TOP)/config/common_make_rules

release: 
	rm -f html/index.html html/festvox.ps.gz html/festvox.tar.gz
	cp -p doc/festvox.ps html/festvox.ps
	gzip html/festvox.ps
	ln -s festvox_toc.html html/index.html
	$(MAKE) dist
	cp -p $(PROJECT_PREFIX)-$(PROJECT_VERSION)-$(PROJECT_STATE).tar.gz html/
	ln html/$(PROJECT_PREFIX)-$(PROJECT_VERSION)-$(PROJECT_STATE).tar.gz html/festvox.tar.gz

backup: time-stamp
	@ $(RM) -f $(TOP)/FileList
	@ $(MAKE) file-list
	@ echo .time-stamp >>FileList
	@ sed 's/^\.\///' <FileList | sed 's/^/festvox\//' >.file-list-all
	@ (cd ..; tar zcvf festvox/$(PROJECT_PREFIX)-$(PROJECT_VERSION)-$(PROJECT_STATE).tar.gz `cat festvox/.file-list-all`)
	@ $(RM) -f $(TOP)/.file-list-all
	@ ls -l $(PROJECT_PREFIX)-$(PROJECT_VERSION)-$(PROJECT_STATE).tar.gz

# dist doesn't include the festvox.org site html files
dist: time-stamp
	@ $(RM) -f $(TOP)/FileList
	@ $(MAKE) file-list
	@ echo .time-stamp >>FileList
	@ sed 's/^\.\///' <FileList | grep -v "festvox.org" | grep -v "^course/" | grep -v "^docbook/" | sed 's/^/festvox\//' >.file-list-all
	@ ls html/*.html | sed 's/^/festvox\//' >>.file-list-all
	@ ls html/*.png | sed 's/^/festvox\//' >>.file-list-all
	@ ls html/festvox.ps.gz | sed 's/^/festvox\//' >>.file-list-all
	@ (cd ..; tar zcvf festvox/$(PROJECT_PREFIX)-$(PROJECT_VERSION)-$(PROJECT_STATE).tar.gz `cat festvox/.file-list-all`)
	@ $(RM) -f $(TOP)/.file-list-all
	@ ls -l $(PROJECT_PREFIX)-$(PROJECT_VERSION)-$(PROJECT_STATE).tar.gz

time-stamp :
	@ echo $(PROJECT_NAME) >.time-stamp
	@ echo $(PROJECT_PREFIX) >>.time-stamp
	@ echo $(PROJECT_VERSION) >>.time-stamp
	@ echo $(PROJECT_DATA) >>.time-stamp
	@ echo $(PROJECT_STATE) >>.time-stamp
	@ date >>.time-stamp

