/*************************************************************************/
/*                                                                       */
/*                   Carnegie Mellon University and                      */
/*                   Alan W Black and Kevin A. Lenzo                     */
/*                      Copyright (c) 1998-2000                          */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE      */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*  Move the pitch mark to the nearest peak.  This shouldn't really be   */
/*  necessary but it does help even for EGG extracted pms                */
/*                                                                       */
/*************************************************************************/


#include "EST.h"

int main(int argc,char **argv)
{
    EST_Wave w;
    EST_Track pm_in, pm_out;
    EST_Option al;
    EST_StrList files;
    int i,j,window,max;

    parse_command_line
	(argc,argv,
	 EST_String("[options]")+
	 "Summary: move pitchmark with respect to wave\n"+
	 "-h        Options help\n"+
	 "-wave <ifile>\n"+
	 "-pm <ifile>\n"+
	 "-window <int> {16}\n"+
	 "-o <ofile> Output pm file\n",
	 files,al);

    window = al.ival("-window");
    w.load(al.val("-wave"));
    pm_in.load(al.val("-pm"));
    pm_out.resize(pm_in.num_frames(),pm_in.num_channels());
    pm_out.copy_setup(pm_in);

    for (i=0; i<pm_in.num_frames(); i++)
    {
	int pos = (int)(pm_in.t(i)*w.sample_rate());
	for (max=pos,j=pos-window; 
	     (j > 0) && (j < w.num_samples()) && (j < pos+window);
	     j++)
	    if (w(j) > w(max))
		max = j;
	pm_out.t(i) = ((float)max)/w.sample_rate();
    }

    pm_out.save(al.val("-o"));

    return 0;
}
