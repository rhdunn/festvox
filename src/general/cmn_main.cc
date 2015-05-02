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
/*  Cepstral mean normalization.                                         */
/*  This is done in two passes, first to find means and std then to      */
/*  apply these to a given track                                         */
/*                                                                       */
/*************************************************************************/

#include "EST.h"

static void find_meanstd(EST_Track &ss, EST_StrList &files);
static void cep_normalize(EST_Track &tt, const EST_Track &ss);

int main(int argc,char **argv)
{
    EST_Option al;
    EST_StrList files;
    EST_Litem *p;
    EST_Track ss;

    parse_command_line
	(argc,argv,
	 EST_String("[options]")+
	 "Summary: find (and apply) means/stddev for cepstral normalization\n"+
         "         in applymsd case files are overwritten\n"+
	 "-h        Options help\n"+
	 "-findmsd  Args are treated as (example) cep files form which\n"+
         "          stats are generated\n"+
         "-omsd <ofile> {msd.out}\n"+
         "          Output file for means and stddev for coefficients\n"+
         "-applymsd Args are treated as files to be normalized\n"+
	 "-imsd <ifile> {msd.out}\n"+
         "          Input file for means and stddev\n",
	 files,al);

    if (al.present("-findmsd"))
    {
	find_meanstd(ss,files);
	ss.save(al.val("-omsd"));
    }
    else if (al.present("-applymsd"))
    {
	EST_Track tt;

	ss.load(al.val("-imsd"));
	for (p=files.head(); p != 0; p=next(p))
	{
	    tt.load(files(p));
	    cep_normalize(tt,ss);
	    tt.save(files(p));
	}
    }
    else
    {
	cerr << "cmn: neither -findmsd or -applymsd specified" << endl;
	exit(-1);
    }

    return 0;
}

static void find_meanstd(EST_Track &ss, EST_StrList &files)
{
    // Find means and stddev for each coefficient
    int i,j;
    float v;
    EST_Litem *p;
    EST_Track tt;
    EST_SuffStats *sstable = 0;

    p = files.head();
    if (p == NULL)
    {
	cerr << "cmn: no files to build stats from" << endl;
	exit(-1);
    }
    tt.load(files(p));

    ss = tt;
    ss.resize(2,tt.num_channels());
    sstable = new EST_SuffStats[tt.num_channels()];
    
    for (p=files.head(); p != 0; p=next(p))
    {
	tt.load(files(p));
	for (i=0; i<tt.num_frames(); i++)
	    for (j=0; j<tt.num_channels(); j++)
	    {
		v = tt.a_no_check(i,j);
		if (!finite(v))   // sigh, better safe that sorry
		    v = 100;
		if (fabs(v) > 100)
		    v = 100;
		sstable[j] += v;
	    }
    }

    for (j=0; j<ss.num_channels(); j++)
    {
	ss.a_no_check(0,j) = sstable[j].mean();
	ss.a_no_check(1,j) = sstable[j].stddev();
    }

    delete [] sstable;
}

static void cep_normalize(EST_Track &tt, const EST_Track &ss)
{
    // Normalize coefficeints in tt from means and stddevs in ss
    int i,j;

    if (tt.num_channels() != ss.num_channels())
    {
	cerr << "cmn: meanstd files has " << ss.num_channels() <<
	    " while cep track has " << tt.num_channels() << endl;
	exit(-1);
    }

    for (i=0; i < tt.num_frames(); i++)
	for (j=0; j < tt.num_channels(); j++)
	{
//	    printf("i %d j %d\n",i,j);
//	    printf("tt(i,j) %f ss.mean %f ss.stddev %f\n",
//		   tt.a_no_check(i,j),ss.a_no_check(0,j),ss.a_no_check(1,j));
	    tt.a_no_check(i,j) = 
		(tt.a_no_check(i,j) - ss.a_no_check(0,j)) / ss.a_no_check(1,j);
//	    if (tt.a_no_check(i,j) > 50)
//		tt.a_no_check(i,j) = 50;
//	    else if (tt.a_no_check(i,j) < -50)
//		tt.a_no_check(i,j) = -50;
	}
}

