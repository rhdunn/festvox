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
/*  Multi-pulse LPC, find a pulse equivalent for pitch-synchronous LPC   */
/*  that sounds good                                                     */
/*                                                                       */
/*************************************************************************/

#include "EST.h"

int main(int argc,char **argv)
{
    EST_Wave res, sig;
    EST_Track lpc;
    EST_Option al;
    EST_StrList files;
    EST_String op,owave;
    int i,j,p,v,s,e,ab,q;
    float pow,abf;

    parse_command_line
	(argc,argv,
	 EST_String("[options]")+
	 "Summary: move pitchmark with respect to wave\n"+
	 "-h        Options help\n"+
	 "-res <ifile>\n"+
	 "-lpc <ifile>\n"+
	 "-op <string>\n"+
	 "-val <int>\n"+
	 "-o <ofile> Output pm file\n",
	 files,al);

    res.load(al.val("-res"));
    lpc.load(al.val("-lpc"));
    owave = al.val(al.val("-o"));
    op = al.val("-op");

    if (op == "resynth")
    {
	lpc_filter_fast(lpc,res,sig);
    }
    if (op == "robot")
    {
	res.resize(res.num_samples(),1,0);  // no reseting of values
	res.set_sample_rate(res.sample_rate());
	v = al.ival("-val");
	printf("num_samples is %d\n",sig.num_samples());

	for (i=0; i<res.num_samples(); i++)
	    sig.a(i) = 0;
	s = 0;
	e = (int)((float)res.sample_rate()/0.010);
	for (s=0; s<sig.num_samples(); s+=e)
	{
	    pow = 0;
	    for (i=s; i<s+e; i++)
	    {
		abf = pow = 0;
		for (q=0; ((q+i)<e)&&q<v; q++)
		{
		    abf +=res.a(i+q);
		    pow += res.a(i+q)*res.a(i+q);
		}
		pow = sqrt(pow/(float)q);
		for (q=0; ((q+i)<e)&&q<v; q++)
		    sig.a(i+q) = (short)(pow*(abf/fabs(abf)));
		i+=q;
	    }
	    s = e;
	}
    }
    else if (op == "spike")
    {
	sig.resize(res.num_samples(),1,0);  // no reseting of values
	sig.set_sample_rate(res.sample_rate());
	v = al.ival("-val");
	printf("num_samples is %d\n",sig.num_samples());

	for (i=0; i<sig.num_samples(); i++)
	    sig.a(i) = 0;
	s = 0;
	for (j=0; j<lpc.num_frames(); j++)
	{
	    e = (int)(lpc.t(j)*(float)res.sample_rate());
	    pow = 0;
	    for (i=s; i<e; )
	    {
		abf = pow = 0;
		for (q=0; ((q+i)<e)&&q<v; q++)
		{
		    abf +=res.a(i+q);
		    pow += res.a(i+q)*res.a(i+q);
		}
		pow = sqrt(pow/(float)q);
		for (q=0; ((q+i)<e)&&q<v; q++)
		    sig.a(i+q) = (short)(pow*(abf/fabs(abf)));
		i+=q;
	    }
	    s = e;
	}
    }

    sig.save(al.val("-o"));

    return 0;
}
