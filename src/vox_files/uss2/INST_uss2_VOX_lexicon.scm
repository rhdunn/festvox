;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CMUSPHINX2 lexicon for US English
;;;

;;; Load any necessary files here
(require 'postlex)

;;; CMUSPHINX2 lexicon and LST rules
(load (string-append lexdir "/cmusphinx2/cmusphinx2-lex.scm"))

(define (INST_uss2_VOX_postlex_apos_S_check utt)
  "(INST_uss2_VOX_postlex_apos_S_check utt)
Deal with possesive s for English (American and British).  Delete
schwa of 's if previous is not a fricative or affricative, and
change voiced to unvoiced s if previous is not voiced."
  (mapcar
   (lambda (seg)
     (if (string-equal "'s" (item.feat 
			     seg "R:SylStructure.parent.parent.name"))
	 (if (string-equal "a" (item.feat seg 'ph_vlng))
	     (if (and (member_string (item.feat seg 'p.ph_ctype) 
				     '(f a))
		      (not (member_string
			    (item.feat seg "p.ph_cplace") 
			    '(d b g))))
		 t;; don't delete schwa
		 (item.delete seg))
	     (if (string-equal "-" (item.feat seg "p.ph_cvox"))
		 (item.set_name seg "S")))));; from "Z"
   (utt.relation.items utt 'Segment))
  utt)


(define (INST_uss2_VOX::select_lexicon)
  "(INST_uss2_VOX::select_lexicon)
Set up the CMUSPHINX2 lexicon for US English."
  (lex.select "cmusphinx2")

  ;; Post lexical rules
  (set! postlex_rules_hooks (list INST_uss2_VOX_postlex_apos_S_check))
  (set! postlex_vowel_reduce_cart_tree 
	postlex_vowel_reduce_cart_data)
)

(define (INST_uss2_VOX::reset_lexicon)
  "(INST_uss2_VOX::reset_lexicon)
Reset lexicon information."
  t
)

(provide 'INST_uss2_VOX_lexicon)
