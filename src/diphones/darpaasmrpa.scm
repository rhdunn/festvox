;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                    Alan W Black and Kevin Lenzo                       ;;
;;;                         Copyright (c) 1998                            ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission to use, copy, modify,  and licence this software and its  ;;
;;;  documentation for any purpose, is hereby granted without fee,        ;;
;;;  subject to the following conditions:                                 ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;                                                                       ;;
;;;  THE AUTHORS OF THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO      ;;
;;;  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ;;
;;;  AND FITNESS, IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY         ;;
;;;  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES            ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;  This file is part "Building Voices in the Festival Speech            ;;
;;;  Synthesis System" by Alan W Black and Kevin Lenzo written at         ;;
;;;  Robotics Institute, Carnegie Mellon University, fall 98              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;  Mapping code to allow generation of DARPA prompts (US) with a mrpa   ;;
;;;  voice.                                                               ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (Diphone_Prompt_Setup)
 "(Diphone_Prompt_Setup)
Sets up a UK English speaker but will actually map the US phones to
a close approximation in the RP set."
 (voice_ked_diphone)  ;; for US durations
 (voice_rab_diphone)  ;; UK male voice
 (set! UniSyn_module_hooks nil)
; (set! FP_F0 90)      ;; lower F0 than ked
 )

;;; This table maps the darpa (radio) phones to mrpa names
;;; Most of the consonats are the same but some approximation is
;;; necessary for the vowels.  Note the generate prompts will probabaly
;;; not be very good without careful supervision.  The US speaker will
;;; probably subconsciously copy the UK phones, but rhese are good enough
;;; for autoaligning

(set! darpa_to_mrpa_map
      '(; b p d t g k ch jh th dh f v s z sh zh l r w j m n ng are the same
	(hh h)
	(pau #)
	;; Vowels require more guesses
	(aa aa)
	(ae a)
	(ah @)
	(ao oo)
	(aw au)
	(ax @)
	(ay ai)
	(eh e)
	(ey ei)
	(ih i)
	(iy ii)
	(ow ou)
	(oy oi)
	(uh u)
	(uw uu)
	(er @@)
	(dx t)
	(en n)
	(el l)
	(em m)))

(define (Diphone_Prompt_Word utt)
  "(Diphone_Prompt_Word utt)
Map the darpa phones to mrpa, as an example of using a speaker
from a different language/dialect to present the prompts."
  (mapcar
   (lambda (s)
     (let ((np (assoc_string (item.name s) darpa_to_mrpa_map)))
       (if np
	   (item.set_feat s "us_diphone" (car (cdr np))))))
   (utt.relation.items utt 'Segment))
  (set! phoneme_durations kd_durs)
  (Parameter.set 'Duration_Stretch '1.2)
  (Duration_Averages utt))
  

(provide 'darpaasmrpa)
