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
;;;  Simple example of code for generating nonsense words containing      ;;
;;;  all diphones in a particular phone set.  Inspired by Steve Isard's   ;;
;;;  diphone schema's from CSTR, University of Edinburgh                  ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; You want to also include a definition of diphone-gen-list
;;; for the phone set you are building for

(define (remove-list l1 l2)
  "(remove-list l1 l2)
Remove all members of l2 from l1."
  (if l2
      (remove-list (remove (car l2) l1) (cdr l2))
      l1))

(define (remove-all i l)
  (cond
   ((null l) l)
   ((string-equal i (car l))
    (remove-all i (cdr l)))
   (t
    (cons (car l) (remove-all i (cdr l))))))
   

(define (synth-nonsense-word phones)
  "(synth-nonsense-word phones)
Synthesize given phone list.  It is assumed that the current voice
is appropriate."
  (let ((utt (eval (list 'Utterance 'Phones (remove-all '- phones)))))
    (Initialize utt)
    (Fixed_Prosody utt)
    (if (boundp 'Diphone_Prompt_Word)
	(Diphone_Prompt_Word utt))
    (Wave_Synth utt)
    utt))

(define (diphone-gen-waves wavedir labdir dlist)
  "(diphone-gen-waves wavedir labdir dlist)
Synthesis each nonsense word with fixed duration and fixed pitch
saving the waveforms for playing as prompts (as autolabelling)."
  (if (boundp 'Diphone_Prompt_Setup)
      (Diphone_Prompt_Setup))
  (mapcar
   (lambda (d)
     (let ((utt (synth-nonsense-word (car (cdr (cdr d))))))
       (format t "%l\n" d)
       (utt.save.wave utt (format nil "%s/%s.wav" wavedir (car d)))
       (if labdir
	   (utt.save.segs utt (format nil "%s/%s.lab" labdir (car d))))
       t))
   dlist)
  t
)

(define (diphone-gen-schema name outfile wavedir labdir)
  "(diphone-gen-schema name outfile wavedir labdir)
Saves a complete diphone schema in named file.  FIlenames for
each diphone are generated as name_XXXX.  If wavedir is non-nil
it is treated as a directory and waveform files for each nonsense
word will be synthesized with fixed duration and prosody."
  (let ((ofd (fopen outfile "w"))
	(dlist (diphone-gen-list))
	(n 0))
    ;; Name them
    (set! dlist-all
	  (mapcar
	   (lambda (d)
	     (cons (format nil "%s_%04d" name (set! n (+ 1 n))) d))
	   dlist))
    ;; save the info to file
    (mapcar
     (lambda (d)
       (format ofd "%s %l\t%l\n"
	       (car d) (car (cdr d)) (car (cdr (cdr d)))))
     dlist-all)
    (fclose ofd)
    (if wavedir
	(diphone-gen-waves wavedir labdir dlist-all))
    t))

(provide 'diphlist)
