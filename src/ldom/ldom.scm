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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;; Code for building data for prompts and aligning                     ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do_prompt name text) 
  (let ((utt1 (utt.synth (eval (list 'Utterance 'Text text)))))
    (utt.save.segs utt1 (format nil "prompt-lab/%s.lab" name))
    (utt.save.wave utt1 (format nil "prompt-wav/%s.wav" name))))

(define (build_prompts file)
  (voice_kal_diphone) 
 (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (format t "%s\n" (car l))
       (do_prompt (car l) (cadr l)))
     p)
    t))

(define (build_utts file)
  (voice_kal_diphone)
  (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (format t "%s\n" (car l))
       (align_utt (car l) (cadr l)))
     p)
    t))

(define (align_utt name text)
  (let ((utt1 (utt.synth (eval (list 'Utterance 'Text text)))))
    (utt.relation.load utt1 'actual-segment 
		       (format nil "lab/%s.lab" name))
    (mapcar
     (lambda (a b)
       (item.set_feat a "end" (item.feat b "end")))
     (utt.relation.items utt1 'Segment)
     (utt.relation.items utt1 'actual-segment))
    (utt.relation.delete utt1 'actual-segment)
    (utt.set_feat utt1 "fileid" name)
    (utt.save utt1 (format nil "festival/utts/%s.utt" name))
    t))

(provide 'ldom)

