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
;;;                                                                      ;;
;;;  Scheme for diphones (US English)                                    ;;
;;;  Inspired by Steve Isard's diphone schemas from CSTR, University of  ;;
;;;  Edinburgh                                                           ;;
;;;                                                                      ;;
;;;  This is based on the cmusphinx2 phoneset yet another darpabet       ;;
;;;  Butu this allows consistency between Festival, Sphinx2 and CMUDICT  ;;
;;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A diphone list for DARPAbet

(set! vowels '(AA AE AH AO AW AX AY EH EY IH IY OW OY UH UW ))
(set! consonants '(B P D T G K CH JH HH TH DH F V S Z SH ZH L R W Y M N NG ER))
(set! onset-only '(HH Y W))
(set! nocvcs '(Y W R L M N NG))
(set! coda-only '(NG))
(set! silence 'SIL)
;; for consonant clusters
(set! stops '(P B T D K G))
(set! nasals '(N M))
(set! liquids '(L R W))
(set! clusters1
      (append
       (apply
	append
	(mapcar (lambda (b) (mapcar (lambda (a) (list a b)) stops)) liquids))
       (mapcar (lambda (b) (list 'S b)) '(L W N M P T K F))
       (mapcar (lambda (b) (list 'F b)) '(L R ))))
(set! clusters2
      (mapcar (lambda (b) (list b 'Y))  (append stops nasals '(S F V HH))))

(set! cvc-carrier '((SIL T AA ) (AA SIL)))
(set! ooer-carrier '((SIL T AA ) (AA SIL)))
(set! vc-carrier '((SIL T AA T) (AA SIL)))
(set! cv-carrier '((SIL T AA ) (T AA SIL)))
(set! cc-carrier '((SIL T AA ) (AA T AA SIL)))
(set! vv-carrier '((SIL T AA T) (T AA SIL)))
(set! silv-carrier '(() (T AA SIL)))
(set! silc-carrier '(() (AA T AA SIL)))
(set! vsil-carrier '((SIL T AA T ) ()))
(set! csil-carrier '((SIL T AA T AA) ()))

;;; These are only the minimum ones, you should (if possible)
;;; consider consonant clusters i.e to distringuish
;;; T AA T - R AA t and T AA - T R AA t
(set! cc1-carrier '((SIL T AA -) (AA T AA SIL)))
;;; for T AA - T _Y_ UW T AA 
(set! cc2-carrier '((SIL T AA -) (UW T AA SIL)))
;;; Open and vowels to (syllable end after vowel)
(set! vcopen-carrier '((SIL T AA T) (AA SIL)))
;;; Syllabics
(set! syllabics-carrier1 '((SIL T AA ) ( AA SIL))) ;; c-syl
(set! syllabics-carrier2 '((SIL T AA T) (AA SIL))) ;; syl-c
(set! syllabics-carrier3 '((SIL T AA T) (T AA SIL))) ;; syl-v
(set! syllabics-carrier4 '((SIL T AA T) (T AA SIL))) ;; syl-syl
;;; taps
(set! taps1 '((SIL T ) ( DX AX L SIL)))
(set! taps2 '((SIL T AA ) ( DX AX L SIL)))


;;; These functions simply fill out the nonsense words
;;; from the carriers and vowel consonant definitions

(define (list-cvcs)
  (apply
   append
   (mapcar
    (lambda (v)
      (mapcar
       (lambda (c)
	 (list
	  (list (string-append c "-" v) (string-append v "-" c))
	  (append (car cvc-carrier) (list c v c) (car (cdr cvc-carrier)))))
       (remove-list consonants nocvcs)))
    vowels)))

(define (list-vcs)
  (apply
   append
   (mapcar
    (lambda (v)
      (mapcar
       (lambda (c)
	 (list
	  (list (string-append v "-" c))
	  (append (car vc-carrier) (list v c) (car (cdr vc-carrier)))))
       nocvcs))
    vowels)))

(define (list-cvs)
  (apply
   append
   (mapcar
    (lambda (c)
      (mapcar 
       (lambda (v) 
	 (list
	  (list (string-append c "-" v))
	  (append (car cv-carrier) (list c v) (car (cdr cv-carrier)))))
       vowels))
    nocvcs)))

(define (list-vvs)
  (apply
   append
   (mapcar
    (lambda (v1)
      (mapcar 
       (lambda (v2) 
	 (list
	  (list (string-append v1 "-" v2))
	  (append (car vv-carrier) (list v1 v2) (car (cdr vv-carrier)))))
       vowels))
    vowels)))

(define (list-ccs)
  (apply
   append
   (mapcar
    (lambda (c1)
      (mapcar 
       (lambda (c2) 
       (list
	(list (string-append c1 "-" c2))
	(append (car cc-carrier) (list c1 '- c2) (car (cdr cc-carrier)))))
       (remove-list consonants coda-only)))
    (remove-list consonants onset-only))))

(define (list-ooer)
  (mapcar 
   (lambda (o) 
     (list
      (list (string-append o "-" "ER"))
      (append (car ooer-carrier) (list o "ER") (car (cdr ooer-carrier)))))
   onset-only))

(define (list-silv)
  (mapcar 
   (lambda (v) 
     (list
      (list (string-append silence "-" v))
      (append (car silv-carrier) (list silence v) (car (cdr silv-carrier)))))
   vowels))

(define (list-silc)
  (mapcar 
   (lambda (c) 
     (list
      (list (string-append silence "-" c))
      (append (car silc-carrier) (list silence c) (car (cdr silc-carrier)))))
   (remove-list consonants coda-only)))

(define (list-vsil)
  (mapcar 
   (lambda (v) 
     (list
      (list (string-append v "-" silence))
      (append (car vsil-carrier) (list v silence) (car (cdr vsil-carrier)))))
   vowels))

(define (list-csil)
  (mapcar 
   (lambda (c) 
     (list
      (list (string-append c "-" silence))
      (append (car csil-carrier) (list c silence) (car (cdr csil-carrier)))))
   (remove-list consonants onset-only)))

(define (list-ccclust1)
  (mapcar
   (lambda (c1c2)
     (list
      (list (string-append (car c1c2) "_-_" (car (cdr c1c2))))
      (append (car cc1-carrier) c1c2 (car (cdr cc1-carrier)))))
   clusters1))

(define (list-ccclust2)
  (mapcar
   (lambda (c1c2)
     (list
      (list (string-append (car c1c2) "_-_" (car (cdr c1c2))))
      (append (car cc2-carrier) c1c2 (car (cdr cc2-carrier)))))
   clusters2))

(define (list-vcopen)
  (apply
   append
   (mapcar
    (lambda (v)
      (mapcar 
       (lambda (c) 
	 (list
	  (list (string-append v "$-" c))
	  (append (car vc-carrier) (list v '- c) (car (cdr vc-carrier)))))
       consonants))
    vowels)))

(define (list-syllabics)
  (append
   (apply
    append
    (mapcar
     (lambda (s)
       (mapcar 
	(lambda (c) 
	  (list
	   (list (string-append c "-" s))
	   (append (car syllabics-carrier1) (list c s) (car (cdr syllabics-carrier1)))))
	(remove-list consonants onset-only)))
     syllabics))
   (apply
    append
    (mapcar
     (lambda (s)
       (mapcar 
	(lambda (c) 
	  (list
	   (list (string-append s "-" c))
	   (append (car syllabics-carrier2) (list s '- c) (car (cdr syllabics-carrier2)))))
	(remove-list consonants coda-only)))
     syllabics))
   (apply
    append
    (mapcar
     (lambda (s)
       (mapcar 
	(lambda (v) 
	  (list
	   (list (string-append s "-" v))
	   (append (car syllabics-carrier3) (list s '- v) (car (cdr syllabics-carrier3)))))
	vowels))
       syllabics))))


(define (list-taps)
  (append
   (mapcar
    (lambda (v1)
      (list
       (if (string-equal v1 "AA")
	   (list (string-append v1 "-DX")
		 (string-append "DX-AX"))
	   (list (string-append v1 "-DX")))
       (append (car taps1) (list v1) (car (cdr taps1)))))
    (delq 'AX vowels))
   (mapcar
    (lambda (c1)
      (list
       (list (string-append c1 "-DX"))
       (append (car taps2) (list c1) (car (cdr taps2)))))
    '(N R))))


;;; End of individual generation functions

(define (diphone-gen-list)
  "(diphone-gen-list)
Returns a list of nonsense words as phone strings."
  (append
   (list-cvcs)  ;; consonant-vowel and vowel-consonant
   (list-vcs)  ;; one which don't go in cvc
   (list-cvs)  ;; 
   (list-vvs)  ;; vowel-vowel
   (list-ccs)  ;; consonant-consonant
   (list-ccclust1)   ;; consonant clusters
   (list-ccclust2)   ;; consonant clusters
   (list-ooer) 
;   (list-syllabics)
   (list-silv)
   (list-silc)
   (list-csil)
   (list-vsil)
   (list-taps)
   (list
    '(("SIL-SIL") (SIL T AA T AA SIL SIL)))
;   (list-vcopen)    ;; open vowels
   ))

(define (Diphone_Prompt_Word utt)
  "(Diphone_Prompt_Word utt)
Specify specific modifications of the utterance before synthesis
specific to this particulat phone set."
  (mapcar
   (lambda (s)
     (let ((n (item.name s)))
       (item.set_feat s "base_name" n)
       ;; no mapping cuase we are in cmusphinx
       ))
   (utt.relation.items utt 'Segment))
  )

(define (Diphone_Prompt_Setup)
 "(Diphone_Prompt_Setup)
Called before synthesizing the prompt waveforms.  Uses the KAL speakers
(the most standard US voice)."
 (voice_cmu_us_kal2_diphone)  ;; US male voice
 (set! FP_F0 90)      ;; lower F0 than kal
 (set! diph_do_db_boundaries t)
 )

(provide 'cmusphinx2_schema)
