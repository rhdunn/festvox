;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;-*-mode:scheme-*-
;;;                                                                     ;;;
;;;                  Language Technologies Institute                    ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                         Copyright (c) 2002                          ;;;
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
;;;             Author: Alan W Black (awb@cs.cmu.edu)                   ;;;
;;;               Date: December 2002                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;; Take lots of text and generated .data files one utterance per line  ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Because this is a --script type file it has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))


;;; Process command line arguments
(define (text2utts_help)
  (format t "%s\n"
  "text2utts [options] textfile(s)
  Convert textfile(s) to .data files
  Options
  -all             Output all utterances, not just \"nice\" ones
  -o ofile         File to save waveform (default is stdout).
  -dbname <string> Set dbname prefix (default is txt_)
  -num <int>       Set number to start from (default is 1)
  -level <string>  Which level to print out, (default Token), Word Segment
  -otype <string>  data (default) or raw.
  -itype <string>  raw (default) or data.
  -eval <string>   File or lisp s-expression to be evaluated before
                   processing.
")
  (quit))

;;; No gc messages
(gc-status nil)

;;; Default argument values
(defvar outfile "-")
(defvar text_files '("-"))
(defvar nice_utts_only t)
(defvar dbname "txt_")
(defvar txtnum 1)
(defvar level "Token")
(defvar raw nil)
(defvar itype "raw")

;;; Get options
(define (get_options)
  (let ((files nil)
	(o argv))
    (if (or (member_string "-h" argv)
	    (member_string "-help" argv)
	    (member_string "--help" argv)
	    (member_string "-?" argv))
	(text2utts_help))
    (while o
      (begin
	(cond
	 ((string-equal "-o" (car o))
	  (if (not (cdr o))
	      (text2utts_error "no output file specified"))
	  (set! outfile (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-all" (car o))
          (set! nice_utts_only nil))
	 ((string-equal "-otype" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no otype specified"))
	  (set! otype (car (cdr o)))
	  (if (string-equal otype "raw")
	      (set! raw t))
	  (set! o (cdr o)))
	 ((string-equal "-dbname" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no dbname specified"))
	  (set! dbname (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-level" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no level specified"))
	  (set! level (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-itype" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no itype specified"))
	  (set! itype (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-num" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no num specified"))
	  (set! txtnum (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-eval" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no file specified to load"))
	  (if (string-matches (car (cdr o)) "^(.*")
	      (eval (read-from-string (car (cdr o))))
	      (load (car (cdr o))))
	  (set! o (cdr o)))
	 (t
	  (set! files (cons (car o) files))))
	(set! o (cdr o))))
    (if files
	(set! text_files (reverse files)))))

(define (text2uttse_error message)
  (format stderr "%s: %s\n" "text2utts" message)
  (text2utts_help))

(define (no_duplicates words)
  (let ((no_dups t))

    (while (and words (cdr words) no_dups)
       (if (string-equal (item.name (car words)) (item.name (cadr words)))
	   (set! no_dups nil))
       (set! words (cdr words)))
    no_dups))

(define (all_in_lex words)
  (let ((all_lex t))

    (while (and words all_lex)
       (if (not (lex.lookup_all (item.name (car words))))
	   (set! all_lex nil))
       (set! words (cdr words)))
    all_lex))

(define (nice_utt utt)
  (let ((words (utt.relation.items utt 'Word)))
;    (format t "%s %d\n" (and words (item.name (car words))) (length words))
    (if (and (> (length words) 5)
	     (< (length words) 15)
	     (no_duplicates words)
	     (all_in_lex words))
	t
	nil)))

(define (escape_characters s)
  (let ((nn ""))
    (while (string-matches s ".*\\\\.*")
       (set! nn (string-append nn (string-before s "\\") "\\\\"))
       (set! s (string-after s "\\")))
    (set! s (string-append nn s))
    (set! nn "")
    (while (string-matches s ".*\".*")
       (set! nn (string-append nn (string-before s "\"") "\\\""))
       (set! s (string-after s "\"")))
    (set! s (string-append nn s))
    s))

(define (utt_output_token utt)
  (let ((token (utt.relation.first utt 'Token)))
    (if (not raw)
	(format ofd "( %s%05d \"" dbname txtnum))
    (set! txtnum (+ 1 txtnum))
    (set! whitespace "")
    (while token
;	 (format t ">%s<\n" (item.name token))
       (let ((punc (item.feat token "punc"))
	     (prepunctuation (item.feat token "prepunctuation")))
	 (set! name (item.name token))
	 (if (string-equal "0" punc) (set! punc ""))
	 (if (string-equal "0" prepunctuation) (set! prepunctuation ""))
	 (if (not raw)
	     (begin
	       (set! prepunctuation (escape_characters prepunctuation))
	       (set! punc (escape_characters punc))
	       (set! name (escape_characters name))))
	 (format ofd "%s%s%s%s"
		 whitespace
		 prepunctuation
		 name
		 punc)
	 (set! whitespace " ")
	 )
       (set! token (item.next token)))
    (if (not raw)
	(format ofd "\" )"))
    (format ofd "\n")))

(define (utt_output_word utt)
  (let ((word (utt.relation.first utt 'Word)))
    (if (not raw)
	(format ofd "( %s%05d \"" dbname txtnum))
    (set! txtnum (+ 1 txtnum))
    (set! whitespace "")
    (while word
;	 (format t ">%s<\n" (item.name word))
       (let ((punc 
	      (if (item.next (item.relation word "Token"))
		  "0"
		  (item.feat word "R:Token.parent.punc")))
	     (prepunctuation 
	      (if (item.prev (item.relation word "Token"))
		  "0"
		  (item.feat word "R:Token.parent.prepunctuation"))))
	 (set! name (item.name word))
	 (if (string-equal "0" punc) (set! punc ""))
	 (if (string-equal "0" prepunctuation) (set! prepunctuation ""))
	 (if (not raw)
	     (begin
	       (set! prepunctuation (escape_characters prepunctuation))
	       (set! punc (escape_characters punc))
	       (set! name (escape_characters name))))
	 (format ofd "%s%s%s%s"
		 whitespace
		 prepunctuation
		 name
		 punc)
	 (set! whitespace " ")
	 )
       (set! word (item.next word)))
    (if (not raw)
	(format ofd "\" )"))
    (format ofd "\n")))

(define (utt_output_segment utt)
  (let ((segment (utt.relation.first utt 'Segment)))
    (if (not raw)
	(format ofd "( %s%05d \"" dbname txtnum))
    (set! txtnum (+ 1 txtnum))
    (set! whitespace "")
    (while segment
       (set! name (item.name segment))
       (format ofd "%s%s" whitespace name)
       (set! whitespace " ")
       (set! segment (item.next segment)))
    (if (not raw)
	(format ofd "\" )"))
    (format ofd "\n")))

(define (check_utt_output utt)
  (cond
   ((or (not nice_utts_only)
	(nice_utt utt))
    (cond
     ((string-equal level "Token")
      (utt_output_token utt))
     ((string-equal level "Word")
      (utt_output_word utt))
     ((string-equal level "Segment")
      (utt_output_segment utt))
     (t
      (format stderr "Unknown level %s\n" level)
      (error))))
   (t  ;; not an interesting utt 
    t))
)

;;;
;;; Redefine what happens to utterances during text to speech 
;;;
(set! tts_hooks 
      (list 
       check_utt_output))

(define (utt.synth_toToken utt)
  utt)

(define (utt.synth_toWord utt)
  (Token_POS utt) 
  (Token utt))

(define (utt.synth_toSegment utt)
  (Token_POS utt)    ;; when utt.synth is called
  (Token utt)        
  (POS utt)
  (Phrasify utt)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt))

(define (main)
  (get_options)

  (cond 
   ((string-equal level "Token")
    (set! tts_hooks (cons utt.synth_toToken tts_hooks)))
   ((string-equal level "Word")
    (set! tts_hooks (cons utt.synth_toWord tts_hooks)))
   ((string-equal level "Segment")
    (set! tts_hooks (cons utt.synth_toSegment tts_hooks)))
   (t
    (set! tts_hooks (cons utt.synth tts_hooks))))

  (set! ofd (fopen outfile "w"))
  ;; do the synthesis
  (cond
   ((string-equal itype "raw")
      (mapcar
       (lambda (f) 
	 (tts_file f (tts_find_text_mode f auto-text-mode-alist)))
       text_files))
   ((string-equal itype "data")
      (mapcar
       (lambda (f) 
	 (mapcar
	  (lambda (s)
	    (let ((utt (eval (list 'Utterance 'Text (cadr s)))))
	      (apply_hooks tts_hooks utt)))
	  (load f t)))
       text_files))
   (t
    (format stderr "Unknown itype %s\n" itype)))

  (fclose ofd)

)

;;;  Do the work
(main)
