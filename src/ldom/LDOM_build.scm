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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;  A generic parameter file for the clunits cluster method for use in   ;;
;;;  in the limit domain type synthesizer.                                ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar LDOM_ldom_dir "CURRENTDIR")

;;; Basic voice definition file with voice defines and clunit
;;; parameter definitoin for run time.
(load "festvox/LDOM_ldom.scm")

;;; Add Build time parameters
(set! LDOM_dt_params
      (cons
       ;; in case LDOM_ldom defines this too, put this at start
       '(db_dir "CURRENTDIR/")
       (append
	LDOM_dt_params
	(list
	;;; In LDOM_ldom.scm
	 ;;'(coeffs_dir "lpc/")
	 ;;'(coeffs_ext ".lpc")
	 '(disttabs_dir "festival/disttabs/")
	 '(utts_dir "festival/utts/")
	 '(utts_ext ".utt")
	 '(dur_pen_weight 0.0)
	 '(get_stds_per_unit t)
	 '(ac_left_context 0.8)
	 '(ac_weights
	   (5.0
	    0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 
	    ))
	 ;; Join weights in LDOM_ldom.scm
	 ;; Features for extraction
	 '(feats_dir "festival/feats/")
	 '(feats 
	   (occurid
	    p.name p.ph_vc p.ph_ctype 
	    p.ph_vheight p.ph_vlng 
	    p.ph_vfront  p.ph_vrnd 
	    p.ph_cplace  p.ph_cvox    
	    n.name n.ph_vc n.ph_ctype 
	    n.ph_vheight n.ph_vlng 
	    n.ph_vfront  n.ph_vrnd 
	    n.ph_cplace  n.ph_cvox
	    segment_duration 
	    seg_pitch p.seg_pitch n.seg_pitch
	    R:SylStructure.parent.stress 
	    seg_onsetcoda n.seg_onsetcoda p.seg_onsetcoda
	    R:SylStructure.parent.accented 
	    pos_in_syl 
	    syl_initial
	    syl_final
	    R:SylStructure.parent.syl_break 
	    R:SylStructure.parent.R:Syllable.p.syl_break
	    pp.name pp.ph_vc pp.ph_ctype 
	    pp.ph_vheight pp.ph_vlng 
	    pp.ph_vfront  pp.ph_vrnd 
	    pp.ph_cplace pp.ph_cvox
	    ))
	 ;; Wagon tree building params
					; (trees_dir "festvox/")  ;; in LDOM_ldom.scm
	 '(wagon_field_desc "festival/clunits/all.desc")
	 '(wagon_progname "ESTDIR/bin/wagon")
	 '(wagon_cluster_size 20)
	 '(prune_reduce 0)
	 ;; The dictionary of units used at run time
					; (catalogue_dir "festvox/")   ;; in LDOM_ldom.scm
	 ;;  Run time parameters 
	 ;; all in LDOM_ldom.scm
	 ;; Files in db
	 (list
	  'files
	  (mapcar car (load "CURRENTDIR/etc/LDOM.data" t)))))))

(eval (list closest_voice))
(set! dt_params LDOM_dt_params)
(set! clunits_params LDOM_dt_params)

(provide 'LDOM_build)

