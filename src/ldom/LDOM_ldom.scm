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
;;;                                                                       ;;
;;;  A generic voice definition file for a limited domain synthesizer     ;;
;;;  Cutomsized for: LDOM                                                 ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require_module 'clunits)
(if (null (member 'clunits provided))
    (load (path-append libdir "../src/modules/clunits/acost.scm")))

(if (assoc 'LDOM_ldom voice-locations)
    (defvar LDOM_ldom_dir (cdr (assoc 'LDOM_ldom voice-locations)))
    (defvar LDOM_ldom_dir (pwd)))

(if (not (probe_file (path-append LDOM_ldom_dir "festvox/")))
    (begin
     (format stderr "LDOM_ldom: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append  LDOM_ldom_dir "festvox/"))
     (format stderr "   Either the voice isn't linked in Festival library\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))

;;;  Add the directory contains general voice stuff to load-path
(set! load-path (cons (path-append LDOM_ldom_dir "festvox/") load-path))

;;; The front end to the limited domain
(require 'LDOM)

;;;
(set! closest_voice 'voice_kal_diphone)

;;;  These are the parameters which are needed at run time
;;;  build time parameters are added to his list in LDOM_build.scm
(set! LDOM_dt_params
      (list
       (list 'db_dir LDOM_ldom_dir)
       '(name LDOM)
       '(index_name LDOM)
       '(join_weights
         (10.0
	   0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
	   ))
       '(trees_dir "festival/trees/")
       '(catalogue_dir "festival/clunits/")
       '(coeffs_dir "mcep/")
       '(coeffs_ext ".mcep")
       ;;  Run time parameters 
       '(join_method windowed)
       '(continuity_weight 100)
       '(optimal_coupling 1)
       '(pm_coeffs_dir "pm/")
       '(pm_coeffs_ext ".pm")
       '(sig_dir "wav/")
       '(sig_ext ".wav")
))

(define (voice_LDOM_ldom)
  "(voice_LDOM_ldom)
Define voice for limited domain: LDOM."
  ;; Blindly use basic parameters of closest voice
  (eval (list closest_voice))
  (set! dt_params LDOM_dt_params)
  (set! clunits_params LDOM_dt_params)
  (clunits:load_db clunits_params)
  (if (not (boundp 'clunit_selection_trees))
      (load (string-append
	     (string-append 
	      LDOM_ldom_dir "/"
	      (get_param 'trees_dir dt_params "trees/")
	      (get_param 'index_name dt_params "all")
	      ".tree"))))
  (Parameter.set 'Synth_Method 'Cluster)

  (set! current-voice 'LDOM_ldom)
)

(provide 'LDOM_ldom)

