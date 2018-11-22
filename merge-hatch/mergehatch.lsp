;;By Juan Villarreal
;;This routine will recreate hatches into a single hatch as if "create separate hatches" was not selected.
;;
;;Known Problems and Solutions:
;;1. Associativity can be lost.
;; - To retain the associative parameter, all selected objects must be associative.
;; - Non-Associative Hatch Objects do not contain the entity name of the object used during hatch creation within their DXF group codes.
;;
;;2. The routine may not function if joining a hatch object created via "select object" with that created via "select point".
;; - If joining hatch objects created using both methods, select the hatch object created via "select point" as the "Hatch Pattern to MATCH". 

;;---------------------------------------------------------------------------
(defun massoc  (key alst /)
 (vl-remove-if '(lambda (pair) (/= (car pair) key)) alst))
;;---------------------------------------------------------------------------
(defun c:MergeHatch  (/ hentinfo ss i ent ent# seedpt# entinfo entinfo2 seedpts MergedHatchList Turnoff BGColor)
 (vl-load-com)
 (while (/= (cdr (assoc 0 hentinfo)) "HATCH")
  (setq hentinfo (car (entsel "\nSelect Hatch Pattern to MATCH:")))
  (If hentinfo
   (setq BGColor  (vla-get-backgroundcolor (vlax-ename->vla-object hentinfo))
         hentinfo (entget hentinfo))
   (princ "\nMissed. Try again.")))
 (while (not ss)
  (princ "\nSelect ALL hatch entities to merge:")
  (setq ss (ssget '((0 . "HATCH")))))
 (setq MergedHatchList (list (cons 0 "HATCH") (cons 100 "AcDbEntity") (assoc 8 hentinfo)))
 (if (assoc 62 hentinfo)
  (setq MergedHatchList (append MergedHatchList (list (assoc 62 hentinfo)))))
 (setq MergedHatchList (append MergedHatchList
                               (list (cons 100 "AcDbHatch")
                                     (assoc 10 hentinfo)
                                     (assoc 210 hentinfo)
                                     (assoc 2 hentinfo)
                                     (assoc 70 hentinfo)
                                     (assoc 71 hentinfo)
                                     (cons 91 (sslength ss))))
       i               -1
       seedpt#         0
       ent#            0)
 (setvar 'cmdecho 0)
 (repeat (sslength ss)
  (setq entinfo         (entget (ssname ss (setq i (1+ i))))
        entinfo2        (member (assoc 92 entinfo) entinfo)
        entinfo2        (reverse (cdr (member (assoc 75 entinfo2) (reverse entinfo2))))
        ent#            (+ ent# (cdr (assoc 91 entinfo)))
        seedpt#         (+ seedpt# (cdr (assoc 98 entinfo)))
        seedpts         (append seedpts (massoc 10 (member (assoc 98 entinfo) entinfo)))
        MergedHatchList (append MergedHatchList entinfo2))
  (if (zerop (cdr (assoc 71 entinfo)))
   (setq turnoff T))
  (entdel (ssname ss i)))
 (setq MergedHatchList (subst (cons 91 ent#) (assoc 91 MergedHatchList) MergedHatchList)
       MergedHatchList (append MergedHatchList
                               (append (reverse
                                        (cdr (member (assoc 98 hentinfo)
                                                     (reverse (member (assoc 75 hentinfo) hentinfo)))))
                                       (cons (cons 98 seedpt#) seedpts))))
 (if (= (cdr (assoc 70 hentinfo)) 1)
  (setq MergedHatchList (append MergedHatchList (member (assoc 450 hentinfo) hentinfo))))
 (entmake MergedHatchList)
 (setq ent (entlast))
 (if (and (= (cdr (assoc 71 hentinfo)) 1) (not turnoff))
  (mapcar '(lambda (x / entlist)
            (setq entlist (entget (cdr x)))
            (entmod (subst (cons 330 ent) (assoc 330 entlist) entlist)))
          (massoc 330 MergedHatchList))
  (princ "\nAll Hatch Associativity has been removed."))
 (vla-put-backgroundcolor (vlax-ename->vla-object ent) BGColor)
 (princ))
(defun c:MH () (c:MergeHatch))
