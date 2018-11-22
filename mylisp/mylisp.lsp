;Alias Overwrite
(defun c:a () (command "._matchprop"))
(defun c:e () (command "._scale"))
(defun c:ch () (command "._chamfer"))
(defun c:d () (command "._laydel"))
(defun c:dwg () (command "._dwg-purge"))
(defun c:lc () (command "._laymcur"))
(defun c:loo () (command "._layerp"))
(defun c:lm () (command "._laymch"))
(defun c:m () (command "._measuregeom"))
(defun c:ou () (command "._draworderf"))
(defun c:oa () (command "._drawordera"))
(defun c:r () (command "._rotate"))
(defun c:re () (command "._rectangle"))
(defun c:rg () (command "._regen"))
(defun c:w () (command "._move"))

;==============================================================
;天正命令
(defun c:2w () (command "._tswall"))
(defun c:atr () (command "._tclipaxis"))
(defun c:bg () (command "._tmelev"))
(defun c:fw () (command "._tfixwall"))
(defun c:rad () (command "._tdimrad"))
(defun c:t () (command "._ttext"))
(defun c:tt () (command "._tmtext"))
(defun c:st () (command "._alytop"))
(defun c:sb () (command "._alybottom"))                                         ;TVERTSLAB was overwrite
(defun c:sl () (command "._alxleft"))
(defun c:sr () (command "._alxright"))                                          ;SROOF was overwrite

;天正顺序调整
(defun c:ssh () (command "._toutlna"))
(defun c:ss () (command "._tseoutline"))
(defun c:ssf () (command "._tupdspace"))

;==============================================================

                                                                                ;create new layer named after 0_Plot
(defun createlayplot  ()                                                        ;Custom layer
 (command "._layer" "n" "0_Plot"                                                ;Layer name
          "p" "n" "0_Plot"                                                      ;Layer plot
          "c" "40" "0_Plot"                                                     ;Layer color
          "d" "Aid layer" "0_Plot" ""))                                         ;layer description)

                                                                                ;change the property of last xline
(defun changexline  ()
 (command "._change" "l" "" "p" "la" "0_Plot" ""))

(defun C:XL  ()
 (command "._xline")
 (while (= 1 (getvar "cmdactive")) (command pause))                             ;wait for command finish then execute
 (cond ((tblsearch "layer" "0_plot" (changexline)))
       ((not (tblsearch "layer" "0_plot")
             (progn (createlayplot) (changexline))))))


                                                                                ;Selection set
(defun C:SS1 () (setq SS1 (ssget)))
(defun C:S1 () (command "._pselect" ss1 ""))

(defun C:SS2 () (setq SS2 (ssget)))
(defun C:S2 () (command "._pselect" ss2 ""))

(defun C:SS3 () (setq SS3 (ssget)))
(defun C:S3 () (command "._pselect" ss3 ""))

(defun C:SS4 () (setq SS4 (ssget)))
(defun C:S4 () (command "._pselect" ss4 ""))

(defun C:SS5 () (setq SS5 (ssget)))
(defun C:S5 () (command "._pselect" ss5 ""))


                                                                                ;Outline
(defun c:oo  (/ *error* sel)
 (defun *error*  (msg)
  (LM:endundo (LM:acdoc))
  (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
   (princ (strcat "\nError: " msg)))
  (princ))
 (if (setq sel (ssget))
  (progn (LM:startundo (LM:acdoc))
         (LM:outline sel)
         (LM:endundo (LM:acdoc))))
 (princ))

;; Outline Objects  -  Lee Mac
;; Attempts to generate a polyline outlining the selected objects.
;; sel - [sel] Selection Set to outline
;; Returns: [sel] A selection set of all objects created

(defun LM:outline  (sel / app are box cmd dis enl ent lst obj rtn tmp)
 (if (setq box (LM:ssboundingbox sel))
  (progn (setq app (vlax-get-acad-object)
               dis (/ (apply 'distance box) 20.0)
               lst (mapcar '(lambda (a o) (mapcar o a (list dis dis))) box '(- +))
               are (apply '* (apply 'mapcar (cons '- (reverse lst))))
               dis (* dis 1.5)
               ent (entmakex
                    (append '((000 . "LWPOLYLINE")
                              (100 . "AcDbEntity")
                              (100 . "AcDbPolyline")
                              (090 . 4)
                              (070 . 1))
                            (mapcar '(lambda (x) (cons 10 (mapcar '(lambda (y) ((eval y) lst)) x)))
                                    '((caar cadar) (caadr cadar) (caadr cadadr) (caar cadadr))))))
         (apply 'vlax-invoke
                (vl-list* app
                          'zoomwindow
                          (mapcar '(lambda (a o) (mapcar o a (list dis dis 0.0))) box '(- +))))
         (setq cmd (getvar 'cmdecho)
               enl (entlast)
               rtn (ssadd))
         (while (setq tmp (entnext enl)) (setq enl tmp))
         (setvar 'cmdecho 0)
         (command "_.-boundary"
                  "_a"
                  "_b"
                  "_n"
                  sel
                  ent
                  ""
                  "_i"
                  "_y"
                  "_o"
                  "_p"
                  ""
                  "_non"
                  (trans (mapcar '- (car box) (list (/ dis 3.0) (/ dis 3.0))) 0 1)
                  "")
         (while (< 0 (getvar 'cmdactive)) (command ""))
         (entdel ent)
         (while (setq enl (entnext enl))
          (if (and (vlax-property-available-p
                    (setq obj (vlax-ename->vla-object enl))
                    'area)
                   (equal (vla-get-area obj) are 1e-4))
           (entdel enl)
           (ssadd enl rtn)))
         (vla-zoomprevious app)
         (setvar 'cmdecho cmd)
         rtn)))

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; s - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox  (s / a b i m n o)
 (repeat (setq i (sslength s))
  (if (and (setq o (vlax-ename->vla-object (ssname s (setq i (1- i)))))
           (vlax-method-applicable-p o 'getboundingbox)
           (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-getboundingbox (list o 'a 'b)))))
   (setq m (cons (vlax-safearray->list a) m)
         n (cons (vlax-safearray->list b) n))))
 (if (and m n)
  (mapcar '(lambda (a b) (apply 'mapcar (cons a b)))
          '(min max)
          (list m n))))

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo (doc) (LM:endundo doc) (vla-startundomark doc))

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo  (doc)
 (while (= 8 (logand 8 (getvar 'undoctl))) (vla-endundomark doc)))

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc  nil
 (eval (list 'defun
             'LM:acdoc
             'nil
             (vla-get-activedocument (vlax-get-acad-object))))
 (LM:acdoc))

(vl-load-com)
(princ)



                                                                                ;Outline Polyline

(defun c:OLPL  (/ _subst _fixLast _reverseLWPPoints filter ss wd/2 offlst dxf210)
 ;; Outline selected LWPolylines
 ;; Alan J. Thomspon, 04.07.11
 (vl-load-com)
 (defun _subst (p v l) (subst (cons p v) (assoc p l) l))
 (defun _fixLast (l) (reverse (cons '(42 . 0.) (cdr (reverse l)))))
 (defun _reverseLWPPoints  (d / l n)
  (foreach x  d
   (and (vl-position (car x) '(10 40 41 42)) (setq l (cons x l))))
  (setq l (append (cdddr l) (list (car l) (cadr l) (caddr l))))
  (while l
   (setq n (append n
                   (list (assoc 10 l)
                         (cons 40 (cdr (assoc 41 l)))
                         (cons 41 (cdr (assoc 40 l)))
                         (cons 42 (- (cdr (assoc 42 l))))))
         l (cddddr l)))
  n)
 (setq filter '((0 . "LWPOLYLINE") (-4 . ">") (43 . 0.)))
 (if (or (ssget "_I" filter)
         (prompt "\nSelect LWPolylines to outline: ")
         (ssget filter))
  (progn
   (initget "Yes No")
   (setq *OPL:Option* (cond ((getkword (strcat "\nDelete LWPolyline after outlined? [Yes/No] <"
                                               (cond (*OPL:Option*)
                                                     ((setq *OPL:Option* "No")))
                                               ">: ")))
                            (*OPL:Option*)))
   (vlax-for x  (setq ss (vla-get-activeselectionset
                          (cond (*AcadDoc*)
                                ((setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object)))))))
    (setq offlst nil)
    (if (vl-every '(lambda (d / o)
                    (if (not (vl-catch-all-error-p
                              (setq o (vl-catch-all-apply 'vlax-invoke (list x 'Offset d)))))
                     (setq offlst (cons (entget (vlax-vla-object->ename (car o))) offlst))))
                  (list (setq wd/2 (/ (vla-get-constantwidth x) 2.)) (- wd/2)))
     (progn (if (vlax-curve-isClosed x)
             (foreach e offlst (entmod (_subst 43 0. e)))
             (progn (entmod
                     (_subst 43
                             0.
                             (_subst 70
                                     (if (zerop (vlax-get x 'LinetypeGeneration))
                                      1
                                      129)
                                     (_subst 90
                                             (+ 2 (cdr (assoc 90 (car offlst))) (cdr (assoc 90 (cadr offlst))))
                                             (append (_fixLast
                                                      (vl-remove (setq dxf210 (assoc 210 (car offlst))) (car offlst)))
                                                     (_fixLast (_reverseLWPPoints (cadr offlst)))
                                                     (list dxf210))))))
                    (entdel (cdr (assoc -1 (cadr offlst))))))
            (and (eq *OPL:Option* "Yes")
                 (vl-catch-all-apply 'vla-delete (list x))))
     (foreach e offlst (entdel (cdr (assoc -1 e))))))
   (vla-delete ss)))
 (princ))



                                                                                ; Merge Hatch
(defun massoc  (key alst /)
 (vl-remove-if '(lambda (pair) (/= (car pair) key)) alst))
                                                                                ;---------------------------------------------------------------------------
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
