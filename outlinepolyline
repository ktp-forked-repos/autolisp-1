(defun c:OPL  (/ _subst _fixLast _reverseLWPPoints filter ss wd/2 offlst dxf210)
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







