;create new layer named after 0_Plot
(defun createlayplot ()                 ;Custom layer
  (command "._layer" "n" "0_Plot"       ;Layer name
    "p" "n" "0_Plot"                    ;Layer plot
    "c" "40" "0_Plot"                   ;Layer color
    "d" "Aid layer" "0_Plot" ""))       ;layer description)

;change the property of last xline
(defun changexline ()
  (command "._change" "l" "" "p" "la" "0_Plot" ""))

(defun C:XL ()
  (command "._xline")
  (while (= 1 (getvar "cmdactive"))
   (command pause))                     ;wait for command finish then execute
  (cond
    ((tblsearch "layer" "0_plot") (changexline))
    ((not (tblsearch "layer" "0_plot"))
      (progn (createlayplot) (changexline)))))
