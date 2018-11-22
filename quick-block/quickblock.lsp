;|
Quick Block
Creates a block instantly out of the objects that you select
Found at http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Quick-block/td-p/3454228
|;

(defun c:QB	 (/ selectionset insertionpoint number Blockname)
;;; Tharwat 11. May. 2012 ;;
 (if (and (setq selectionset (ssget "_:L"))
		  (setq insertionpoint (getpoint "\n Specify insertion point :")))
  (progn (setq number	 1
			   Blockname (strcat "MyBlock" (itoa number)))
		 (while	(tblsearch "BLOCK" Blockname)
		  (setq Blockname (strcat "MyBlock" (itoa (setq number (1+ number))))))
		 (command "_.-Block" Blockname insertionpoint selectionset "")
		 (command "_.-insert" Blockname insertionpoint "" "" "")) 
  (princ))
 (princ))
