(defun C:SS1 ()
  (setq SS1 (ssget)))
(defun C:S1 ()
  (command "._pselect" ss1 ""))

(defun C:SS2 ()
  (setq SS2 (ssget)))
(defun C:S2 ()
  (command "._pselect" ss2 ""))

(defun C:SS3 ()
  (setq SS3 (ssget)))
(defun C:S3 ()
  (command "._pselect" ss3 ""))

(defun C:SS4 ()
  (setq SS4 (ssget)))
(defun C:S4 ()
  (command "._pselect" ss4 ""))

(defun C:SS5 ()
  (setq SS5 (ssget)))
(defun C:S5 ()
  (command "._pselect" ss5 ""))
