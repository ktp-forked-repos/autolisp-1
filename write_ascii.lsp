
    ; BASE converts from a decimal integer to a string in another base.

    (defun BASE (bas int / ret yyy zot)

      (defun zot (i1 i2 / xxx)

        (if (> (setq xxx (rem i2 i1)) 9)

          (chr (+ 55 xxx))

          (itoa xxx)

        )

      )



      (setq ret (zot bas int)

            yyy (/ int bas)

      )

      (setq ret (strcat (zot bas yyy) ret))

      (setq yyy (/ yyy bas))



      (strcat (zot bas yyy) ret)

    )



    (defun C:ASCII (/)                       ;chk out ct code dec oct hex )

      (initget "Yes")

      (setq chk (getkword "\nWriting to ASCII.TXT , continue? <Y>: "))

      (if (or (= chk "Yes") (= chk nil))

        (progn

          (setq out  (open "ascii.txt" "w")

                chk  1

                code 0

                ct   0

          )

          (princ "\n \n CHAR DEC OCT HEX \n")

          (princ "\n \n CHAR DEC OCT HEX \n" out)

          (while chk

            (setq dec (strcat "  " (itoa code))

oct (base 8 code)

             hex (base 16 code)

       )

       (setq dec (substr dec (- (strlen dec) 2) 3))

       (if (< (strlen oct) 3)

         (setq oct (strcat "0" oct))

       )



       (princ (strcat "\n " (chr code) " " dec " " oct " " hex))

       (princ (strcat "\n " (chr code) " " dec " " oct " " hex)

              out

       )



       (cond

         ((= code 255) (setq chk nil))

         ((= ct 20)

          (setq

            xxx (getstring

                  "\n \nPress  'X ' to eXit or any key to continue: "

                )

          )

          (if (= (strcase xxx) "X")

            (setq chk nil)

            (progn

              (setq ct 0)

              (princ "\n \n CHAR DEC OCT HEX \n")

            )

          )

         )

       )

       (setq ct   (1+ ct)

             code (1+ code)

       )

     )

     (close out)

     (setq out nil)

   )

 )

(princ)

)
