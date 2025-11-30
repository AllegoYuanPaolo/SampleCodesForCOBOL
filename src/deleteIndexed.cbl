        IDENTIFICATION DIVISION.
        PROGRAM-ID. deleteIndexed.
        
        environment division.
           input-output section.
               file-control.
                   select ACMA
                       assign to "data\ACMA.dat"
                       organization is indexed
                       record key is memberName
                       access mode is dynamic.

        DATA DIVISION.
           file section.
               fd ACMA.
                   01 ACMA-rec.
                       02 memberName pic x(16).
                       02 nickname pic x(16).
            local-STORAGE SECTION.

            01 searchName pic x(16).
            01 delChoice pic x value 'n'.
        
        PROCEDURE DIVISION.
           display "Enter Name to delete: " no advancing
           accept searchName

           move searchName to memberName

           open i-o ACMA
               read ACMA key is memberName
                   invalid key
                       display "Name not found!"
                   not invalid key
                       display "| Name: " memberName " | "
                               " Nickname: " nickname " |"
                      
                       display "Are you sure to delete this record?"
                       display "[y/n] >" no advancing
                       accept delChoice

                       if delChoice = 'y'
                           delete ACMA
                           display "Deleted Successfully"
                       else if delChoice = 'n'
                           display "Exiting. . ."
                       end-if

               end-read
           close ACMA
        
       GOBACK.
 