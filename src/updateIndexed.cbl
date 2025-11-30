        IDENTIFICATION DIVISION.
        PROGRAM-ID. updateIndexed.
       environment division.
           input-output section.
               file-control.
                   select optional ACMA 
                     assign to "data/ACMA.dat"
                     access mode is dynamic
                     organization is indexed
                     record key is memberName.
        DATA DIVISION.
           file section.
            fd ACMA.
                   01 ACMA-rec.
                       02 memberName pic x(16).
                       02 nickname pic x(16).

            local-STORAGE SECTION.
            01 newName pic x(16).
        PROCEDURE DIVISION.
           
           display "Enter member to edit: " no advancing
           accept newName

           open i-o ACMA      

           move newName to memberName
           move spaces to newName

               read ACMA key is memberName
                   invalid key
                       display "Name not found!"
                   not invalid key
                       display "| Name: " memberName " | "
                               " Nickname: " nickname " |"
                       display "Enter new nickname: " no advancing
                       accept newName

                       move newName to nickname
                       
                       rewrite ACMA-rec
                       display "Updated!"

                       display "| Name: " memberName " | "
                               " Nickname: " nickname " |"
               end-read
               
           close ACMA

           
        
       GOBACK.
 