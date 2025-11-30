        IDENTIFICATION DIVISION.
        PROGRAM-ID. searchIndexed.
        environment division.
           input-output section.
               file-control.
                   select ACMA 
                       assign to "data\ACMA.dat"
                       record key is memberName
                       organization is indexed
                       access mode is dynamic.
        DATA DIVISION.
           file section.
               fd ACMA.
                   01 ACMA-rec.
                       02 memberName pic x(16).
                       02 nickname pic x(16).
            local-storage section.
            01 searchKey pic x(16).
        
        PROCEDURE DIVISION.
           display "Enter Member name to search: " no advancing
           accept searchKey
           
           open i-o ACMA
               move searchKey to memberName

               read ACMA key is memberName
                   invalid key
                       display "Name not found!"
                   not invalid key
                         display "| Name: " memberName " | "
                               " Nickname: " nickname " |"
               end-read

           close ACMA
                
       goback.
 