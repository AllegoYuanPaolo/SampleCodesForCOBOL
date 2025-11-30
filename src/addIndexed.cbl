        IDENTIFICATION DIVISION.
        PROGRAM-ID. addIndexed.
       
       environment division.
           input-output section.
               file-control.
                   select optional ACMA 
                     assign to "data/ACMA.dat"
                     organization is indexed
                     access mode is dynamic
                     record key is memberName.

        DATA DIVISION.
           file section.
               fd ACMA.
               *> use a group as a record
                   01 ACMA-rec.
                       02 memberName pic x(16).
                       02 nickname pic x(16).
            local-STORAGE SECTION.
               01 input-Rec. 
                   02 inputName pic x(16).
                   02 inputNickName pic x(16).
        
        PROCEDURE DIVISION.
         
           display "Enter Member Name: " no advancing
           accept inputName

           display "Enter Member Nickname: " no advancing
           accept inputNickName

               if inputNickName = spaces
                   move "NOT SET" to inputNickName
               end-if

           open i-o ACMA *> OPEN I-O <fileDescriptor> is INPUT-OUTPUT
                  move input-Rec to ACMA-rec

               write ACMA-rec

           close ACMA


       GOBACK.
 