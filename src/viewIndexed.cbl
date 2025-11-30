        IDENTIFICATION DIVISION.
        PROGRAM-ID. viewIndexed.
       
       environment division.
           input-output section.
               file-control.
                       select optional ACMA 
                     assign to "data/ACMA.dat"
                     organization is indexed
                     record key is memberName.

        DATA DIVISION.
           file section.
                 fd ACMA.
                   01 ACMA-rec.
                       02 memberName pic x(16).
                       02 nickname pic x(16).
            local-STORAGE SECTION.
               01 eof pic x value 'n'.
               01 ctr pic 99.
        PROCEDURE DIVISION.
           
           open i-o ACMA *> open the file first

           
               *> Put it on a loop
               perform until eof = 'y'
                   *> Read file name, then move to the next record
                   read ACMA next record
                       at end 
                           display "No more data to show"
                           move 'y' to eof
                       not at end
                           display "| Name: " memberName " | "
                                   " Nickname: " nickname " |"
                      
                          
                       *> this is just for lines, this is not neceassary
                        perform varying ctr from 1 by 1 until ctr = 57
                               display "-" no advancing
                        end-perform
                        display spaces
                   end-read
               end-perform
           close ACMA
        
       GOBACK.
 