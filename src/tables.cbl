        IDENTIFICATION DIVISION.
        PROGRAM-ID. tables.
 
        DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 sample.
               02 sampleTable occurs 3 times.
                   03 sampleName pic x(16).
                   03 sampleNickname pic x(16).
           
           01 ctr pic 9.
        PROCEDURE DIVISION.
           *> Move values into the each table
           move "Yuan" to sampleName(1)
           move "Snitch" to sampleNickname(1)

           move "Narciso" to sampleName(2)
           move "Namiram" to sampleNickname(2)

           move "Zyron" to sampleName(3)
           move "Arabisaya" to sampleNickname(3)

           *> Print the tables
           perform varying ctr from 1 by 1 until ctr > 3
               display "Name: " samplename(ctr) 
               " | Nickname: " sampleNickname(ctr)
           end-perform
       STOP RUN.
 