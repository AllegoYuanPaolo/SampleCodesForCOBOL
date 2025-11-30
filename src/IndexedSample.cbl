        IDENTIFICATION DIVISION.
        PROGRAM-ID. IndexedSample.
 
        DATA DIVISION.
            local-STORAGE SECTION.
              01 choice pic 9.
        PROCEDURE DIVISION.
           
           perform with test after until choice = 00
               call "SYSTEM" using "cls"
               display "   Indexed File Sample"
               display spaces
               display "[1] - Create"
               display "[2] - Retrieve (View all)"
               display "[3] - Retrieve (Search)"
               display "[4] - Update"
               display "[5] - Delete"
               display "[00] - Exit"
               display spaces
               display "Enter option >" no advancing
               accept choice

               evaluate choice
                   when 1
                       call "SYSTEM" using "cls"
                       call "addIndexed"
                       call "SYSTEM" using "pause"
                   when 2
                       call "SYSTEM" using "cls"
                       call "viewIndexed"
                       call "SYSTEM" using "pause"
                   when 3
                       call "SYSTEM" using "cls"
                       call "searchIndexed"
                       call "SYSTEM" using "pause"
                   when 4
                       call "SYSTEM" using "cls"
                       call "updateIndexed"
                       call "SYSTEM" using "pause"
                   when 5
                       call "SYSTEM" using "cls"
                       call "deleteIndexed"
                       call "SYSTEM" using "pause"
                   when 00
                       exit perform
               end-evaluate
           end-perform

       GOBACK.
 