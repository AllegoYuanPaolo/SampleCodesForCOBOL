# Code documentation for Indexed Sample
---
# For better view:
For a better view of this markdown file, it is highly recommended *(but optional)* to use **Obsidian**. *[Click here to download the latest version](https://obsidian.md/download)* 
# `INDEXED` 
 is  another type of File Organization.  Unlike `ORGANIZATION IS SEQUENTIAL`, `INDEXED` makes your `.dat` file output as a structured binary file.  This means that you can't open it in notepad or preview it.

This kind of file is perfect for practical applications such as database-like files.  It can be used like a SQL database, but simpler and less control.  Think of it as like an array where you can easily access an element using it's index; although technically speaking, it works as a binary tree under the hood.

Do take note that this document will only focus on the `ORGANIZATION IS INDEXED`, glossing over the third file organization, `RELATIVE`.  It is recommended that you research the said organization if you're interested

---
 <h1 style="font-family: Courier">Author's note:</h1>
 
- I use the GnuCOBOL 3.3 (SuperBOL) alongside COBOL by bitlang (bitlang.COBOL) on my machine which allows me to have a less strict implementation of COBOL syntax. 
- It allows me to  use indentions without causing errors
- I also use Windows 11 (important because you can see the scripts that compile and run the codes are batch scripts (`.bat`), exclusive to Windows)
- It may be different to your environment (especially if you use openCOBOL)

<blockquote>
	<b>
		<em>
			I recommend using this code only as a reference and only run the <code>.exe</code>  files instead of compiling when making changes.  If you want to try to make changes to it, you may copy and write the code yourself in your preferred environment to understand each line better
		</em>
	</b>
</blockquote>


---
# Table of Contents:
- [Declaration](#Declaration)
- [CRUD](#CRUD%20Operations)
	- [Create](#Create)
    - [Retrieve](#Retrieve)
        - [by all](#retrieve%20by%20all)
        - [by specific record](#retrieve%20a%20specific%20record)
    - [Update](#update)
    - [Delete](#delete)
---
### Declaration
#### `ENVIRONMENT DIVISION`
In the `INPUT-OUTPUT SECTION\FILE-CONTROL.`, you can declare an indexed file by using `FILE ORGANIZATION IS INDEXED` instead of `SEQUENTIAL`. 

```COBOL
			select optional ACMA 
                     assign to "data/ACMA.dat"
                     access mode is dynamic
                     organization is indexed
                     record key is memberName.
```
In the given code above, we use the following key words:
- `SELECT OPTIONAL <fileDescriptor>`
	- `OPTIONAL` is a keyword that checks if the file already exists
		- If it does, it will access it;  else,  it will create the file
- `ASSIGN TO <fileName or filePath>`
	- Assigns the \<fileDescriptor> into a file name
	- It is pathed as `data\ACMA.dat` because the `bin` file tree goes as follows
	  
```
├───programs
│   └───data
└───scripts
```

- `ACCESS MODE IS DYNAMIC`
	- `ACCESS MODE IS` tells the program how you want to read a file
		- `SEQUENTIAL` - reads/writes in sequential order.  Applicable for all organization types
		- `RANDOM` - allows to jump into a specific record using the `key`.  Not applicable to `SEQUENTIAL` organization
		- `DYNAMIC` - allows `INDEXED` files to be access by both `SEQUENTIAL` and `RANDOM`
	
	- It tells COBOL that you want to access the file in both `SEQUENTIAL` and `RANDOM`
- `ORGANIZATION IS INDEXED`
	- This declares the organization of the file into `INDEXED`
- `RECORD KEY IS <keyVariable>`
	- This declares the key variable that the file which variable will be used as an index; the search key
	- This declaration refers to the Primary Key which are keys must be *unique*, or without duplicates
		- You can also assign Secondary Keys for `INDEXED` with
		- ```cobol
			  ...
			  RECORD KEY IS memberName
			  ALTERNATE KEY IS nickname
			  ALTERNATE KEY IS age WITH DUPLICATES
			  ...
		  ```

		- `WITH DUPLICATES` allows alternate keys to have duplicates

#### `DATA DIVISION`
In the `FILE SECTION` comes the most important part of an `INDEXED` file: the record format.  This is where you declare a group that defines the contents of your file.

For example:
```COBOL
		 FD ACMA.
		   01 ACMA-rec.
			   02 memberName pic x(16).
			   02 nickname pic x(16).
```

This is similar to how `SEQUENTIAL` organized files are described but let's unpack it regardless:

- `FD ACMA.`
	- This is essentially a pointer variable that points to the file: in this case, it points to `data\ACMA.dat`
- `01 ACMA-rec` 
	- This the group name of your record
	- It is the variable that will be used for `WRITE` and `REWRITE` operations
- `02 memberName pic x(16)`, `02 nickname pic x(16)`
	- This is the data fields for your record

The record file dictates your file's structure.  In extremely simplified terms, consider that it is written as:

```
Arvin           Rumerespeto
Luther          Saving Grace
Narciso         Namiram
Vincent         Santino
Zyron           Arabisaya                       
```

Where the data field is occupied by the value and space padding.  For example, 
`"Arvin           "` consists of 5 characters (`['A', 'r', 'v', 'i', 'n']`) and 11 spaces for padding.  This is due to the fixed-size alphanumeric field from COBOL where it cannot have garbage values if the size is not fully occupied; so it fills the rest of the bytes with spaces ( ascii value 32(space) ).  One record is counted as the total size of the group record; in this case, it's 32 characters, spaces included.

<fieldset>
	<p>
		NOTE: This is also present in numeric fields (<code>PIC 9</code>).  But instead of spaces, it is filled with zeros (0).  Additionally, what <code>PIC Z</code> does is change the ascii value of 0 into the ascii value of spaces; which makes a <code>PIC Z</code> variable an alphanumeric.
	</p>
</fieldset>


### CRUD Operations
(Create, Retrieve, Update, Delete)

#### Create
Create operations are done with the COBOL keyword of `WRITE`




For example; ask for user input, then write it into the file
Example from *`addIndexed.cbl`*
```COBOL
	  . . .
		   display "Enter Member Name: " no advancing
           accept inputName

           display "Enter Member Nickname: " no advancing
           accept inputNickName
              . . .
           open i-o ACMA 
                move input-Rec to ACMA-rec
               write ACMA-rec
           close ACMA
           
	   . . .
```

`input-Rec` is a group record declared as
```cobol
01 input-Rec.
	02 inputName pic x(16).
	02 inputNickName pic x(16).
```

`move input-Rec to ACMA-rec` copies the values of `input-Rec` into `ACMA-rec`

Let's say the user inputted:
```
Enter Name: Xander
Enter Nickname: Ragebait
```

It will then be copied into the file record, `ACMA-Rec`.  

You can notice that it used a new `OPEN` mode:
```cobol
		OPEN I-O ACMA
```
`I-O` is an `OPEN` mode that is meant for `INDEXED` and `RELATIVE` files, read and write operations; it is also only appends (adds) to the file, not overwrite it unlike in `OPEN OUTPUT`.  This is not allowed for `SEQUENTIAL` 

Here is a reference for the `OPEN`modes:
```COBOL
		OPEN INPUT ACMA  *> Reading
		OPEN OUTPUT ACMA *> Writing (Overwrites; if file doesn't exist, it creates)
		OPEN EXTEND ACMA *> Appending
		OPEN I-O ACMA    *> Reading and Updating (not available for SEQUENTIAL)
```

The Create operation is done with `WRITE ACMA-rec` where it does as the clause implies; write the record into the file


---
#### Retrieve

##### Retrieve by all
When retrieving or viewing data with `INDEXED` files, we use the `READ` clause paired with `NEXT RECORD` enclosed in a `PERFORM UNTIL` loop to go through all the records you have.

Example from *`viewIndexed.cbl`*
```COBOL
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
        
```

Stripping it down to the logic that reads and displays the records:
```cobol
		perform until eof = 'y'
			read ACMA next record
				at end
					display "No more data to show"
					move 'y' to eof
				not at end
					display "| Name: " memberName " | "
						    " Nickname: " nickname " |"
			end-read
		end-perform
```



`PEFORM UNTIL eof = 'y'`
- The loop the encloses our read sentence
- if the variable `eof`'s value becomes `'y'`, it will stop

`READ <fileDescriptor> NEXT RECORD`
- This reads one record from the file and puts those current data into the record fields (variables), which in this case, is `ACMA-rec`
- `NEXT RECORD` points to the next record after reading the current one since `INDEXED` files are not stored sequentially that it would read it all 

`AT END`
- part of the `READ` sentence, the instructions what to do when the `READ` reaches the file's EOF (end of file);
- here, we tell it to:
	- A) display "No more data to show" to make it visible to the output when it reached the EOF
	- B) set `eof`'s value to 'y' to break out of the `PERFORM UNTIL` loop 

`NOT AT END
- part of the `READ` sentence where the instructions if the file being read is not at EOF
- Here we tell it to display the `memberName` and `nickname`

Sample output:
```
| Name: Arvin            |  Nickname: Rumerespeto      |
--------------------------------------------------------
| Name: Hanz             |  Nickname: Lipotato         |
--------------------------------------------------------
| Name: Luther           |  Nickname: Saving Grace     |
--------------------------------------------------------
| Name: Narciso          |  Nickname: Namiram          |
--------------------------------------------------------
| Name: Vincent          |  Nickname: Santino          |
--------------------------------------------------------
| Name: Zyron            |  Nickname: Arabisaya        |
--------------------------------------------------------
No more data to show
```

Using this method displays the primary keys in alphabetical order


##### Retrieve a specific record
On the other hand, searching for a specific record requires using the keyword `KEY IS`

Example from *`searchIndexed.cbl`*:
```cobol
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
```

In this code, the `READ` clause is appended by `KEY IS`.  This tells read to search the given key.  Take note that you can use the declared key (either `RECORD KEY IS` or `ALTERNATE KEY IS` ) in your `FILE-CONTROL`; in this case, `memberName` is the key being used

Think of this entire block as 
<blockquote>
	<em>
		"Look for <code>memberName</code> in <code>ACMA</code><br><b>If</b>  the key is not in the file, display "Name not found!".  <b>Else</b>, display <code>memberName</code> and <code>nickname</code>"
	</em>
</blockquote>

#### Update
In updating a record file, the algorithm goes as follows;
1. Check if the record you want to update is in the file
2. Update the record

Example from *`updateIndexed.cbl`*:
```cobol
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
```

- In this block, it first searches in the file if the record you want to update exists using `READ ACMA KEY IS memberName`
- If the key doesn't exist, display *"Name not found!"*
- Else, display the `name` and `nickname` then prompt the user to input the new nickname
	- move the `newName` to `nickname` so you can then
	- `REWRITE ACMA-rec`

***NOTE:*** You ***cannot*** update your primary key (`RECORD KEY IS`), and only update the other data fields


#### Delete
Similarly, you can use the same method (search the key in the file first before modifying) to delete a record, just with a little tweak.  Instead of using `REWRITE ACMA-rec`, you change it into `DELETE ACMA`.  Notably, you're no longer using the record file but the file descriptor

Example from *`deleteIndexed.cbl`*:
```cobol
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
                      
		                *> prompt the user to confirm deletion
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
```

This code block only has a few validation to confirm if the user wants to delete the record, but the core principle here is `DELETE ACMA`



<br><br><br><br><br><br><br><br><blockquote>
	<em>
	Galing talaga ng bebe ko (Nov 29, 2025)<br>Ang cute-cute pa; tangina, sarap ibulsa (Dec 3, 2025)
	</em>
</blockquote>
