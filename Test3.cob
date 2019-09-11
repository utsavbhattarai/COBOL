       *> Test File I/O: READ, WRITE
       *> An example program which
       *> reades (TFileIn.dat) records from a input file,
       *> displays the records,
       *> and writes (TFileOut.dat)them to a sequential file.
       *>
       *> Program-ID: TFile.cob
       *> Author: Kuo-pao Yang
       *> OS: Ubuntu 18
       *> Compiler: OpenCOBOL
       *> Note:
       *> The following instructions are used to
       *> edit, compile, and run this program
       *> $ nano TFile.cob
       *> $ cobc -x -free TFile.cob
       *> $ ./TFile
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFile.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT myInFile ASSIGN TO "TestFile.dat"
            organization is line sequential.

        SELECT myOutFile ASSIGN TO "TFileOut.dat"
            organization is line sequential.

       DATA DIVISION.
       FILE SECTION.
       FD myInFile.
       01 inRecord.
        02 StudentName    PIC X(15).
        02 StudentWNbr    PIC X(8).
        02 Years          PIC X(9).
        02 Course         PIC X(9).
        02 CourseD        PIC X(28).
        02 Grade          PIC X(1).
        02 CreditHr       PIC 9.
        02 FILLER         PIC X(1).
       FD myOutFile.
       01 studentOutRecord.
        02 DatFile PIC X(10).
        02 DatFilevar PIC 99.99.              
       WORKING-STORAGE SECTION.
       01 w PIC X(3) VALUE "YES".
       01 stor PIC X(9).
       01 TestVar PIC 99V99 VALUE 33.99.
       01 x PIC 9V.
       01 y PIC 9V99.
       PROCEDURE DIVISION.
        OPEN INPUT myInFile.
        OPEN OUTPUT myOutFile.
        PERFORM subRead
        PERFORM UNTIL w = "NO" 
       
        
        PERFORM subRead
        END-PERFORM.
        CLOSE myInFile.
        CLOSE myOutFile.
        STOP RUN.
       subRead.
        READ myInFile
        AT END
        MOVE "NO" TO w
        NOT AT END
         COMPUTE x = TestVar / 10
        IF x = 0
        MOVE TestVar to y
        end-if

        DISPLAY x
        MOVE TestVar to DatFilevar
        Move "ABV" to DatFile
        WRITE studentOutRecord 
        END-READ.
