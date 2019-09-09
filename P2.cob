       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT myInFile ASSIGN TO "TestFile.dat".
        SELECT myOutFile ASSIGN TO "TFileOut.dat".         
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
       *>FD myOutFile.
       *>01 outRecord.        
        *>02 CoursesOut           PIC X(9).
        *>02 CourseDOut           PIC A(38).
        *>02 GradeOut             PIC X(4).       
        *>02 CreditHrOut          PIC 99.99.           
       WORKING-STORAGE SECTION.
       01 CourseVar PIC X(9) VALUE "Course". 
       01 CourseTitle PIC A(38) VALUE "TITLE ".
       01 GradeTitle  PIC X(4) VALUE "GR ".
       01 CreditHrTitle PIC X(6) VALUE "Earned".
       01 QualityPtsTitle  PIC X(4) VALUE "QPTS".
       01 w PIC X(3) VALUE "YES".
       01 CalculateQualityPts PIC 99V99.
       01 CalculateTotalCredit PIC 99V99.
       PROCEDURE DIVISION.       
       OPEN INPUT myInFile.
       OPEN OUTPUT myOutFile.
       *>DISPLAY "SOUTHEASTERN LOUISIANA UNIVERSITY"
       *>DISPLAY CourseVar, CourseTitle, GradeTitle, CreditHrTitle    
       PERFORM subRead
       PERFORM UNTIL w = "NO"  
           *>MOVE inRecord TO outRecord                    
           *>WRITE outRecord
           PERFORM subRead           
       END-PERFORM.
       CLOSE myInFile.
       CLOSE myOutFile.
       STOP RUN.   
       subRead.     
       READ myInFile
       AT END MOVE "NO" TO w       
       NOT AT END   
       IF Grade = "A"
          MULTIPLY 4.00 BY CreditHr GIVING CalculateQualityPts 
          end-multiply 
       ELSE
           COMPUTE CalculateQualityPts = 0
       END-IF
       IF Grade = "B"
           MULTIPLY 3.00 BY CreditHr GIVING CalculateQualityPts 
          end-multiply
       END-IF
       IF Grade = "C"
           MULTIPLY 3.00 BY CreditHr GIVING CalculateQualityPts 
          end-multiply
       END-IF
       *>MULTIPLY 4.00 BY CreditHr GIVING CalculateQualityPts end-multiply
       COMPUTE CalculateTotalCredit = CalculateTotalCredit + CreditHr       
       *>COMPUTE CalculateQualityPts = ( CreditHr)
       DISPLAY CalculateQualityPts
       END-READ.
