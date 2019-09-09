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
       FD myOutFile.
       01 outRecord.        
        02 StudentNameOut    PIC X(15).
        02 StudentWNbrOut    PIC X(8).
        02 YearsOut          PIC X(9).
        02 CourseOut         PIC X(9).
        02 CourseDOut        PIC X(28).
        02 GradeOut          PIC X(1).
        02 CreditHrOut       PIC 9.
        02 FILLER         PIC X(1).           
       WORKING-STORAGE SECTION.
       01 CourseVar PIC X(9) VALUE "Course". 
       01 CourseTitle PIC A(38) VALUE "TITLE ".
       01 GradeTitle  PIC X(4) VALUE "GR ".
       01 CreditHrTitle PIC X(6) VALUE "Earned".
       01 QualityPtsTitle  PIC X(4) VALUE "QPTS".
       01 w PIC X(3) VALUE "YES".
       01 CalculateQualityPts PIC 99V99.
       01 CalculateTotalCredit PIC 99V99.
       01 CounterVar PIC 9.
       01 GradeSpace PIC X(10).
       01 EarnedSpace PIC X(3).
       01 QualityPtsSpace PIC X(8).
       01 SemesterSpace PIC X(40).
       01 SemCalSpace PIC x(3).
       01 YearSaver PIC x(9).
       01 CumulativeCalc PIC 99V99.
       01 CalculateTotalQP PIC 99V99.
       01 CalculateTotalQPONEFIVE PIC 99V99.
       01 CalculateTotalQPONESIX PIC 99V99.
       01 SemGPA   PIC 9V99.
       01 CumGPA   PIC 9V99.
       PROCEDURE DIVISION.       
       OPEN INPUT myInFile.
       OPEN OUTPUT myOutFile.
       DISPLAY "SOUTHEASTERN LOUISIANA UNIVERSITY"
       DISPLAY Years
       DISPLAY CourseVar, CourseTitle, GradeTitle, CreditHrTitle, 
       EarnedSpace, QualityPtsTitle    
       PERFORM subRead
       PERFORM UNTIL w = "NO"  
           MOVE inRecord TO outRecord                    
           WRITE outRecord
           PERFORM subRead           
       END-PERFORM.
       CLOSE myInFile.
       CLOSE myOutFile.
       STOP RUN.   
       subRead.     
       READ myInFile
       AT END MOVE "NO" TO w       
       if w = "NO"
           COMPUTE CalculateTotalCredit = CalculateTotalCredit + 
           CreditHr 
           COMPUTE CounterVar = CounterVar + 1 
           COMPUTE CumulativeCalc = CumulativeCalc + 
           CalculateTotalCredit 
           COMPUTE CalculateTotalQP = CalculateTotalQP + 
           CalculateQualityPts
           COMPUTE SemGPA = 
               CalculateTotalQPONEFIVE/CalculateTotalCredit
               COMPUTE CumGPA =  CalculateTotalQP / CumulativeCalc
           DISPLAY SemesterSpace, "SEMESTER", SemCalSpace,
           CalculateTotalCredit, SemGPA
           DISPLAY SemesterSpace, "CUMULATIVE ", CumulativeCalc,
           CumGPA
           DISPLAY "AT END"
       END-IF
       NOT AT END   
       EVALUATE Years
           WHEN "FALL 2014"
            MOVE Years TO YearSaver
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
           MULTIPLY 2.00 BY CreditHr GIVING CalculateQualityPts 
          end-multiply
       END-IF
       COMPUTE CalculateTotalCredit = CalculateTotalCredit + CreditHr
       COMPUTE CalculateTotalQP = CalculateTotalQP + CalculateQualityPts 
       COMPUTE CounterVar = CounterVar + 1                 
           WHEN "FALL 2015"
               IF YearSaver IS NOT EQUAL Years 
               COMPUTE CumulativeCalc = CumulativeCalc + 
               CalculateTotalCredit
               COMPUTE SemGPA = CalculateTotalQP/CalculateTotalCredit  
               DISPLAY SemesterSpace, "SEMESTER", SemCalSpace,
               CalculateTotalCredit, "    ",CalculateTotalQP, 
               "  ", SemGPA
               DISPLAY SemesterSpace, "CUMULATIVE ", CumulativeCalc
               , "    ",CalculateTotalQP, " ",SemGPA
                           
               Compute CalculateQualityPts = 0
               COMPUTE CounterVar = 0
               COMPUTE CalculateTotalCredit = 0          
               display " "
               MOVE "FALL 2015" TO YearSaver 
               DISPLAY Years
               END-IF                           
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
           MULTIPLY 2.00 BY CreditHr GIVING CalculateQualityPts 
          end-multiply
       END-IF
       COMPUTE CalculateTotalCredit = CalculateTotalCredit + CreditHr
       COMPUTE CalculateTotalQP = CalculateTotalQP + CalculateQualityPts
       COMPUTE CalculateTotalQPONEFIVE = CalculateTotalQPONEFIVE +
       CalculateQualityPts
       COMPUTE CounterVar = CounterVar + 1         
            CONTINUE 
       WHEN "FALL 2016"
               IF YearSaver IS NOT EQUAL Years 
               COMPUTE CumulativeCalc = CumulativeCalc + 
               CalculateTotalCredit
               COMPUTE SemGPA = 
               CalculateTotalQPONEFIVE/CalculateTotalCredit
               COMPUTE CumGPA =  CalculateTotalQP / CumulativeCalc
               DISPLAY SemesterSpace, "SEMESTER", SemCalSpace,
               CalculateTotalCredit, "    ",CalculateTotalQPONEFIVE, 
               SemGPA
               DISPLAY SemesterSpace, "CUMULATIVE ", CumulativeCalc,
               "    ",CalculateTotalQP,   CumGPA           
               Compute CalculateQualityPts = 0
               COMPUTE CounterVar = 0
               COMPUTE CalculateTotalCredit = 0          
               display " "
               MOVE "FALL 2016" TO YearSaver 
               DISPLAY Years
               END-IF                           
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
           MULTIPLY 2.00 BY CreditHr GIVING CalculateQualityPts 
          end-multiply
       END-IF
       COMPUTE CounterVar = CounterVar + 1         
            CONTINUE
       END-EVALUATE       
       DISPLAY Course, CourseD, GradeSpace, Grade, EarnedSpace, CreditHr
       , QualityPtsSpace, CalculateQualityPts
       END-READ.
