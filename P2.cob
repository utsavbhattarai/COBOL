       *> Progrm-ID: TFile.cob
       *> Authors:    Utsav Bhattarai, Biraj Basnet, Bisheshwor Ghimire, Eraj Khatiwada
       *> OS:        Ubuntu 18
       *> Compiler:  OpenCOBOL
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT myInFile ASSIGN TO "P2In.dat"
        organization is line sequential.
        SELECT myOutFile ASSIGN TO "P2Out.dat"
        organization is line sequential.
                   
       DATA DIVISION.
       FILE SECTION.
       FD myInFile.           
       01 inRecord.
        02 StudentName    PIC X(15).
        02 StudentWNbr    PIC X(8).
        02 Years          PIC X(11).
        02 Course         PIC X(9).
        02 CourseD        PIC X(28).
        02 Grade          PIC X(1).
        02 CreditHr       PIC 9.
        02 FILLER         PIC X(1).   
       FD myOutFile.
       01 outRecord. 
        02 DatFile		PIC X(100).      
       01 outRecordCalc.
           02 stringvar PIC X(51).
           02 CalculateTotalCreditOut PIC 99.99.
           02 spaceVar PIC X(4).           
           02 CalculateTotalQPOut PIC 99.99.
           02 spaceVar PIC X(4).
           02 calcVal PIC 9.99.
       WORKING-STORAGE SECTION.
       01 CourseVar PIC X(9) VALUE "Course". 
       01 CourseTitle PIC A(38) VALUE "TITLE ".
       01 GradeTitle  PIC X(4) VALUE "GR ".
       01 CreditHrTitle PIC X(6) VALUE "Earned".
       01 QualityPtsTitle  PIC X(4) VALUE "QPTS".
       01 w PIC X(3) VALUE "YES".
       01 StuName  PIC X(15) VALUE "UTSAV BHATTARAI".
       01 StuWNbr PIC X(8) VALUE "W0655844".
       01 CalculateQualityPts PIC 99V99.
       01 CalculateTotalCredit PIC 99V99.
       01 CounterVar PIC 9.
       01 GradeSpace PIC X(10).
       01 EarnedSpace PIC X(3).
       01 QualityPtsSpace PIC X(5).
       01 SemesterSpace PIC X(40).
       01 SemCalSpace PIC x(3).
       01 YearSaver PIC x(11).
       01 CumulativeCalc PIC 99V99.
       01 CalculateTotalQP PIC 99V99.
       01 CalculateTotalQPONEFIVE PIC 99V99.
       01 CalculateTotalQPONESIX PIC 99V99.
       01 SemGPA   PIC 9V99.
       01 CumGPA   PIC 9V99.
       01 QpValueDisplay PIC 99V.
       01 StudentNameOut    PIC X(15).
       01 StudentWNbr    PIC X(8).
       01 TruncateValue PIC 9V.
       01 QpValueDisplayOne PIC 9V.
       
       PROCEDURE DIVISION.       
       OPEN INPUT myInFile.
       OPEN OUTPUT myOutFile.
       DISPLAY "               SOUTHEASTERN LOUISIANA UNIVERSITY"
       DISPLAY "                    HAMMOND, LA 70402           "
       MOVE "               SOUTHEASTERN LOUISIANA UNIVERSITY" TO 
       DatFile
       WRITE outRecord
       MOVE "                    HAMMOND, LA 70402           " TO 
       DatFile
       WRITE outRecord     
       DISPLAY StuName
       MOVE StuName TO DatFile
       WRITE outRecord
       display StuWNbr
       MOVE StuWNbr TO DatFile
       WRITE outRecord       
       display " "
       MOVE " " TO DatFile
       WRITE outRecord
       MOVE "FALL 2014" TO DatFile
       WRITE outRecord
       DISPLAY "FALL 2014" 
       STRING CourseVar, CourseTitle, GradeTitle, CreditHrTitle, 
       EarnedSpace, QualityPtsTitle INTO DatFile
       WRITE outRecord 
       MOVE ' ' TO DatFile
       DISPLAY CourseVar, CourseTitle, GradeTitle, CreditHrTitle, 
       EarnedSpace, QualityPtsTitle 

       PERFORM subRead
       PERFORM UNTIL w = "NO"
           *>MOVE Course to CourseOut                             
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
           COMPUTE CounterVar = CounterVar + 1
           COMPUTE SemGPA = 
               CalculateTotalQPONESIX/CalculateTotalCredit
               COMPUTE CumGPA =  CalculateTotalQP / CumulativeCalc
           DISPLAY SemesterSpace, "SEMESTER", SemCalSpace,
           CalculateTotalCredit, "    ",CalculateTotalQPONESIX, "    ", 
           SemGPA           
           DISPLAY SemesterSpace, "CUMULATIVE ", CumulativeCalc,
           "    ",CalculateTotalQP, "    ", CumGPA
           MOVE SemGPA TO calcVal           
           STRING SemesterSpace, "SEMESTER", SemCalSpace INTO 
               outRecordCalc
               Move CalculateTotalCredit to CalculateTotalCreditOut               
               Move CalculateTotalQPONESIX to CalculateTotalQPOut
               WRITE outRecordCalc                         
               STRING SemesterSpace, "CUMULATIVE  ", SemesterSpace, 
               INTO stringvar               
               Move CumulativeCalc to CalculateTotalCreditOut
               MOVE CumGPA TO calcVal
               MOVE CalculateTotalQP TO CalculateTotalQPOut
               WRITE outRecordCalc
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
       COMPUTE CalculateTotalQP = CalculateTotalQP + 
       CalculateQualityPts 
       COMPUTE CounterVar = CounterVar + 1                 
           WHEN "SPRING 2015"
               IF YearSaver IS NOT EQUAL Years 
               COMPUTE CumulativeCalc = CumulativeCalc + 
               CalculateTotalCredit
               COMPUTE SemGPA = CalculateTotalQP/CalculateTotalCredit  
               DISPLAY SemesterSpace, "SEMESTER", SemCalSpace,
               CalculateTotalCredit, "    ",CalculateTotalQP, 
               "    ", SemGPA
               DISPLAY SemesterSpace, "CUMULATIVE ", CumulativeCalc
               , "    ",CalculateTotalQP, "    ",SemGPA
               Move CalculateTotalCredit to CalculateTotalCreditOut
               STRING SemesterSpace, "SEMESTER", SemCalSpace INTO 
               outRecordCalc               
               MOVE SemGPA TO calcVal
               MOVE CalculateTotalQP TO CalculateTotalQPOut
               WRITE outRecordCalc           
               STRING SemesterSpace, "CUMULATIVE  ", SemesterSpace, 
               INTO stringvar
               MOVE SemGPA TO calcVal
               MOVE CalculateTotalQP TO CalculateTotalQPOut
               WRITE outRecordCalc
               Compute CalculateQualityPts = 0
               COMPUTE CounterVar = 0
               COMPUTE CalculateTotalCredit = 0          
               display " "
               MOVE "SPRING 2015" TO YearSaver 
               MOVE Years TO DatFile
               WRITE outRecord
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
       WHEN "FALL 2015"
               IF YearSaver IS NOT EQUAL Years 
               COMPUTE CumulativeCalc = CumulativeCalc + 
               CalculateTotalCredit
               COMPUTE SemGPA = 
               CalculateTotalQPONEFIVE/CalculateTotalCredit
               COMPUTE CumGPA =  CalculateTotalQP / CumulativeCalc
               DISPLAY SemesterSpace, "SEMESTER", SemCalSpace,
               CalculateTotalCredit, "    ",CalculateTotalQPONEFIVE, 
               "    ",SemGPA
               DISPLAY SemesterSpace, "CUMULATIVE ", CumulativeCalc,
               "    ",CalculateTotalQP,   "    ",CumGPA
               STRING SemesterSpace, "SEMESTER", SemCalSpace INTO 
               outRecordCalc
               Move CalculateTotalCredit to CalculateTotalCreditOut
               MOVE SemGPA TO calcVal
               MOVE CalculateTotalQPONEFIVE TO CalculateTotalQPOut
               WRITE outRecordCalc           
               STRING SemesterSpace, "CUMULATIVE  ", SemesterSpace, 
               INTO stringvar
               Move CumulativeCalc to CalculateTotalCreditOut
               MOVE CumGPA TO calcVal
               MOVE CalculateTotalQP TO CalculateTotalQPOut
               WRITE outRecordCalc           
               Compute CalculateQualityPts = 0
               COMPUTE CounterVar = 0
               COMPUTE CalculateTotalCredit = 0          
               display " "
               MOVE "FALL 2015" TO YearSaver 
               MOVE Years TO DatFile
               WRITE outRecord
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
       COMPUTE CumulativeCalc = CumulativeCalc + 
           CalculateTotalCredit
           COMPUTE CalculateTotalCredit = CalculateTotalCredit + 
           CreditHr            
           COMPUTE CalculateTotalQPONESIX = CalculateTotalQPONESIX +
       CalculateQualityPts
       COMPUTE CumulativeCalc = CumulativeCalc + 
           CalculateTotalCredit           
           
           COMPUTE CalculateTotalQP = CalculateTotalQP + 
           CalculateQualityPts         
           
            CONTINUE
       END-EVALUATE
       COMPUTE TruncateValue = CalculateQualityPts / 10 
       IF (TruncateValue = 0)
           MOVE CalculateQualityPts TO QpValueDisplayOne
           STRING Course, CourseD, GradeSpace, Grade, EarnedSpace, 
           CreditHr, 
       ".00", QualityPtsSpace, QpValueDisplayOne, ".00" INTO DatFile                     
       DISPLAY Course, CourseD, GradeSpace, Grade, EarnedSpace, CreditHr,
       ".00", QualityPtsSpace, QpValueDisplayOne, ".00"    
       WRITE outRecord      
       MOVE " " TO DatFile
        else 
        MOVE CalculateQualityPts TO QpValueDisplay
        STRING Course, CourseD, GradeSpace, Grade, EarnedSpace, CreditHr, 
       ".00", QualityPtsSpace, QpValueDisplay, ".00" INTO DatFile                     
       DISPLAY Course, CourseD, GradeSpace, Grade, EarnedSpace, CreditHr,
       ".00", QualityPtsSpace, QpValueDisplay,".00"     
       WRITE outRecord      
       MOVE " " TO DatFile
       END-IF
       *>MOVE CalculateQualityPts TO QpValueDisplay
       
       END-READ.
