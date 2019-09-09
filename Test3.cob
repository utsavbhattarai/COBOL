       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT StudentFile ASSIGN TO "TestFile.dat"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OutputFile ASSIGN TO "Report.dat"
       ORGANIZATION IS LINE SEQUENTIAL.
       
       
       DATA DIVISION.
       FILE SECTION.
       FD StudentFile.
       
       *>Student details will only be printed once
       01 StudentDetails.
       05 STUDENT-NAME         PIC X(16).
       05 STUDENT-ID           PIC X(9).
       
       *>Semester info that will be on one line and not repeated
       01 SemesterDetails.
       05 SEMESTER             PIC X(9).
       
       *> Details in the class that need to be seperate
       01 ClassDetails.
       05 CLASS-NAME           PIC X(32).
       05 GRADE                PIC X(2).
       05 HOURS                PIC X(4).
       05 POINTS               PIC X(2).
       
       *>values that need to be calculated
       01 CalculatedValues.
       05 CUMULATIVE-GPA-IN    PIC 99v99 VALUE ZERO.
       05 CUMULATIVE-QP-IN     PIC 99v99 VALUE ZERO.
       05 CUMULATIVE-HOURS-IN  PIC 99v99 VALUE ZERO.
       
       FD OutputFile.
       01 PrintLine                    PIC X(70).
       
       WORKING-STORAGE SECTION.
       
       01 SWITCHES.
       05 EOF-SWITCH           PIC X VALUE "N".
       01 COUNTERS.
       05 REC-COUNTER          PIC 9(3) VALUE 0.
       01 CUMULATIVE.
       05 CUMULATIVE-QP        PIC 99V99 value zero.
       
       PROCEDURE DIVISION.
       *>main paragraph, everything starts here
       Main.
       PERFORM Begin.
       PERFORM ProcessData.
       PERFORM PrintLines
       UNTIL EOF-SWITCH = "Y".
       
       *>opening read
       Begin.
       OPEN INPUT StudentFile
       OPEN OUTPUT OutputFile
       
       READ StudentFile
       AT END
       MOVE "Y" TO EOF-SWITCH
       NOT AT END
       COMPUTE REC-COUNTER = REC-COUNTER + 1
       END-READ.
       
       ProcessData.
       READ StudentFile
       AT END
       MOVE "Y" TO EOF-SWITCH
       NOT AT END
       IF GRADE = "A"
       COMPUTE CUMULATIVE-QP = CUMULATIVE-QP + 4
       ELSE
       IF GRADE = "B"
       COMPUTE CUMULATIVE-QP = CUMULATIVE-QP + 3
       ELSE
       IF GRADE = "C"
       COMPUTE CUMULATIVE-QP = CUMULATIVE-QP + 2
       ELSE
       IF GRADE = "D"
       COMPUTE CUMULATIVE-QP = CUMULATIVE-QP + 1
       END-IF.
       
       
       *>printing out our lines to terminal
       PrintLines.
       
       READ StudentFile
       AT END
       MOVE "Y" TO EOF-SWITCH
       
       NOT AT END
       DISPLAY CUMULATIVE-QP
       END-READ.