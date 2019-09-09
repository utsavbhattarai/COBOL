       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE SECTION.
           SELECT In-File ASSIGN TO "TFileIn.dat".
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD In-File.
       01 IN-RECORD.
           02 Courses           PIC X(8).
           02 CourseName        PIC X(36).
           02 Grade             PIC X(1).
           02 CreditHour        PIC 9(4).
       WORKING-STORAGE SECTION.
       01 Switches.
           05 eof-switch PIC X VALUE "N".
       01 COUNTERS.
           05 REC-COUNTER PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-RECORDS
               UNTIL eof-switch = "Y"
       100-INITIALIZE.

       OPEN INPUT In-File.
    
       READ In-File
          AT END
           MOVE "Y" TO eof-switch
           NOT AT END
           COMPUTE REC-COUNTER = REC-COUNTER + 1
       END-READ.
       200-PROCESS-RECORDS.
       DISPLAY "Courses" Courses.
       DISPLAY "COURSE NAME" CourseName.
       DISPLAY "GRADE" Grade.

       READ In-File
           AT END 
               MOVE "Y" TO eof-switch
            NOT AT END
               COMPUTE REC-COUNTER = REC-COUNTER + 1
        END READ.
       
       