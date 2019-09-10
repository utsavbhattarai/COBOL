       *> Progrm-ID: P2.cob
       *> Author: Santosh Aryal
       *> OS: Ubuntu 12 
       *> Complier: OpenCOBOL 
       
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. Test.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUTFILE ASSIGN TO "P2In.dat"
       	ORGANIZATION IS LINE SEQUENTIAL ACCESS MODE IS SEQUENTIAL.
       SELECT OUTPUTFILE ASSIGN TO "P2Out.dat"
       	*>ORGANIZATION IS LINE SEQUENTIAL ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD OUTPUTFILE.
       01	OutDatFile.
       	02 DatFile		PIC X(100).
       FD INPUTFILE.
       01 INPUTFILELINE.
       	02 RunningLine		PIC X(100).
       
       WORKING-STORAGE SECTION.
       01 VARIABLES.
       	02 LineLength		PIC X(100).	
       	02 StudentName		PIC X(15).
       	02 StudentWnumber	PIC X(8).
       	02 NameAvailable	PIC X(1) VALUE "Y".
       	02 NewSemAvailable	PIC X(1) VALUE "Y".
       	02 NewSem 			PIC X(1) VALUE "N".
       	02 LargeSpace		PIC X(4) VALUE SPACES.
       	02 SemesterName		PIC X(20).
       	02 SubjectID		PIC X(10).
       	02 SubjectName		PIC X(30).
       	02 SubjectPTS       PIC 99.
       	02 SubjectCredit	PIC 99.
       	02 SubjectGrade		PIC X(1).
       	02 obtainedGrade	PIC 99.
       	02 SemesterGrade	PIC 99.
       	02 SemesterToalHour	PIC 9(1).9(2).
       	02 SemesterGPA		PIC 9(1).9(2).
       	02 CommulativeGpa	PIC 9(1).9(2).
       	02 SemesterCredit	PIC 99.
       	02 TotalHours		PIC 99.
       	02 QtPoints			PIC 99.
       	02 QualityPoints    PIC 999.
       	02 FirstSem			PIC X(1) VALUE "Y".
       
       01 FLAGS.
           02 EndOfFile		PIC X(3)	VALUE "NO".
       
       PROCEDURE DIVISION.
       BEGIN.
       	
       	PERFORM 005-INITIALIZEFILE.
       
       	PERFORM 002-WRITEHEADING.
       
        PERFORM 004-DISPLAYSEMESTERHEADING.
       
       	PERFORM 006-SubRead
       		 UNTIL EndOfFile = "YES".
       	
            
            CLOSE INPUTFILE, OUTPUTFILE
       STOP RUN.
       
       002-WRITEHEADING.
       	MOVE "" TO DatFile
       	STRING LargeSpace,
           LargeSpace,LargeSpace,LargeSpace,LargeSpace,LargeSpace,
           LargeSpace,"SOUTHEASTERN LOUISIANA UNIVERSITY" INTO DatFile	
       	WRITE OutDatFile
       	MOVE "" TO DatFile
       	STRING LargeSpace,
           LargeSpace,LargeSpace,LargeSpace,LargeSpace,LargeSpace,
           LargeSpace,LargeSpace,LargeSpace,"HAMMOND, LA, 70402",x'0a' 
               INTO DatFile	
       	WRITE OutDatFile	
       	DISPLAY "				","SOUTHEASTERN LOUISIANA UNIVERSITY"
       	DISPLAY " 					","HAMMOND, LA, 70402",x'0a'.
       
       
       003-WriteNameandWnumber.
       	MOVE RunningLine to StudentName.
       	MOVE StudentName TO DatFile.
       	WRITE OutDatFile
       	MOVE StudentWnumber TO DatFile.
       	WRITE OutDatFile.
       	DISPLAY StudentName.
       	DISPLAY StudentWnumber.
       
       004-DISPLAYSEMESTERHEADING.
       	STRING "COURSE",LargeSpace,
           LargeSpace,"TITLE",LargeSpace,LargeSpace,LargeSpace,
           LargeSpace,LargeSpace,LargeSpace,"  GR",LargeSpace,"EARNED",
           "  ","QPTS" INTO DatFile	
       	WRITE OutDatFile
       	MOVE "" TO DatFile
       	DISPLAY "COURSE",LargeSpace,LargeSpace,"TITLE",LargeSpace,
           LargeSpace,LargeSpace,LargeSpace,LargeSpace,LargeSpace," GR",
           LargeSpace,"EARNED","  ","QPTS".
       
       005-INITIALIZEFILE.
       	OPEN INPUT INPUTFILE.
       	OPEN OUTPUT OUTPUTFILE.
       	READ INPUTFILE
       		AT END
       		    MOVE "YES" TO EndOfFile		 
       		END-READ.
       
       006-SubRead.
       		MOVE RunningLine TO LineLength
       		IF NameAvailable = "Y"
       			MOVE "" TO DatFile
       			UNSTRING RunningLine  
       			DELIMITED BY '  ' INTO StudentName, StudentWnumber
       					DISPLAY StudentName 
       					DISPLAY  StudentWnumber
       		   			STRING StudentName INTO DatFile
       		   			WRITE OutDatFile
       		   			MOVE "" TO DatFile 
       		   			STRING StudentWnumber INTO DatFile
       		   			WRITE OutDatFile
       		   			MOVE "N" TO NameAvailable
       		ELSE
       			IF RunningLine = "$$"							
       				MOVE "Y" TO NewSem
       				IF FirstSem ="N"
       					MOVE "" TO DatFile 
       					Compute SemesterGPA = 
                           SemesterGrade / SemesterCredit
       					Compute TotalHours = TotalHours + SemesterCredit
       					Compute QualityPoints = 
                           QualityPoints + SemesterGrade
       					Compute CommulativeGpa = 
                           QualityPoints /  TotalHours
       					DISPLAY LargeSpace,LargeSpace,LargeSpace,
                           LargeSpace,LargeSpace,LargeSpace,LargeSpace,
                           "SEMESTER", LargeSpace,LargeSpace,LargeSpace,
                           "    ", SemesterCredit, LargeSpace," ",
                           SemesterGrade,LargeSpace,SemesterGPA
       					STRING LargeSpace,LargeSpace,LargeSpace,
                           LargeSpace,LargeSpace,LargeSpace,LargeSpace,
                           "SEMESTER", LargeSpace,LargeSpace,LargeSpace,
                           "     ", SemesterCredit, LargeSpace," ",
                           SemesterGrade,LargeSpace,SemesterGPA INTO  
                           OutDatFile
       					WRITE OutDatFile
       				   	MOVE "" TO DatFile
       				   	DISPLAY LargeSpace,LargeSpace,LargeSpace,
                              LargeSpace,LargeSpace,LargeSpace,
                              LargeSpace,"CUMULATIVE", LargeSpace,
                              LargeSpace,LargeSpace,"  ", TotalHours, 
                              LargeSpace," ",QualityPoints,LargeSpace,
                              CommulativeGpa
       				   	STRING LargeSpace,LargeSpace,LargeSpace,
                              LargeSpace,LargeSpace,LargeSpace,
                              LargeSpace,"CUMULATIVE", LargeSpace,
                              LargeSpace,LargeSpace,"   ", TotalHours, 
                              LargeSpace," ",QualityPoints,LargeSpace,
                              CommulativeGpa INTO OutDatFile
       					WRITE OutDatFile
       				   	MOVE "" TO DatFile
       				   	MOVE "0" TO CommulativeGpa
       					MOVE "0" TO SemesterCredit
       					MOVE "0" TO SemesterGPA
       					MOVE "0" TO SemesterGrade
       				END-IF
       				MOVE "N" TO FirstSem
       			ELSE
       				IF RunningLine = "**"
       					 MOVE "YES" TO EndOfFile
       				ELSE	 
       					IF NewSem = "Y"				
       						MOVE RunningLine TO SemesterName
       						DISPLAY x'0a',SemesterName
       						STRING x'0a',SemesterName INTO OutDatFile
       						WRITE OutDatFile
       			   			MOVE "" TO DatFile
       						MOVE "N" TO NewSem
       						PERFORM 004-DISPLAYSEMESTERHEADING
       					ELSE
       						UNSTRING RunningLine 
       						DELIMITED BY '__' INTO  SubjectID, 
                               SubjectName, SubjectCredit,SubjectGrade
       						PERFORM 007-ComputeQualityPoints	
       						DISPLAY SubjectID,LargeSpace,SubjectName,
                               ""SubjectGrade,LargeSpace,"   ",
                               SubjectCredit,LargeSpace," ",
                               obtainedGrade
       						STRING SubjectID,LargeSpace,SubjectName,"  "
                                   ,SubjectGrade,LargeSpace,"  ",
                                   SubjectCredit,LargeSpace," ",
                                   obtainedGrade INTO OutDatFile
       						COMPUTE SemesterCredit = 
                               SemesterCredit + SubjectCredit
       						COMPUTE SemesterGrade = 
                               SemesterGrade + obtainedGrade
       						COMPUTE SemesterGPA = 
                   SemesterGPA + obtainedGrade 
       						WRITE OutDatFile
       			   			MOVE "" TO DatFile 
       						END-IF
       				END-IF		   		
       			END-IF
       		END-IF
       		READ INPUTFILE
         		AT END
       		    MOVE "YES" TO EndOfFile		 
       		END-READ.
              007-ComputeQualityPoints.
       	  IF SubjectGrade = "A"
               MOVE "4" TO SubjectPTS
               COMPUTE obtainedGrade = SubjectPTS * SubjectCredit
       
               END-IF
               IF SubjectGrade = "B"
                      MOVE "3" TO SubjectPTS
               		COMPUTE obtainedGrade = SubjectPTS * SubjectCredit
               END-IF
               IF SubjectGrade = "C"
                      MOVE "2" TO SubjectPTS
               		COMPUTE obtainedGrade = SubjectPTS * SubjectCredit
               END-IF
               IF SubjectGrade = "D"
                   MOVE "1" TO SubjectPTS
               	COMPUTE obtainedGrade = SubjectPTS * SubjectCredit
               END-IF
               IF SubjectGrade = "F"
                       MOVE "0" TO SubjectPTS
               		COMPUTE obtainedGrade = SubjectPTS * SubjectCredit
               END-IF.
       
       
       