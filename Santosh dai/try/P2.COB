       *>Program-Id: P2.cob
       *>Author: Diwas Bhattarai
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       	SELECT myInFile ASSIGN TO "P2In.dat".
       	SELECT myOutFile ASSIGN TO "P2Out.dat".
       DATA DIVISION.
       FILE SECTION.
       FD myInFile.
       01 inRecord.
       	02 name PIC X(16).
       	02 wNumb PIC X(9).
       	02 semester PIC X(12).
       	02 shortSub PIC X(8).
       	02 longSub PIC X(26).
       	02 grade PIC X(1).
       	02 credit PIC 9.
       FD myOutFile.
       01 outfile PIC X(80).
       WORKING-STORAGE SECTION.
       01 u PIC 9 VALUE 1.
       01 i PIC 9 VALUE 5.
       01 j PIC 9 VALUE 1.
       01 oSemNumTot PIC 99v99.
       01 oSemQTot PIC 99v99.
       01 oCumNumTot PIC 999v99.
       01 oCumQTot PIC 999v99.
       01 oSemAvg PIC 99v99.
       01 oCumAvg PIC 999v99.
       01 tempQOut PIC 99.99.
       01 schoolName.
       	02 PIC X(23) VALUE SPACES.
       	02 PIC X(33) VALUE "SOUTHEASTERN LOUISIANA UNIVERSITY".
       01 addressLine.
       	02 PIC X(31) VALUE SPACES.
       	02 PIC X(17) VALUE "Hammond, LA 70402".
       01 blankLine.
       	02 PIC X(80) VALUE SPACES.
       01 myNameOut.
       	02 PIC X(16).
       01 wNumbOut.
       	02 PIC X(9).
       01 semesterOut.
       	02 PIC X(12).
       01 columnTitles.
       	02 PIC X(9) VALUE "COURSE ". 
         	02 PIC X(28) VALUE "TITLE ". 
         	02 PIC XXXX VALUE "GR ". 
         	02 PIC X(7) VALUE "EARNED ". 
         	02 PIC X(5) VALUE "QPTS ". 
       01 outfile2. 
        	02 shortSubOut PIC X(8). 
         	02 PIC X VALUE SPACES. 
         	02 longSubOut PIC X(26). 
         	02 PIC X VALUE SPACES. 
         	02 letterGradeOut PIC X. 
         	02 PIC XXX VALUE SPACES. 
         	02 numberGradeOut PIC 9.99. 
         	02 PIC XXX VALUE SPACES. 
         	02 qualityPointsOut PIC 99v99.  
       01 lineSem. 
         	02 PIC X(20) VALUE SPACES.
       	02 PIC X(8) VALUE "SEMESTER".
       	02 PIC X(14) VALUE SPACES. 
         	02 tempSemNum PIC 99.99. 
         	02 PIC X(1) VALUE SPACES. 
         	02 tempSemQ PIC 99.99. 
         	02 PIC XXX VALUE SPACES. 
         	02 tempGPA PIC 9.99.  
       01 lineCum. 
         	02 PIC X(18) VALUE SPACES.
       	02 PIC X(10) VALUE "CUMULATIVE".
       	02 PIC X(13) VALUE SPACES. 
        	02 tempCumNum PIC 999.99. 
       	02 PIC X(1) VALUE SPACES.
         	02 tempCumQ PIC 999.99. 
         	02 PIC XX VALUE SPACES. 
         	02 tempCumGPA    PIC 9.99.
       PROCEDURE DIVISION.
       main.
       	OPEN INPUT myInFile.
       	OPEN OUTPUT myOutFile.
       
       	DISPLAY schoolName
       	MOVE schoolName TO outfile
       	WRITE outfile
       
       	DISPLAY addressLine
       	MOVE addressLine TO outfile
       	WRITE outfile
       
       	DISPLAY blankLine
       	MOVE blankLine TO outfile
       	WRITE outfile
       	MOVE semester TO semesterOut
       	COMPUTE i = 1
       	COMPUTE oSemNumTot = 0
       	COMPUTE oSemQTot = 0
       	PERFORM subRead
       	PERFORM until u=0 
       		
       		IF i > 0 THEN
       			DISPLAY name
       			MOVE name to outfile
       			WRITE outfile
       			DISPLAY wNumb
       			MOVE wNumb to outfile
       			WRITE outfile
       			COMPUTE i = i - 1
       		END-IF
       		
       		IF ((semesterOut NOT = semester) AND (i = 1)) THEN
       			DISPLAY blankLine
       			MOVE blankLine to outfile
       			WRITE outfile
       			DISPLAY semester
       			MOVE semester to outfile
       			WRITE outfile
       			MOVE semester to semesterOut
       			COMPUTE i = i - 1
       		ELSE 
       			IF semesterOut NOT= semester THEN
       				
       
       				COMPUTE tempGPA = oSemQTot/oSemNumTot
       				COMPUTE tempCumGPA = oCumQTot/oCumNumTot
       				MOVE oCumNumTot TO tempCumNum
       				MOVE oCumQTot TO tempCumQ
       
       				DISPLAY lineSem
       				MOVE lineSem to outfile
       				WRITE outfile
       
       				DISPLAY lineCum
       				MOVE lineCum to outfile	
       				WRITE outfile
       
       				DISPLAY blankLine
       				MOVE blankLine to outfile
       				WRITE outfile
       	
       				DISPLAY semester
       				MOVE semester to outfile
       				WRITE outfile
       
       				MOVE semester to semesterOut
       
       				COMPUTE oSemNumTot = 0
       				COMPUTE oSemQTot = 0
       
       			END-IF
       			
       		END-IF 
       
       	
       
       		IF j > 0 THEN
       			DISPLAY columnTitles
       			MOVE columnTitles TO outfile
       			WRITE outfile
       			COMPUTE j = j - 1
       		END-IF
       
       		MOVE shortSub TO shortSubOut
       		MOVE longSub TO longSubOut
       		MOVE grade TO letterGradeOut
       		MOVE credit TO numberGradeOut
       
       		IF grade = "A" THEN
       			COMPUTE qualityPointsOut = 4.0*credit
       		END-IF
       		IF grade = "B" THEN
       			COMPUTE qualityPointsOut = 3.0*credit
       		END-IF
       		IF grade = "C" THEN
       			COMPUTE qualityPointsOut = 2.0*credit
       		END-If
       		IF grade = "D" THEN
       			Compute qualityPointsOut = 1.0*credit
       		END-IF
       		IF grade = "E" THEN
       			COMPUTE qualityPointsOut = 0.0
       		END-IF
       		
       			COMPUTE oSemNumTot = oSemNumTot + credit
       			COMPUTE oSemQTot = oSemQTot + qualityPointsOut
       			COMPUTE oCumNumTot = oCumNumTot + credit
       			COMPUTE oCumQTot = oCumQTot + qualityPointsOut
       	
       
       		MOVE oSemNumTot TO tempSemNum
       		MOVE oSemQTot TO tempSemQ
       		MOVE qualityPointsOut TO tempQOut
       	
       		
       		DISPLAY shortSub, " ", longSub, "   ", grade,"    ",
               credit,"    ",tempQOut
       		
       		MOVE outfile2 to outfile
       		WRITE outfile
       
       		
       		PERFORM subRead
       		
       	END-PERFORM.
       
       	MOVE oCumNumTot TO tempCumNum
       	MOVE oCumQTot TO tempCumQ
       	COMPUTE tempCumGPA = oCumQTot/oCumNumTot
       
       	DISPLAY lineSem
       	MOVE lineSem TO outfile
       	WRITE outfile
       
       	DISPLAY lineCum	
       	MOVE lineCum TO outfile
       	WRITE outfile
       
       	CLOSE myInFile.
       	CLOSE myOutFile.
       	STOP RUN.
       subRead.
       	READ myInFile
       	AT END
       		MOVE 0 TO u
       	END-READ.
       