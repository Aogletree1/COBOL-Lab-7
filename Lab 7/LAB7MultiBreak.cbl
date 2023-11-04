       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LAB7MULTIBREAK.
       AUTHOR.         AUSTIN OGLETREE.
      *************************************************
      *      LAB 7 - MULTI-LEVEL CONTROL BREAK/ARRAYS
      *
      *    THIS PROGRAM READS A STUDENT FILE.  YOU WILL NEED TO
      *    INSERT THE EVALUATE STATEMENT FOR A MULTI-LEVEL 
      *    CONTROL BREAK ON DEPT CODE AND CLASS CODE. 
      *******
      *  INPUT: THIS PROGRAM WILL USE THE FOLLOWING FIELDS 
      *         FROM THE STUDENT INPUT FILE:
      *         DEPT CODE, CLASS CODE, STUDENT NAME, ARRAY OF 
      *         4 TEST SCORES
      *******
      *  OUTPUT: PRINTED DETAIL SUMMARY REPORT
      *          THE REPORT WILL BE GROUPED TOGETHER 
      *          BASED ON DEPT(MAJOR BREAK) 
      *          AND CLASS(MINOR BREAK)
      *******
      *  CALCULATIONS:
      *      SUM 4 TEST SCORES
      *      GET AVERAGE OF INDIVIDUAL TEST SCORES
      *      ACCUMULATE A COUNT OF STUDENTS FOR EACH CLASS
      *      ACCUMULATE A COUNT OF STUDENTS FOR EACH DEPT
      *******
      *   INSTRUCTIONS
      *   1. Code the array in the input record
      *   2. Code the Hold Fields to be used in the control break
      *   3. Code the Control Break Check USING AN EVALUATE STATEMENT
      *   4. Code the Major Break Paragraph
      *   5. Code the End of Job Paragraph that force prints
      *      the last class and department lines
      ******
      *  NOTE:  Open the correct out put and your output 
      *         with Word or other good word processor.
      *         Don't use Notepad. 
      *********************************************      
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT STUDENT-FILE
               ASSIGN TO "STUDENT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER "LAB8OUTPUT.TXT".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD STUDENT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  STUDENT-RECORD.
           05  SR-DEPT-CODE                    PIC A(4).
           05  SR-CLASS-CODE                   PIC X(5).
           05  SR-NAME                         PIC X(20).
      * CODE THE ARRAY FOR THE INCOMING GRADES HERE
           05  SR-GRADE-ARRAY-IN OCCURS 4 TIMES.
               10 SR-TEST-IN                   PIC 9(3).
        


           05  FILLER                          PIC X(39).

      *
       FD  STUDENT-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-LINE                     PIC X(80).

      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  FIRST-RECORD                PIC X(3)    VALUE 'YES'.
           05  SUB                         PIC 9       VALUE 1.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC 9       VALUE 1.
      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
      *
       01  DETAIL-FIELDS.
           05  DF-TEST-TOTAL                PIC S9(5)    VALUE +0.
           05  DF-TEST-AVERAGE              PIC S9(5)V99 VALUE +0.
      *
       01  CLASS-FIELDS.
           05  CF-STUDENT-COUNT             PIC S99      VALUE +0.
      *
       01  DEPT-FIELDS.
           05  DF-STUDENT-COUNT             PIC S99      VALUE +0.      
      *
       01  HOLD-FIELDS.
      *  CODE HOLD FIELDS FOR THE CONTROL BREAKS HERE
           05 DEPT-HOLD                     PIC A(4).
           05 CLASS-HOLD                    PIC X(5).



      ********************OUTPUT AREA*********************************

       01  HEADING-1.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(7) VALUE SPACES.
           05                              PIC X(25) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(17) VALUE 'XXX'.
           05                              PIC X(5) VALUE SPACES.
           05 H1-PAGE-NO                   PIC 99 VALUE ZERO.
      *
       01  HEADING-2.
           05                              PIC X(5) VALUE SPACES.
           05                              PIC X(20) VALUE
                                               'DEPARTMENT CODE: '.
           05                              PIC X(5) VALUE SPACES.
           05 H2-DEPT-CODE                 PIC A(4).
      *
       01  HEADING-3.
           05                              PIC X(5) VALUE SPACES.
           05                              PIC X(12) VALUE
                                               'CLASS CODE: '.
           05                              PIC X(5) VALUE SPACES.
           05 H3-CLASS-CODE                PIC X(5).
      *
       01  HEADING-4.
           05                              PIC X(19) VALUE SPACES.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(3)  VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7)  VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                              PIC X(7) VALUE SPACES.
           05  DL-NAME                     PIC X(20).
           05                              PIC X(7).
      * CODE ARRAY FOR 4 SCORE WITH 5 SPACES BETWEEN COLUMNS OUTPUT HERE
           05  GRADE-ARRAY-OUT OCCURS 4 TIMES.
               10 TEST-OUT                 PIC X(3).
               10 FILLER                   PIC X(5) VALUES SPACES.

           05  DL-GRADE                    PIC X.

      *
       01  CLASS-GROUP-LINE.
           05                              PIC X(45)   VALUE
                            'TOTAL MUMBER OF STUDENTS FOR CLASS '.
           05  CGL-CLASS-CODE              PIC X(5).
           05                              PIC X(5)    VALUE ' IS  '.
           05  CGL-CLASS-TOTAL             PIC ZZ9.

       01  DEPART-GROUP-LINE.
           05                              PIC X(45) VALUE
                            'TOTAL NUMBER OF STUDENTS FOR DEPT '.
           05  DGL-DEPT-CODE               PIC A(4).
           05                              PIC X(6)    VALUE ' IS  '.
           05  DGL-DEPT-TOTAL              PIC ZZ9.


      *
       PROCEDURE DIVISION.
      *
       100-PRINT-STUDENT-REPORT.
           PERFORM 200-HSKPING-ROUTINE
           PERFORM 400-READ-STUDENT-FILE
           PERFORM 1100-END-OF-JOB-ROUTINE
           PERFORM 1200-FINAL-ROUTINE
        .

       200-HSKPING-ROUTINE.
           OPEN INPUT  STUDENT-FILE
                OUTPUT STUDENT-REPORT-FILE

           ACCEPT WS-CURRENT-DATE FROM DATE

           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           PERFORM 300-REPORT-HEADER
       .

       300-REPORT-HEADER.

           ADD 1 TO H1-PAGE-NO

           WRITE REPORT-LINE FROM HEADING-1
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING
       .

       400-READ-STUDENT-FILE.

           PERFORM UNTIL NO-MORE-DATA
               READ STUDENT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 700-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM
       .

       500-PRINT-DEPT-HEADER.

           MOVE SR-DEPT-CODE TO H2-DEPT-CODE
           WRITE REPORT-LINE FROM HEADING-2
               AFTER ADVANCING 2 LINES
       .

       600-PRINT-CLASS-HEADER.

           MOVE SR-CLASS-CODE TO H3-CLASS-CODE
           WRITE REPORT-LINE FROM HEADING-3
               AFTER ADVANCING 2 LINES

      *  This prints the column headers
           WRITE REPORT-LINE FROM HEADING-4
               AFTER ADVANCING 2 LINES
       .

       700-PROCESS-STUDENT-RECORD.
      *  WRITE THE CODE FOR CONTROL BREAKS USE AN EVALUATE STATEMENT 
      *  TO TEST FOR FIRST RECORD, DEPT CODE, CLASS CODE.  
      *   REMEMBER ORDER MATTERS
      **********************
   
         EVALUATE TRUE
           WHEN FIRST-RECORD = 'YES'
              MOVE 'NO' TO FIRST-RECORD
              MOVE SR-DEPT-CODE TO DEPT-HOLD
              MOVE SR-CLASS-CODE TO CLASS-HOLD
              PERFORM 500-PRINT-DEPT-HEADER
              PERFORM 600-PRINT-CLASS-HEADER

           WHEN SR-DEPT-CODE NOT= DEPT-HOLD
              PERFORM 900-DEPT-BREAK
              PERFORM 300-REPORT-HEADER
              PERFORM 500-PRINT-DEPT-HEADER
              PERFORM 600-PRINT-CLASS-HEADER

           WHEN SR-CLASS-CODE NOT = CLASS-HOLD
              PERFORM 1000-CLASS-BREAK
              PERFORM 600-PRINT-CLASS-HEADER
         
          END-EVALUATE
















      ***********************

           MOVE SR-NAME TO DL-NAME
      * WRITE THE CODE TO TRAVERSE THE ARRAY AND ADD PROCESS THE
      * INCOMING TEST SCORES



          PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
            MOVE SR-TEST-IN(SUB) TO TEST-OUT(SUB)
            ADD SR-TEST-IN(SUB) TO DF-TEST-TOTAL
           
          END-PERFORM






           DIVIDE DF-TEST-TOTAL BY 4
                  GIVING DF-TEST-AVERAGE ROUNDED 

           ADD 1 TO CF-STUDENT-COUNT
                    DF-STUDENT-COUNT                                

          IF DF-TEST-AVERAGE > 89
                   MOVE 'A' TO DL-GRADE
          ELSE

      * After seeing how to do this with an eval
      * this if block disturbs me, LOL.

             IF DF-TEST-AVERAGE >= 80 AND DF-TEST-AVERAGE <= 89
                   MOVE 'B' TO DL-GRADE
             ELSE

                IF DF-TEST-AVERAGE >= 70 AND DF-TEST-AVERAGE <= 79
                   MOVE 'C' TO DL-GRADE
                ELSE

                   IF DF-TEST-AVERAGE >= 60 AND DF-TEST-AVERAGE <= 69
                       MOVE 'D' TO DL-GRADE
                   ELSE

                       IF DF-TEST-AVERAGE < 60
                          MOVE 'F' TO DL-GRADE
                       END-IF
                    END-IF
                 END-IF
               END-IF
            END-IF


           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 800-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE ZEROS TO DF-TEST-AVERAGE
           MOVE ZEROS TO DF-TEST-TOTAL

           .

       800-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
           .

      *
       900-DEPT-BREAK.

      *  WRITE THE CODE THAT HANDLES THE PRINTING OF THE 
      *  DEPARTMENT TOTAL LINE


         PERFORM 1000-CLASS-BREAK

         MOVE DEPT-HOLD TO DGL-DEPT-CODE
         MOVE DF-STUDENT-COUNT TO DGL-DEPT-TOTAL
         MOVE DEPART-GROUP-LINE TO REPORT-LINE
         MOVE 2 TO PROPER-SPACING

         PERFORM 800-WRITE-A-LINE

         MOVE ZEROS TO DF-STUDENT-COUNT
         MOVE ZEROS TO DGL-DEPT-TOTAL

         MOVE SR-DEPT-CODE TO DEPT-HOLD




         .


       1000-CLASS-BREAK.

         MOVE CLASS-HOLD TO CGL-CLASS-CODE
         MOVE CF-STUDENT-COUNT TO CGL-CLASS-TOTAL
         MOVE CLASS-GROUP-LINE TO REPORT-LINE
         MOVE 2 TO PROPER-SPACING

         PERFORM 800-WRITE-A-LINE

         MOVE ZEROS TO CF-STUDENT-COUNT
         MOVE ZEROS TO CGL-CLASS-TOTAL

         MOVE SR-CLASS-CODE TO CLASS-HOLD

         .

       1100-END-OF-JOB-ROUTINE.
      * WRITE THE CODE TO FORCE PRINT THAT LAST CLASS TOTAL LINE 
      * AND THE LAST DEPARTMENT TOTAL LINE

        PERFORM 900-DEPT-BREAK

        .


       1200-FINAL-ROUTINE.
           CLOSE STUDENT-FILE
                 STUDENT-REPORT-FILE

            STOP RUN
            .
