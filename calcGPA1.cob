       identification division.
       program-id. calcGPA1.
       data division.
       file section.
       working-storage section.
       01 user-input.
           05 letter-input    pic X.
           05 credits         pic 99.
           05 grade-points    pic 99.
       01 work-fields.
           05 first-run       pic X        value "Y".
           05 gpa             pic 99v99.
       01 presentation.
           05 gpa-edited      pic Z9.99.
       procedure division.
       000-main-procedure.
           perform 100-prompt-execution
             until letter-input = "N".
           display "End of session.".
           stop run.
       100-prompt-execution.
           display "--------------------------------------------------".
           if first-run = "Y"
             display "Do you want to calculate a GPA (Y/N)?"
           else
             display "Do you want to calculate another GPA (Y/N)?"
           end-if.
           accept letter-input.
           if letter-input = "Y"
             perform 200-calculate-gpa.
       200-calculate-gpa.
           display "--------------------------------------------------".
           display "Enter the number of grade points for the semester.".
           accept grade-points.
           display "Enter the number of credits taken.".
           accept credits.
      *    # calculate and present gpa
           compute gpa = grade-points / credits.
           move gpa to gpa-edited.
           display "Your grade point average is " gpa-edited.
      *    # set first-run switch to false
           move "N" to first-run.