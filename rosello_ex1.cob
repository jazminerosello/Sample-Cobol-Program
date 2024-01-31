        *>ROSELLO, MA.JAZMINE P.
        *>2021-09665
        *>CMSC 124-ST7L
        *> One reference I used for this exer is the switch statement or switch case which I used for the menu.
        *> I prefer using switch statement for this kind of program because it is easy to catch error like invalid input
        *> link of reference: http://www.pgrocer.net/Cis12/coblcase.html
        IDENTIFICATION DIVISION.
        PROGRAM-ID. sample.

        ENVIRONMENT DIVISION.

        DATA DIVISION. *> THIS IS WHERE VARIABLE AND ARRAY DECLARATION HAPPENS
            WORKING-STORAGE SECTION.
            77 CHOICE PIC 9(2).
            77 Current PIC 9. *>this is to point the current number in the outer loop that will be used for getting the greatest ratio
            *>for counter in loops
            77 COUNTER PIC 9 VALUE 1.
            77 COUNTER2 PIC 9 VALUE 1.
            77 COUNTER3 PIC 9 VALUE 1.
            77 diff PIC 9 VALUE 1.
            77 GreatestRatio PIC 9(2)V9(2) VALUE 0. *>storage ng greatest ratio
            77 temp PIC 9 VALUE 0. *> temporary storage nung greatest ratio ng current na nasa loop para icompare sa value ng variable na greatest ratio
            *>for arrays nung 5 positive one-digit
            01 NUM OCCURS 5 TIMES PIC 9(1).
            01 OP OCCURS 2 TIMES PIC 9(1). *>array to store 'yung ordered pair na may greatest ratio

        PROCEDURE DIVISION .
        MENUDISPLAY. *>to display menu
            DISPLAY " ".
            DISPLAY "===========>> MENU <<===========".
            DISPLAY "[1] Fill Array ".
            DISPLAY "[2] Print Array ".
            DISPLAY "[3] Ordered Pair with Greatest Ratio".
            DISPLAY "[4] Exit"
            DISPLAY "Choice : " WITH NO ADVANCING.
            ACCEPT CHOICE.
            PERFORM Check_choice.

        Check_choice.
             EVALUATE CHOICE *>using switch statement to call functions when choice is 1 etc.
                WHEN 1
                    PERFORM Fill_Array
                WHEN 2
                    PERFORM Print
                WHEN 3
                    PERFORM Ordered_Pair
                WHEN 4
                    STOP RUN *>to terminate the program
                WHEN OTHER
                    PERFORM Error_message
            END-EVALUATE.

            *> it can also perform using for loop
            *>IF CHOICE < 0 OR CHOICE > 9
                *>PERFORM Error_message
            *>ELSE IF CHOICE = 1
                *>PERFORM Fill_Array
            *>ELSE IF CHOICE = 2
                *>PERFORM Print
            *>ELSE IF CHOICE = 3
                *>PERFORM Ordered_Pair
            *>ELSE IF CHOICE = 4
                *>STOP RUN
            *>ELSE    
                *>PERFORM Error_message
            *>END-IF.

        *>to display error message when choice input of user is not of the choices in the menu
        Error_message.
            DISPLAY " ".
            DISPLAY "INVALID INPUT!"
            PERFORM MENUDISPLAY.

        *>to repeatedly ask users for 5 times for digits to be stored in the array
        Fill_Array.
            DISPLAY " ".
            DISPLAY "FILL ARRAY (Input 1 digit per element) ".
            PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 5 *>using a for loop 
                DISPLAY "Enter a positive one-digit: " WITH NO ADVANCING
                ACCEPT NUM(COUNTER) *>accepts the input of user and store it in array with index of the ith of loop
            END-PERFORM.
            PERFORM MENUDISPLAY. *>call again the menudisplay

        *>to print the elements in the array
        Print.
            DISPLAY " ".
            DISPLAY "PRINT ARRAY".

            PERFORM VARYING COUNTER2 FROM 1 BY 1 UNTIL COUNTER2 > 5 *>use for loop to iterate accessing the elements in array
                DISPLAY NUM(COUNTER2), " " WITH NO ADVANCING *>display or print
            END-PERFORM.
            DISPLAY " ".
            PERFORM MENUDISPLAY. *>call again the menudisplay

        *>to get the ordered pair with the greatest ration
        *>
        Ordered_Pair.
            SET GreatestRatio TO 0
            *> outer loop to iterate to the first element
            PERFORM VARYING COUNTER3 FROM 1 BY 1 UNTIL COUNTER3 > 5 
                *> move the num with index counter3 to current
                MOVE NUM(COUNTER3) TO Current
                *> inner loop to get the quotient of the current and it's next elements to it  
                PERFORM VARYING diff FROM 1 BY 1 UNTIL diff > 5 
                    *> store to temp the quotient then compare temp to greatestRatio variable to know if temp is greater than 
                    COMPUTE temp = Current / NUM(diff) 
                    *> if temp is greater than current value of GreatestRatio, change the Greatest Ratio value to what temp holds
                    IF GreatestRatio < temp
                        MOVE temp TO GreatestRatio
                        *> to get the ordered pair, store the current value variable to the array also the current value of ith element
                        MOVE Current TO OP(1) 
                        MOVE NUM(diff) TO OP(2)
                    END-IF
                END-PERFORM
            END-PERFORM.
            DISPLAY " ".
            DISPLAY "ORDERED PAIR WITH GREATEST RATIO". *>display the ratio
            DISPLAY "(", OP(1),",", OP(2), ")".
            PERFORM MENUDISPLAY.
