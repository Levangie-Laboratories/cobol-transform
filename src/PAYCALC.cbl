      *****************************************************************
      * Program: PAYCALC.cbl                                          *
      *                                                               *
      * Purpose: Main payroll calculation program for the Payroll     *
      *          Processing System. This program reads employee       *
      *          records and payroll data, calculates gross pay,      *
      *          calls tax and deduction calculation modules,         *
      *          calculates net pay, and generates pay stubs and      *
      *          reports.                                             *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYCALC.
       AUTHOR. COBOL PAYROLL SYSTEM.
       DATE-WRITTEN. 2025-04-10.
       DATE-COMPILED. 2025-04-10.
      
      *****************************************************************
      * Program Description:                                           *
      *                                                                *
      * PAYCALC is the main program of the Payroll Processing System.  *
      * It coordinates the entire payroll process by:                  *
      *                                                                *
      * 1. Reading employee master records                             *
      * 2. Reading payroll data for the current pay period             *
      * 3. Calculating gross pay based on hours worked and pay rates   *
      * 4. Calling the tax calculation module to determine taxes       *
      * 5. Calling the deductions module to process employee           *
      *    deductions                                                  *
      * 6. Calculating net pay                                         *
      * 7. Generating pay stubs via the pay stub module                *
      * 8. Creating payroll reports                                    *
      * 9. Updating employee master records with new YTD totals        *
      * 10. Writing payroll results to output files                    *
      *                                                                *
      * The program handles multiple employees in a single run and      *
      * processes an entire pay period's worth of data.                *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPFILE'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EMP-ID
           FILE STATUS IS EMP-FILE-STATUS.
           
           SELECT PAYROLL-DATA-FILE ASSIGN TO 'PAYDATA'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS PAY-FILE-STATUS.
           
           SELECT TAX-RATES-FILE ASSIGN TO 'TAXRATES'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS TAX-FILE-STATUS.
           
           SELECT DEDUCTION-FILE ASSIGN TO 'DEDUCFILE'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS DEDUCT-FILE-STATUS.
           
           SELECT PAYSTUB-FILE ASSIGN TO 'PAYSTUBS'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS STUB-FILE-STATUS.
           
           SELECT PAYROLL-REPORT-FILE ASSIGN TO 'PAYRPT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REPORT-FILE-STATUS.
           
           SELECT ERROR-LOG-FILE ASSIGN TO 'ERRORLOG'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ERROR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
           01  EMPLOYEE-RECORD-FILE.
               COPY EMPFILE.
               
       FD  PAYROLL-DATA-FILE.
           01  PAYROLL-DATA-RECORD-FILE.
               COPY PAYDATA.
               
       FD  TAX-RATES-FILE.
           01  TAX-RATES-RECORD-FILE.
               COPY TAXRATES.
               
       FD  DEDUCTION-FILE.
           01  DEDUCTION-RECORD-FILE.
               COPY DEDUCFILE.
               
       FD  PAYSTUB-FILE.
           01  PAYSTUB-RECORD               PIC X(132).
           
       FD  PAYROLL-REPORT-FILE.
           01  REPORT-RECORD                PIC X(132).
           
       FD  ERROR-LOG-FILE.
           01  ERROR-LOG-RECORD            PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  EMP-FILE-STATUS            PIC X(2).
               88  EMP-FILE-SUCCESS       VALUE '00'.
               88  EMP-FILE-EOF           VALUE '10'.
           05  PAY-FILE-STATUS            PIC X(2).
               88  PAY-FILE-SUCCESS       VALUE '00'.
               88  PAY-FILE-EOF           VALUE '10'.
           05  TAX-FILE-STATUS            PIC X(2).
               88  TAX-FILE-SUCCESS       VALUE '00'.
               88  TAX-FILE-EOF           VALUE '10'.
           05  DEDUCT-FILE-STATUS         PIC X(2).
               88  DEDUCT-FILE-SUCCESS    VALUE '00'.
               88  DEDUCT-FILE-EOF        VALUE '10'.
           05  STUB-FILE-STATUS           PIC X(2).
               88  STUB-FILE-SUCCESS      VALUE '00'.
           05  REPORT-FILE-STATUS         PIC X(2).
               88  REPORT-FILE-SUCCESS    VALUE '00'.
           05  ERROR-FILE-STATUS          PIC X(2).
               88  ERROR-FILE-SUCCESS     VALUE '00'.
       
       01  WS-EMPLOYEE-RECORD.
           COPY EMPFILE.
       
       01  WS-PAYROLL-DATA.
           COPY PAYDATA.
           
       01  WS-TAX-RATES.
           COPY TAXRATES.
           
       01  WS-DEDUCTION-TYPE.
           COPY DEDUCFILE.
           
       01  WS-CALCULATION-WORK-AREA.
           05  WS-GROSS-PAY                PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-REGULAR-PAY              PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-OVERTIME-PAY             PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-OTHER-PAY                PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-TAXES              PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-FEDERAL-TAX              PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-STATE-TAX                PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-LOCAL-TAX                PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-SOCIAL-SEC-TAX           PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-MEDICARE-TAX             PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-DEDUCTIONS         PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-NET-PAY                  PIC 9(7)V99 COMP-3 VALUE ZEROS.
           
       01  WS-TAX-CALCULATION-AREA.
           05  WS-TAX-CALC-GROSS           PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TAX-CALC-YTD-GROSS       PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-TAX-FILING-STATUS        PIC X.
           05  WS-TAX-STATE-CODE           PIC X(2).
           05  WS-TAX-ALLOWANCES           PIC 9(2) COMP-3 VALUE ZEROS.
           05  WS-TAX-ADDITIONAL           PIC 9(5)V99 COMP-3 VALUE ZEROS.
           
       01  WS-DEDUCTION-CALCULATION-AREA.
           05  WS-DEDUCT-CALC-GROSS        PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-DEDUCT-CALC-TYPE         PIC X(3).
           05  WS-DEDUCT-CALC-AMOUNT       PIC 9(7)V99 COMP-3 VALUE ZEROS.
           
       01  WS-COUNTERS.
           05  WS-EMPLOYEE-COUNT           PIC 9(5) COMP-3 VALUE ZEROS.
           05  WS-PAYROLL-REC-COUNT        PIC 9(5) COMP-3 VALUE ZEROS.
           05  WS-ERROR-COUNT              PIC 9(5) COMP-3 VALUE ZEROS.
           
       01  WS-PAY-PERIOD-TOTALS.
           05  WS-TOTAL-GROSS-PAY          PIC 9(9)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-TAXES              PIC 9(9)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-DEDUCTIONS         PIC 9(9)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-NET-PAY            PIC 9(9)V99 COMP-3 VALUE ZEROS.
           
       01  WS-DATETIME.
           05  WS-DATE.
               10  WS-YEAR                 PIC 9(4).
               10  WS-MONTH                PIC 9(2).
               10  WS-DAY                  PIC 9(2).
           05  WS-TIME.
               10  WS-HOUR                 PIC 9(2).
               10  WS-MINUTE               PIC 9(2).
               10  WS-SECOND               PIC 9(2).
       
       01  WS-ERROR-MESSAGE.
           05  WS-ERROR-TEXT               PIC X(100).
           05  FILLER                      PIC X(32).
       
       01  WS-FLAGS.
           05  WS-END-OF-FILE-FLAG         PIC X VALUE 'N'.
               88  END-OF-FILE             VALUE 'Y'.
           05  WS-PROCESS-FLAG             PIC X VALUE 'Y'.
               88  PROCESS-EMPLOYEE        VALUE 'Y'.
           05  WS-ERROR-FLAG               PIC X VALUE 'N'.
               88  ERROR-FOUND             VALUE 'Y'.
           
       PROCEDURE DIVISION.
       000-MAIN-PROCESS.
      *****************************************************************
      * Main process coordinates the overall payroll processing flow   *
      *****************************************************************
           PERFORM 100-INITIALIZATION
           
           PERFORM UNTIL END-OF-FILE
               PERFORM 200-READ-PAYROLL-DATA
               IF NOT END-OF-FILE
                   PERFORM 300-PROCESS-EMPLOYEE-PAYROLL
               END-IF
           END-PERFORM
           
           PERFORM 900-WRAP-UP
           
           STOP RUN
           .
           
       100-INITIALIZATION.
      *****************************************************************
      * Initialize the program by opening files and reading reference  *
      * data like tax rates and deduction types                        *
      *****************************************************************
           DISPLAY 'PAYROLL CALCULATION PROGRAM - INITIALIZATION'
           
           OPEN INPUT EMPLOYEE-FILE
           IF NOT EMP-FILE-SUCCESS
               DISPLAY 'ERROR OPENING EMPLOYEE FILE: ' EMP-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           OPEN INPUT PAYROLL-DATA-FILE
           IF NOT PAY-FILE-SUCCESS
               DISPLAY 'ERROR OPENING PAYROLL DATA FILE: ' PAY-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           OPEN INPUT TAX-RATES-FILE
           IF NOT TAX-FILE-SUCCESS
               DISPLAY 'ERROR OPENING TAX RATES FILE: ' TAX-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           OPEN INPUT DEDUCTION-FILE
           IF NOT DEDUCT-FILE-SUCCESS
               DISPLAY 'ERROR OPENING DEDUCTION FILE: ' DEDUCT-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           OPEN OUTPUT PAYSTUB-FILE
           IF NOT STUB-FILE-SUCCESS
               DISPLAY 'ERROR OPENING PAYSTUB FILE: ' STUB-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           OPEN OUTPUT PAYROLL-REPORT-FILE
           IF NOT REPORT-FILE-SUCCESS
               DISPLAY 'ERROR OPENING REPORT FILE: ' REPORT-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           OPEN OUTPUT ERROR-LOG-FILE
           IF NOT ERROR-FILE-SUCCESS
               DISPLAY 'ERROR OPENING ERROR LOG FILE: ' ERROR-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           PERFORM 110-LOAD-TAX-TABLES
           PERFORM 120-LOAD-DEDUCTION-TABLES
           
           MOVE 'N' TO WS-END-OF-FILE-FLAG
           MOVE ZEROS TO WS-EMPLOYEE-COUNT
           MOVE ZEROS TO WS-PAYROLL-REC-COUNT
           MOVE ZEROS TO WS-ERROR-COUNT
           
           DISPLAY 'INITIALIZATION COMPLETE - BEGINNING PAYROLL PROCESSING'
           .
           
       110-LOAD-TAX-TABLES.
      *****************************************************************
      * Load tax rates and brackets from the tax rates file           *
      *****************************************************************
           DISPLAY 'LOADING TAX TABLES...'
           
           READ TAX-RATES-FILE INTO WS-TAX-RATES
           IF NOT TAX-FILE-SUCCESS
               DISPLAY 'ERROR READING TAX RATES: ' TAX-FILE-STATUS
               PERFORM 950-ABNORMAL-TERMINATION
           END-IF
           
           DISPLAY 'TAX TABLES LOADED SUCCESSFULLY FOR YEAR: '
                   TAX-YEAR IN WS-TAX-RATES
           .
           
       120-LOAD-DEDUCTION-TABLES.
      *****************************************************************
      * Load deduction types and rules from the deduction file        *
      *****************************************************************
           DISPLAY 'LOADING DEDUCTION TABLES...'
           
           PERFORM UNTIL DEDUCT-FILE-EOF
               READ DEDUCTION-FILE INTO WS-DEDUCTION-TYPE
                   AT END
                       SET DEDUCT-FILE-EOF TO TRUE
                   NOT AT END
                       CONTINUE
               END-READ
           END-PERFORM
           
           DISPLAY 'DEDUCTION TABLES LOADED SUCCESSFULLY'
           CLOSE DEDUCTION-FILE
           .
           
       200-READ-PAYROLL-DATA.
      *****************************************************************
      * Read the next payroll data record from the input file         *
      *****************************************************************
           READ PAYROLL-DATA-FILE INTO WS-PAYROLL-DATA
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE-FLAG
               NOT AT END
                   ADD 1 TO WS-PAYROLL-REC-COUNT
                   MOVE 'Y' TO WS-PROCESS-FLAG
           END-READ
           .
           
       300-PROCESS-EMPLOYEE-PAYROLL.
      *****************************************************************
      * Process payroll for a single employee:                        *
      * 1. Read employee record                                       *
      * 2. Calculate gross pay                                        *
      * 3. Calculate taxes                                            *
      * 4. Calculate deductions                                       *
      * 5. Calculate net pay                                          *
      * 6. Generate pay stub                                          *
      * 7. Update employee totals                                     *
      *****************************************************************
           PERFORM 310-READ-EMPLOYEE-RECORD
           
           IF PROCESS-EMPLOYEE
               DISPLAY 'PROCESSING EMPLOYEE: ' EMP-ID IN WS-EMPLOYEE-RECORD
                       ' - ' EMP-LAST-NAME IN WS-EMPLOYEE-RECORD
                       ', ' EMP-FIRST-NAME IN WS-EMPLOYEE-RECORD
               
               PERFORM 320-CALCULATE-GROSS-PAY
               PERFORM 330-CALCULATE-TAXES
               PERFORM 340-CALCULATE-DEDUCTIONS
               PERFORM 350-CALCULATE-NET-PAY
               PERFORM 360-GENERATE-PAY-STUB
               PERFORM 370-UPDATE-EMPLOYEE-RECORD
               
               ADD 1 TO WS-EMPLOYEE-COUNT
               ADD WS-GROSS-PAY TO WS-TOTAL-GROSS-PAY
               ADD WS-TOTAL-TAXES TO WS-TOTAL-TAXES
               ADD WS-TOTAL-DEDUCTIONS TO WS-TOTAL-DEDUCTIONS
               ADD WS-NET-PAY TO WS-TOTAL-NET-PAY
           END-IF
           .
           
       310-READ-EMPLOYEE-RECORD.
      *****************************************************************
      * Read the employee record for the current payroll data record  *
      *****************************************************************
           MOVE PAY-EMPLOYEE-ID IN WS-PAYROLL-DATA TO EMP-ID IN WS-EMPLOYEE-RECORD
           
           READ EMPLOYEE-FILE INTO WS-EMPLOYEE-RECORD
               KEY IS EMP-ID IN WS-EMPLOYEE-RECORD
               INVALID KEY
                   MOVE 'N' TO WS-PROCESS-FLAG
                   STRING 'EMPLOYEE ID NOT FOUND: ' PAY-EMPLOYEE-ID IN WS-PAYROLL-DATA
                          DELIMITED BY SIZE
                          INTO WS-ERROR-TEXT
                   PERFORM 800-LOG-ERROR
           END-READ
           
           IF EMP-ACTIVE IN WS-EMPLOYEE-RECORD
               CONTINUE
           ELSE
               MOVE 'N' TO WS-PROCESS-FLAG
               STRING 'EMPLOYEE NOT ACTIVE: ' PAY-EMPLOYEE-ID IN WS-PAYROLL-DATA
                      ' STATUS: ' EMP-STATUS IN WS-EMPLOYEE-RECORD
                      DELIMITED BY SIZE
                      INTO WS-ERROR-TEXT
               PERFORM 800-LOG-ERROR
           END-IF
           .
           
       320-CALCULATE-GROSS-PAY.
      *****************************************************************
      * Calculate gross pay based on hours worked and pay rates       *
      *****************************************************************
           INITIALIZE WS-GROSS-PAY
                      WS-REGULAR-PAY
                      WS-OVERTIME-PAY
                      WS-OTHER-PAY
           
           IF EMP-HOURLY IN WS-EMPLOYEE-RECORD
               COMPUTE WS-REGULAR-PAY = 
                   PAY-REGULAR-HOURS IN WS-PAYROLL-DATA * 
                   EMP-HOURLY-RATE IN WS-EMPLOYEE-RECORD
               
               COMPUTE WS-OVERTIME-PAY = 
                   PAY-OVERTIME-HOURS IN WS-PAYROLL-DATA * 
                   EMP-HOURLY-RATE IN WS-EMPLOYEE-RECORD * 
                   EMP-OVERTIME-RATE IN WS-EMPLOYEE-RECORD
           ELSE
               IF EMP-SALARY IN WS-EMPLOYEE-RECORD
                   IF EMP-MONTHLY IN WS-EMPLOYEE-RECORD
                       MOVE EMP-SALARY-AMOUNT IN WS-EMPLOYEE-RECORD TO WS-REGULAR-PAY
                   ELSE
                       IF EMP-BIWEEKLY IN WS-EMPLOYEE-RECORD
                           COMPUTE WS-REGULAR-PAY = 
                               EMP-SALARY-AMOUNT IN WS-EMPLOYEE-RECORD / 2
                       ELSE
                           IF EMP-WEEKLY IN WS-EMPLOYEE-RECORD
                               COMPUTE WS-REGULAR-PAY = 
                                   EMP-SALARY-AMOUNT IN WS-EMPLOYEE-RECORD / 4
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           
           MOVE PAY-BONUS-AMOUNT IN WS-PAYROLL-DATA TO WS-OTHER-PAY
           ADD PAY-COMMISSION-AMOUNT IN WS-PAYROLL-DATA TO WS-OTHER-PAY
           
           COMPUTE WS-GROSS-PAY = WS-REGULAR-PAY + WS-OVERTIME-PAY + WS-OTHER-PAY
           
           DISPLAY 'GROSS PAY: ' WS-GROSS-PAY
                   ' (REG: ' WS-REGULAR-PAY
                   ' OT: ' WS-OVERTIME-PAY
                   ' OTHER: ' WS-OTHER-PAY ')'
           .
           
       330-CALCULATE-TAXES.
      *****************************************************************
      * Calculate taxes by calling the tax calculation module          *
      *****************************************************************
           INITIALIZE WS-TOTAL-TAXES
                      WS-FEDERAL-TAX
                      WS-STATE-TAX
                      WS-LOCAL-TAX
                      WS-SOCIAL-SEC-TAX
                      WS-MEDICARE-TAX
                      
           MOVE WS-GROSS-PAY TO WS-TAX-CALC-GROSS
           MOVE EMP-YTD-GROSS IN WS-EMPLOYEE-RECORD TO WS-TAX-CALC-YTD-GROSS
           MOVE EMP-FEDERAL-FILING-STATUS IN WS-EMPLOYEE-RECORD TO WS-TAX-FILING-STATUS
           MOVE EMP-STATE IN WS-EMPLOYEE-RECORD TO WS-TAX-STATE-CODE
           MOVE EMP-FEDERAL-ALLOWANCES IN WS-EMPLOYEE-RECORD TO WS-TAX-ALLOWANCES
           MOVE EMP-ADDITIONAL-FIT IN WS-EMPLOYEE-RECORD TO WS-TAX-ADDITIONAL
           
           CALL 'TAXCALC' USING WS-TAX-CALC-GROSS
                                WS-TAX-CALC-YTD-GROSS
                                WS-TAX-FILING-STATUS
                                WS-TAX-STATE-CODE
                                WS-TAX-ALLOWANCES
                                WS-TAX-ADDITIONAL
                                WS-TAX-RATES
                                WS-FEDERAL-TAX
                                WS-STATE-TAX
                                WS-LOCAL-TAX
                                WS-SOCIAL-SEC-TAX
                                WS-MEDICARE-TAX
           
           COMPUTE WS-TOTAL-TAXES = 
               WS-FEDERAL-TAX + WS-STATE-TAX + WS-LOCAL-TAX + 
               WS-SOCIAL-SEC-TAX + WS-MEDICARE-TAX
               
           DISPLAY 'TAXES: ' WS-TOTAL-TAXES
                   ' (FED: ' WS-FEDERAL-TAX
                   ' STATE: ' WS-STATE-TAX
                   ' LOCAL: ' WS-LOCAL-TAX
                   ' SS: ' WS-SOCIAL-SEC-TAX
                   ' MED: ' WS-MEDICARE-TAX ')'
           .
           
       340-CALCULATE-DEDUCTIONS.
      *****************************************************************
      * Calculate deductions by calling the deduction calculation     *
      * module                                                        *
      *****************************************************************
           INITIALIZE WS-TOTAL-DEDUCTIONS
           
           MOVE WS-GROSS-PAY TO WS-DEDUCT-CALC-GROSS
           
           CALL 'DEDCALC' USING WS-DEDUCT-CALC-GROSS
                                WS-EMPLOYEE-RECORD
                                WS-TOTAL-DEDUCTIONS
           
           DISPLAY 'DEDUCTIONS: ' WS-TOTAL-DEDUCTIONS
           .
           
       350-CALCULATE-NET-PAY.
      *****************************************************************
      * Calculate net pay: gross pay - taxes - deductions             *
      *****************************************************************
           COMPUTE WS-NET-PAY = 
               WS-GROSS-PAY - WS-TOTAL-TAXES - WS-TOTAL-DEDUCTIONS
               
           IF WS-NET-PAY < 0
               MOVE 0 TO WS-NET-PAY
               STRING 'NEGATIVE NET PAY CALCULATED FOR EMPLOYEE: '
                      PAY-EMPLOYEE-ID IN WS-PAYROLL-DATA
                      ' - ADJUSTED TO ZERO'
                      DELIMITED BY SIZE
                      INTO WS-ERROR-TEXT
               PERFORM 800-LOG-ERROR
           END-IF
           
           DISPLAY 'NET PAY: ' WS-NET-PAY
           .
           
       360-GENERATE-PAY-STUB.
      *****************************************************************
      * Generate pay stub output by calling the pay stub module       *
      *****************************************************************
           CALL 'PAYSTUB' USING WS-EMPLOYEE-RECORD
                                WS-PAYROLL-DATA
                                WS-GROSS-PAY
                                WS-REGULAR-PAY
                                WS-OVERTIME-PAY
                                WS-OTHER-PAY
                                WS-FEDERAL-TAX
                                WS-STATE-TAX
                                WS-LOCAL-TAX
                                WS-SOCIAL-SEC-TAX
                                WS-MEDICARE-TAX
                                WS-TOTAL-DEDUCTIONS
                                WS-NET-PAY
                                PAYSTUB-RECORD
           
           WRITE PAYSTUB-RECORD
           IF NOT STUB-FILE-SUCCESS
               STRING 'ERROR WRITING PAY STUB FOR EMPLOYEE: '
                      PAY-EMPLOYEE-ID IN WS-PAYROLL-DATA
                      ' - STATUS: ' STUB-FILE-STATUS
                      DELIMITED BY SIZE
                      INTO WS-ERROR-TEXT
               PERFORM 800-LOG-ERROR
           END-IF
           .
           
       370-UPDATE-EMPLOYEE-RECORD.
      *****************************************************************
      * Update employee YTD totals and last pay date                  *
      *****************************************************************
           ADD WS-GROSS-PAY TO EMP-YTD-GROSS IN WS-EMPLOYEE-RECORD
           ADD WS-FEDERAL-TAX TO EMP-YTD-FEDERAL-TAX IN WS-EMPLOYEE-RECORD
           ADD WS-STATE-TAX TO EMP-YTD-STATE-TAX IN WS-EMPLOYEE-RECORD
           ADD WS-LOCAL-TAX TO EMP-YTD-LOCAL-TAX IN WS-EMPLOYEE-RECORD
           ADD WS-SOCIAL-SEC-TAX TO EMP-YTD-SOCIAL-SEC IN WS-EMPLOYEE-RECORD
           ADD WS-MEDICARE-TAX TO EMP-YTD-MEDICARE IN WS-EMPLOYEE-RECORD
           
           ADD WS-TOTAL-DEDUCTIONS TO EMP-YTD-OTHER-DEDUCT IN WS-EMPLOYEE-RECORD
           ADD WS-NET-PAY TO EMP-YTD-NET-PAY IN WS-EMPLOYEE-RECORD
           
           MOVE PAY-PERIOD-END-DATE IN WS-PAYROLL-DATA TO EMP-LAST-PAY-DATE IN WS-EMPLOYEE-RECORD
           
           REWRITE EMPLOYEE-RECORD-FILE FROM WS-EMPLOYEE-RECORD
           IF NOT EMP-FILE-SUCCESS
               STRING 'ERROR UPDATING EMPLOYEE RECORD: '
                      PAY-EMPLOYEE-ID IN WS-PAYROLL-DATA
                      ' - STATUS: ' EMP-FILE-STATUS
                      DELIMITED BY SIZE
                      INTO WS-ERROR-TEXT
               PERFORM 800-LOG-ERROR
           END-IF
           .
           
       800-LOG-ERROR.
      *****************************************************************
      * Log error messages to the error log file                      *
      *****************************************************************
           MOVE 'Y' TO WS-ERROR-FLAG
           ADD 1 TO WS-ERROR-COUNT
           
           WRITE ERROR-LOG-RECORD FROM WS-ERROR-MESSAGE
           IF NOT ERROR-FILE-SUCCESS
               DISPLAY 'ERROR WRITING TO ERROR LOG: ' ERROR-FILE-STATUS
           END-IF
           
           DISPLAY 'ERROR: ' WS-ERROR-TEXT
           .
           
       900-WRAP-UP.
      *****************************************************************
      * Wrap up processing, generate summary report, and close files  *
      *****************************************************************
           DISPLAY 'PAYROLL PROCESSING COMPLETE'
           DISPLAY 'EMPLOYEES PROCESSED: ' WS-EMPLOYEE-COUNT
           DISPLAY 'PAYROLL RECORDS READ: ' WS-PAYROLL-REC-COUNT
           DISPLAY 'ERRORS ENCOUNTERED: ' WS-ERROR-COUNT
           DISPLAY 'TOTAL GROSS PAY: ' WS-TOTAL-GROSS-PAY
           DISPLAY 'TOTAL TAXES: ' WS-TOTAL-TAXES
           DISPLAY 'TOTAL DEDUCTIONS: ' WS-TOTAL-DEDUCTIONS
           DISPLAY 'TOTAL NET PAY: ' WS-TOTAL-NET-PAY
           
           PERFORM 910-GENERATE-SUMMARY-REPORT
           
           CLOSE EMPLOYEE-FILE
                 PAYROLL-DATA-FILE
                 TAX-RATES-FILE
                 PAYSTUB-FILE
                 PAYROLL-REPORT-FILE
                 ERROR-LOG-FILE
           .
           
       910-GENERATE-SUMMARY-REPORT.
      *****************************************************************
      * Generate a summary report of the payroll processing           *
      *****************************************************************
           MOVE SPACES TO REPORT-RECORD
           STRING '================================================='
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING '          PAYROLL PROCESSING SUMMARY REPORT'
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING '================================================='
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'EMPLOYEES PROCESSED: ' WS-EMPLOYEE-COUNT
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'PAYROLL RECORDS READ: ' WS-PAYROLL-REC-COUNT
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'ERRORS ENCOUNTERED: ' WS-ERROR-COUNT
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'TOTAL GROSS PAY: $' WS-TOTAL-GROSS-PAY
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'TOTAL TAXES: $' WS-TOTAL-TAXES
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'TOTAL DEDUCTIONS: $' WS-TOTAL-DEDUCTIONS
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING 'TOTAL NET PAY: $' WS-TOTAL-NET-PAY
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           STRING '================================================='
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           .
           
       950-ABNORMAL-TERMINATION.
      *****************************************************************
      * Handle abnormal program termination due to errors             *
      *****************************************************************
           DISPLAY 'PAYROLL PROCESSING TERMINATED ABNORMALLY'
           
           IF EMP-FILE-SUCCESS
               CLOSE EMPLOYEE-FILE
           END-IF
           
           IF PAY-FILE-SUCCESS
               CLOSE PAYROLL-DATA-FILE
           END-IF
           
           IF TAX-FILE-SUCCESS
               CLOSE TAX-RATES-FILE
           END-IF
           
           IF DEDUCT-FILE-SUCCESS
               CLOSE DEDUCTION-FILE
           END-IF
           
           IF STUB-FILE-SUCCESS
               CLOSE PAYSTUB-FILE
           END-IF
           
           IF REPORT-FILE-SUCCESS
               CLOSE PAYROLL-REPORT-FILE
           END-IF
           
           IF ERROR-FILE-SUCCESS
               CLOSE ERROR-LOG-FILE
           END-IF
           
           STOP RUN
           .