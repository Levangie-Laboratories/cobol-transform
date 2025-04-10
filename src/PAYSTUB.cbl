      *****************************************************************
      * Program: PAYSTUB.cbl                                          *
      *                                                               *
      * Purpose: Pay stub generation module for the Payroll           *
      *          Processing System. This program formats and          *
      *          generates employee pay stubs, displaying earnings,   *
      *          taxes, deductions, and totals in a clear format.     *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYSTUB.
       AUTHOR. COBOL PAYROLL SYSTEM.
       DATE-WRITTEN. 2025-04-10.
       DATE-COMPILED. 2025-04-10.
      
      *****************************************************************
      * Program Description:                                           *
      *                                                                *
      * PAYSTUB is the pay stub generation module for the Payroll      *
      * Processing System. It formats and produces employee pay stubs  *
      * with the following information:                                *
      *                                                                *
      * 1. Company and employee information header                     *
      * 2. Pay period information                                      *
      * 3. Earnings breakdown (regular, overtime, other)               *
      * 4. Tax withholdings (federal, state, local, FICA)              *
      * 5. Deductions (health, dental, vision, retirement, etc.)       *
      * 6. Summary totals (gross pay, total taxes, total deductions,   *
      *    net pay)                                                    *
      * 7. Year-to-date totals                                         *
      *                                                                *
      * The module formats this information into a readable layout     *
      * and returns it to the calling program for output.              *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FORMATTING-WORK-AREA.
           05  WS-LINE                    PIC X(132) VALUE SPACES.
           05  WS-CURRENT-DATE.
               10  WS-YEAR                PIC 9(4).
               10  WS-MONTH               PIC 9(2).
               10  WS-DAY                 PIC 9(2).
           05  WS-FORMATTED-DATE          PIC X(10).
           05  WS-FORMATTED-AMOUNT        PIC Z,ZZZ,ZZ9.99.
           05  WS-TEMP-AMOUNT             PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-YTD-GROSS               PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-YTD-TOTAL-TAXES         PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-YTD-NET                 PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-INDEX                   PIC 9(2)    COMP-3 VALUE ZEROS.
       
       01  WS-CONSTANTS.
           05  WS-COMPANY-NAME            PIC X(30) VALUE 'ACME CORPORATION'.
           05  WS-COMPANY-ADDRESS-1       PIC X(30) VALUE '123 MAIN STREET'.
           05  WS-COMPANY-ADDRESS-2       PIC X(30) VALUE 'ANYTOWN, US 12345'.
           05  WS-COMPANY-PHONE           PIC X(15) VALUE '(555) 555-5555'.
           05  WS-PAGE-TITLE              PIC X(20) VALUE 'EMPLOYEE PAY STUB'.
           05  WS-PAY-STUB-LINE           PIC X(132) VALUE ALL '-'.
           
       LINKAGE SECTION.
      *****************************************************************
      * Input Parameters:                                              *
      * - Employee-Record: Employee master record with personal info   *
      * - Payroll-Data: Pay period data with hours, dates, etc.        *
      * - Gross-Pay: Calculated gross pay                              *
      * - Regular-Pay: Regular hours pay component                     *
      * - Overtime-Pay: Overtime pay component                         *
      * - Other-Pay: Other earnings component                          *
      * - Federal-Tax: Calculated federal income tax                   *
      * - State-Tax: Calculated state income tax                       *
      * - Local-Tax: Calculated local tax                              *
      * - Social-Sec-Tax: Calculated Social Security tax               *
      * - Medicare-Tax: Calculated Medicare tax                         *
      * - Total-Deductions: Calculated total deductions                *
      * - Net-Pay: Calculated net pay                                  *
      *                                                                *
      * Output Parameters:                                             *
      * - Paystub-Record: Formatted pay stub line for output           *
      *****************************************************************
       01  EMPLOYEE-RECORD.
           COPY EMPFILE.
       01  PAYROLL-DATA.
           COPY PAYDATA.
       01  GROSS-PAY                    PIC 9(7)V99 COMP-3.
       01  REGULAR-PAY                  PIC 9(7)V99 COMP-3.
       01  OVERTIME-PAY                 PIC 9(7)V99 COMP-3.
       01  OTHER-PAY                    PIC 9(7)V99 COMP-3.
       01  FEDERAL-TAX                  PIC 9(7)V99 COMP-3.
       01  STATE-TAX                    PIC 9(7)V99 COMP-3.
       01  LOCAL-TAX                    PIC 9(7)V99 COMP-3.
       01  SOCIAL-SEC-TAX               PIC 9(7)V99 COMP-3.
       01  MEDICARE-TAX                 PIC 9(7)V99 COMP-3.
       01  TOTAL-DEDUCTIONS             PIC 9(7)V99 COMP-3.
       01  NET-PAY                      PIC 9(7)V99 COMP-3.
       01  PAYSTUB-RECORD               PIC X(132).
       
       PROCEDURE DIVISION USING EMPLOYEE-RECORD
                                PAYROLL-DATA
                                GROSS-PAY
                                REGULAR-PAY
                                OVERTIME-PAY
                                OTHER-PAY
                                FEDERAL-TAX
                                STATE-TAX
                                LOCAL-TAX
                                SOCIAL-SEC-TAX
                                MEDICARE-TAX
                                TOTAL-DEDUCTIONS
                                NET-PAY
                                PAYSTUB-RECORD.

       000-MAIN-PROCESS.
      *****************************************************************
      * Main pay stub generation process - coordinates all formatting  *
      *****************************************************************
           PERFORM 050-INITIALIZE-PAY-STUB
           PERFORM 100-GENERATE-HEADER
           PERFORM 200-FORMAT-EARNINGS
           PERFORM 300-FORMAT-TAXES
           PERFORM 400-FORMAT-DEDUCTIONS
           PERFORM 500-FORMAT-TOTALS
           PERFORM 600-FORMAT-YTD-SUMMARY
           PERFORM 700-FORMAT-FOOTER
           
           GOBACK
           .
           
       050-INITIALIZE-PAY-STUB.
      *****************************************************************
      * Initialize pay stub values and get current date                *
      *****************************************************************
      **** Calculate YTD values for summary section ****
           COMPUTE WS-YTD-GROSS = 
               EMP-YTD-GROSS IN EMPLOYEE-RECORD + GROSS-PAY
               
           COMPUTE WS-YTD-TOTAL-TAXES =
               EMP-YTD-FEDERAL-TAX IN EMPLOYEE-RECORD + FEDERAL-TAX +
               EMP-YTD-STATE-TAX IN EMPLOYEE-RECORD + STATE-TAX +
               EMP-YTD-LOCAL-TAX IN EMPLOYEE-RECORD + LOCAL-TAX +
               EMP-YTD-SOCIAL-SEC IN EMPLOYEE-RECORD + SOCIAL-SEC-TAX +
               EMP-YTD-MEDICARE IN EMPLOYEE-RECORD + MEDICARE-TAX
               
           COMPUTE WS-YTD-NET =
               EMP-YTD-NET-PAY IN EMPLOYEE-RECORD + NET-PAY
           .
           
       100-GENERATE-HEADER.
      *****************************************************************
      * Generate the pay stub header with company and employee info    *
      *****************************************************************
      **** Company Header Line ****
           MOVE SPACES TO WS-LINE
           MOVE WS-COMPANY-NAME TO WS-LINE(5:30)
           MOVE WS-PAGE-TITLE TO WS-LINE(60:20)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE WS-COMPANY-ADDRESS-1 TO WS-LINE(5:30)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE WS-COMPANY-ADDRESS-2 TO WS-LINE(5:30)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE WS-COMPANY-PHONE TO WS-LINE(5:15)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Employee Information ****
           MOVE SPACES TO WS-LINE
           STRING 'Employee: ' EMP-LAST-NAME IN EMPLOYEE-RECORD
                  ', ' EMP-FIRST-NAME IN EMPLOYEE-RECORD
                  ' ' EMP-MIDDLE-INIT IN EMPLOYEE-RECORD
                  DELIMITED BY SIZE
                  INTO WS-LINE(5:40)
           STRING 'Employee ID: ' EMP-ID IN EMPLOYEE-RECORD
                  DELIMITED BY SIZE
                  INTO WS-LINE(50:20)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           STRING 'Department: ' EMP-DEPARTMENT IN EMPLOYEE-RECORD
                  ' - ' EMP-POSITION IN EMPLOYEE-RECORD
                  DELIMITED BY SIZE
                  INTO WS-LINE(5:50)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Pay Period Information ****
           MOVE SPACES TO WS-LINE
           
           MOVE PAY-START-MONTH IN PAYROLL-DATA TO WS-MONTH
           MOVE PAY-START-DAY IN PAYROLL-DATA TO WS-DAY
           MOVE PAY-START-YEAR IN PAYROLL-DATA TO WS-YEAR
           PERFORM 800-FORMAT-DATE
           MOVE WS-FORMATTED-DATE TO WS-LINE(15:10)
           
           MOVE PAY-END-MONTH IN PAYROLL-DATA TO WS-MONTH
           MOVE PAY-END-DAY IN PAYROLL-DATA TO WS-DAY
           MOVE PAY-END-YEAR IN PAYROLL-DATA TO WS-YEAR
           PERFORM 800-FORMAT-DATE
           MOVE WS-FORMATTED-DATE TO WS-LINE(30:10)
           
           MOVE PAY-CHECK-MONTH IN PAYROLL-DATA TO WS-MONTH
           MOVE PAY-CHECK-DAY IN PAYROLL-DATA TO WS-DAY
           MOVE PAY-CHECK-YEAR IN PAYROLL-DATA TO WS-YEAR
           PERFORM 800-FORMAT-DATE
           
           STRING 'Pay Period: ' DELIMITED BY SIZE
                  INTO WS-LINE(5:12)
           STRING ' to ' DELIMITED BY SIZE
                  INTO WS-LINE(25:4)
           STRING 'Check Date: ' WS-FORMATTED-DATE
                  DELIMITED BY SIZE
                  INTO WS-LINE(50:22)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           .
           
       200-FORMAT-EARNINGS.
      *****************************************************************
      * Format the earnings section of the pay stub                    *
      *****************************************************************
           MOVE SPACES TO WS-LINE
           MOVE 'EARNINGS' TO WS-LINE(5:8)
           MOVE 'HOURS' TO WS-LINE(30:5)
           MOVE 'RATE' TO WS-LINE(40:4)
           MOVE 'CURRENT' TO WS-LINE(55:7)
           MOVE 'YTD' TO WS-LINE(70:3)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Regular Pay' TO WS-LINE(5:11)
           MOVE PAY-REGULAR-HOURS IN PAYROLL-DATA TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(25:11)
           
           IF EMP-HOURLY IN EMPLOYEE-RECORD
               MOVE EMP-HOURLY-RATE IN EMPLOYEE-RECORD TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(40:11)
           END-IF
           
           MOVE REGULAR-PAY TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           IF OVERTIME-PAY > 0
               MOVE SPACES TO WS-LINE
               MOVE 'Overtime Pay' TO WS-LINE(5:12)
               MOVE PAY-OVERTIME-HOURS IN PAYROLL-DATA TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(25:11)
               
               IF EMP-HOURLY IN EMPLOYEE-RECORD
                   COMPUTE WS-TEMP-AMOUNT = 
                       EMP-HOURLY-RATE IN EMPLOYEE-RECORD * 
                       EMP-OVERTIME-RATE IN EMPLOYEE-RECORD
                   MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
                   MOVE WS-FORMATTED-AMOUNT TO WS-LINE(40:11)
               END-IF
               
               MOVE OVERTIME-PAY TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
           IF OTHER-PAY > 0
               MOVE SPACES TO WS-LINE
               MOVE 'Other Earnings' TO WS-LINE(5:14)
               MOVE OTHER-PAY TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
           MOVE SPACES TO WS-LINE
           MOVE 'Gross Pay' TO WS-LINE(5:9)
           MOVE GROSS-PAY TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE WS-YTD-GROSS TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           .
           
       300-FORMAT-TAXES.
      *****************************************************************
      * Format the taxes section of the pay stub                       *
      *****************************************************************
           MOVE SPACES TO WS-LINE
           MOVE 'TAX WITHHOLDINGS' TO WS-LINE(5:16)
           MOVE 'CURRENT' TO WS-LINE(55:7)
           MOVE 'YTD' TO WS-LINE(70:3)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Federal Income Tax' TO WS-LINE(5:18)
           MOVE FEDERAL-TAX TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE EMP-YTD-FEDERAL-TAX IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
           ADD FEDERAL-TAX TO WS-TEMP-AMOUNT
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'State Income Tax' TO WS-LINE(5:16)
           MOVE STATE-TAX TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE EMP-YTD-STATE-TAX IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
           ADD STATE-TAX TO WS-TEMP-AMOUNT
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           IF LOCAL-TAX > 0
               MOVE SPACES TO WS-LINE
               MOVE 'Local Tax' TO WS-LINE(5:9)
               MOVE LOCAL-TAX TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE EMP-YTD-LOCAL-TAX IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
               ADD LOCAL-TAX TO WS-TEMP-AMOUNT
               MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
           MOVE SPACES TO WS-LINE
           MOVE 'Social Security Tax' TO WS-LINE(5:19)
           MOVE SOCIAL-SEC-TAX TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE EMP-YTD-SOCIAL-SEC IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
           ADD SOCIAL-SEC-TAX TO WS-TEMP-AMOUNT
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Medicare Tax' TO WS-LINE(5:12)
           MOVE MEDICARE-TAX TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE EMP-YTD-MEDICARE IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
           ADD MEDICARE-TAX TO WS-TEMP-AMOUNT
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Total Taxes' TO WS-LINE(5:11)
           COMPUTE WS-TEMP-AMOUNT = 
               FEDERAL-TAX + STATE-TAX + LOCAL-TAX + 
               SOCIAL-SEC-TAX + MEDICARE-TAX
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE WS-YTD-TOTAL-TAXES TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           .
           
       400-FORMAT-DEDUCTIONS.
      *****************************************************************
      * Format the deductions section of the pay stub                  *
      *****************************************************************
           MOVE SPACES TO WS-LINE
           MOVE 'DEDUCTIONS' TO WS-LINE(5:10)
           MOVE 'CURRENT' TO WS-LINE(55:7)
           MOVE 'YTD' TO WS-LINE(70:3)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           IF EMP-HEALTH-PLAN-CODE IN EMPLOYEE-RECORD NOT = SPACES AND
              EMP-HEALTH-PLAN-CODE IN EMPLOYEE-RECORD NOT = '000'
               MOVE SPACES TO WS-LINE
               MOVE 'Health Insurance' TO WS-LINE(5:16)
               MOVE EMP-HEALTH-DEDUCTION IN EMPLOYEE-RECORD 
                   TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE EMP-YTD-HEALTH-DEDUCT IN EMPLOYEE-RECORD 
                   TO WS-TEMP-AMOUNT
               ADD EMP-HEALTH-DEDUCTION IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
               MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
           IF EMP-DENTAL-PLAN-CODE IN EMPLOYEE-RECORD NOT = SPACES AND
              EMP-DENTAL-PLAN-CODE IN EMPLOYEE-RECORD NOT = '000'
               MOVE SPACES TO WS-LINE
               MOVE 'Dental Insurance' TO WS-LINE(5:16)
               MOVE EMP-DENTAL-DEDUCTION IN EMPLOYEE-RECORD 
                   TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE EMP-YTD-DENTAL-DEDUCT IN EMPLOYEE-RECORD 
                   TO WS-TEMP-AMOUNT
               ADD EMP-DENTAL-DEDUCTION IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
               MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
           IF EMP-VISION-PLAN-CODE IN EMPLOYEE-RECORD NOT = SPACES AND
              EMP-VISION-PLAN-CODE IN EMPLOYEE-RECORD NOT = '000'
               MOVE SPACES TO WS-LINE
               MOVE 'Vision Insurance' TO WS-LINE(5:16)
               MOVE EMP-VISION-DEDUCTION IN EMPLOYEE-RECORD 
                   TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE EMP-YTD-VISION-DEDUCT IN EMPLOYEE-RECORD 
                   TO WS-TEMP-AMOUNT
               ADD EMP-VISION-DEDUCTION IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
               MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
           IF EMP-401K-YES IN EMPLOYEE-RECORD
               MOVE SPACES TO WS-LINE
               MOVE '401(k) Retirement' TO WS-LINE(5:16)
               COMPUTE WS-TEMP-AMOUNT ROUNDED =
                   GROSS-PAY * (EMP-401K-PERCENT IN EMPLOYEE-RECORD / 100)
               MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
               MOVE EMP-YTD-401K IN EMPLOYEE-RECORD TO WS-TEMP-AMOUNT
               ADD WS-TEMP-AMOUNT TO WS-TEMP-AMOUNT
               MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
               MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           
      **** Other deductions would be listed here ****
      **** For brevity, not all deductions are shown ****
           
           MOVE SPACES TO WS-LINE
           MOVE 'Total Deductions' TO WS-LINE(5:16)
           MOVE TOTAL-DEDUCTIONS TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           COMPUTE WS-TEMP-AMOUNT = 
               EMP-YTD-HEALTH-DEDUCT IN EMPLOYEE-RECORD +
               EMP-YTD-DENTAL-DEDUCT IN EMPLOYEE-RECORD +
               EMP-YTD-VISION-DEDUCT IN EMPLOYEE-RECORD +
               EMP-YTD-401K IN EMPLOYEE-RECORD +
               EMP-YTD-OTHER-DEDUCT IN EMPLOYEE-RECORD +
               TOTAL-DEDUCTIONS
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           .
           
       500-FORMAT-TOTALS.
      *****************************************************************
      * Format the totals section of the pay stub                      *
      *****************************************************************
           MOVE SPACES TO WS-LINE
           MOVE 'NET PAY' TO WS-LINE(5:7)
           MOVE NET-PAY TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(55:11)
           MOVE WS-YTD-NET TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(70:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           .
           
       600-FORMAT-YTD-SUMMARY.
      *****************************************************************
      * Format the year-to-date summary section of the pay stub        *
      *****************************************************************
           MOVE SPACES TO WS-LINE
           MOVE 'YEAR-TO-DATE SUMMARY' TO WS-LINE(5:20)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Gross Earnings:' TO WS-LINE(5:15)
           MOVE WS-YTD-GROSS TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(25:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Total Taxes:' TO WS-LINE(5:12)
           MOVE WS-YTD-TOTAL-TAXES TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(25:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Total Deductions:' TO WS-LINE(5:17)
           COMPUTE WS-TEMP-AMOUNT = 
               EMP-YTD-HEALTH-DEDUCT IN EMPLOYEE-RECORD +
               EMP-YTD-DENTAL-DEDUCT IN EMPLOYEE-RECORD +
               EMP-YTD-VISION-DEDUCT IN EMPLOYEE-RECORD +
               EMP-YTD-401K IN EMPLOYEE-RECORD +
               EMP-YTD-OTHER-DEDUCT IN EMPLOYEE-RECORD +
               TOTAL-DEDUCTIONS
           MOVE WS-TEMP-AMOUNT TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(25:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'Net Pay:' TO WS-LINE(5:8)
           MOVE WS-YTD-NET TO WS-FORMATTED-AMOUNT
           MOVE WS-FORMATTED-AMOUNT TO WS-LINE(25:11)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
      **** Separator Line ****
           MOVE WS-PAY-STUB-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           .
           
       700-FORMAT-FOOTER.
      *****************************************************************
      * Format the footer section of the pay stub                      *
      *****************************************************************
           MOVE SPACES TO WS-LINE
           MOVE 'PLEASE KEEP THIS STATEMENT FOR YOUR RECORDS' 
               TO WS-LINE(40:40)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           MOVE 'DIRECT DEPOSIT INFORMATION' TO WS-LINE(5:25)
           MOVE WS-LINE TO PAYSTUB-RECORD
           PERFORM 900-WRITE-LINE
           
           IF EMP-DD-YES IN EMPLOYEE-RECORD
               MOVE SPACES TO WS-LINE
               STRING 'Your pay has been deposited to account: '
                      EMP-BANK-ACCOUNT-INFO IN EMPLOYEE-RECORD
                      DELIMITED BY SIZE
                      INTO WS-LINE(5:60)
               MOVE WS-LINE TO PAYSTUB-RECORD
               PERFORM 900-WRITE-LINE
           END-IF
           .
           
       800-FORMAT-DATE.
      *****************************************************************
      * Format date from YYYYMMDD to MM/DD/YYYY format                 *
      *****************************************************************
           MOVE SPACES TO WS-FORMATTED-DATE
           STRING WS-MONTH '/' WS-DAY '/' WS-YEAR
                  DELIMITED BY SIZE
                  INTO WS-FORMATTED-DATE
           .
           
       900-WRITE-LINE.
      *****************************************************************
      * Utility routine to write a line to the pay stub output         *
      *****************************************************************
      **** In a real system, would write to file or print ****
      **** For this module, just return line to calling program ****
           .