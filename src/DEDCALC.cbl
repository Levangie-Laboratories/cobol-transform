      *****************************************************************
      * Program: DEDCALC.cbl                                          *
      *                                                               *
      * Purpose: Deduction calculation module for the Payroll         *
      *          Processing System. This program calculates employee  *
      *          deductions, including health insurance, retirement   *
      *          plans, garnishments, and other voluntary deductions  *
      *          based on employee elections and deduction types.     *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEDCALC.
       AUTHOR. COBOL PAYROLL SYSTEM.
       DATE-WRITTEN. 2025-04-10.
       DATE-COMPILED. 2025-04-10.
      
      *****************************************************************
      * Program Description:                                           *
      *                                                                *
      * DEDCALC is the deduction calculation module for the Payroll    *
      * Processing System. It calculates:                              *
      *                                                                *
      * 1. Health Insurance Deductions - Based on selected plan        *
      * 2. Dental Insurance Deductions - Based on selected plan        *
      * 3. Vision Insurance Deductions - Based on selected plan        *
      * 4. Retirement Plan Contributions - Based on percentage or      *
      *    fixed amount                                                *
      * 5. Garnishments - Based on court orders                        *
      * 6. Loan Repayments - Based on loan terms                       *
      * 7. Charitable Contributions - Based on employee elections      *
      * 8. Other Voluntary Deductions - Based on election type         *
      *                                                                *
      * The module receives gross pay and employee information from    *
      * the calling program, performs the calculations, and returns    *
      * the total deduction amount.                                    *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CALCULATION-WORK-AREA.
           05  WS-DEDUCTION-AMOUNT        PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-PERCENTAGE-AMOUNT       PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-PRE-TAX-TOTAL           PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-POST-TAX-TOTAL          PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-DEDUCTION-LIMIT         PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-DEDUCTION-YTD           PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-DEDUCTION-REMAINING     PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-DEDUCTION-CODE          PIC X(3)   VALUE SPACES.
           05  WS-DEDUCTION-INDEX         PIC 9(2)   COMP-3 VALUE ZEROS.
       
       LINKAGE SECTION.
      *****************************************************************
      * Input Parameters:                                              *
      * - Deduct-Calc-Gross: Current period gross earnings             *
      * - Employee-Record: Employee master record with deduction info  *
      *                                                                *
      * Output Parameters:                                             *
      * - Total-Deductions: Calculated total deductions                *
      *****************************************************************
       01  DEDUCT-CALC-GROSS              PIC 9(7)V99 COMP-3.
       01  EMPLOYEE-RECORD.
           COPY EMPFILE.
       01  TOTAL-DEDUCTIONS               PIC 9(7)V99 COMP-3.
       
       PROCEDURE DIVISION USING DEDUCT-CALC-GROSS
                                EMPLOYEE-RECORD
                                TOTAL-DEDUCTIONS.

       000-MAIN-PROCESS.
      *****************************************************************
      * Main deduction calculation process                             *
      *****************************************************************
           INITIALIZE TOTAL-DEDUCTIONS
                      WS-PRE-TAX-TOTAL
                      WS-POST-TAX-TOTAL
                      
           PERFORM 100-PROCESS-HEALTH-INSURANCE
           PERFORM 200-PROCESS-DENTAL-INSURANCE
           PERFORM 300-PROCESS-VISION-INSURANCE
           PERFORM 400-PROCESS-RETIREMENT
           PERFORM 500-PROCESS-LOAN-DEDUCTION
           PERFORM 600-PROCESS-GARNISHMENT
           PERFORM 700-PROCESS-CHARITY
           PERFORM 800-PROCESS-UNION-DUES
           PERFORM 900-PROCESS-ADDITIONAL-DEDUCTIONS
           
           COMPUTE TOTAL-DEDUCTIONS = WS-PRE-TAX-TOTAL + WS-POST-TAX-TOTAL
           
           GOBACK
           .
           
       100-PROCESS-HEALTH-INSURANCE.
      *****************************************************************
      * Process Health Insurance Deduction                             *
      *                                                                *
      * This section calculates health insurance deductions based on   *
      * the employee's selected plan (if any).                         *
      *****************************************************************
           IF EMP-HEALTH-PLAN-CODE IN EMPLOYEE-RECORD NOT = SPACES AND
              EMP-HEALTH-PLAN-CODE IN EMPLOYEE-RECORD NOT = '000'
              
               MOVE EMP-HEALTH-DEDUCTION IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               
      **** Apply per-pay period amount ****
               ADD WS-DEDUCTION-AMOUNT TO WS-PRE-TAX-TOTAL
           END-IF
           .
           
       200-PROCESS-DENTAL-INSURANCE.
      *****************************************************************
      * Process Dental Insurance Deduction                             *
      *                                                                *
      * This section calculates dental insurance deductions based on   *
      * the employee's selected plan (if any).                         *
      *****************************************************************
           IF EMP-DENTAL-PLAN-CODE IN EMPLOYEE-RECORD NOT = SPACES AND
              EMP-DENTAL-PLAN-CODE IN EMPLOYEE-RECORD NOT = '000'
              
               MOVE EMP-DENTAL-DEDUCTION IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               
      **** Apply per-pay period amount ****
               ADD WS-DEDUCTION-AMOUNT TO WS-PRE-TAX-TOTAL
           END-IF
           .
           
       300-PROCESS-VISION-INSURANCE.
      *****************************************************************
      * Process Vision Insurance Deduction                             *
      *                                                                *
      * This section calculates vision insurance deductions based on   *
      * the employee's selected plan (if any).                         *
      *****************************************************************
           IF EMP-VISION-PLAN-CODE IN EMPLOYEE-RECORD NOT = SPACES AND
              EMP-VISION-PLAN-CODE IN EMPLOYEE-RECORD NOT = '000'
              
               MOVE EMP-VISION-DEDUCTION IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               
      **** Apply per-pay period amount ****
               ADD WS-DEDUCTION-AMOUNT TO WS-PRE-TAX-TOTAL
           END-IF
           .
           
       400-PROCESS-RETIREMENT.
      *****************************************************************
      * Process Retirement Plan Deduction                              *
      *                                                                *
      * This section calculates retirement plan contributions based on *
      * the employee's election (percentage of gross pay).             *
      *****************************************************************
           IF EMP-401K-YES IN EMPLOYEE-RECORD
      **** Calculate retirement contribution based on percentage ****
               COMPUTE WS-PERCENTAGE-AMOUNT ROUNDED =
                   DEDUCT-CALC-GROSS * (EMP-401K-PERCENT IN EMPLOYEE-RECORD / 100)
               
      **** Check for annual limits (simplified for demonstration) ****
               MOVE 19500 TO WS-DEDUCTION-LIMIT
               MOVE EMP-YTD-401K IN EMPLOYEE-RECORD TO WS-DEDUCTION-YTD
               
               SUBTRACT WS-DEDUCTION-YTD FROM WS-DEDUCTION-LIMIT
                   GIVING WS-DEDUCTION-REMAINING
               
               IF WS-PERCENTAGE-AMOUNT > WS-DEDUCTION-REMAINING
                   MOVE WS-DEDUCTION-REMAINING TO WS-PERCENTAGE-AMOUNT
               END-IF
               
      **** Apply retirement deduction ****
               ADD WS-PERCENTAGE-AMOUNT TO WS-PRE-TAX-TOTAL
           END-IF
           .
           
       500-PROCESS-LOAN-DEDUCTION.
      *****************************************************************
      * Process Loan Repayment Deduction                               *
      *                                                                *
      * This section processes loan repayment deductions based on      *
      * fixed repayment amount.                                        *
      *****************************************************************
           IF EMP-LOAN-DEDUCTION IN EMPLOYEE-RECORD > 0
      **** Apply fixed loan repayment amount ****
               MOVE EMP-LOAN-DEDUCTION IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               ADD WS-DEDUCTION-AMOUNT TO WS-POST-TAX-TOTAL
           END-IF
           .
           
       600-PROCESS-GARNISHMENT.
      *****************************************************************
      * Process Garnishment Deduction                                  *
      *                                                                *
      * This section processes garnishment deductions (court-ordered   *
      * deductions such as child support or tax levies).               *
      *****************************************************************
           IF EMP-GARNISH-DEDUCTION IN EMPLOYEE-RECORD > 0
      **** Apply garnishment amount ****
      **** In a real system, might apply percentage limits based on laws ****
               MOVE EMP-GARNISH-DEDUCTION IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               ADD WS-DEDUCTION-AMOUNT TO WS-POST-TAX-TOTAL
           END-IF
           .
           
       700-PROCESS-CHARITY.
      *****************************************************************
      * Process Charitable Contribution Deduction                      *
      *                                                                *
      * This section processes voluntary charitable contribution        *
      * deductions based on employee election.                         *
      *****************************************************************
           IF EMP-CHARITY-DEDUCTION IN EMPLOYEE-RECORD > 0
      **** Apply charitable contribution amount ****
               MOVE EMP-CHARITY-DEDUCTION IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               ADD WS-DEDUCTION-AMOUNT TO WS-POST-TAX-TOTAL
           END-IF
           .
           
       800-PROCESS-UNION-DUES.
      *****************************************************************
      * Process Union Dues Deduction                                   *
      *                                                                *
      * This section processes union dues deductions if applicable.     *
      *****************************************************************
           IF EMP-UNION-DUES IN EMPLOYEE-RECORD > 0
      **** Apply union dues amount ****
               MOVE EMP-UNION-DUES IN EMPLOYEE-RECORD TO WS-DEDUCTION-AMOUNT
               ADD WS-DEDUCTION-AMOUNT TO WS-POST-TAX-TOTAL
           END-IF
           .
           
       900-PROCESS-ADDITIONAL-DEDUCTIONS.
      *****************************************************************
      * Process Additional Voluntary Deductions                        *
      *                                                                *
      * This section processes any additional voluntary deductions     *
      * elected by the employee.                                       *
      *****************************************************************
           PERFORM VARYING WS-DEDUCTION-INDEX FROM 1 BY 1
               UNTIL WS-DEDUCTION-INDEX > 5
               
               MOVE EMP-ADD-DEDUCT-CODE IN EMPLOYEE-RECORD (WS-DEDUCTION-INDEX)
                   TO WS-DEDUCTION-CODE
               
               IF WS-DEDUCTION-CODE NOT = SPACES AND
                  WS-DEDUCTION-CODE NOT = '000'
                  
                   MOVE EMP-ADD-DEDUCT-AMT IN EMPLOYEE-RECORD (WS-DEDUCTION-INDEX)
                       TO WS-DEDUCTION-AMOUNT
                   
      **** Determine if pre-tax or post-tax based on deduction code ****
      **** For demonstration, assuming deduction codes starting with 'P' are pre-tax ****
                   IF WS-DEDUCTION-CODE(1:1) = 'P'
                       ADD WS-DEDUCTION-AMOUNT TO WS-PRE-TAX-TOTAL
                   ELSE
                       ADD WS-DEDUCTION-AMOUNT TO WS-POST-TAX-TOTAL
                   END-IF
               END-IF
           END-PERFORM
           .