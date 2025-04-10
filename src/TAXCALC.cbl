      *****************************************************************
      * Program: TAXCALC.cbl                                          *
      *                                                               *
      * Purpose: Tax calculation module for the Payroll Processing    *
      *          System. This program calculates federal income tax,  *
      *          state income tax, local tax, Social Security tax,    *
      *          and Medicare tax based on employee earnings, filing  *
      *          status, and tax rates.                               *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAXCALC.
       AUTHOR. COBOL PAYROLL SYSTEM.
       DATE-WRITTEN. 2025-04-10.
       DATE-COMPILED. 2025-04-10.
      
      *****************************************************************
      * Program Description:                                           *
      *                                                                *
      * TAXCALC is the tax calculation module for the Payroll         *
      * Processing System. It calculates:                              *
      *                                                                *
      * 1. Federal Income Tax - Based on tax brackets, filing status,  *
      *    and allowances                                              *
      * 2. State Income Tax - Based on state-specific tax rates        *
      * 3. Local Tax - Based on local tax rates if applicable         *
      * 4. Social Security Tax - 6.2% of earnings up to the annual    *
      *    wage base limit                                            *
      * 5. Medicare Tax - 1.45% of all earnings, plus additional 0.9%  *
      *    for high-income employees                                   *
      *                                                                *
      * The module receives gross pay, YTD earnings, filing status,    *
      * and other parameters from the calling program, performs the    *
      * calculations, and returns the calculated tax amounts.          *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CALCULATION-WORK-AREA.
           05  WS-TAXABLE-INCOME          PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-ANNUALIZED-INCOME       PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-TAX-ALLOWANCE-VALUE     PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-CURRENT-BRACKET-TAX     PIC 9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TAX-RATE                PIC 9(2)V99 COMP-3 VALUE ZEROS.
           05  WS-BRACKET-FLOOR           PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-BRACKET-CEILING         PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-BRACKET-INCOME          PIC 9(8)V99 COMP-3 VALUE ZEROS.
           05  WS-BRACKET-INDEX           PIC 9(2)    COMP-3 VALUE ZEROS.
           05  WS-STATE-INDEX             PIC 9(2)    COMP-3 VALUE ZEROS.
           05  WS-FOUND-FLAG              PIC X       VALUE 'N'.
               88  FOUND-STATE            VALUE 'Y'.
               88  NOT-FOUND-STATE        VALUE 'N'.
           05  WS-SS-REMAIN-WAGES         PIC 9(8)V99 COMP-3 VALUE ZEROS.
       
       LINKAGE SECTION.
      *****************************************************************
      * Input Parameters:                                              *
      * - Tax-Calc-Gross: Current period gross earnings               *
      * - Tax-Calc-YTD-Gross: Year-to-date gross earnings            *
      * - Tax-Filing-Status: Federal filing status (S/M/H)            *
      * - Tax-State-Code: State code for state tax calculation        *
      * - Tax-Allowances: Number of tax allowances/exemptions         *
      * - Tax-Additional: Additional tax withholding requested         *
      * - Tax-Rates: Tax rates table with brackets and rates          *
      *                                                                *
      * Output Parameters:                                             *
      * - Federal-Tax: Calculated federal income tax                   *
      * - State-Tax: Calculated state income tax                       *
      * - Local-Tax: Calculated local tax                              *
      * - Social-Sec-Tax: Calculated Social Security tax               *
      * - Medicare-Tax: Calculated Medicare tax                        *
      *****************************************************************
       01  TAX-CALC-GROSS               PIC 9(7)V99 COMP-3.
       01  TAX-CALC-YTD-GROSS           PIC 9(8)V99 COMP-3.
       01  TAX-FILING-STATUS            PIC X.
       01  TAX-STATE-CODE               PIC X(2).
       01  TAX-ALLOWANCES               PIC 9(2) COMP-3.
       01  TAX-ADDITIONAL               PIC 9(5)V99 COMP-3.
       01  TAX-RATES.
           COPY TAXRATES.
       01  FEDERAL-TAX                  PIC 9(7)V99 COMP-3.
       01  STATE-TAX                    PIC 9(7)V99 COMP-3.
       01  LOCAL-TAX                    PIC 9(7)V99 COMP-3.
       01  SOCIAL-SEC-TAX               PIC 9(7)V99 COMP-3.
       01  MEDICARE-TAX                 PIC 9(7)V99 COMP-3.
       
       PROCEDURE DIVISION USING TAX-CALC-GROSS
                                TAX-CALC-YTD-GROSS
                                TAX-FILING-STATUS
                                TAX-STATE-CODE
                                TAX-ALLOWANCES
                                TAX-ADDITIONAL
                                TAX-RATES
                                FEDERAL-TAX
                                STATE-TAX
                                LOCAL-TAX
                                SOCIAL-SEC-TAX
                                MEDICARE-TAX.

       000-MAIN-PROCESS.
      *****************************************************************
      * Main tax calculation process - coordinates all tax calculations*
      *****************************************************************
           INITIALIZE FEDERAL-TAX
                      STATE-TAX
                      LOCAL-TAX
                      SOCIAL-SEC-TAX
                      MEDICARE-TAX
                      
           PERFORM 100-CALCULATE-FEDERAL-TAX
           PERFORM 200-CALCULATE-STATE-TAX
           PERFORM 300-CALCULATE-LOCAL-TAX
           PERFORM 400-CALCULATE-SOCIAL-SECURITY
           PERFORM 500-CALCULATE-MEDICARE
           
           GOBACK
           .
           
       100-CALCULATE-FEDERAL-TAX.
      *****************************************************************
      * Federal Income Tax Calculation                                 *
      *                                                                *
      * This section calculates federal income tax based on:           *
      * 1. Filing status (Single, Married, Head of Household)          *
      * 2. Income level and corresponding tax bracket                  *
      * 3. Number of tax allowances claimed                            *
      * 4. Additional withholding requested                            *
      *****************************************************************
           MOVE ZEROS TO FEDERAL-TAX
           
      **** Calculate taxable income (annualized) ****
           MULTIPLY TAX-CALC-GROSS BY 24 GIVING WS-ANNUALIZED-INCOME
           
      **** Reduce taxable income by allowances ****
           MULTIPLY TAX-ALLOWANCES BY PERSONAL-EXEMPTION-AMOUNT IN TAX-RATES
               GIVING WS-TAX-ALLOWANCE-VALUE
               
           SUBTRACT WS-TAX-ALLOWANCE-VALUE FROM WS-ANNUALIZED-INCOME
               GIVING WS-TAXABLE-INCOME
               
           IF WS-TAXABLE-INCOME <= 0
               MOVE 0 TO FEDERAL-TAX
               GO TO 100-EXIT
           END-IF
               
      **** Find applicable tax bracket based on filing status ****
           MOVE 1 TO WS-BRACKET-INDEX
           
           EVALUATE TAX-FILING-STATUS
               WHEN 'S'
                   PERFORM VARYING WS-BRACKET-INDEX FROM 1 BY 1
                       UNTIL WS-BRACKET-INDEX > 7
                       MOVE FED-BRACKET-FLOOR (WS-BRACKET-INDEX)
                           TO WS-BRACKET-FLOOR
                       MOVE FED-BRACKET-CEILING (WS-BRACKET-INDEX)
                           TO WS-BRACKET-CEILING
                       IF WS-TAXABLE-INCOME >= WS-BRACKET-FLOOR AND
                          (WS-TAXABLE-INCOME < WS-BRACKET-CEILING OR
                           WS-BRACKET-CEILING = 0)
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   
               WHEN 'M'
                   PERFORM VARYING WS-BRACKET-INDEX FROM 1 BY 1
                       UNTIL WS-BRACKET-INDEX > 7
                       MOVE FED-BRACKET-FLOOR (WS-BRACKET-INDEX)
                           TO WS-BRACKET-FLOOR
                       MOVE FED-BRACKET-CEILING (WS-BRACKET-INDEX)
                           TO WS-BRACKET-CEILING
                       IF WS-TAXABLE-INCOME >= WS-BRACKET-FLOOR AND
                          (WS-TAXABLE-INCOME < WS-BRACKET-CEILING OR
                           WS-BRACKET-CEILING = 0)
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   
               WHEN 'H'
                   PERFORM VARYING WS-BRACKET-INDEX FROM 1 BY 1
                       UNTIL WS-BRACKET-INDEX > 7
                       MOVE FED-BRACKET-FLOOR (WS-BRACKET-INDEX)
                           TO WS-BRACKET-FLOOR
                       MOVE FED-BRACKET-CEILING (WS-BRACKET-INDEX)
                           TO WS-BRACKET-CEILING
                       IF WS-TAXABLE-INCOME >= WS-BRACKET-FLOOR AND
                          (WS-TAXABLE-INCOME < WS-BRACKET-CEILING OR
                           WS-BRACKET-CEILING = 0)
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
           END-EVALUATE
           
      **** Calculate tax based on applicable bracket ****
           MOVE FED-BRACKET-RATE (WS-BRACKET-INDEX) TO WS-TAX-RATE
           MOVE FED-BRACKET-BASE-TAX (WS-BRACKET-INDEX) TO FEDERAL-TAX
           
           SUBTRACT WS-BRACKET-FLOOR FROM WS-TAXABLE-INCOME
               GIVING WS-BRACKET-INCOME
               
           MULTIPLY WS-BRACKET-INCOME BY WS-TAX-RATE
               GIVING WS-CURRENT-BRACKET-TAX
               
           ADD WS-CURRENT-BRACKET-TAX TO FEDERAL-TAX
           
      **** Add additional withholding requested ****
           ADD TAX-ADDITIONAL TO FEDERAL-TAX
           
      **** Prorate annual tax to pay period ****
           DIVIDE FEDERAL-TAX BY 24 GIVING FEDERAL-TAX ROUNDED
           
       100-EXIT.
           EXIT
           .
           
       200-CALCULATE-STATE-TAX.
      *****************************************************************
      * State Income Tax Calculation                                  *
      *                                                                *
      * This section calculates state income tax based on:             *
      * 1. State code (determines which state's tax rules to apply)    *
      * 2. Income level and state-specific tax brackets                *
      * 3. State filing status                                         *
      *****************************************************************
           MOVE ZEROS TO STATE-TAX
           MOVE 'N' TO WS-FOUND-FLAG
           
      **** Find applicable state in the tax table ****
           PERFORM VARYING WS-STATE-INDEX FROM 1 BY 1
               UNTIL WS-STATE-INDEX > 50 OR FOUND-STATE
               IF STATE-CODE (WS-STATE-INDEX) = TAX-STATE-CODE
                   MOVE 'Y' TO WS-FOUND-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
           IF NOT-FOUND-STATE OR 
              STATE-NO-TAX (WS-STATE-INDEX)
               MOVE 0 TO STATE-TAX
               GO TO 200-EXIT
           END-IF
           
      **** Calculate taxable income (using same as federal for simplicity) ****
           MOVE WS-TAXABLE-INCOME TO WS-TAXABLE-INCOME
           
      **** Find applicable state tax bracket ****
           PERFORM VARYING WS-BRACKET-INDEX FROM 1 BY 1
               UNTIL WS-BRACKET-INDEX > 5
               MOVE STATE-BRACKET-FLOOR (WS-STATE-INDEX, WS-BRACKET-INDEX)
                   TO WS-BRACKET-FLOOR
               MOVE STATE-BRACKET-CEILING (WS-STATE-INDEX, WS-BRACKET-INDEX)
                   TO WS-BRACKET-CEILING
               
               IF WS-BRACKET-FLOOR = 0 AND WS-BRACKET-CEILING = 0
                   EXIT PERFORM
               END-IF
               
               IF WS-TAXABLE-INCOME >= WS-BRACKET-FLOOR AND
                  (WS-TAXABLE-INCOME < WS-BRACKET-CEILING OR
                   WS-BRACKET-CEILING = 0)
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
      **** Calculate state tax based on applicable bracket ****
           MOVE STATE-BRACKET-RATE (WS-STATE-INDEX, WS-BRACKET-INDEX)
               TO WS-TAX-RATE
           MOVE STATE-BRACKET-BASE-TAX (WS-STATE-INDEX, WS-BRACKET-INDEX)
               TO STATE-TAX
           
           SUBTRACT WS-BRACKET-FLOOR FROM WS-TAXABLE-INCOME
               GIVING WS-BRACKET-INCOME
               
           MULTIPLY WS-BRACKET-INCOME BY WS-TAX-RATE
               GIVING WS-CURRENT-BRACKET-TAX
               
           ADD WS-CURRENT-BRACKET-TAX TO STATE-TAX
           
      **** Prorate annual tax to pay period ****
           DIVIDE STATE-TAX BY 24 GIVING STATE-TAX ROUNDED
           
       200-EXIT.
           EXIT
           .
           
       300-CALCULATE-LOCAL-TAX.
      *****************************************************************
      * Local Tax Calculation                                         *
      *                                                                *
      * This section calculates local income/wage taxes based on:      *
      * 1. Local tax code (if applicable)                              *
      * 2. Local tax rate                                              *
      * 3. Local tax wage limits                                       *
      *****************************************************************
           MOVE ZEROS TO LOCAL-TAX
           
      **** For demonstration, use a simple flat percentage ****
      **** In a real system, would look up local tax code ****
           COMPUTE LOCAL-TAX = TAX-CALC-GROSS * 0.01
           
       300-EXIT.
           EXIT
           .
           
       400-CALCULATE-SOCIAL-SECURITY.
      *****************************************************************
      * Social Security Tax Calculation                               *
      *                                                                *
      * This section calculates Social Security tax:                   *
      * 1. 6.2% of earnings up to the annual wage base ($142,800)      *
      * 2. No tax on earnings above the wage base                      *
      *****************************************************************
           MOVE ZEROS TO SOCIAL-SEC-TAX
           
      **** Check available wage base ****
           IF TAX-CALC-YTD-GROSS >= SOC-SEC-WAGE-BASE IN TAX-RATES
               MOVE 0 TO SOCIAL-SEC-TAX
               GO TO 400-EXIT
           END-IF
           
      **** Calculate remaining taxable wage base ****
           SUBTRACT TAX-CALC-YTD-GROSS FROM SOC-SEC-WAGE-BASE IN TAX-RATES
               GIVING WS-SS-REMAIN-WAGES
               
      **** Calculate taxable Social Security wages for this period ****
           IF TAX-CALC-GROSS > WS-SS-REMAIN-WAGES
               MOVE WS-SS-REMAIN-WAGES TO WS-SS-REMAIN-WAGES
           ELSE
               MOVE TAX-CALC-GROSS TO WS-SS-REMAIN-WAGES
           END-IF
           
      **** Calculate Social Security tax ****
           MULTIPLY WS-SS-REMAIN-WAGES BY SOC-SEC-RATE IN TAX-RATES
               GIVING SOCIAL-SEC-TAX ROUNDED
               
       400-EXIT.
           EXIT
           .
           
       500-CALCULATE-MEDICARE.
      *****************************************************************
      * Medicare Tax Calculation                                      *
      *                                                                *
      * This section calculates Medicare tax:                          *
      * 1. 1.45% of all earnings                                       *
      * 2. Additional 0.9% on earnings over $200,000 (annual)         *
      *****************************************************************
           MOVE ZEROS TO MEDICARE-TAX
           
      **** Calculate regular Medicare tax ****
           MULTIPLY TAX-CALC-GROSS BY MEDICARE-RATE IN TAX-RATES
               GIVING MEDICARE-TAX ROUNDED
               
      **** Calculate additional Medicare tax for high earners ****
           IF TAX-CALC-YTD-GROSS > MEDICARE-ADDL-THRESHOLD IN TAX-RATES
               COMPUTE WS-TAXABLE-INCOME = 
                   TAX-CALC-GROSS - 
                   (MEDICARE-ADDL-THRESHOLD IN TAX-RATES - TAX-CALC-YTD-GROSS)
                   
               IF WS-TAXABLE-INCOME > 0
                   MULTIPLY WS-TAXABLE-INCOME BY MEDICARE-ADDL-RATE IN TAX-RATES
                       GIVING WS-CURRENT-BRACKET-TAX ROUNDED
                   ADD WS-CURRENT-BRACKET-TAX TO MEDICARE-TAX
               END-IF
           END-IF
           
       500-EXIT.
           EXIT
           .