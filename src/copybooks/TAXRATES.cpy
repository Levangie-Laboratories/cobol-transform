      *****************************************************************
      * TAXRATES.cpy - Tax Rates Table Structure                      *
      *                                                               *
      * This copybook defines the structure of tax rates and brackets *
      * for the Payroll Processing System. It contains definitions    *
      * for federal and state income tax brackets, fixed-rate taxes   *
      * like Social Security and Medicare, and other tax parameters.  *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       01  TAX-RATES-TABLE.
           05  TAX-YEAR                    PIC 9(4).
           05  TAX-EFFECTIVE-DATE.
               10  TAX-EFF-YEAR           PIC 9(4).
               10  TAX-EFF-MONTH          PIC 9(2).
               10  TAX-EFF-DAY            PIC 9(2).
           05  TAX-EXPIRATION-DATE.
               10  TAX-EXP-YEAR           PIC 9(4).
               10  TAX-EXP-MONTH          PIC 9(2).
               10  TAX-EXP-DAY            PIC 9(2).
      
      **** FEDERAL INCOME TAX BRACKETS ****
           05  FEDERAL-TAX-BRACKETS.
               10  FED-FILING-STATUS      PIC X.
                   88  FED-SINGLE         VALUE 'S'.
                   88  FED-MARRIED        VALUE 'M'.
                   88  FED-HEAD-HOUSEHOLD VALUE 'H'.
               10  FED-TAX-BRACKET OCCURS 7 TIMES.
                   15  FED-BRACKET-FLOOR  PIC 9(8)V99 COMP-3.
                   15  FED-BRACKET-CEILING PIC 9(8)V99 COMP-3.
                   15  FED-BRACKET-RATE   PIC 9(2)V99 COMP-3.
                   15  FED-BRACKET-BASE-TAX PIC 9(8)V99 COMP-3.
      
      **** STATE INCOME TAX BRACKETS ****
           05  STATE-TAX-TABLE OCCURS 50 TIMES.
               10  STATE-CODE             PIC X(2).
               10  STATE-HAS-INCOME-TAX   PIC X.
                   88  STATE-WITH-TAX     VALUE 'Y'.
                   88  STATE-NO-TAX       VALUE 'N'.
               10  STATE-FILING-STATUS    PIC X.
                   88  STATE-SINGLE       VALUE 'S'.
                   88  STATE-MARRIED      VALUE 'M'.
                   88  STATE-HEAD-HOUSEHOLD VALUE 'H'.
               10  STATE-TAX-BRACKET OCCURS 5 TIMES.
                   15  STATE-BRACKET-FLOOR PIC 9(8)V99 COMP-3.
                   15  STATE-BRACKET-CEILING PIC 9(8)V99 COMP-3.
                   15  STATE-BRACKET-RATE  PIC 9(2)V99 COMP-3.
                   15  STATE-BRACKET-BASE-TAX PIC 9(6)V99 COMP-3.
      
      **** SOCIAL SECURITY AND MEDICARE TAXES ****
           05  FICA-TAXES.
               10  SOC-SEC-RATE           PIC 9(2)V99 COMP-3.
               10  SOC-SEC-WAGE-BASE      PIC 9(8)V99 COMP-3.
               10  SOC-SEC-MAX-TAX        PIC 9(8)V99 COMP-3.
               10  MEDICARE-RATE          PIC 9(2)V99 COMP-3.
               10  MEDICARE-ADDL-RATE     PIC 9(2)V99 COMP-3.
               10  MEDICARE-ADDL-THRESHOLD PIC 9(8)V99 COMP-3.
      
      **** LOCAL TAX RATES ****
           05  LOCAL-TAX-TABLE OCCURS 100 TIMES.
               10  LOCAL-CODE             PIC X(5).
               10  LOCAL-NAME             PIC X(20).
               10  LOCAL-TAX-RATE         PIC 9(2)V99 COMP-3.
               10  LOCAL-TAX-MIN-WAGE     PIC 9(8)V99 COMP-3.
               10  LOCAL-TAX-MAX-WAGE     PIC 9(8)V99 COMP-3.
      
      **** UNEMPLOYMENT TAXES ****
           05  UNEMPLOYMENT-TAXES.
               10  FUTA-RATE              PIC 9(2)V99 COMP-3.
               10  FUTA-WAGE-BASE         PIC 9(8)V99 COMP-3.
               10  SUTA-TABLE OCCURS 50 TIMES.
                   15  SUTA-STATE-CODE    PIC X(2).
                   15  SUTA-RATE          PIC 9(2)V99 COMP-3.
                   15  SUTA-WAGE-BASE     PIC 9(8)V99 COMP-3.
      
      **** TAX CONSTANTS AND PARAMETERS ****
           05  TAX-CONSTANTS.
               10  STANDARD-DEDUCTION-SINGLE PIC 9(6)V99 COMP-3.
               10  STANDARD-DEDUCTION-MARRIED PIC 9(6)V99 COMP-3.
               10  STANDARD-DEDUCTION-HEAD   PIC 9(6)V99 COMP-3.
               10  PERSONAL-EXEMPTION-AMOUNT PIC 9(6)V99 COMP-3.
               10  PERSONAL-EXEMPTION-PHASEOUT PIC 9(8)V99 COMP-3.
               10  TAX-CREDIT-RATE          PIC 9(2)V99 COMP-3.
      
           05  TAX-TABLE-FILLER            PIC X(50).