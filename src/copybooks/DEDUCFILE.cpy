      *****************************************************************
      * DEDUCFILE.cpy - Deduction Types Structure                     *
      *                                                               *
      * This copybook defines the structure of deduction types for     *
      * the Payroll Processing System. It contains definitions for    *
      * various deduction categories, calculation methods, limits,     *
      * and other parameters needed for processing employee           *
      * deductions during payroll calculations.                        *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       01  DEDUCTION-TYPE-RECORD.
           05  DEDUCT-CODE                PIC X(3).
           05  DEDUCT-NAME                PIC X(20).
           05  DEDUCT-DESCRIPTION         PIC X(50).
           05  DEDUCT-CATEGORY            PIC X(2).
               88  DEDUCT-HEALTH-INS       VALUE 'HI'.
               88  DEDUCT-DENTAL-INS       VALUE 'DI'.
               88  DEDUCT-VISION-INS       VALUE 'VI'.
               88  DEDUCT-LIFE-INS         VALUE 'LI'.
               88  DEDUCT-RETIREMENT       VALUE 'RT'.
               88  DEDUCT-GARNISHMENT      VALUE 'GN'.
               88  DEDUCT-LOAN             VALUE 'LN'.
               88  DEDUCT-CHARITY          VALUE 'CH'.
               88  DEDUCT-UNION            VALUE 'UN'.
               88  DEDUCT-OTHER            VALUE 'OT'.
           05  DEDUCT-TAX-STATUS          PIC X.
               88  DEDUCT-PRE-TAX          VALUE 'P'.
               88  DEDUCT-POST-TAX         VALUE 'T'.
           05  DEDUCT-CALCULATION-METHOD   PIC X.
               88  DEDUCT-FLAT-AMOUNT      VALUE 'F'.
               88  DEDUCT-PERCENTAGE       VALUE 'P'.
               88  DEDUCT-HOURLY-RATE      VALUE 'H'.
               88  DEDUCT-GRADUATED        VALUE 'G'.
           05  DEDUCT-CALCULATION-PARAMS.
               10  DEDUCT-FLAT-AMOUNT      PIC 9(7)V99 COMP-3.
               10  DEDUCT-PERCENTAGE-RATE  PIC 9(3)V99 COMP-3.
               10  DEDUCT-HOURLY-RATE      PIC 9(3)V99 COMP-3.
               10  DEDUCT-GRAD-RANGES OCCURS 5 TIMES.
                   15  DEDUCT-GRAD-MIN-SALARY PIC 9(8)V99 COMP-3.
                   15  DEDUCT-GRAD-MAX-SALARY PIC 9(8)V99 COMP-3.
                   15  DEDUCT-GRAD-AMOUNT    PIC 9(7)V99 COMP-3.
                   15  DEDUCT-GRAD-PERCENTAGE PIC 9(3)V99 COMP-3.
           05  DEDUCT-LIMIT-PARAMS.
               10  DEDUCT-MAX-AMOUNT-PER-PAY PIC 9(7)V99 COMP-3.
               10  DEDUCT-ANNUAL-MAX-AMOUNT  PIC 9(8)V99 COMP-3.
               10  DEDUCT-MIN-AMOUNT-PER-PAY PIC 9(7)V99 COMP-3.
               10  DEDUCT-MAX-PERCENTAGE     PIC 9(3)V99 COMP-3.
           05  DEDUCT-FREQUENCY           PIC X.
               88  DEDUCT-EVERY-PAY        VALUE 'E'.
               88  DEDUCT-FIRST-PAY        VALUE 'F'.
               88  DEDUCT-SECOND-PAY       VALUE 'S'.
               88  DEDUCT-LAST-PAY         VALUE 'L'.
               88  DEDUCT-BIWEEKLY         VALUE 'B'.
               88  DEDUCT-MONTHLY          VALUE 'M'.
               88  DEDUCT-QUARTERLY        VALUE 'Q'.
               88  DEDUCT-ANNUAL           VALUE 'A'.
           05  DEDUCT-PRIORITY            PIC 9(3).
           05  DEDUCT-VENDOR-INFO.
               10  DEDUCT-VENDOR-ID        PIC X(10).
               10  DEDUCT-VENDOR-NAME      PIC X(30).
               10  DEDUCT-VENDOR-ACCOUNT   PIC X(20).
           05  DEDUCT-EFFECTIVE-DATE.
               10  DEDUCT-EFF-YEAR        PIC 9(4).
               10  DEDUCT-EFF-MONTH       PIC 9(2).
               10  DEDUCT-EFF-DAY         PIC 9(2).
           05  DEDUCT-EXPIRATION-DATE.
               10  DEDUCT-EXP-YEAR        PIC 9(4).
               10  DEDUCT-EXP-MONTH       PIC 9(2).
               10  DEDUCT-EXP-DAY         PIC 9(2).
           05  DEDUCT-STATUS             PIC X.
               88  DEDUCT-ACTIVE          VALUE 'A'.
               88  DEDUCT-INACTIVE        VALUE 'I'.
               88  DEDUCT-PENDING         VALUE 'P'.
           05  DEDUCT-REQUIRED-FLAG      PIC X.
               88  DEDUCT-REQUIRED        VALUE 'Y'.
               88  DEDUCT-OPTIONAL        VALUE 'N'.
           05  DEDUCT-EMPLOYER-CONTRIB   PIC X.
               88  DEDUCT-EMPLOYER-MATCH   VALUE 'Y'.
               88  DEDUCT-NO-EMPLOYER-MATCH VALUE 'N'.
           05  DEDUCT-EMPLOYER-MATCH-RATE PIC 9(3)V99 COMP-3.
           05  DEDUCT-EMPLOYER-MATCH-MAX  PIC 9(7)V99 COMP-3.
           05  DEDUCT-SPECIAL-PROC-FLAG   PIC X.
               88  DEDUCT-SPECIAL-PROC     VALUE 'Y'.
               88  DEDUCT-STANDARD-PROC    VALUE 'N'.
           05  DEDUCT-FILLER              PIC X(50).