      *****************************************************************
      * EMPFILE.cpy - Employee Record Structure                        *
      *                                                               *
      * This copybook defines the structure of an employee record for  *
      * the Payroll Processing System. It contains all fields needed   *
      * for employee information, pay calculation, tax processing,     *
      * and deduction management.                                      *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       01  EMPLOYEE-RECORD.
           05  EMP-ID                      PIC X(6).
           05  EMP-PERSONAL-INFO.
               10  EMP-LAST-NAME           PIC X(20).
               10  EMP-FIRST-NAME          PIC X(15).
               10  EMP-MIDDLE-INIT         PIC X.
               10  EMP-GENDER              PIC X.
                   88  EMP-MALE            VALUE 'M'.
                   88  EMP-FEMALE          VALUE 'F'.
                   88  EMP-OTHER           VALUE 'O'.
               10  EMP-BIRTH-DATE.
                   15  EMP-BIRTH-YEAR      PIC 9(4).
                   15  EMP-BIRTH-MONTH     PIC 9(2).
                   15  EMP-BIRTH-DAY       PIC 9(2).
               10  EMP-SSN                 PIC 9(9).
               10  EMP-MARITAL-STATUS      PIC X.
                   88  EMP-SINGLE          VALUE 'S'.
                   88  EMP-MARRIED         VALUE 'M'.
                   88  EMP-DIVORCED        VALUE 'D'.
                   88  EMP-WIDOWED         VALUE 'W'.
           05  EMP-CONTACT-INFO.
               10  EMP-ADDRESS-LINE-1      PIC X(30).
               10  EMP-ADDRESS-LINE-2      PIC X(30).
               10  EMP-CITY               PIC X(20).
               10  EMP-STATE              PIC X(2).
               10  EMP-ZIP                PIC X(10).
               10  EMP-PHONE              PIC X(15).
               10  EMP-EMAIL              PIC X(50).
           05  EMP-EMPLOYMENT-INFO.
               10  EMP-HIRE-DATE.
                   15  EMP-HIRE-YEAR      PIC 9(4).
                   15  EMP-HIRE-MONTH     PIC 9(2).
                   15  EMP-HIRE-DAY       PIC 9(2).
               10  EMP-DEPARTMENT         PIC X(4).
               10  EMP-POSITION           PIC X(20).
               10  EMP-STATUS             PIC X.
                   88  EMP-ACTIVE         VALUE 'A'.
                   88  EMP-TERMINATED     VALUE 'T'.
                   88  EMP-LEAVE          VALUE 'L'.
                   88  EMP-RETIRED        VALUE 'R'.
               10  EMP-TERM-DATE          PIC 9(8) VALUE ZEROS.
           05  EMP-PAY-INFO.
               10  EMP-PAY-TYPE           PIC X.
                   88  EMP-HOURLY         VALUE 'H'.
                   88  EMP-SALARY         VALUE 'S'.
               10  EMP-PAY-FREQUENCY      PIC X.
                   88  EMP-WEEKLY         VALUE 'W'.
                   88  EMP-BIWEEKLY       VALUE 'B'.
                   88  EMP-MONTHLY        VALUE 'M'.
               10  EMP-HOURLY-RATE        PIC 9(4)V99  COMP-3.
               10  EMP-SALARY-AMOUNT      PIC 9(7)V99  COMP-3.
               10  EMP-STANDARD-HOURS     PIC 9(3)V99  COMP-3.
               10  EMP-OVERTIME-RATE      PIC 9(1)V99  COMP-3 VALUE 1.50.
               10  EMP-LAST-PAY-DATE      PIC 9(8)     VALUE ZEROS.
               10  EMP-DIRECT-DEPOSIT-IND PIC X        VALUE 'N'.
                   88  EMP-DD-YES         VALUE 'Y'.
                   88  EMP-DD-NO          VALUE 'N'.
               10  EMP-BANK-ACCOUNT-INFO  PIC X(30).
           05  EMP-TAX-INFO.
               10  EMP-FEDERAL-FILING-STATUS PIC X.
                   88  EMP-FILING-SINGLE     VALUE 'S'.
                   88  EMP-FILING-MARRIED    VALUE 'M'.
                   88  EMP-FILING-HEAD       VALUE 'H'.
               10  EMP-STATE-FILING-STATUS   PIC X.
               10  EMP-FEDERAL-ALLOWANCES    PIC 9(2).
               10  EMP-STATE-ALLOWANCES      PIC 9(2).
               10  EMP-ADDITIONAL-FIT        PIC 9(5)V99 COMP-3.
               10  EMP-ADDITIONAL-SIT        PIC 9(5)V99 COMP-3.
               10  EMP-TAX-BLOCKED-IND       PIC X.
                   88  EMP-TAX-BLOCKED-YES   VALUE 'Y'.
                   88  EMP-TAX-BLOCKED-NO    VALUE 'N'.
           05  EMP-DEDUCTION-INFO.
               10  EMP-HEALTH-PLAN-CODE     PIC X(3).
               10  EMP-HEALTH-DEDUCTION     PIC 9(5)V99 COMP-3.
               10  EMP-DENTAL-PLAN-CODE     PIC X(3).
               10  EMP-DENTAL-DEDUCTION     PIC 9(5)V99 COMP-3.
               10  EMP-VISION-PLAN-CODE     PIC X(3).
               10  EMP-VISION-DEDUCTION     PIC 9(5)V99 COMP-3.
               10  EMP-401K-IND             PIC X.
                   88  EMP-401K-YES         VALUE 'Y'.
                   88  EMP-401K-NO          VALUE 'N'.
               10  EMP-401K-PERCENT         PIC 9(2)V99 COMP-3.
               10  EMP-LOAN-DEDUCTION      PIC 9(5)V99 COMP-3.
               10  EMP-GARNISH-DEDUCTION   PIC 9(5)V99 COMP-3.
               10  EMP-CHARITY-DEDUCTION   PIC 9(5)V99 COMP-3.
               10  EMP-UNION-DUES          PIC 9(5)V99 COMP-3.
               10  EMP-ADDITIONAL-DEDUCTIONS OCCURS 5 TIMES.
                   15  EMP-ADD-DEDUCT-CODE  PIC X(3).
                   15  EMP-ADD-DEDUCT-AMT   PIC 9(5)V99 COMP-3.
           05  EMP-YTD-AMOUNTS.
               10  EMP-YTD-GROSS           PIC 9(8)V99 COMP-3.
               10  EMP-YTD-FEDERAL-TAX     PIC 9(7)V99 COMP-3.
               10  EMP-YTD-STATE-TAX       PIC 9(7)V99 COMP-3.
               10  EMP-YTD-LOCAL-TAX       PIC 9(7)V99 COMP-3.
               10  EMP-YTD-SOCIAL-SEC      PIC 9(7)V99 COMP-3.
               10  EMP-YTD-MEDICARE        PIC 9(7)V99 COMP-3.
               10  EMP-YTD-401K            PIC 9(7)V99 COMP-3.
               10  EMP-YTD-HEALTH-DEDUCT   PIC 9(7)V99 COMP-3.
               10  EMP-YTD-DENTAL-DEDUCT   PIC 9(7)V99 COMP-3.
               10  EMP-YTD-VISION-DEDUCT   PIC 9(7)V99 COMP-3.
               10  EMP-YTD-OTHER-DEDUCT    PIC 9(7)V99 COMP-3.
               10  EMP-YTD-NET-PAY         PIC 9(8)V99 COMP-3.
           05  EMP-FILLER                  PIC X(50).