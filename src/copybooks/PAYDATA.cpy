      *****************************************************************
      * PAYDATA.cpy - Payroll Data Input Structure                    *
      *                                                               *
      * This copybook defines the structure of payroll input data for  *
      * the Payroll Processing System. It contains fields for         *
      * recording hours worked, overtime, bonuses, leave time, and    *
      * other variable data that changes with each pay period.        *
      *                                                               *
      * Date Created: 2025-04-10                                      *
      * Author: COBOL Payroll System                                  *
      *****************************************************************

       01  PAYROLL-DATA-RECORD.
           05  PAY-EMPLOYEE-ID            PIC X(6).
           05  PAY-PERIOD-INFO.
               10  PAY-PERIOD-ID          PIC 9(6).
               10  PAY-PERIOD-START-DATE.
                   15  PAY-START-YEAR     PIC 9(4).
                   15  PAY-START-MONTH    PIC 9(2).
                   15  PAY-START-DAY      PIC 9(2).
               10  PAY-PERIOD-END-DATE.
                   15  PAY-END-YEAR       PIC 9(4).
                   15  PAY-END-MONTH      PIC 9(2).
                   15  PAY-END-DAY        PIC 9(2).
               10  PAY-CHECK-DATE.
                   15  PAY-CHECK-YEAR     PIC 9(4).
                   15  PAY-CHECK-MONTH    PIC 9(2).
                   15  PAY-CHECK-DAY      PIC 9(2).
           05  PAY-WORK-HOURS.
               10  PAY-REGULAR-HOURS      PIC 9(3)V99 COMP-3.
               10  PAY-OVERTIME-HOURS     PIC 9(3)V99 COMP-3.
               10  PAY-DOUBLETIME-HOURS   PIC 9(3)V99 COMP-3.
               10  PAY-SHIFT-DIFF-HOURS   PIC 9(3)V99 COMP-3.
               10  PAY-SHIFT-DIFF-RATE    PIC 9(2)V99 COMP-3.
               10  PAY-ON-CALL-HOURS      PIC 9(3)V99 COMP-3.
               10  PAY-ON-CALL-RATE       PIC 9(2)V99 COMP-3.
               10  PAY-HOLIDAY-HOURS      PIC 9(3)V99 COMP-3.
               10  PAY-HOLIDAY-RATE       PIC 9(2)V99 COMP-3.
           05  PAY-ADDITIONAL-AMOUNTS.
               10  PAY-BONUS-AMOUNT       PIC 9(7)V99 COMP-3.
               10  PAY-COMMISSION-AMOUNT  PIC 9(7)V99 COMP-3.
               10  PAY-COMMISSION-RATE    PIC 9(2)V99 COMP-3.
               10  PAY-COMMISSION-SALES   PIC 9(9)V99 COMP-3.
               10  PAY-TIPS-AMOUNT        PIC 9(7)V99 COMP-3.
               10  PAY-ALLOWANCE-AMOUNT   PIC 9(7)V99 COMP-3.
               10  PAY-REIMBURSEMENT-AMT  PIC 9(7)V99 COMP-3.
               10  PAY-OTHER-EARNINGS     PIC 9(7)V99 COMP-3.
               10  PAY-OTHER-EARNINGS-DESC PIC X(20).
           05  PAY-LEAVE-TIME.
               10  PAY-VACATION-HOURS     PIC 9(3)V99 COMP-3.
               10  PAY-SICK-HOURS         PIC 9(3)V99 COMP-3.
               10  PAY-PERSONAL-HOURS     PIC 9(3)V99 COMP-3.
               10  PAY-BEREAVEMENT-HOURS  PIC 9(3)V99 COMP-3.
               10  PAY-JURY-DUTY-HOURS    PIC 9(3)V99 COMP-3.
               10  PAY-FMLA-HOURS         PIC 9(3)V99 COMP-3.
               10  PAY-MILITARY-HOURS     PIC 9(3)V99 COMP-3.
               10  PAY-OTHER-LEAVE-HOURS  PIC 9(3)V99 COMP-3.
               10  PAY-OTHER-LEAVE-DESC   PIC X(20).
           05  PAY-ADJUSTMENTS.
               10  PAY-MANUAL-TAX-ADJUST  PIC S9(7)V99 COMP-3.
               10  PAY-MANUAL-DEDUCT-ADJ  PIC S9(7)V99 COMP-3.
               10  PAY-RETRO-PAY-AMOUNT   PIC 9(7)V99 COMP-3.
               10  PAY-ADVANCE-AMOUNT     PIC 9(7)V99 COMP-3.
               10  PAY-GARNISH-OVERRIDE   PIC 9(7)V99 COMP-3.
               10  PAY-OTHER-ADJUST-AMT   PIC S9(7)V99 COMP-3.
               10  PAY-OTHER-ADJUST-DESC  PIC X(20).
           05  PAY-OVERRIDE-FLAGS.
               10  PAY-OVERRIDE-RATE-FLAG PIC X.
                   88  PAY-OVERRIDE-RATE   VALUE 'Y'.
                   88  PAY-USE-NORMAL-RATE VALUE 'N'.
               10  PAY-OVERRIDE-RATE      PIC 9(4)V99 COMP-3.
               10  PAY-SKIP-TAX-FLAG      PIC X.
                   88  PAY-SKIP-TAX        VALUE 'Y'.
                   88  PAY-APPLY-TAX       VALUE 'N'.
               10  PAY-SKIP-DEDUCT-FLAG   PIC X.
                   88  PAY-SKIP-DEDUCT     VALUE 'Y'.
                   88  PAY-APPLY-DEDUCT    VALUE 'N'.
               10  PAY-SPECIAL-CALC-FLAG  PIC X.
                   88  PAY-SPECIAL-CALC    VALUE 'Y'.
                   88  PAY-NORMAL-CALC     VALUE 'N'.
           05  PAY-STATUS-FLAGS.
               10  PAY-RECORD-STATUS      PIC X.
                   88  PAY-STATUS-PENDING  VALUE 'P'.
                   88  PAY-STATUS-APPROVED VALUE 'A'.
                   88  PAY-STATUS-PROCESSED VALUE 'C'.
                   88  PAY-STATUS-ERROR    VALUE 'E'.
               10  PAY-ERROR-CODE         PIC X(4).
               10  PAY-ERROR-DESC         PIC X(50).
           05  PAY-RECORD-CREATED.
               10  PAY-CREATED-TIMESTAMP  PIC X(20).
               10  PAY-CREATED-USER       PIC X(15).
           05  PAY-RECORD-UPDATED.
               10  PAY-UPDATED-TIMESTAMP  PIC X(20).
               10  PAY-UPDATED-USER       PIC X(15).
           05  PAY-FILLER                 PIC X(50).