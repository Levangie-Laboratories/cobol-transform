# COBOL Payroll Processing System - Data Dictionary

## Table of Contents

1. [Introduction](#introduction)
2. [EMPFILE - Employee Master Record](#empfile---employee-master-record)
3. [PAYDATA - Payroll Data](#paydata---payroll-data)
4. [TAXRATES - Tax Rates](#taxrates---tax-rates)
5. [DEDUCFILE - Deduction Types](#deducfile---deduction-types)
6. [Cross-Reference of Data Elements](#cross-reference-of-data-elements)
7. [Special Values and Flags](#special-values-and-flags)

## Introduction

This Data Dictionary provides a comprehensive reference for all data elements used in the COBOL Payroll Processing System. It documents the fields in each data structure, their types, purposes, valid values, and usage in different modules. This reference is intended for developers who need to understand, modify, or extend the system's data structures and processing logic.

The data structures in the system are defined in four main copybooks:
- **EMPFILE.cpy**: Employee master record structure
- **PAYDATA.cpy**: Payroll data structure for pay period processing
- **TAXRATES.cpy**: Tax rates and brackets structure
- **DEDUCFILE.cpy**: Deduction types and parameters structure

## EMPFILE - Employee Master Record

The employee master record contains comprehensive information about each employee, including personal details, employment information, pay rates, tax status, deduction elections, and year-to-date totals.

### EMPLOYEE-RECORD (01 Level)

#### EMP-ID (05 Level)
- **Type**: PIC X(6)
- **Description**: Unique employee identifier
- **Valid Values**: Alphanumeric, must be unique
- **Used By**: All modules as the primary key for employee records
- **Example**: '000001' for John Smith

#### EMP-PERSONAL-INFO (05 Level - Group Item)

##### EMP-LAST-NAME (10 Level)
- **Type**: PIC X(20)
- **Description**: Employee's last name
- **Valid Values**: Any alphabetic characters, left-justified
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'SMITH'

##### EMP-FIRST-NAME (10 Level)
- **Type**: PIC X(15)
- **Description**: Employee's first name
- **Valid Values**: Any alphabetic characters, left-justified
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'JOHN'

##### EMP-MIDDLE-INIT (10 Level)
- **Type**: PIC X
- **Description**: Employee's middle initial
- **Valid Values**: Alphabetic character or space
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'M'

##### EMP-GENDER (10 Level)
- **Type**: PIC X
- **Description**: Employee's gender
- **Valid Values**: 'M' (Male), 'F' (Female), 'O' (Other)
- **Condition Names**: EMP-MALE, EMP-FEMALE, EMP-OTHER
- **Used By**: Not actively used in processing logic
- **Example**: 'M'

##### EMP-BIRTH-DATE (10 Level - Group Item)
- **Description**: Employee's date of birth

###### EMP-BIRTH-YEAR (15 Level)
- **Type**: PIC 9(4)
- **Description**: Year of birth
- **Valid Values**: 1900-Current year
- **Example**: 1985

###### EMP-BIRTH-MONTH (15 Level)
- **Type**: PIC 9(2)
- **Description**: Month of birth
- **Valid Values**: 01-12
- **Example**: 10

###### EMP-BIRTH-DAY (15 Level)
- **Type**: PIC 9(2)
- **Description**: Day of birth
- **Valid Values**: 01-31 (depending on month)
- **Example**: 15

##### EMP-SSN (10 Level)
- **Type**: PIC 9(9)
- **Description**: Social Security Number
- **Valid Values**: 9-digit number, must be unique
- **Used By**: Not actively used in processing logic, stored for reporting
- **Example**: 123456789

##### EMP-MARITAL-STATUS (10 Level)
- **Type**: PIC X
- **Description**: Employee's marital status
- **Valid Values**: 'S' (Single), 'M' (Married), 'D' (Divorced), 'W' (Widowed)
- **Condition Names**: EMP-SINGLE, EMP-MARRIED, EMP-DIVORCED, EMP-WIDOWED
- **Used By**: Not directly used in calculation logic
- **Example**: 'M'

#### EMP-CONTACT-INFO (05 Level - Group Item)

##### EMP-ADDRESS-LINE-1 (10 Level)
- **Type**: PIC X(30)
- **Description**: First line of mailing address
- **Used By**: PAYSTUB for pay stub generation
- **Example**: '123 MAIN STREET'

##### EMP-ADDRESS-LINE-2 (10 Level)
- **Type**: PIC X(30)
- **Description**: Second line of mailing address (apartment, suite, etc.)
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'APT 101'

##### EMP-CITY (10 Level)
- **Type**: PIC X(20)
- **Description**: City of residence
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'ANYTOWN'

##### EMP-STATE (10 Level)
- **Type**: PIC X(2)
- **Description**: State code
- **Valid Values**: Two-letter state abbreviations
- **Used By**: TAXCALC for state tax calculation
- **Example**: 'CA'

##### EMP-ZIP (10 Level)
- **Type**: PIC X(10)
- **Description**: ZIP or postal code
- **Used By**: PAYSTUB for pay stub generation
- **Example**: '12345'

##### EMP-PHONE (10 Level)
- **Type**: PIC X(15)
- **Description**: Contact phone number
- **Used By**: Not actively used in processing logic
- **Example**: '(555) 123-4567'

##### EMP-EMAIL (10 Level)
- **Type**: PIC X(50)
- **Description**: Email address
- **Used By**: Not actively used in processing logic
- **Example**: 'john.smith@email.com'

#### EMP-EMPLOYMENT-INFO (05 Level - Group Item)

##### EMP-HIRE-DATE (10 Level - Group Item)
- **Description**: Date employee was hired

###### EMP-HIRE-YEAR (15 Level)
- **Type**: PIC 9(4)
- **Description**: Year of hire
- **Example**: 2015

###### EMP-HIRE-MONTH (15 Level)
- **Type**: PIC 9(2)
- **Description**: Month of hire
- **Example**: 03

###### EMP-HIRE-DAY (15 Level)
- **Type**: PIC 9(2)
- **Description**: Day of hire
- **Example**: 10

##### EMP-DEPARTMENT (10 Level)
- **Type**: PIC X(4)
- **Description**: Department code
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'ACCT'

##### EMP-POSITION (10 Level)
- **Type**: PIC X(20)
- **Description**: Job title or position
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'ACCOUNTANT'

##### EMP-STATUS (10 Level)
- **Type**: PIC X
- **Description**: Employment status
- **Valid Values**: 'A' (Active), 'T' (Terminated), 'L' (Leave), 'R' (Retired)
- **Condition Names**: EMP-ACTIVE, EMP-TERMINATED, EMP-LEAVE, EMP-RETIRED
- **Used By**: PAYCALC to determine if employee should be processed
- **Example**: 'A'

##### EMP-TERM-DATE (10 Level)
- **Type**: PIC 9(8)
- **Description**: Termination date (if applicable)
- **Valid Values**: ZEROS if not terminated, otherwise YYYYMMDD format
- **Used By**: Not actively used in current processing logic
- **Example**: 00000000 (not terminated)

#### EMP-PAY-INFO (05 Level - Group Item)

##### EMP-PAY-TYPE (10 Level)
- **Type**: PIC X
- **Description**: Type of pay arrangement
- **Valid Values**: 'H' (Hourly), 'S' (Salary)
- **Condition Names**: EMP-HOURLY, EMP-SALARY
- **Used By**: PAYCALC to determine gross pay calculation method
- **Example**: 'H'

##### EMP-PAY-FREQUENCY (10 Level)
- **Type**: PIC X
- **Description**: Pay period frequency
- **Valid Values**: 'W' (Weekly), 'B' (Bi-weekly), 'M' (Monthly)
- **Condition Names**: EMP-WEEKLY, EMP-BIWEEKLY, EMP-MONTHLY
- **Used By**: PAYCALC for salary proration
- **Example**: 'B'

##### EMP-HOURLY-RATE (10 Level)
- **Type**: PIC 9(4)V99 COMP-3
- **Description**: Hourly pay rate for hourly employees
- **Used By**: PAYCALC to calculate gross pay for hourly employees
- **Example**: $18.00 per hour

##### EMP-SALARY-AMOUNT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Annual salary amount for salaried employees
- **Used By**: PAYCALC to calculate gross pay for salaried employees
- **Example**: $95,000.00 per year

##### EMP-STANDARD-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Standard hours in a pay period
- **Used By**: Not actively used in current processing logic
- **Example**: 40.00 hours

##### EMP-OVERTIME-RATE (10 Level)
- **Type**: PIC 9(1)V99 COMP-3
- **Description**: Overtime rate multiplier
- **Used By**: PAYCALC to calculate overtime pay
- **Example**: 1.50 (time and a half)

##### EMP-LAST-PAY-DATE (10 Level)
- **Type**: PIC 9(8)
- **Description**: Date of last paycheck
- **Used By**: Not actively used in calculation logic
- **Example**: 20250331 (March 31, 2025)

##### EMP-DIRECT-DEPOSIT-IND (10 Level)
- **Type**: PIC X
- **Description**: Direct deposit indicator
- **Valid Values**: 'Y' (Yes), 'N' (No)
- **Condition Names**: EMP-DD-YES, EMP-DD-NO
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 'Y'

##### EMP-BANK-ACCOUNT-INFO (10 Level)
- **Type**: PIC X(30)
- **Description**: Banking information for direct deposit
- **Used By**: PAYSTUB for pay stub generation
- **Example**: '123-45-6789'

#### EMP-TAX-INFO (05 Level - Group Item)

##### EMP-FEDERAL-FILING-STATUS (10 Level)
- **Type**: PIC X
- **Description**: Federal tax filing status
- **Valid Values**: 'S' (Single), 'M' (Married), 'H' (Head of Household)
- **Condition Names**: EMP-FILING-SINGLE, EMP-FILING-MARRIED, EMP-FILING-HEAD
- **Used By**: TAXCALC to determine federal tax brackets
- **Example**: 'S'

##### EMP-STATE-FILING-STATUS (10 Level)
- **Type**: PIC X
- **Description**: State tax filing status
- **Valid Values**: 'S' (Single), 'M' (Married), 'H' (Head of Household)
- **Used By**: TAXCALC to determine state tax brackets
- **Example**: 'S'

##### EMP-FEDERAL-ALLOWANCES (10 Level)
- **Type**: PIC 9(2)
- **Description**: Number of federal tax allowances
- **Used By**: TAXCALC to calculate federal tax
- **Example**: 2

##### EMP-STATE-ALLOWANCES (10 Level)
- **Type**: PIC 9(2)
- **Description**: Number of state tax allowances
- **Used By**: TAXCALC to calculate state tax
- **Example**: 2

##### EMP-ADDITIONAL-FIT (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Additional federal income tax withholding
- **Used By**: TAXCALC to calculate federal tax
- **Example**: $0.00

##### EMP-ADDITIONAL-SIT (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Additional state income tax withholding
- **Used By**: TAXCALC to calculate state tax
- **Example**: $0.00

##### EMP-TAX-BLOCKED-IND (10 Level)
- **Type**: PIC X
- **Description**: Tax blocked indicator
- **Valid Values**: 'Y' (Yes), 'N' (No)
- **Condition Names**: EMP-TAX-BLOCKED-YES, EMP-TAX-BLOCKED-NO
- **Used By**: Not actively used in current calculation logic
- **Example**: 'N'

#### EMP-DEDUCTION-INFO (05 Level - Group Item)

##### EMP-HEALTH-PLAN-CODE (10 Level)
- **Type**: PIC X(3)
- **Description**: Health insurance plan code
- **Valid Values**: Plan codes from DEDUCFILE or spaces/zeros
- **Used By**: DEDCALC to calculate health insurance deduction
- **Example**: 'HMO'

##### EMP-HEALTH-DEDUCTION (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Health insurance deduction amount
- **Used By**: DEDCALC to calculate health insurance deduction
- **Example**: $125.00

##### EMP-DENTAL-PLAN-CODE (10 Level)
- **Type**: PIC X(3)
- **Description**: Dental insurance plan code
- **Valid Values**: Plan codes from DEDUCFILE or spaces/zeros
- **Used By**: DEDCALC to calculate dental insurance deduction
- **Example**: 'DEN'

##### EMP-DENTAL-DEDUCTION (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Dental insurance deduction amount
- **Used By**: DEDCALC to calculate dental insurance deduction
- **Example**: $20.00

##### EMP-VISION-PLAN-CODE (10 Level)
- **Type**: PIC X(3)
- **Description**: Vision insurance plan code
- **Valid Values**: Plan codes from DEDUCFILE or spaces/zeros
- **Used By**: DEDCALC to calculate vision insurance deduction
- **Example**: 'VIS'

##### EMP-VISION-DEDUCTION (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Vision insurance deduction amount
- **Used By**: DEDCALC to calculate vision insurance deduction
- **Example**: $10.00

##### EMP-401K-IND (10 Level)
- **Type**: PIC X
- **Description**: 401(k) participation indicator
- **Valid Values**: 'Y' (Yes), 'N' (No)
- **Condition Names**: EMP-401K-YES, EMP-401K-NO
- **Used By**: DEDCALC to calculate retirement deduction
- **Example**: 'Y'

##### EMP-401K-PERCENT (10 Level)
- **Type**: PIC 9(2)V99 COMP-3
- **Description**: 401(k) contribution percentage
- **Valid Values**: 0.00 - 99.99%
- **Used By**: DEDCALC to calculate retirement deduction
- **Example**: 6.00%

##### EMP-LOAN-DEDUCTION (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Loan repayment deduction amount
- **Used By**: DEDCALC to calculate loan deduction
- **Example**: $0.00

##### EMP-GARNISH-DEDUCTION (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Garnishment deduction amount
- **Used By**: DEDCALC to calculate garnishment deduction
- **Example**: $0.00

##### EMP-CHARITY-DEDUCTION (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Charitable contribution amount
- **Used By**: DEDCALC to calculate charity deduction
- **Example**: $0.00

##### EMP-UNION-DUES (10 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Union dues amount
- **Used By**: DEDCALC to calculate union dues deduction
- **Example**: $0.00

##### EMP-ADDITIONAL-DEDUCTIONS (10 Level - Array)
- **Description**: Additional employee deductions
- **Occurs**: 5 times

###### EMP-ADD-DEDUCT-CODE (15 Level)
- **Type**: PIC X(3)
- **Description**: Additional deduction code
- **Valid Values**: Deduction codes from DEDUCFILE or spaces/zeros
- **Used By**: DEDCALC to calculate additional deductions
- **Example**: '   ' (spaces)

###### EMP-ADD-DEDUCT-AMT (15 Level)
- **Type**: PIC 9(5)V99 COMP-3
- **Description**: Additional deduction amount
- **Used By**: DEDCALC to calculate additional deductions
- **Example**: $0.00

#### EMP-YTD-AMOUNTS (05 Level - Group Item)

##### EMP-YTD-GROSS (10 Level)
- **Type**: PIC 9(8)V99 COMP-3
- **Description**: Year-to-date gross earnings
- **Used By**: TAXCALC, PAYSTUB, and updated by PAYCALC
- **Example**: $7,500.00

##### EMP-YTD-FEDERAL-TAX (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date federal tax withheld
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $250.00

##### EMP-YTD-STATE-TAX (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date state tax withheld
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $85.00

##### EMP-YTD-LOCAL-TAX (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date local tax withheld
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $35.00

##### EMP-YTD-SOCIAL-SEC (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date Social Security tax
- **Used By**: TAXCALC, PAYSTUB, and updated by PAYCALC
- **Example**: $20.00

##### EMP-YTD-MEDICARE (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date Medicare tax
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $10.00

##### EMP-YTD-401K (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date 401(k) contributions
- **Used By**: DEDCALC, PAYSTUB, and updated by PAYCALC
- **Example**: $50.00

##### EMP-YTD-HEALTH-DEDUCT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date health insurance deductions
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $0.00

##### EMP-YTD-DENTAL-DEDUCT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date dental insurance deductions
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $0.00

##### EMP-YTD-VISION-DEDUCT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date vision insurance deductions
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $0.00

##### EMP-YTD-OTHER-DEDUCT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Year-to-date other deductions
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $0.00

##### EMP-YTD-NET-PAY (10 Level)
- **Type**: PIC 9(8)V99 COMP-3
- **Description**: Year-to-date net pay
- **Used By**: PAYSTUB and updated by PAYCALC
- **Example**: $3,000.00

#### EMP-FILLER (05 Level)
- **Type**: PIC X(50)
- **Description**: Reserved for future use
- **Used By**: Not used in current processing
- **Example**: Spaces

## PAYDATA - Payroll Data

The payroll data record contains variable information for each pay period, including hours worked, additional earnings, leave time, and processing metadata.

### PAYROLL-DATA-RECORD (01 Level)

#### PAY-EMPLOYEE-ID (05 Level)
- **Type**: PIC X(6)
- **Description**: Employee identifier, links to EMP-ID
- **Used By**: PAYCALC to retrieve employee record
- **Example**: '000001'

#### PAY-PERIOD-INFO (05 Level - Group Item)

##### PAY-PERIOD-ID (10 Level)
- **Type**: PIC 9(6)
- **Description**: Unique identifier for the pay period
- **Used By**: PAYSTUB for pay stub generation
- **Example**: 000001

##### PAY-PERIOD-START-DATE (10 Level - Group Item)
- **Description**: Pay period start date

###### PAY-START-YEAR (15 Level)
- **Type**: PIC 9(4)
- **Description**: Start year
- **Example**: 2025

###### PAY-START-MONTH (15 Level)
- **Type**: PIC 9(2)
- **Description**: Start month
- **Example**: 04

###### PAY-START-DAY (15 Level)
- **Type**: PIC 9(2)
- **Description**: Start day
- **Example**: 01

##### PAY-PERIOD-END-DATE (10 Level - Group Item)
- **Description**: Pay period end date

###### PAY-END-YEAR (15 Level)
- **Type**: PIC 9(4)
- **Description**: End year
- **Example**: 2025

###### PAY-END-MONTH (15 Level)
- **Type**: PIC 9(2)
- **Description**: End month
- **Example**: 04

###### PAY-END-DAY (15 Level)
- **Type**: PIC 9(2)
- **Description**: End day
- **Example**: 15

##### PAY-CHECK-DATE (10 Level - Group Item)
- **Description**: Pay check date

###### PAY-CHECK-YEAR (15 Level)
- **Type**: PIC 9(4)
- **Description**: Check year
- **Example**: 2025

###### PAY-CHECK-MONTH (15 Level)
- **Type**: PIC 9(2)
- **Description**: Check month
- **Example**: 04

###### PAY-CHECK-DAY (15 Level)
- **Type**: PIC 9(2)
- **Description**: Check day
- **Example**: 20

#### PAY-WORK-HOURS (05 Level - Group Item)

##### PAY-REGULAR-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Regular hours worked in pay period
- **Used By**: PAYCALC to calculate regular pay
- **Example**: 40.00 hours

##### PAY-OVERTIME-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Overtime hours worked in pay period
- **Used By**: PAYCALC to calculate overtime pay
- **Example**: 0.00 hours

##### PAY-DOUBLETIME-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Double-time hours worked in pay period
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-SHIFT-DIFF-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Hours with shift differential
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-SHIFT-DIFF-RATE (10 Level)
- **Type**: PIC 9(2)V99 COMP-3
- **Description**: Shift differential rate
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00

##### PAY-ON-CALL-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: On-call hours
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-ON-CALL-RATE (10 Level)
- **Type**: PIC 9(2)V99 COMP-3
- **Description**: On-call rate
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00

##### PAY-HOLIDAY-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Holiday hours
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-HOLIDAY-RATE (10 Level)
- **Type**: PIC 9(2)V99 COMP-3
- **Description**: Holiday pay rate multiplier
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00

#### PAY-ADDITIONAL-AMOUNTS (05 Level - Group Item)

##### PAY-BONUS-AMOUNT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Bonus payment amount
- **Used By**: PAYCALC to calculate gross pay
- **Example**: $1,000.00

##### PAY-COMMISSION-AMOUNT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Commission payment amount
- **Used By**: PAYCALC to calculate gross pay
- **Example**: $500.00

##### PAY-COMMISSION-RATE (10 Level)
- **Type**: PIC 9(2)V99 COMP-3
- **Description**: Commission rate
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00%

##### PAY-COMMISSION-SALES (10 Level)
- **Type**: PIC 9(9)V99 COMP-3
- **Description**: Sales amount for commission calculation
- **Used By**: Not actively used in current calculation logic
- **Example**: $0.00

##### PAY-TIPS-AMOUNT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Reported tips amount
- **Used By**: Not actively used in current calculation logic
- **Example**: $0.00

##### PAY-ALLOWANCE-AMOUNT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Allowance payment amount
- **Used By**: Not actively used in current calculation logic
- **Example**: $0.00

##### PAY-REIMBURSEMENT-AMT (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Reimbursement amount
- **Used By**: Not actively used in current calculation logic
- **Example**: $0.00

##### PAY-OTHER-EARNINGS (10 Level)
- **Type**: PIC 9(7)V99 COMP-3
- **Description**: Other miscellaneous earnings
- **Used By**: Not actively used in current calculation logic
- **Example**: $0.00

##### PAY-OTHER-EARNINGS-DESC (10 Level)
- **Type**: PIC X(20)
- **Description**: Description of other earnings
- **Used By**: Not actively used in current calculation logic
- **Example**: Spaces

#### PAY-LEAVE-TIME (05 Level - Group Item)

##### PAY-VACATION-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Vacation hours taken
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-SICK-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Sick hours taken
- **Used By**: Not actively used in current calculation logic
- **Example**: 8.00 hours (Employee 000004)

##### PAY-PERSONAL-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Personal hours taken
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-BEREAVEMENT-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Bereavement hours taken
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-JURY-DUTY-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Jury duty hours taken
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-FMLA-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Family and Medical Leave Act hours
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-MILITARY-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Military leave hours
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-OTHER-LEAVE-HOURS (10 Level)
- **Type**: PIC 9(3)V99 COMP-3
- **Description**: Other leave hours
- **Used By**: Not actively used in current calculation logic
- **Example**: 0.00 hours

##### PAY-OTHER-LEAVE-DESC (10 Level)
- **Type**: PIC X(20)
- **Description**: Description of other leave
- **Used By**: Not actively used in current calculation logic
- **Example**: Spaces

[Additional PAY-ADJUSTMENTS and PAY-OVERRIDE-FLAGS fields omitted for brevity]

## TAXRATES - Tax Rates

The tax rates record contains comprehensive tax information including federal and state tax brackets, FICA tax rates, local tax rates, and tax constants.

### TAX-RATES-TABLE (01 Level)

#### TAX-YEAR (05 Level)
- **Type**: PIC 9(4)
- **Description**: Tax year for the rates
- **Used By**: Not actively used in calculation logic, for reference
- **Example**: 2025

#### TAX-EFFECTIVE-DATE (05 Level - Group Item)
- **Description**: Date when tax rates become effective

##### TAX-EFF-YEAR (10 Level)
- **Type**: PIC 9(4)
- **Description**: Effective year
- **Example**: 2025

##### TAX-EFF-MONTH (10 Level)
- **Type**: PIC 9(2)
- **Description**: Effective month
- **Example**: 01

##### TAX-EFF-DAY (10 Level)
- **Type**: PIC 9(2)
- **Description**: Effective day
- **Example**: 01

#### TAX-EXPIRATION-DATE (05 Level - Group Item)
- **Description**: Date when tax rates expire

##### TAX-EXP-YEAR (10 Level)
- **Type**: PIC 9(4)
- **Description**: Expiration year
- **Example**: 2025

##### TAX-EXP-MONTH (10 Level)
- **Type**: PIC 9(2)
- **Description**: Expiration month
- **Example**: 12

##### TAX-EXP-DAY (10 Level)
- **Type**: PIC 9(2)
- **Description**: Expiration day
- **Example**: 31

#### FEDERAL-TAX-BRACKETS (05 Level - Group Item)

##### FED-FILING-STATUS (10 Level)
- **Type**: PIC X
- **Description**: Federal filing status for brackets
- **Valid Values**: 'S' (Single), 'M' (Married), 'H' (Head of Household)
- **Condition Names**: FED-SINGLE, FED-MARRIED, FED-HEAD-HOUSEHOLD
- **Used By**: TAXCALC to select appropriate tax brackets
- **Example**: 'S'

##### FED-TAX-BRACKET (10 Level - Array)
- **Description**: Federal tax bracket information
- **Occurs**: 7 times

###### FED-BRACKET-FLOOR (15 Level)
- **Type**: PIC 9(8)V99 COMP-3
- **Description**: Lower income bound for bracket
- **Used By**: TAXCALC to determine applicable tax bracket
- **Example**: $0.00 (first bracket)

###### FED-BRACKET-CEILING (15 Level)
- **Type**: PIC 9(8)V99 COMP-3
- **Description**: Upper income bound for bracket
- **Used By**: TAXCALC to determine applicable tax bracket
- **Example**: $10,700.00 (first bracket)

###### FED-BRACKET-RATE (15 Level)
- **Type**: PIC 9(2)V99 COMP-3
- **Description**: Tax rate for the bracket
- **Used By**: TAXCALC to calculate federal tax
- **Example**: 10.00% (first bracket)

###### FED-BRACKET-BASE-TAX (15 Level)
- **Type**: PIC 9(8)V99 COMP-3
- **Description**: Base tax for the bracket
- **Used By**: TAXCALC to calculate federal tax
- **Example**: $0.00 (first bracket)

[Additional fields for STATE-TAX-TABLE, FICA-TAXES, LOCAL-TAX-TABLE, and TAX-CONSTANTS omitted for brevity]

## DEDUCFILE - Deduction Types

The deduction type record defines various deduction options including health, dental, vision, retirement, and other deductions.

### DEDUCTION-TYPE-RECORD (01 Level)

#### DEDUCT-CODE (05 Level)
- **Type**: PIC X(3)
- **Description**: Unique code for the deduction type
- **Used By**: DEDCALC to match with employee elections
- **Example**: 'HMO'

#### DEDUCT-NAME (05 Level)
- **Type**: PIC X(20)
- **Description**: Short name for the deduction
- **Used By**: Not actively used in calculation logic
- **Example**: 'BASIC HMO PLAN'

#### DEDUCT-DESCRIPTION (05 Level)
- **Type**: PIC X(50)
- **Description**: Detailed description of the deduction
- **Used By**: Not actively used in calculation logic
- **Example**: 'HEALTH INSURANCE PLAN - BASIC COVERAGE'

#### DEDUCT-CATEGORY (05 Level)
- **Type**: PIC X(2)
- **Description**: Category code for the deduction
- **Valid Values**: 'HI' (Health), 'DI' (Dental), 'VI' (Vision), 'LI' (Life), 'RT' (Retirement), etc.
- **Condition Names**: Various (DEDUCT-HEALTH-INS, DEDUCT-DENTAL-INS, etc.)
- **Used By**: DEDCALC to determine deduction handling
- **Example**: 'HI'

#### DEDUCT-TAX-STATUS (05 Level)
- **Type**: PIC X
- **Description**: Tax status of the deduction
- **Valid Values**: 'P' (Pre-tax), 'T' (Post-tax)
- **Condition Names**: DEDUCT-PRE-TAX, DEDUCT-POST-TAX
- **Used By**: DEDCALC to determine tax treatment
- **Example**: 'P'

#### DEDUCT-CALCULATION-METHOD (05 Level)
- **Type**: PIC X
- **Description**: Method used to calculate the deduction
- **Valid Values**: 'F' (Flat amount), 'P' (Percentage), 'H' (Hourly), 'G' (Graduated)
- **Condition Names**: Various (DEDUCT-FLAT-AMOUNT, DEDUCT-PERCENTAGE, etc.)
- **Used By**: DEDCALC to determine calculation method
- **Example**: 'F'

[Additional fields for DEDUCT-CALCULATION-PARAMS, DEDUCT-LIMIT-PARAMS, and other deduction attributes omitted for brevity]

## Cross-Reference of Data Elements

### Employee Identification

- **EMP-ID** in EMPFILE.cpy
- **PAY-EMPLOYEE-ID** in PAYDATA.cpy

### Pay Rate Information

- **EMP-HOURLY-RATE** in EMPFILE.cpy
- **EMP-SALARY-AMOUNT** in EMPFILE.cpy
- **EMP-OVERTIME-RATE** in EMPFILE.cpy

### Tax Information

- **EMP-FEDERAL-FILING-STATUS** in EMPFILE.cpy
- **EMP-STATE-FILING-STATUS** in EMPFILE.cpy
- **FED-FILING-STATUS** in TAXRATES.cpy
- **STATE-FILING-STATUS** in TAXRATES.cpy

### Deduction Information

- **EMP-HEALTH-PLAN-CODE** in EMPFILE.cpy
- **DEDUCT-CODE** in DEDUCFILE.cpy

## Special Values and Flags

### Status Indicators

- **EMP-STATUS**: 'A' (Active), 'T' (Terminated), 'L' (Leave), 'R' (Retired)
- **DEDUCT-STATUS**: 'A' (Active), 'I' (Inactive), 'P' (Pending)
- **PAY-RECORD-STATUS**: 'P' (Pending), 'A' (Approved), 'C' (Processed), 'E' (Error)

### Yes/No Flags

- **EMP-DIRECT-DEPOSIT-IND**: 'Y' (Yes), 'N' (No)
- **EMP-TAX-BLOCKED-IND**: 'Y' (Yes), 'N' (No)
- **EMP-401K-IND**: 'Y' (Yes), 'N' (No)
- **DEDUCT-REQUIRED-FLAG**: 'Y' (Yes), 'N' (No)
- **STATE-HAS-INCOME-TAX**: 'Y' (Yes), 'N' (No)

### Special Values

- **EMP-TERM-DATE**: ZEROS if not terminated
- **DEDUCT-CODE**: Spaces or '000' if no deduction
- **FED-BRACKET-CEILING**: 0 for highest bracket (no ceiling)