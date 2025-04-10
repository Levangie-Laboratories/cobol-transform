# COBOL Payroll Processing System - Technical Reference Guide

## Table of Contents

1. [Introduction](#introduction)
2. [System Architecture](#system-architecture)
3. [Data Structures and File Organization](#data-structures-and-file-organization)
4. [Process Flows and Algorithms](#process-flows-and-algorithms)
5. [Module Interface Specifications](#module-interface-specifications)
6. [Error Handling and Recovery](#error-handling-and-recovery)
7. [System Limitations and Constraints](#system-limitations-and-constraints)
8. [Appendix: Reference Information](#appendix-reference-information)

## Introduction

This Technical Reference Guide provides comprehensive technical documentation for the COBOL Payroll Processing System. It is intended for developers, system administrators, and technical staff who need to understand, maintain, or extend the system.

The COBOL Payroll Processing System is a batch processing application designed to calculate employee salaries, taxes, deductions, and generate pay stubs. It processes employee and payroll data for each pay period, calculating gross pay based on hours worked or salary, computing various taxes, applying deductions, and producing pay stubs and reports.

## System Architecture

### High-Level Architecture

The system follows a modular architecture with clear separation of concerns:

```
+------------------+      +------------------+      +------------------+
|  Input Data Files |----->|  Main Processing |----->|  Output Files    |
|  (EMPFILE,       |      |  (PAYCALC.cbl)   |      |  (PAYSTUBS,      |
|   PAYDATA, etc.) |      |                  |      |   PAYRPT, etc.)  |
+------------------+      +--------+---------+      +------------------+
                                   |
                                   | Calls
                                   v
                          +--------+---------+
                          | Processing Modules|
                          | (TAXCALC.cbl,     |
                          |  DEDCALC.cbl,     |
                          |  PAYSTUB.cbl)     |
                          +------------------+
```

### Detailed Architecture

The system consists of the following major components:

1. **PAYCALC (Main Program)**
   - Functions as the orchestrator for the entire process
   - Handles file I/O operations for all data files
   - Manages the main processing loop for employee payroll calculation
   - Coordinates calls to specialized modules
   - Implements error handling and recovery mechanisms

2. **TAXCALC (Tax Calculation Module)**
   - Calculates federal income tax using tax brackets and filing status
   - Computes state income tax based on state-specific rates
   - Calculates local taxes where applicable
   - Computes Social Security tax with wage base limitations
   - Calculates Medicare tax with additional tax for high earners
   - Receives parameters and returns calculated tax amounts via LINKAGE SECTION

3. **DEDCALC (Deduction Calculation Module)**
   - Processes health, dental, and vision insurance deductions
   - Calculates retirement plan contributions with percentage-based methods
   - Handles loan repayments, garnishments, and charitable contributions
   - Processes union dues and other voluntary deductions
   - Categorizes deductions as pre-tax or post-tax
   - Receives parameters and returns calculated deduction amounts via LINKAGE SECTION

4. **PAYSTUB (Pay Stub Generation Module)**
   - Formats employee and pay period information
   - Creates earnings section with regular, overtime, and other pay
   - Formats tax withholdings section with all tax types
   - Creates deductions section with all employee deductions
   - Generates summary totals and year-to-date information
   - Provides formatted output for printing or distribution

### Component Dependencies

- **PAYCALC** depends on all other modules and all data files
- **TAXCALC** depends on the tax rates structure (TAXRATES.cpy)
- **DEDCALC** depends on the employee record (EMPFILE.cpy) and deduction types (implicitly)
- **PAYSTUB** depends on employee record (EMPFILE.cpy) and payroll data (PAYDATA.cpy)

## Data Structures and File Organization

### File Organization

1. **EMPFILE (Employee Master File)**
   - **Organization**: Indexed
   - **Access Mode**: Dynamic
   - **Record Key**: EMP-ID
   - **Purpose**: Stores employee master information including personal data, employment details, pay rates, tax information, deduction elections, and year-to-date totals

2. **PAYDATA (Payroll Data File)**
   - **Organization**: Sequential
   - **Access Mode**: Sequential
   - **Purpose**: Contains variable payroll data for each pay period including hours worked, overtime, bonuses, and other earnings

3. **TAXRATES (Tax Rates File)**
   - **Organization**: Sequential
   - **Access Mode**: Sequential
   - **Purpose**: Stores tax brackets, rates, and parameters for federal, state, and local taxes, plus FICA taxes

4. **DEDUCFILE (Deduction Types File)**
   - **Organization**: Sequential
   - **Access Mode**: Sequential
   - **Purpose**: Contains deduction type definitions including codes, descriptions, calculation methods, and parameters

5. **Output Files**
   - **PAYSTUBS**: Sequential file containing formatted pay stub records
   - **PAYRPT**: Sequential file containing payroll summary report
   - **ERRORLOG**: Sequential file containing error messages and exceptions

### Key Data Structures

#### Employee Record (EMPFILE.cpy)

The employee record structure contains comprehensive employee information organized in a hierarchical structure:

- Employee identification (ID, name, SSN)
- Personal information (birth date, gender, marital status)
- Contact information (address, phone, email)
- Employment information (hire date, department, position)
- Pay information (pay type, rate, frequency)
- Tax information (filing status, allowances, additional withholding)
- Deduction elections (health, dental, vision, 401k, etc.)
- Year-to-date totals (gross, taxes, deductions, net)

Notable implementation details:
- Uses COMP-3 packed-decimal format for numeric fields to optimize storage
- Employs level-88 condition names for status fields and flags
- Includes array elements for multiple deductions

#### Payroll Data Record (PAYDATA.cpy)

The payroll data record contains variable data for each pay period:

- Employee ID and pay period identification
- Pay period date ranges (start date, end date, check date)
- Work hours (regular, overtime, double-time, shift differential)
- Additional amounts (bonus, commission, tips, allowances)
- Leave time (vacation, sick, personal, etc.)
- Adjustments and overrides
- Status flags and processing metadata

#### Tax Rates Structure (TAXRATES.cpy)

The tax rates structure contains comprehensive tax information:

- Tax year and effective/expiration dates
- Federal tax brackets for different filing statuses
- State tax information with brackets for multiple states
- Social Security and Medicare tax rates and parameters
- Local tax rates for various localities
- Tax constants (standard deduction, personal exemption, etc.)

Notable implementation details:
- Uses OCCURS clauses for arrays of tax brackets
- Supports multiple filing statuses and states
- Includes flags for states with no income tax

#### Deduction Type Record (DEDUCFILE.cpy)

The deduction type record structure defines various deduction options:

- Deduction identification (code, name, description)
- Categorization (health, dental, retirement, etc.)
- Tax status (pre-tax or post-tax)
- Calculation method (flat amount, percentage, etc.)
- Calculation parameters (amounts, rates, ranges)
- Limit parameters (maximum amounts, percentages)
- Frequency and priority information
- Effective dates and status flags

## Process Flows and Algorithms

### Main Processing Flow

The main processing flow in PAYCALC.cbl follows this sequence:

1. **Initialization (100-INITIALIZATION)**
   - Open all files with error checking
   - Load tax tables (110-LOAD-TAX-TABLES)
   - Load deduction tables (120-LOAD-DEDUCTION-TABLES)
   - Initialize counters and work areas

2. **Main Processing Loop**
   - Read payroll data records until end-of-file
   - For each record, process employee payroll

3. **Employee Payroll Processing (300-PROCESS-EMPLOYEE-PAYROLL)**
   - Read employee record (310-READ-EMPLOYEE-RECORD)
   - Calculate gross pay (320-CALCULATE-GROSS-PAY)
   - Calculate taxes (330-CALCULATE-TAXES)
   - Calculate deductions (340-CALCULATE-DEDUCTIONS)
   - Calculate net pay (350-CALCULATE-NET-PAY)
   - Generate pay stub (360-GENERATE-PAY-STUB)
   - Update employee record (370-UPDATE-EMPLOYEE-RECORD)

4. **Wrap-Up (900-WRAP-UP)**
   - Generate summary reports (910-GENERATE-SUMMARY-REPORT)
   - Close all files
   - Display processing statistics

### Key Algorithms

#### Gross Pay Calculation (320-CALCULATE-GROSS-PAY)

```
IF employee is hourly (EMP-HOURLY)
  COMPUTE Regular Pay = Regular Hours × Hourly Rate
  COMPUTE Overtime Pay = Overtime Hours × Hourly Rate × Overtime Rate
ELSE IF employee is salaried (EMP-SALARY)
  IF monthly pay frequency (EMP-MONTHLY)
    Regular Pay = Monthly Salary Amount
  ELSE IF bi-weekly pay frequency (EMP-BIWEEKLY)
    COMPUTE Regular Pay = Annual Salary / 26
  ELSE IF weekly pay frequency (EMP-WEEKLY)
    COMPUTE Regular Pay = Annual Salary / 52
END-IF

Move bonus/commission amounts to Other Pay
COMPUTE Gross Pay = Regular Pay + Overtime Pay + Other Pay
```

#### Federal Tax Calculation (TAXCALC.cbl)

```
1. Annualize gross pay by multiplying by 24 (bi-weekly) or appropriate factor
2. Reduce by allowances (Tax allowances × Personal exemption amount)
3. Find applicable tax bracket based on filing status
4. Calculate tax using bracket rate and base tax amount:
   - Start with base tax amount for the bracket
   - Calculate additional tax for income within bracket:
     (Taxable income - Bracket floor) × Bracket rate
5. Add additional withholding requested
6. Prorate annual tax to pay period by dividing by 24 (or appropriate factor)
```

#### Retirement Plan Deduction (DEDCALC.cbl)

```
IF employee has 401k (EMP-401K-YES)
  COMPUTE contribution = Gross pay × (401k percentage / 100)
  Check against annual limit ($19,500):
    - Calculate remaining allowed contribution
    - If calculated contribution > remaining allowed, adjust down
  Add to pre-tax total
END-IF
```

#### Net Pay Calculation (350-CALCULATE-NET-PAY)

```
COMPUTE Net Pay = Gross Pay - Total Taxes - Total Deductions
IF Net Pay < 0
  MOVE 0 to Net Pay
  Log error condition
END-IF
```

## Module Interface Specifications

### TAXCALC Module Interface

**Purpose**: Calculate tax withholdings based on gross pay, filing status, and tax parameters

**Input Parameters**:
- TAX-CALC-GROSS: Current period gross earnings (PIC 9(7)V99 COMP-3)
- TAX-CALC-YTD-GROSS: Year-to-date gross earnings (PIC 9(8)V99 COMP-3)
- TAX-FILING-STATUS: Federal filing status - S/M/H (PIC X)
- TAX-STATE-CODE: State code for state tax calculation (PIC X(2))
- TAX-ALLOWANCES: Number of tax allowances/exemptions (PIC 9(2) COMP-3)
- TAX-ADDITIONAL: Additional tax withholding requested (PIC 9(5)V99 COMP-3)
- TAX-RATES: Tax rates table with brackets and rates (01 TAX-RATES)

**Output Parameters**:
- FEDERAL-TAX: Calculated federal income tax (PIC 9(7)V99 COMP-3)
- STATE-TAX: Calculated state income tax (PIC 9(7)V99 COMP-3)
- LOCAL-TAX: Calculated local tax (PIC 9(7)V99 COMP-3)
- SOCIAL-SEC-TAX: Calculated Social Security tax (PIC 9(7)V99 COMP-3)
- MEDICARE-TAX: Calculated Medicare tax (PIC 9(7)V99 COMP-3)

**Function**: Tax calculation module receives gross pay and tax parameters, accesses tax rate tables, calculates various tax types, and returns the calculated amounts to the calling program.

### DEDCALC Module Interface

**Purpose**: Calculate employee deductions based on gross pay and employee benefit elections

**Input Parameters**:
- DEDUCT-CALC-GROSS: Current period gross earnings (PIC 9(7)V99 COMP-3)
- EMPLOYEE-RECORD: Employee master record with deduction info (01 EMPLOYEE-RECORD)

**Output Parameters**:
- TOTAL-DEDUCTIONS: Calculated total deductions (PIC 9(7)V99 COMP-3)

**Function**: Deduction calculation module receives gross pay and employee record, processes all applicable deductions based on employee elections, and returns the total deduction amount to the calling program.

### PAYSTUB Module Interface

**Purpose**: Generate formatted pay stub output

**Input Parameters**:
- EMPLOYEE-RECORD: Employee master record with personal info (01 EMPLOYEE-RECORD)
- PAYROLL-DATA: Pay period data with hours, dates, etc. (01 PAYROLL-DATA)
- GROSS-PAY: Calculated gross pay (PIC 9(7)V99 COMP-3)
- REGULAR-PAY: Regular hours pay component (PIC 9(7)V99 COMP-3)
- OVERTIME-PAY: Overtime pay component (PIC 9(7)V99 COMP-3)
- OTHER-PAY: Other earnings component (PIC 9(7)V99 COMP-3)
- FEDERAL-TAX: Calculated federal income tax (PIC 9(7)V99 COMP-3)
- STATE-TAX: Calculated state income tax (PIC 9(7)V99 COMP-3)
- LOCAL-TAX: Calculated local tax (PIC 9(7)V99 COMP-3)
- SOCIAL-SEC-TAX: Calculated Social Security tax (PIC 9(7)V99 COMP-3)
- MEDICARE-TAX: Calculated Medicare tax (PIC 9(7)V99 COMP-3)
- TOTAL-DEDUCTIONS: Calculated total deductions (PIC 9(7)V99 COMP-3)
- NET-PAY: Calculated net pay (PIC 9(7)V99 COMP-3)

**Output Parameters**:
- PAYSTUB-RECORD: Formatted pay stub line for output (PIC X(132))

**Function**: Pay stub generation module receives all employee and payment information, formats it into a readable pay stub layout, and returns the formatted lines to the calling program.

## Error Handling and Recovery

The system implements comprehensive error handling mechanisms:

### File Error Handling

After each file operation, the system checks the file status code:

```cobol
OPEN INPUT EMPLOYEE-FILE
IF NOT EMP-FILE-SUCCESS
    DISPLAY 'ERROR OPENING EMPLOYEE FILE: ' EMP-FILE-STATUS
    PERFORM 950-ABNORMAL-TERMINATION
END-IF
```

Status codes are defined with level-88 condition names for clarity:

```cobol
05  EMP-FILE-STATUS            PIC X(2).
    88  EMP-FILE-SUCCESS       VALUE '00'.
    88  EMP-FILE-EOF           VALUE '10'.
```

### Data Validation

The system validates data during processing:

- Verifies employee records exist and are active
- Checks for negative net pay and adjusts to zero if needed
- Validates calculation results are within expected ranges

### Error Logging

Errors are logged to the ERRORLOG file for later analysis:

```cobol
800-LOG-ERROR.
    MOVE 'Y' TO WS-ERROR-FLAG
    ADD 1 TO WS-ERROR-COUNT
    WRITE ERROR-LOG-RECORD FROM WS-ERROR-MESSAGE
    DISPLAY 'ERROR: ' WS-ERROR-TEXT
```

### Recovery Mechanisms

The system implements several recovery mechanisms:

1. **Non-fatal errors**: Processing continues with the next employee
2. **Fatal errors**: Controlled termination with 950-ABNORMAL-TERMINATION
3. **Clean-up operations**: All open files are properly closed during termination

## System Limitations and Constraints

1. **File Size Limitations**:
   - The system can process up to 99,999 employees (based on counter field size)
   - The tax rate structure supports up to 50 states and 100 localities

2. **Processing Limitations**:
   - Designed for batch processing rather than real-time operations
   - Optimized for bi-weekly payroll processing (monthly and weekly supported)

3. **Calculation Constraints**:
   - Federal tax brackets limited to 7 per filing status
   - State tax brackets limited to 5 per filing status
   - Additional deductions limited to 5 per employee

4. **Technical Constraints**:
   - Requires indexed file support (for EMPFILE)
   - Uses fixed-length records throughout the system
   - Developed for COBOL compiler compatibility (GnuCOBOL 2.2 or later recommended)

## Appendix: Reference Information

### File Status Codes

- **00**: Operation completed successfully
- **10**: End of file reached
- **22**: Duplicate key detected
- **23**: Record not found
- **30**: Permanent I/O error
- **35**: File not found
- **39**: File attribute conflict
- **93**: Resource not available

### Data Type Formats

- **PIC X**: Alphanumeric characters
- **PIC 9**: Numeric digits (display format)
- **PIC 9 COMP-3**: Packed-decimal format (for storage efficiency)
- **Level-88**: Condition names for status flags and validation

### Coding Conventions

- Paragraph naming: Three-digit prefix for hierarchical organization
- Variable naming: Two or three character prefix indicating purpose
  - WS-: Working Storage variables
  - EMP-: Employee record fields
  - PAY-: Payroll data fields
  - TAX-: Tax-related fields
- Group items use hierarchical level numbers (01, 05, 10, 15)
- All calculations use COMPUTE verb for clarity

### Key Constants

- **Social Security Rate**: 6.2%
- **Social Security Wage Base**: $142,800 (2025)
- **Medicare Rate**: 1.45%
- **Additional Medicare Rate**: 0.9% (above $200,000)
- **401(k) Annual Limit**: $19,500 (2025)