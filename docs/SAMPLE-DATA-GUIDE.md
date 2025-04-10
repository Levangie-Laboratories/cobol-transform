# COBOL Payroll Processing System - Sample Data Guide

## Overview

This guide explains the sample data files provided with the COBOL Payroll Processing System. These files are designed to demonstrate the system's functionality and provide a starting point for testing the system. Each file follows the structure defined in the corresponding copybook and contains realistic data for various payroll scenarios.

## Sample Data Files

### 1. Employee Master File (EMPFILE.dat)

The employee master file contains records for 5 sample employees with different characteristics:

- **Employee 000001 (John Smith)**: Hourly employee with health, dental, and vision insurance, and 401(k) participation.
  - Pay Rate: $18.00/hour
  - Filing Status: Married
  - Deductions: HMO health plan, basic dental and vision, 6% 401(k) contribution

- **Employee 000002 (Maria Johnson)**: Salaried employee with bi-weekly pay frequency, premium insurance plans, and 401(k) participation.
  - Salary: $95,000/year
  - Filing Status: Single
  - Deductions: PPO health plan, premium dental and vision, 8% 401(k) contribution

- **Employee 000003 (Robert Williams)**: Salaried employee with monthly pay frequency and maximum benefit elections.
  - Salary: $125,000/year
  - Filing Status: Single
  - Deductions: PPO health plan, premium dental and vision, 10% 401(k) contribution

- **Employee 000004 (Lisa Brown)**: Part-time hourly employee with basic benefits and no retirement plan.
  - Pay Rate: $15.00/hour
  - Filing Status: Married
  - Deductions: HMO health plan, basic dental and vision, no 401(k)

- **Employee 000005 (Michael Davis)**: Salaried manager with bi-weekly pay frequency and mid-level benefits.
  - Salary: $85,000/year
  - Filing Status: Single
  - Deductions: HMO health plan, moderate dental and vision, 5% 401(k) contribution

### 2. Tax Rates File (TAXRATES.dat)

The tax rates file contains tax brackets and rates for:

- **Federal Income Tax**: Brackets for Single, Married, and Head of Household filing statuses
- **State Income Tax**: Brackets for four states (CA, NY, TX, FL) with TX and FL having no state income tax
- **Social Security**: 6.2% rate with a wage base of $142,800
- **Medicare**: 1.45% basic rate plus 0.9% additional rate for earnings over $200,000
- **Local Taxes**: Sample rates for various localities
- **Tax Constants**: Standard deductions, personal exemption amounts, etc.

The tax year is set to 2025 with an effective date range of 01/01/2025 to 12/31/2025.

### 3. Deduction Types File (DEDUCFILE.dat)

The deduction types file contains various benefit plans and deduction options:

- **Health Insurance Plans**:
  - HMO Basic Plan (code: HMO): $125.00 per pay period
  - HMO Premium Plan (code: HM2): $175.00 per pay period
  - PPO Basic Plan (code: PPO): $150.00 per pay period
  - PPO Premium Plan (code: PP2): $200.00 per pay period

- **Dental Insurance Plans**:
  - Basic Dental Plan (code: DEN): $20.00 per pay period
  - Premium Dental Plan (code: DE2): $35.00 per pay period
  - Family Dental Plan (code: DE3): $40.00 per pay period

- **Vision Insurance Plans**:
  - Basic Vision Plan (code: VIS): $10.00 per pay period
  - Premium Vision Plan (code: VI2): $15.00 per pay period
  - Family Vision Plan (code: VI3): $25.00 per pay period

- **Retirement Plan**:
  - 401(k) Plan (code: 401): Percentage-based contribution

- **Other Deductions**:
  - Loan Repayment (code: LON)
  - Garnishment (code: GRN)
  - Charitable Contribution (code: CHR)
  - Union Dues (code: UNN)
  - Flexible Spending Accounts (codes: P01, P02)
  - Commuter Benefits (codes: T01, T02)

### 4. Payroll Data File (PAYDATA.dat)

The payroll data file contains records for the current pay period (04/01/2025 - 04/15/2025) with:

- **Employee 000001**: 40 hours of regular time
- **Employee 000002**: 38 hours of regular time, $1,000 bonus, $500 commission
- **Employee 000003**: 80 hours of regular time (salaried)
- **Employee 000004**: 39 hours of regular time, 2.5 hours of overtime, 8 hours of sick leave
- **Employee 000005**: 80 hours of regular time (salaried), $250 bonus

## Using the Sample Data

### Testing the System

To test the system with the sample data:

1. Ensure all data files are in the correct location (/data directory)
2. Compile the COBOL programs if needed
3. Run the main payroll program

```
cd /home/blabs/cobol_transform
./src/PAYCALC
```

4. Review the output files:
   - Pay stubs in the output directory
   - Payroll report in the output directory
   - Error log for any issues

### Modifying the Sample Data

You can modify the sample data to test different scenarios:

1. **Employee Master File**: Add new employees or modify existing ones to test different pay rates, deduction elections, etc.
2. **Tax Rates File**: Modify tax brackets and rates to test different tax scenarios.
3. **Deduction Types File**: Add new deduction types or modify existing ones to test different benefit plans.
4. **Payroll Data File**: Modify hours worked, bonuses, etc., to test different pay scenarios.

### Expected Results

When processing the sample data, you should expect:

- **Employee 000001 (Hourly)**: Gross pay of $720.00 (40 hours × $18.00)
- **Employee 000002 (Salaried)**: Gross pay of $3,653.85 ($95,000 ÷ 26) + $1,000 bonus + $500 commission
- **Employee 000003 (Salaried)**: Gross pay of $10,416.67 ($125,000 ÷ 12)
- **Employee 000004 (Hourly)**: Gross pay of $693.75 (39 hours × $15.00 + 2.5 hours × $15.00 × 1.5)
- **Employee 000005 (Salaried)**: Gross pay of $3,269.23 ($85,000 ÷ 26) + $250 bonus

Taxes and deductions will be calculated based on the rates and elections in the sample files, with net pay being the difference between gross pay, taxes, and deductions.

## Sample Data Structure

For detailed information about the structure of each data file, refer to the copybooks in the `/src/copybooks` directory:

- EMPFILE.cpy: Structure of employee master records
- TAXRATES.cpy: Structure of tax rates records
- DEDUCFILE.cpy: Structure of deduction types records
- PAYDATA.cpy: Structure of payroll data records

## Creating Your Own Data

To create your own data for testing or production use:

1. Use the sample data as templates
2. Ensure your data follows the structure defined in the copybooks
3. Place your data files in the appropriate location
4. Update any configuration parameters as needed

## Troubleshooting

If you encounter issues when testing with the sample data:

1. Check file locations and permissions
2. Verify file formats match the expected structure
3. Review the error log for specific error messages
4. Consult the User Guide and Technical Documentation for additional guidance

---

This sample data is provided for demonstration and testing purposes only. In a production environment, you would replace these files with actual employee and payroll data for your organization.