# COBOL Payroll Processing System

## System Overview

The COBOL Payroll Processing System is a comprehensive solution for calculating employee salaries, taxes, deductions, and generating pay stubs. This system is designed to demonstrate classic COBOL programming techniques for business data processing.

## System Architecture

The system consists of several modular COBOL programs that work together to process payroll data:

### Directory Structure

```
/
├── src/         # COBOL source code files
├── data/        # Data file definitions and sample data
├── docs/        # System documentation
└── test/        # Test scripts and validation procedures
```

### Core Components

1. **PAYCALC** - Main payroll calculation program
   - Reads employee master file and payroll input data
   - Calls tax and deduction calculation modules
   - Writes to output files for pay stubs and reports

2. **TAXCALC** - Tax calculation module
   - Calculates income tax based on salary and tax brackets
   - Calculates social security and medicare contributions
   - Returns tax amounts to the main program

3. **DEDCALC** - Deductions calculation module
   - Processes employee benefit deductions (health insurance, retirement, etc.)
   - Calculates deduction amounts based on employee elections
   - Returns total deductions to the main program

4. **PAYSTUB** - Pay stub generation program
   - Formats and prints employee pay stubs
   - Shows gross pay, deductions, taxes, and net pay
   - Includes year-to-date totals

5. **PAYRPT** - Payroll report generator
   - Creates management reports for payroll processing
   - Summarizes departmental and company-wide totals
   - Provides audit trails for payroll calculations

### Data Files

1. **EMPFILE** - Employee master file
   - Contains employee information (ID, name, department, pay rate, etc.)
   - Indexed by employee ID for fast access

2. **TAXRATES** - Tax rate table
   - Contains tax brackets and rates for different income levels
   - Used by the tax calculation module

3. **DEDUCFILE** - Deduction reference file
   - Contains deduction types and rules
   - Used by the deduction calculation module

4. **PAYDATA** - Monthly payroll input data
   - Contains hours worked, overtime, and other variable inputs

## System Operation

### Processing Flow

1. The main PAYCALC program reads employee data and payroll input data
2. For each employee, PAYCALC calls TAXCALC to calculate taxes
3. PAYCALC then calls DEDCALC to process employee deductions
4. Net pay is calculated (gross pay - taxes - deductions)
5. PAYCALC calls PAYSTUB to generate pay stubs for employees
6. Finally, PAYCALC calls PAYRPT to generate management reports

### Calculation Logic

- **Gross Pay Calculation**:
  - Regular hours × regular pay rate
  - Overtime hours × overtime pay rate (1.5 × regular rate)
  - Additional bonuses or allowances

- **Tax Calculation**:
  - Federal income tax based on tax brackets
  - State income tax (if applicable)
  - Social Security tax (fixed percentage up to wage cap)
  - Medicare tax (fixed percentage on all earnings)

- **Deductions Calculation**:
  - Health insurance premiums
  - Retirement contributions
  - Other voluntary deductions (union dues, charitable contributions, etc.)

- **Net Pay Calculation**:
  - Gross Pay - Taxes - Deductions

## Using the System

### Prerequisites

- A COBOL compiler (GnuCOBOL recommended)
- Basic understanding of COBOL programming
- File system access for data files

### Running the System

1. Compile the COBOL programs:
   ```
   cobc -x -o PAYCALC PAYCALC.cbl
   cobc -m TAXCALC.cbl
   cobc -m DEDCALC.cbl
   cobc -m PAYSTUB.cbl
   cobc -m PAYRPT.cbl
   ```

2. Prepare input data files (EMPFILE, TAXRATES, DEDUCFILE, PAYDATA)

3. Execute the main program:
   ```
   ./PAYCALC
   ```

4. Review output files and reports

## Maintenance and Customization

### Adding New Deduction Types

To add new deduction types:
1. Update the DEDUCFILE data file with the new deduction code and rules
2. Modify the DEDCALC program to handle the new deduction type if special logic is required

### Updating Tax Rates

To update tax rates for a new tax year:
1. Edit the TAXRATES data file with new tax brackets and rates
2. No program changes required as long as the file format remains the same

### Adding New Employees

To add new employees to the system:
1. Add new records to the EMPFILE with employee information
2. Ensure all required fields are populated correctly

## Troubleshooting

### Common Issues

1. **File Status Errors**:
   - Check file permissions and paths
   - Verify file definitions match actual file structures

2. **Calculation Discrepancies**:
   - Review tax rates and calculation formulas
   - Verify correct deduction codes and amounts

3. **Runtime Errors**:
   - Check for syntax errors in COBOL programs
   - Verify all called modules are available

## System Limitations

- Currently supports up to 1000 employees
- Designed for monthly payroll processing
- Tax calculations simplified for demonstration purposes
- No support for mid-year tax rate changes

## Future Enhancements

- Add support for different pay frequencies (bi-weekly, weekly)
- Implement year-end tax forms generation (W-2, etc.)
- Add employee self-service portal
- Enhance reporting capabilities with more detailed reports

---

*This documentation provides an overview of the COBOL Payroll Processing System. Refer to individual program documentation for detailed information about each component.*