# COBOL Payroll Processing System - Developer Onboarding Guide

## Table of Contents

1. [Introduction](#introduction)
2. [Environment Setup](#environment-setup)
3. [Project Structure](#project-structure)
4. [Development Workflow](#development-workflow)
5. [Common Development Tasks](#common-development-tasks)
6. [Coding Standards](#coding-standards)
7. [Testing Procedures](#testing-procedures)
8. [Debugging and Troubleshooting](#debugging-and-troubleshooting)
9. [References and Resources](#references-and-resources)

## Introduction

Welcome to the COBOL Payroll Processing System development team! This guide will help you get set up and productive with the codebase as quickly as possible. It covers environment setup, development workflows, common tasks, and best practices.

The COBOL Payroll Processing System is a batch processing application designed to calculate employee salaries, taxes, deductions, and generate pay stubs. It uses a modular architecture with separate programs for different aspects of payroll processing.

## Environment Setup

### Prerequisites

To work with the COBOL Payroll Processing System, you'll need:

1. **COBOL Compiler**: GnuCOBOL 2.2 or later is recommended.
   ```bash
   # Ubuntu/Debian
   sudo apt-get install gnucobol
   
   # Fedora/RHEL/CentOS
   sudo dnf install gnucobol
   
   # MacOS (using Homebrew)
   brew install gnu-cobol
   ```

2. **ISAM Support**: The system uses indexed files, so you'll need a compiler with ISAM support.
   ```bash
   # If using D-ISAM with GnuCOBOL
   sudo apt-get install libdisam-dev
   ```

3. **Text Editor/IDE**: Any text editor with COBOL syntax highlighting is recommended.
   - Visual Studio Code with COBOL extension
   - Sublime Text with COBOL package
   - Vim/Emacs with COBOL syntax files

### Initial Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/yourorganization/cobol-payroll
   cd cobol-payroll
   ```

2. Create the necessary directories if they don't exist:
   ```bash
   mkdir -p data/output
   ```

3. Compile the COBOL programs:
   ```bash
   # Compile the main program
   cobc -x -o bin/PAYCALC src/PAYCALC.cbl
   
   # Compile modules
   cobc -m -o lib/TAXCALC src/TAXCALC.cbl
   cobc -m -o lib/DEDCALC src/DEDCALC.cbl
   cobc -m -o lib/PAYSTUB src/PAYSTUB.cbl
   ```

4. Set up environment variables:
   ```bash
   # Add this to your ~/.bashrc or ~/.zshrc
   export COB_LIBRARY_PATH=$HOME/cobol-payroll/lib
   export COB_COPY_DIR=$HOME/cobol-payroll/src/copybooks
   ```

## Project Structure

The project is organized into the following directories:

```
/
├── src/             # COBOL source code
│   ├── copybooks/   # Copybook definitions
│   │   ├── EMPFILE.cpy    # Employee record structure
│   │   ├── TAXRATES.cpy   # Tax rates structure
│   │   ├── DEDUCFILE.cpy  # Deduction types structure
│   │   └── PAYDATA.cpy    # Payroll data structure
│   ├── PAYCALC.cbl  # Main program
│   ├── TAXCALC.cbl  # Tax calculation module
│   ├── DEDCALC.cbl  # Deduction calculation module
│   └── PAYSTUB.cbl  # Pay stub generation module
├── data/            # Data files
│   ├── EMPFILE.dat  # Employee master file
│   ├── TAXRATES.dat # Tax rates file
│   ├── DEDUCFILE.dat # Deduction types file
│   ├── PAYDATA.dat  # Payroll data file
│   └── output/      # Output files (PAYSTUBS, PAYRPT, ERRORLOG)
├── bin/             # Compiled executables
├── lib/             # Compiled modules
└── docs/            # Documentation
```

### Key Files and Their Purposes

- **PAYCALC.cbl**: Main program that orchestrates the entire payroll process
- **TAXCALC.cbl**: Calculates federal, state, and local taxes, plus Social Security and Medicare
- **DEDCALC.cbl**: Processes employee benefit deductions
- **PAYSTUB.cbl**: Formats and generates employee pay stubs
- **\*.cpy**: Copybooks that define data structures used across multiple programs
- **\*.dat**: Data files containing employee, tax, deduction, and payroll information

## Development Workflow

### Typical Development Cycle

1. **Understand Requirements**: Clearly define what needs to be changed or added.

2. **Make Code Changes**: Modify the appropriate COBOL programs and copybooks.

3. **Compile Programs**:
   ```bash
   cobc -x -o bin/PAYCALC src/PAYCALC.cbl
   cobc -m -o lib/MODULE src/MODULE.cbl  # For any modules you modified
   ```

4. **Test Changes**:
   - Run with sample data
   - Verify outputs
   - Check for errors

5. **Review and Commit**: Review changes and commit to version control.

### Version Control Best Practices

- Make small, focused commits with clear messages
- Pull and merge regularly to avoid conflicts
- Use feature branches for significant changes
- Include issue/ticket numbers in commit messages

## Common Development Tasks

### Adding a New Deduction Type

1. **Update DEDUCFILE.dat**:
   - Add a new record with the deduction details
   - Include code, name, description, calculation method, etc.

2. **Modify DEDCALC.cbl** (if needed):
   - If the new deduction requires special processing, add logic to the appropriate section
   - Most standard deductions can be added without code changes

3. **Update PAYSTUB.cbl** (if needed):
   - If the deduction should appear separately on the pay stub, add formatting logic

### Modifying Tax Calculation Logic

1. **Identify the Appropriate Section**:
   - Federal tax: 100-CALCULATE-FEDERAL-TAX in TAXCALC.cbl
   - State tax: 200-CALCULATE-STATE-TAX in TAXCALC.cbl
   - Local tax: 300-CALCULATE-LOCAL-TAX in TAXCALC.cbl
   - Social Security: 400-CALCULATE-SOCIAL-SECURITY in TAXCALC.cbl
   - Medicare: 500-CALCULATE-MEDICARE in TAXCALC.cbl

2. **Modify the Calculation Logic**:
   - Update the calculation algorithm as needed
   - Be careful with computational logic to ensure accuracy

3. **Update Tax Rates** (if needed):
   - Modify TAXRATES.dat with new rates or brackets

### Adding a New Report

1. **Create a New COBOL Program**:
   - Use the existing reports as templates
   - Include appropriate file definitions and record structures

2. **Add to Main Processing**:
   - Update PAYCALC.cbl to call the new report generator
   - Add file handling for the new report output

### Updating the Employee Record Structure

1. **Modify EMPFILE.cpy**:
   - Add new fields as needed
   - Maintain level numbering consistency
   - Consider storage efficiency (COMP-3 for numeric fields)

2. **Update Programs**:
   - Any program using the employee record may need updates
   - Check for field references in all programs

3. **Update Data Files**:
   - Existing data files may need conversion for the new structure

## Coding Standards

### Naming Conventions

- **Programs**: All uppercase with descriptive names (e.g., PAYCALC, TAXCALC)
- **Paragraphs**: Three-digit prefix for hierarchy, followed by descriptive name (e.g., 100-INITIALIZATION)
- **Data Items**:
  - Prefix indicates purpose (WS-, EMP-, PAY-, etc.)
  - All uppercase with hyphens between words
  - Descriptive and clear purpose

### Code Organization

- Group related paragraphs together
- Use appropriate level numbers for data hierarchy (01, 05, 10, 15)
- Include comments for complex logic
- Keep paragraphs focused on a single task

### Documentation Practices

- Include program header with purpose, author, date
- Document non-obvious algorithms or business rules
- Add comments for complex calculations
- Keep comments updated when code changes

## Testing Procedures

### Manual Testing

1. **Setup Test Data**:
   - Use sample data files in the data directory
   - Modify as needed for your test case

2. **Run the Program**:
   ```bash
   cd bin
   ./PAYCALC
   ```

3. **Verify Results**:
   - Check output files (PAYSTUBS, PAYRPT, ERRORLOG)
   - Verify calculations are correct
   - Check for any error messages

### Sample Test Cases

1. **Regular Pay Calculation**:
   - Test with different hourly rates and hours
   - Verify gross pay calculation is correct

2. **Overtime Calculation**:
   - Test with overtime hours
   - Verify correct application of overtime rate

3. **Tax Calculation**:
   - Test with different tax situations (filing statuses, states)
   - Verify tax withholding amounts

4. **Deduction Processing**:
   - Test with various deduction elections
   - Verify pre-tax and post-tax handling

5. **Error Handling**:
   - Test with invalid or missing data
   - Verify appropriate error handling

## Debugging and Troubleshooting

### Common Issues and Solutions

1. **File Not Found Errors**:
   - Verify file paths in your environment
   - Check file permissions
   - Ensure data files exist in the expected location

2. **Compilation Errors**:
   - Check for syntax errors
   - Verify copybook paths are correct
   - Ensure all required modules are available

3. **Calculation Discrepancies**:
   - Add debugging output (DISPLAY statements)
   - Trace through the calculation step by step
   - Verify input values and intermediate results

### Debugging Techniques

1. **Add DISPLAY Statements**:
   ```cobol
   DISPLAY "DEBUG: Variable value = " VARIABLE-NAME
   ```

2. **Use Interactive Debugging**:
   If your COBOL environment supports it, use an interactive debugger to step through the code.

3. **Check File Status Codes**:
   - After file operations, check the file status code
   - Refer to the status code reference in the Technical Guide

4. **Inspect Output Files**:
   - Check the ERRORLOG file for error messages
   - Review output files for unexpected values

## References and Resources

### Internal Documentation

- [Technical Reference Guide](TECHNICAL-REFERENCE-GUIDE.md) - Detailed system architecture and implementation
- [User Guide](USER-GUIDE.md) - Guide for system users
- [Sample Data Guide](SAMPLE-DATA-GUIDE.md) - Explanation of sample data files

### External Resources

- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)
- [COBOL File Handling](https://www.ibm.com/docs/en/cobol-zos/6.3?topic=programs-handling-files)

### Contact Information

- Technical Lead: [Name] ([email])
- System Administrator: [Name] ([email])
- Documentation Maintainer: [Name] ([email])

---

This guide is intended to help you get started quickly with developing and maintaining the COBOL Payroll Processing System. For more detailed technical information, please refer to the Technical Reference Guide.
