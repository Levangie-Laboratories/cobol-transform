# COBOL Payroll Processing System

## Overview

The COBOL Payroll Processing System is a comprehensive batch processing application designed to calculate employee salaries, taxes, deductions, and generate pay stubs. It processes employee and payroll data for each pay period, performs various calculations, and produces output files including pay stubs and summary reports.

This system demonstrates traditional COBOL business application development with a focus on modular design, separation of concerns, and comprehensive error handling.

## Features

- **Employee Data Management**: Maintains employee master records with personal information, employment details, pay rates, and deduction elections.

- **Payroll Calculation**: Processes pay period data to calculate gross pay based on hours worked or salary.

- **Tax Calculation**: Computes federal, state, and local income taxes, plus Social Security and Medicare taxes based on appropriate rates and brackets.

- **Deduction Processing**: Handles various employee deductions including health insurance, retirement plans, garnishments, and voluntary deductions.

- **Pay Stub Generation**: Produces formatted pay stubs showing earnings, taxes, deductions, and year-to-date totals.

- **Payroll Reporting**: Generates summary reports of payroll processing results.

## System Requirements

- COBOL compiler (GnuCOBOL 2.2 or later recommended)
- Support for indexed files (ISAM)
- Standard file I/O capabilities

## Installation and Setup

1. Install a COBOL compiler if needed:
   ```bash
   # Ubuntu/Debian
   sudo apt-get install gnucobol
   
   # Fedora/RHEL/CentOS
   sudo dnf install gnucobol
   
   # MacOS (using Homebrew)
   brew install gnu-cobol
   ```

2. Clone or extract the payroll system files to your preferred location:
   ```bash
   git clone https://github.com/yourorganization/cobol-payroll.git
   cd cobol-payroll
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

4. Ensure the data files are in the expected location:
   ```bash
   # Create output directory if it doesn't exist
   mkdir -p data/output
   ```

## Quick Start Guide

1. Prepare your input data files (or use the provided sample data):
   - EMPFILE.dat - Employee master records
   - TAXRATES.dat - Tax rates and brackets
   - DEDUCFILE.dat - Deduction types and parameters
   - PAYDATA.dat - Pay period data with hours, earnings, etc.

2. Run the payroll program:
   ```bash
   cd bin
   ./PAYCALC
   ```

3. Review the output files:
   - PAYSTUBS - Pay stubs for each employee
   - PAYRPT - Payroll summary report
   - ERRORLOG - Error messages and exceptions

## Directory Structure

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
├── docs/            # Documentation
└── bin/             # Compiled executables
```

## Documentation

Comprehensive documentation is available to help you understand, use, and modify the system:

- [Technical Reference Guide](docs/TECHNICAL-REFERENCE-GUIDE.md): Detailed explanation of system architecture, data structures, process flows, and implementation details

- [Developer Onboarding Guide](docs/DEVELOPER-ONBOARDING-GUIDE.md): Practical guide for setting up the development environment, navigating the codebase, and performing common development tasks

- [Data Dictionary](docs/DATA-DICTIONARY.md): Comprehensive field-by-field reference for all data structures

- [Sample Data Guide](docs/SAMPLE-DATA-GUIDE.md): Explanation of the sample data files provided with the system

- [User Guide](docs/USER-GUIDE.md): Guide for system users explaining how to use the system

## System Architecture

The system follows a modular architecture with clear separation of concerns:

1. **PAYCALC** (Main Program):
   - Orchestrates the entire payroll process
   - Handles file I/O for all data files
   - Manages the processing flow and error handling

2. **TAXCALC** (Tax Calculation Module):
   - Calculates federal, state, and local income taxes
   - Computes Social Security and Medicare taxes
   - Handles tax brackets, rates, and special rules

3. **DEDCALC** (Deduction Calculation Module):
   - Processes various employee benefit deductions
   - Handles pre-tax and post-tax deductions
   - Applies appropriate calculation methods and limitations

4. **PAYSTUB** (Pay Stub Generation Module):
   - Formats and generates employee pay stubs
   - Organizes earnings, taxes, and deductions information
   - Creates summary sections and year-to-date totals

## License

[Specify license information here]

---

This project demonstrates classic COBOL development principles and serves as a reference implementation for batch processing payroll systems.

For detailed information about using, modifying, or extending the system, please refer to the documentation in the docs directory.