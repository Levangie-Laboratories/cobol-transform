# User Guide

## Introduction

Welcome to the Java Payroll System! This guide will help you navigate the system and perform common tasks. The system provides comprehensive payroll processing functionality including employee management, payroll calculation, tax and deduction processing, and reporting.

## Getting Started

### System Requirements

To access the Payroll System, you need:

- A modern web browser (Chrome, Firefox, Edge, or Safari)
- Authorized system credentials
- Network access to the application server

### Logging In

1. Navigate to the payroll system URL (provided by your administrator)
2. Enter your username and password
3. Click the "Log In" button

![Login Screen](../images/login_screen.png)

### Navigation

The system uses a menu-based navigation structure:

- **Dashboard**: Overview of system status and recent activities
- **Employees**: Employee management functions
- **Payroll**: Payroll processing functions
- **Reports**: Reporting functions
- **Administration**: System administration (for authorized users)

## Employee Management

### Viewing Employee Information

1. Click on the "Employees" menu
2. Use the search function to find an employee by name or ID
3. Click on an employee's name to view their details

### Adding a New Employee

1. Navigate to the "Employees" section
2. Click the "Add New Employee" button
3. Fill in the required information in the form:
   - Personal Information
   - Contact Information
   - Employment Information
   - Pay Information
   - Tax Information
   - Deduction Elections
4. Click "Save" to create the employee record

### Updating Employee Information

1. Locate the employee record (as described in "Viewing Employee Information")
2. Click the "Edit" button
3. Update the necessary fields
4. Click "Save" to update the record

### Terminating an Employee

1. Locate the employee record
2. Click the "Terminate" button
3. Enter the termination date and reason
4. Click "Confirm Termination"

## Payroll Processing

### Setting Up a Pay Period

1. Navigate to the "Payroll" section
2. Click "Set Up Pay Period"
3. Enter the pay period start and end dates
4. Select the payment date
5. Click "Create Pay Period"

### Entering Hours and Variable Pay

1. Navigate to the "Payroll" section
2. Select the active pay period
3. Click "Enter Hours/Pay"
4. Choose between:
   - "Batch Entry" (for multiple employees)
   - "Individual Entry" (for single employee)
5. Enter hours worked, overtime, bonuses, etc.
6. Click "Save"

### Processing Payroll

1. Navigate to the "Payroll" section
2. Select the pay period to process
3. Click "Process Payroll"
4. Review the pre-processing summary
5. Click "Confirm and Process" to start processing
6. Monitor the progress indicator
7. Review the processing summary when complete

### Reviewing and Approving Payroll

1. After processing, click "Review Payroll"
2. Check the payroll summary for accuracy
3. Review any exceptions or warnings
4. Make adjustments if necessary by clicking "Adjust"
5. Click "Approve Payroll" when ready

### Generating Pay Stubs

1. After approving payroll, click "Generate Pay Stubs"
2. Select the output format (PDF, HTML, or Email)
3. Click "Generate"
4. Download or distribute the pay stubs as needed

## Reports

### Standard Reports

The system includes several standard reports:

- **Payroll Summary**: Summary of a processed payroll
- **Employee Directory**: List of all employees
- **Tax Withholding**: Tax withholding details
- **Deduction Summary**: Summary of all deductions
- **YTD Summary**: Year-to-date payroll totals

To run a standard report:

1. Navigate to the "Reports" section
2. Select the desired report type
3. Set the report parameters (date range, departments, etc.)
4. Click "Generate Report"
5. View, download, or print the report

### Custom Reports

To create a custom report:

1. Navigate to the "Reports" section
2. Click "Custom Report"
3. Select the data elements to include
4. Set filtering and sorting options
5. Choose the output format
6. Click "Generate Report"

## Tax Administration

### Updating Tax Tables

1. Navigate to "Administration" > "Tax Management"
2. Click "Tax Tables"
3. Select the tax year to update
4. Choose from:
   - "Import Tax Tables" (from a file)
   - "Manual Update" (for individual rates)
5. Follow the prompts to complete the update
6. Click "Save Changes"

### Tax Forms

To generate tax forms (W-2, etc.):

1. Navigate to "Administration" > "Tax Forms"
2. Select the form type and tax year
3. Choose between generating for:
   - All employees
   - Selected employees
   - Specific departments
4. Click "Generate Forms"
5. Review and distribute as needed

## Deduction Management

### Setting Up Deduction Types

1. Navigate to "Administration" > "Deduction Management"
2. Click "Deduction Types"
3. Click "Add New Deduction Type"
4. Fill in the details:
   - Code and Description
   - Category (Health, Retirement, etc.)
   - Calculation Method
   - Tax Status (Pre-tax/Post-tax)
   - Limits and Constraints
5. Click "Save"

### Managing Employee Deductions

1. Locate the employee record
2. Click "Deductions"
3. Add or modify deduction elections
4. Set amounts or percentages as needed
5. Click "Save Changes"

## Year-End Processing

### Closing a Payroll Year

1. Navigate to "Administration" > "Year-End Processing"
2. Click "Close Payroll Year"
3. Select the year to close
4. Review the year-end checklist
5. Click "Proceed" when ready
6. Follow the step-by-step wizard

### Year-End Reports

1. Navigate to "Reports" > "Year-End Reports"
2. Select the desired reports and tax year
3. Click "Generate Reports"
4. Review and distribute as needed

## Troubleshooting

### Common Issues

#### Issue: Calculation Discrepancy

**Solution**: 
1. Verify employee pay and tax information
2. Check for manual adjustments
3. Review tax and deduction settings
4. Contact support if issue persists

#### Issue: Missing Employee

**Solution**:
1. Check employee status (terminated employees may not appear by default)
2. Verify search criteria
3. Confirm access permissions

#### Issue: Processing Error

**Solution**:
1. Check error logs (Administration > System Logs)
2. Verify all required data is complete
3. Try processing smaller batches
4. Contact support with the error code

### Getting Help

For additional support:

- Click the "Help" icon in any screen
- Email support at payroll-support@company.com
- Call the support desk at 555-123-4567

## System Administration

### User Management

1. Navigate to "Administration" > "User Management"
2. Add, modify, or deactivate user accounts
3. Assign roles and permissions

### System Configuration

1. Navigate to "Administration" > "System Configuration"
2. Configure system parameters and defaults
3. Customize system behavior

### Audit Trails

1. Navigate to "Administration" > "Audit Trails"
2. View records of system activities and changes
3. Filter by date, user, or activity type

## Conclusion

This user guide covers the most common tasks in the Payroll System. For more detailed information, please refer to the online help system or contact your system administrator.
