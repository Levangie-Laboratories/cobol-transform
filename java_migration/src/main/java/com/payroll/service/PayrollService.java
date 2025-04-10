package com.payroll.service;

import com.payroll.domain.Employee;
import com.payroll.domain.PayrollData;
import com.payroll.service.DeductionCalculationService.DeductionResult;
import com.payroll.service.PayStubService.PayStub;
import com.payroll.service.TaxCalculationService.TaxResult;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * Service interface for payroll processing.
 * This corresponds to the functionality in PAYCALC.cbl from the original COBOL system.
 * Acts as the main orchestrator of the payroll process.
 */
public interface PayrollService {

    /**
     * Represents a summary of payroll processing results.
     */
    class PayrollSummary {
        private LocalDate payPeriodStartDate;
        private LocalDate payPeriodEndDate;
        private LocalDate processDate;
        private int employeeCount;
        private BigDecimal totalGrossPay;
        private BigDecimal totalRegularPay;
        private BigDecimal totalOvertimePay;
        private BigDecimal totalOtherPay;
        private BigDecimal totalFederalTax;
        private BigDecimal totalStateTax;
        private BigDecimal totalLocalTax;
        private BigDecimal totalSocialSecurityTax;
        private BigDecimal totalMedicareTax;
        private BigDecimal totalTaxes;
        private BigDecimal totalDeductions;
        private BigDecimal totalNetPay;
        
        // Getters and setters for all fields would be here
        // Omitted for brevity
        
        /**
         * Adds an employee's payroll results to the summary totals.
         * 
         * @param grossPay The employee's gross pay
         * @param regularPay The employee's regular pay
         * @param overtimePay The employee's overtime pay
         * @param otherPay The employee's other pay
         * @param taxResult The employee's tax calculation results
         * @param deductionResult The employee's deduction calculation results
         * @param netPay The employee's net pay
         */
        public void addEmployeeResults(
                BigDecimal grossPay, BigDecimal regularPay, 
                BigDecimal overtimePay, BigDecimal otherPay,
                TaxResult taxResult, DeductionResult deductionResult, 
                BigDecimal netPay) {
            
            // Increment employee count
            employeeCount++;
            
            // Add to pay totals
            if (grossPay != null) {
                totalGrossPay = totalGrossPay.add(grossPay);
            }
            if (regularPay != null) {
                totalRegularPay = totalRegularPay.add(regularPay);
            }
            if (overtimePay != null) {
                totalOvertimePay = totalOvertimePay.add(overtimePay);
            }
            if (otherPay != null) {
                totalOtherPay = totalOtherPay.add(otherPay);
            }
            
            // Add to tax totals
            if (taxResult != null) {
                if (taxResult.getFederalTax() != null) {
                    totalFederalTax = totalFederalTax.add(taxResult.getFederalTax());
                }
                if (taxResult.getStateTax() != null) {
                    totalStateTax = totalStateTax.add(taxResult.getStateTax());
                }
                if (taxResult.getLocalTax() != null) {
                    totalLocalTax = totalLocalTax.add(taxResult.getLocalTax());
                }
                if (taxResult.getSocialSecurityTax() != null) {
                    totalSocialSecurityTax = totalSocialSecurityTax.add(taxResult.getSocialSecurityTax());
                }
                if (taxResult.getMedicareTax() != null) {
                    totalMedicareTax = totalMedicareTax.add(taxResult.getMedicareTax());
                }
                if (taxResult.getTotalTax() != null) {
                    totalTaxes = totalTaxes.add(taxResult.getTotalTax());
                }
            }
            
            // Add to deduction totals
            if (deductionResult != null && deductionResult.getTotalDeductions() != null) {
                totalDeductions = totalDeductions.add(deductionResult.getTotalDeductions());
            }
            
            // Add to net pay total
            if (netPay != null) {
                totalNetPay = totalNetPay.add(netPay);
            }
        }
    }
    
    /**
     * Process payroll for a specific pay period.
     * This is the main entry point for payroll processing.
     * 
     * @param payPeriodStartDate The start date of the pay period
     * @param payPeriodEndDate The end date of the pay period
     * @return A summary of the payroll processing results
     */
    PayrollSummary processPayroll(LocalDate payPeriodStartDate, LocalDate payPeriodEndDate);
    
    /**
     * Process payroll for a specific employee in a specific pay period.
     * 
     * @param employee The employee
     * @param payrollData The pay period data for the employee
     * @return The pay stub generated for the employee
     */
    PayStub processEmployeePayroll(Employee employee, PayrollData payrollData);
    
    /**
     * Calculate gross pay for an employee based on their pay type and hours worked.
     * 
     * @param employee The employee
     * @param payrollData The pay period data containing hours worked
     * @return A breakdown of pay components (regular, overtime, other, total)
     */
    class PayCalculationResult {
        private BigDecimal regularPay;
        private BigDecimal overtimePay;
        private BigDecimal otherPay;
        private BigDecimal grossPay;
        
        // Getters and setters
        public BigDecimal getRegularPay() { return regularPay; }
        public void setRegularPay(BigDecimal regularPay) { this.regularPay = regularPay; }
        
        public BigDecimal getOvertimePay() { return overtimePay; }
        public void setOvertimePay(BigDecimal overtimePay) { this.overtimePay = overtimePay; }
        
        public BigDecimal getOtherPay() { return otherPay; }
        public void setOtherPay(BigDecimal otherPay) { this.otherPay = otherPay; }
        
        public BigDecimal getGrossPay() { return grossPay; }
        public void setGrossPay(BigDecimal grossPay) { this.grossPay = grossPay; }
    }
    
    /**
     * Calculate gross pay for an employee.
     * 
     * @param employee The employee
     * @param payrollData The pay period data containing hours worked
     * @return The calculated pay components
     */
    PayCalculationResult calculateGrossPay(Employee employee, PayrollData payrollData);
    
    /**
     * Update an employee's year-to-date totals after processing a pay period.
     * 
     * @param employee The employee to update
     * @param grossPay The gross pay amount
     * @param taxResult The tax calculation results
     * @param deductionResult The deduction calculation results
     * @param netPay The net pay amount
     * @return The updated employee record
     */
    Employee updateEmployeeYtdTotals(Employee employee, BigDecimal grossPay,
                                  TaxResult taxResult, DeductionResult deductionResult,
                                  BigDecimal netPay);
    
    /**
     * Generate a detailed payroll report for a specific pay period.
     * 
     * @param payPeriodStartDate The start date of the pay period
     * @param payPeriodEndDate The end date of the pay period
     * @return The report as a formatted string or document
     */
    String generatePayrollReport(LocalDate payPeriodStartDate, LocalDate payPeriodEndDate);
    
    /**
     * Generate a summary payroll report for a specific pay period.
     * 
     * @param payrollSummary The payroll summary data
     * @return The report as a formatted string or document
     */
    String generatePayrollSummaryReport(PayrollSummary payrollSummary);
}
