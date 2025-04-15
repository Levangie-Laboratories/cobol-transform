package com.payroll.service;

import com.payroll.domain.Employee;
import com.payroll.domain.PayrollData;
import com.payroll.service.DeductionCalculationService.DeductionResult;
import com.payroll.service.TaxCalculationService.TaxResult;
import lombok.Getter;
import lombok.Setter;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Map;

/**
 * Service interface for pay stub generation.
 * This corresponds to the functionality in PAYSTUB.cbl from the original COBOL system.
 */
public interface PayStubService {

    /**
     * Represents a complete pay stub with all required information.
     */
    @Getter
    @Setter
    class PayStub {
        // Pay stub identification
        private String payStubId;
        private LocalDate payDate;
        private int payPeriodId;
        private LocalDate payPeriodStartDate;
        private LocalDate payPeriodEndDate;
        
        // Employee information
        private String employeeId;
        private String employeeName;
        private String department;
        private String position;
        
        // Current pay information
        private BigDecimal regularHours;
        private BigDecimal overtimeHours;
        private BigDecimal regularRate;
        private BigDecimal overtimeRate;
        
        private BigDecimal regularPay;
        private BigDecimal overtimePay;
        private BigDecimal otherPay; // Bonuses, commissions, etc.
        private BigDecimal grossPay;
        
        // Tax information
        private BigDecimal federalTax;
        private BigDecimal stateTax;
        private BigDecimal localTax;
        private BigDecimal socialSecurityTax;
        private BigDecimal medicareTax;
        private BigDecimal totalTaxes;
        
        // Deduction information
        private BigDecimal healthInsurance;
        private BigDecimal dentalInsurance;
        private BigDecimal visionInsurance;
        private BigDecimal retirement401k;
        private BigDecimal otherDeductions;
        private Map<String, BigDecimal> detailedDeductions; // Code -> Amount
        private BigDecimal totalDeductions;
        
        // Net pay
        private BigDecimal netPay;
        
        // YTD information
        private BigDecimal ytdGross;
        private BigDecimal ytdFederalTax;
        private BigDecimal ytdStateTax;
        private BigDecimal ytdSocialSecurityTax;
        private BigDecimal ytdMedicareTax;
        private BigDecimal ytdRetirement;
        private BigDecimal ytdTotalDeductions;
        private BigDecimal ytdNetPay;
        
        // Accrual information
        private BigDecimal vacationHoursAccrued;
        private BigDecimal vacationHoursUsed;
        private BigDecimal vacationHoursBalance;
        private BigDecimal sickHoursAccrued;
        private BigDecimal sickHoursUsed;
        private BigDecimal sickHoursBalance;
        
        // Additional information for the pay stub
        private String message;
        private String companyName;
        private String companyAddress;
        
        // Constructor
        public PayStub() {}
        
        // Getters for fields used in PayStubServiceImpl
        // Getters for all fields
        public String getPayStubId() {
            return payStubId;
        }
        
        public LocalDate getPayDate() {
            return payDate;
        }
        
        public int getPayPeriodId() {
            return payPeriodId;
        }
        
        public LocalDate getPayPeriodStartDate() {
            return payPeriodStartDate;
        }
        
        public LocalDate getPayPeriodEndDate() {
            return payPeriodEndDate;
        }
        
        public String getEmployeeId() {
            return employeeId;
        }
        
        public String getEmployeeName() {
            return employeeName;
        }
        
        public String getDepartment() {
            return department;
        }
        
        public String getPosition() {
            return position;
        }
        
        public BigDecimal getRegularHours() {
            return regularHours;
        }
        
        public BigDecimal getOvertimeHours() {
            return overtimeHours;
        }
        
        public BigDecimal getRegularRate() {
            return regularRate;
        }
        
        public BigDecimal getOvertimeRate() {
            return overtimeRate;
        }
        
        public BigDecimal getRegularPay() {
            return regularPay;
        }
        
        public BigDecimal getOvertimePay() {
            return overtimePay;
        }
        
        public BigDecimal getOtherPay() {
            return otherPay;
        }
        
        public BigDecimal getGrossPay() {
            return grossPay;
        }
        
        public BigDecimal getFederalTax() {
            return federalTax;
        }
        
        public BigDecimal getStateTax() {
            return stateTax;
        }
        
        public BigDecimal getLocalTax() {
            return localTax;
        }
        
        public BigDecimal getSocialSecurityTax() {
            return socialSecurityTax;
        }
        
        public BigDecimal getMedicareTax() {
            return medicareTax;
        }
        
        public BigDecimal getTotalTaxes() {
            return totalTaxes;
        }
        
        public BigDecimal getHealthInsurance() {
            return healthInsurance;
        }
        
        public BigDecimal getDentalInsurance() {
            return dentalInsurance;
        }
        
        public BigDecimal getVisionInsurance() {
            return visionInsurance;
        }
        
        public BigDecimal getRetirement401k() {
            return retirement401k;
        }
        
        public BigDecimal getOtherDeductions() {
            return otherDeductions;
        }
        
        public Map<String, BigDecimal> getDetailedDeductions() {
            return detailedDeductions;
        }
        
        public BigDecimal getTotalDeductions() {
            return totalDeductions;
        }
        
        public BigDecimal getNetPay() {
            return netPay;
        }
        
        public BigDecimal getYtdGross() {
            return ytdGross;
        }
        
        public BigDecimal getYtdFederalTax() {
            return ytdFederalTax;
        }
        
        public BigDecimal getYtdStateTax() {
            return ytdStateTax;
        }
        
        public BigDecimal getYtdSocialSecurityTax() {
            return ytdSocialSecurityTax;
        }
        
        public BigDecimal getYtdMedicareTax() {
            return ytdMedicareTax;
        }
        
        public BigDecimal getYtdRetirement() {
            return ytdRetirement;
        }
        
        public BigDecimal getYtdTotalDeductions() {
            return ytdTotalDeductions;
        }
        
        public BigDecimal getYtdNetPay() {
            return ytdNetPay;
        }
        
        public BigDecimal getVacationHoursAccrued() {
            return vacationHoursAccrued;
        }
        
        public BigDecimal getVacationHoursUsed() {
            return vacationHoursUsed;
        }
        
        public BigDecimal getVacationHoursBalance() {
            return vacationHoursBalance;
        }
        
        public BigDecimal getSickHoursAccrued() {
            return sickHoursAccrued;
        }
        
        public BigDecimal getSickHoursUsed() {
            return sickHoursUsed;
        }
        
        public BigDecimal getSickHoursBalance() {
            return sickHoursBalance;
        }
        
        public String getMessage() {
            return message;
        }
        
        public String getCompanyName() {
            return companyName;
        }
        
        public String getCompanyAddress() {
            return companyAddress;
        }
        
        // Setters for fields used in PayStubServiceImpl
        public void setPayStubId(String payStubId) {
            this.payStubId = payStubId;
        }
        
        public void setCompanyName(String companyName) {
            this.companyName = companyName;
        }
        
        public void setCompanyAddress(String companyAddress) {
            this.companyAddress = companyAddress;
        }
        
        public void setEmployeeName(String employeeName) {
            this.employeeName = employeeName;
        }
        
        public void setEmployeeId(String employeeId) {
            this.employeeId = employeeId;
        }
    }
    
    /**
     * Generate a pay stub for an employee based on the provided information.
     * 
     * @param employee The employee
     * @param payrollData The pay period data
     * @param grossPay The gross pay amount
     * @param regularPay The regular pay amount
     * @param overtimePay The overtime pay amount
     * @param otherPay Other pay (bonuses, commissions, etc.)
     * @param taxResult The tax calculation results
     * @param deductionResult The deduction calculation results
     * @param netPay The net pay amount
     * @return A complete PayStub object
     */
    PayStub generatePayStub(Employee employee, PayrollData payrollData,
                          BigDecimal grossPay, BigDecimal regularPay,
                          BigDecimal overtimePay, BigDecimal otherPay,
                          TaxResult taxResult, DeductionResult deductionResult,
                          BigDecimal netPay);
    
    /**
     * Format a pay stub for display or printing.
     * 
     * @param payStub The pay stub to format
     * @param format The desired format (e.g., "TEXT", "HTML", "PDF")
     * @return The formatted pay stub as a String
     */
    String formatPayStub(PayStub payStub, String format);
    
    /**
     * Save a pay stub to the database for historical records.
     * 
     * @param payStub The pay stub to save
     * @return The saved pay stub with any generated IDs or timestamps
     */
    PayStub savePayStub(PayStub payStub);
    
    /**
     * Retrieve a previously generated pay stub.
     * 
     * @param payStubId The ID of the pay stub to retrieve
     * @return The requested pay stub, if found
     */
    PayStub getPayStub(String payStubId);
    
    /**
     * Retrieve all pay stubs for a specific employee.
     * 
     * @param employeeId The employee ID
     * @return A list of pay stubs for the employee
     */
    java.util.List<PayStub> getPayStubsForEmployee(String employeeId);
    
    /**
     * Generate a PDF document from a pay stub.
     * 
     * @param payStub The pay stub to convert to PDF
     * @return The PDF document as a byte array
     */
    byte[] generatePdfPayStub(PayStub payStub);
}
