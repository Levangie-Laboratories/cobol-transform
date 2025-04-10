package com.payroll.service;

import com.payroll.domain.DeductionType;
import com.payroll.domain.Employee;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * Service interface for deduction calculations.
 * This corresponds to the functionality in DEDCALC.cbl from the original COBOL system.
 */
public interface DeductionCalculationService {

    /**
     * Represents the result of deduction calculations, including all deduction types.
     */
    class DeductionResult {
        private BigDecimal healthInsurance;
        private BigDecimal dentalInsurance;
        private BigDecimal visionInsurance;
        private BigDecimal retirement401k;
        private BigDecimal loanRepayment;
        private BigDecimal garnishment;
        private BigDecimal charityContribution;
        private BigDecimal unionDues;
        private Map<String, BigDecimal> additionalDeductions; // Code -> Amount
        private BigDecimal totalPreTaxDeductions;
        private BigDecimal totalPostTaxDeductions;
        private BigDecimal totalDeductions;
        
        // Getters and setters
        public BigDecimal getHealthInsurance() { return healthInsurance; }
        public void setHealthInsurance(BigDecimal healthInsurance) { this.healthInsurance = healthInsurance; }
        
        public BigDecimal getDentalInsurance() { return dentalInsurance; }
        public void setDentalInsurance(BigDecimal dentalInsurance) { this.dentalInsurance = dentalInsurance; }
        
        public BigDecimal getVisionInsurance() { return visionInsurance; }
        public void setVisionInsurance(BigDecimal visionInsurance) { this.visionInsurance = visionInsurance; }
        
        public BigDecimal getRetirement401k() { return retirement401k; }
        public void setRetirement401k(BigDecimal retirement401k) { this.retirement401k = retirement401k; }
        
        public BigDecimal getLoanRepayment() { return loanRepayment; }
        public void setLoanRepayment(BigDecimal loanRepayment) { this.loanRepayment = loanRepayment; }
        
        public BigDecimal getGarnishment() { return garnishment; }
        public void setGarnishment(BigDecimal garnishment) { this.garnishment = garnishment; }
        
        public BigDecimal getCharityContribution() { return charityContribution; }
        public void setCharityContribution(BigDecimal charityContribution) { this.charityContribution = charityContribution; }
        
        public BigDecimal getUnionDues() { return unionDues; }
        public void setUnionDues(BigDecimal unionDues) { this.unionDues = unionDues; }
        
        public Map<String, BigDecimal> getAdditionalDeductions() { return additionalDeductions; }
        public void setAdditionalDeductions(Map<String, BigDecimal> additionalDeductions) { this.additionalDeductions = additionalDeductions; }
        
        public BigDecimal getTotalPreTaxDeductions() { return totalPreTaxDeductions; }
        public void setTotalPreTaxDeductions(BigDecimal totalPreTaxDeductions) { this.totalPreTaxDeductions = totalPreTaxDeductions; }
        
        public BigDecimal getTotalPostTaxDeductions() { return totalPostTaxDeductions; }
        public void setTotalPostTaxDeductions(BigDecimal totalPostTaxDeductions) { this.totalPostTaxDeductions = totalPostTaxDeductions; }
        
        public BigDecimal getTotalDeductions() { return totalDeductions; }
        public void setTotalDeductions(BigDecimal totalDeductions) { this.totalDeductions = totalDeductions; }
        
        /**
         * Calculates and sets the total deductions by summing pre-tax and post-tax deductions.
         */
        public void calculateTotal() {
            // Calculate total pre-tax deductions if not already set
            if (totalPreTaxDeductions == null) {
                totalPreTaxDeductions = BigDecimal.ZERO;
                // Add up all pre-tax deductions here
            }
            
            // Calculate total post-tax deductions if not already set
            if (totalPostTaxDeductions == null) {
                totalPostTaxDeductions = BigDecimal.ZERO;
                // Add up all post-tax deductions here
            }
            
            // Calculate total of all deductions
            totalDeductions = (totalPreTaxDeductions != null ? totalPreTaxDeductions : BigDecimal.ZERO)
                .add(totalPostTaxDeductions != null ? totalPostTaxDeductions : BigDecimal.ZERO);
        }
    }
    
    /**
     * Calculate all deductions for an employee based on the provided gross pay and other parameters.
     * 
     * @param employee The employee for whom deductions are being calculated
     * @param grossPay The gross pay amount for the current pay period
     * @param hours The hours worked in the current pay period (for hourly deductions)
     * @param deductionTypes The list of available deduction types
     * @param manualAdjustment Any manual adjustment to deductions
     * @return A DeductionResult object containing all calculated deduction amounts
     */
    DeductionResult calculateDeductions(Employee employee, BigDecimal grossPay, 
                                     BigDecimal hours, List<DeductionType> deductionTypes,
                                     BigDecimal manualAdjustment);
    
    /**
     * Calculate health insurance deduction for an employee.
     * 
     * @param employee The employee
     * @param deductionTypes The list of available deduction types
     * @return The calculated health insurance deduction amount
     */
    BigDecimal calculateHealthInsurance(Employee employee, List<DeductionType> deductionTypes);
    
    /**
     * Calculate dental insurance deduction for an employee.
     * 
     * @param employee The employee
     * @param deductionTypes The list of available deduction types
     * @return The calculated dental insurance deduction amount
     */
    BigDecimal calculateDentalInsurance(Employee employee, List<DeductionType> deductionTypes);
    
    /**
     * Calculate vision insurance deduction for an employee.
     * 
     * @param employee The employee
     * @param deductionTypes The list of available deduction types
     * @return The calculated vision insurance deduction amount
     */
    BigDecimal calculateVisionInsurance(Employee employee, List<DeductionType> deductionTypes);
    
    /**
     * Calculate retirement plan (401k) deduction for an employee.
     * 
     * @param employee The employee
     * @param grossPay The gross pay amount
     * @param deductionTypes The list of available deduction types
     * @return The calculated retirement deduction amount
     */
    BigDecimal calculateRetirement(Employee employee, BigDecimal grossPay, 
                                List<DeductionType> deductionTypes);
    
    /**
     * Calculate loan repayment deduction for an employee.
     * 
     * @param employee The employee
     * @param deductionTypes The list of available deduction types
     * @return The calculated loan repayment deduction amount
     */
    BigDecimal calculateLoanRepayment(Employee employee, List<DeductionType> deductionTypes);
    
    /**
     * Calculate garnishment deduction for an employee.
     * 
     * @param employee The employee
     * @param grossPay The gross pay amount
     * @param deductionTypes The list of available deduction types
     * @param garnishmentOverride Optional override amount for garnishment
     * @return The calculated garnishment deduction amount
     */
    BigDecimal calculateGarnishment(Employee employee, BigDecimal grossPay,
                                 List<DeductionType> deductionTypes,
                                 BigDecimal garnishmentOverride);
    
    /**
     * Calculate charity contribution deduction for an employee.
     * 
     * @param employee The employee
     * @param grossPay The gross pay amount
     * @param deductionTypes The list of available deduction types
     * @return The calculated charity deduction amount
     */
    BigDecimal calculateCharity(Employee employee, BigDecimal grossPay,
                             List<DeductionType> deductionTypes);
    
    /**
     * Calculate union dues deduction for an employee.
     * 
     * @param employee The employee
     * @param grossPay The gross pay amount
     * @param deductionTypes The list of available deduction types
     * @return The calculated union dues deduction amount
     */
    BigDecimal calculateUnionDues(Employee employee, BigDecimal grossPay,
                               List<DeductionType> deductionTypes);
    
    /**
     * Calculate employer-matching contributions for applicable deductions.
     * 
     * @param employee The employee
     * @param grossPay The gross pay amount
     * @param employeeDeductions The employee's deduction amounts
     * @param deductionTypes The list of available deduction types
     * @return A map of deduction codes to employer contribution amounts
     */
    Map<String, BigDecimal> calculateEmployerContributions(Employee employee, 
                                                       BigDecimal grossPay,
                                                       DeductionResult employeeDeductions,
                                                       List<DeductionType> deductionTypes);
}
