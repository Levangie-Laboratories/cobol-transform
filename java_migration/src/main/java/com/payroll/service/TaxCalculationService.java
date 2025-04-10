package com.payroll.service;

import com.payroll.domain.Employee;
import com.payroll.domain.enums.FilingStatus;

import java.math.BigDecimal;

/**
 * Service interface for tax calculations.
 * This corresponds to the functionality in TAXCALC.cbl from the original COBOL system.
 */
public interface TaxCalculationService {

    /**
     * Represents the result of tax calculations, including all tax types.
     */
    class TaxResult {
        private BigDecimal federalTax;
        private BigDecimal stateTax;
        private BigDecimal localTax;
        private BigDecimal socialSecurityTax;
        private BigDecimal medicareTax;
        private BigDecimal totalTax;
        
        // Getters and setters
        public BigDecimal getFederalTax() { return federalTax; }
        public void setFederalTax(BigDecimal federalTax) { this.federalTax = federalTax; }
        
        public BigDecimal getStateTax() { return stateTax; }
        public void setStateTax(BigDecimal stateTax) { this.stateTax = stateTax; }
        
        public BigDecimal getLocalTax() { return localTax; }
        public void setLocalTax(BigDecimal localTax) { this.localTax = localTax; }
        
        public BigDecimal getSocialSecurityTax() { return socialSecurityTax; }
        public void setSocialSecurityTax(BigDecimal socialSecurityTax) { this.socialSecurityTax = socialSecurityTax; }
        
        public BigDecimal getMedicareTax() { return medicareTax; }
        public void setMedicareTax(BigDecimal medicareTax) { this.medicareTax = medicareTax; }
        
        public BigDecimal getTotalTax() { return totalTax; }
        public void setTotalTax(BigDecimal totalTax) { this.totalTax = totalTax; }
        
        /**
         * Calculates and sets the total tax by summing all individual tax components.
         */
        public void calculateTotal() {
            this.totalTax = BigDecimal.ZERO;
            if (federalTax != null) this.totalTax = this.totalTax.add(federalTax);
            if (stateTax != null) this.totalTax = this.totalTax.add(stateTax);
            if (localTax != null) this.totalTax = this.totalTax.add(localTax);
            if (socialSecurityTax != null) this.totalTax = this.totalTax.add(socialSecurityTax);
            if (medicareTax != null) this.totalTax = this.totalTax.add(medicareTax);
        }
    }
    
    /**
     * Calculate all taxes for an employee based on the provided gross pay and other parameters.
     * 
     * @param employee The employee for whom taxes are being calculated
     * @param grossPay The gross pay amount for the current pay period
     * @param ytdGross The year-to-date gross pay (for FICA tax wage bases)
     * @param adjustments Any manual tax adjustments
     * @param taxYear The tax year for which taxes are being calculated
     * @return A TaxResult object containing all calculated tax amounts
     */
    TaxResult calculateTaxes(Employee employee, BigDecimal grossPay, BigDecimal ytdGross,
                           BigDecimal adjustments, int taxYear);
    
    /**
     * Calculate federal income tax based on the provided parameters.
     * 
     * @param grossPay The gross pay amount
     * @param filingStatus The federal filing status (Single, Married, etc.)
     * @param allowances The number of federal allowances claimed
     * @param additionalWithholding Any additional federal withholding requested
     * @param taxYear The tax year
     * @return The calculated federal tax amount
     */
    BigDecimal calculateFederalTax(BigDecimal grossPay, FilingStatus filingStatus,
                                int allowances, BigDecimal additionalWithholding,
                                int taxYear);
    
    /**
     * Calculate state income tax based on the provided parameters.
     * 
     * @param grossPay The gross pay amount
     * @param stateCode The state code (e.g., "NY", "CA")
     * @param filingStatus The state filing status
     * @param allowances The number of state allowances claimed
     * @param additionalWithholding Any additional state withholding requested
     * @param taxYear The tax year
     * @return The calculated state tax amount
     */
    BigDecimal calculateStateTax(BigDecimal grossPay, String stateCode,
                              FilingStatus filingStatus, int allowances,
                              BigDecimal additionalWithholding, int taxYear);
    
    /**
     * Calculate local tax based on the provided parameters.
     * 
     * @param grossPay The gross pay amount
     * @param localCode The local tax jurisdiction code
     * @param taxYear The tax year
     * @return The calculated local tax amount
     */
    BigDecimal calculateLocalTax(BigDecimal grossPay, String localCode, int taxYear);
    
    /**
     * Calculate Social Security tax based on the provided parameters.
     * 
     * @param grossPay The gross pay amount
     * @param ytdGross The year-to-date gross pay (for wage base limit)
     * @param taxYear The tax year
     * @return The calculated Social Security tax amount
     */
    BigDecimal calculateSocialSecurityTax(BigDecimal grossPay, BigDecimal ytdGross,
                                        int taxYear);
    
    /**
     * Calculate Medicare tax based on the provided parameters.
     * 
     * @param grossPay The gross pay amount
     * @param ytdGross The year-to-date gross pay (for additional Medicare tax threshold)
     * @param taxYear The tax year
     * @return The calculated Medicare tax amount
     */
    BigDecimal calculateMedicareTax(BigDecimal grossPay, BigDecimal ytdGross,
                                  int taxYear);
}
