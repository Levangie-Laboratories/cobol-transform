package com.payroll.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.math.BigDecimal;

/**
 * Embeddable class representing a graduated range for deduction calculations.
 * Maps to the DEDUCT-GRAD-RANGES structure in DEDUCFILE.cpy.
 */
@Embeddable
@Getter
@Setter
public class GraduatedRange {

    // Default constructor required by JPA
    public GraduatedRange() {
        // No initialization needed
    }

    @Column(name = "min_salary", precision = 10, scale = 2)
    private BigDecimal minSalary; // Maps to DEDUCT-GRAD-MIN-SALARY
    
    @Column(name = "max_salary", precision = 10, scale = 2)
    private BigDecimal maxSalary; // Maps to DEDUCT-GRAD-MAX-SALARY
    
    @Column(name = "deduction_amount", precision = 9, scale = 2)
    private BigDecimal deductionAmount; // Maps to DEDUCT-GRAD-AMOUNT
    
    @Column(name = "deduction_percentage", precision = 5, scale = 2)
    private BigDecimal deductionPercentage; // Maps to DEDUCT-GRAD-PERCENTAGE
    
    /**
     * Checks if a given salary falls within this range.
     * 
     * @param salary The salary amount to check
     * @return true if the salary is within this range
     */
    public boolean appliesTo(BigDecimal salary) {
        if (salary.compareTo(minSalary) < 0) {
            return false;
        }
        
        // If max salary is zero or salary is less than max, this range applies
        return maxSalary.compareTo(BigDecimal.ZERO) == 0 || 
               salary.compareTo(maxSalary) <= 0;
    }
    
    /**
     * Calculates the deduction amount for this range based on the calculation method.
     * 
     * @param salary The salary amount
     * @param usePercentage Whether to use percentage or flat amount
     * @return The calculated deduction amount
     */
    public BigDecimal calculateDeduction(BigDecimal salary, boolean usePercentage) {
        if (!appliesTo(salary)) {
            return BigDecimal.ZERO;
        }
        
        if (usePercentage && deductionPercentage != null) {
            return salary.multiply(deductionPercentage.divide(new BigDecimal("100")));
        } else {
            return deductionAmount != null ? deductionAmount : BigDecimal.ZERO;
        }
    }
    
    // Getter and setter methods to replace Lombok functionality
    public BigDecimal getMinSalary() {
        return minSalary;
    }
    
    public void setMinSalary(BigDecimal minSalary) {
        this.minSalary = minSalary;
    }
    
    public BigDecimal getMaxSalary() {
        return maxSalary;
    }
    
    public void setMaxSalary(BigDecimal maxSalary) {
        this.maxSalary = maxSalary;
    }
    
    public BigDecimal getDeductionAmount() {
        return deductionAmount;
    }
    
    public void setDeductionAmount(BigDecimal deductionAmount) {
        this.deductionAmount = deductionAmount;
    }
    
    public BigDecimal getDeductionPercentage() {
        return deductionPercentage;
    }
    
    public void setDeductionPercentage(BigDecimal deductionPercentage) {
        this.deductionPercentage = deductionPercentage;
    }
}
