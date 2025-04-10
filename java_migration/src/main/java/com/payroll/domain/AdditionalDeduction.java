package com.payroll.domain;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.math.BigDecimal;

/**
 * Embeddable class representing an additional deduction for an employee.
 * Maps to the EMP-ADDITIONAL-DEDUCTIONS structure in EMPFILE.cpy.
 */
@Embeddable
public class AdditionalDeduction {

    // Default constructor required by JPA
    public AdditionalDeduction() {
        // No initialization needed
    }

    @Column(name = "deduction_code", length = 3)
    private String deductionCode; // Maps to EMP-ADD-DEDUCT-CODE
    
    @Column(name = "deduction_amount", precision = 7, scale = 2)
    private BigDecimal deductionAmount; // Maps to EMP-ADD-DEDUCT-AMT
    
    /**
     * Checks if this additional deduction is active (has a valid code and amount).
     * 
     * @return true if the deduction has a non-empty code and non-null amount
     */
    public boolean isActive() {
        return deductionCode != null && 
               !deductionCode.isEmpty() && 
               !"000".equals(deductionCode) &&
               deductionAmount != null;
    }
    
    @Override
    public String toString() {
        return "AdditionalDeduction{" +
                "deductionCode='" + deductionCode + '\'' +
                ", deductionAmount=" + deductionAmount +
                '}';
    }
    
    // Manual getter methods to resolve compilation issues with Lombok
    public String getDeductionCode() {
        return deductionCode;
    }
    
    public BigDecimal getDeductionAmount() {
        return deductionAmount;
    }
    
    // Setter methods to replace Lombok functionality
    public void setDeductionCode(String deductionCode) {
        this.deductionCode = deductionCode;
    }
    
    public void setDeductionAmount(BigDecimal deductionAmount) {
        this.deductionAmount = deductionAmount;
    }
}
