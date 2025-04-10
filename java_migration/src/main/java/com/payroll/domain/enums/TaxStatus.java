package com.payroll.domain.enums;

/**
 * Enum representing deduction tax status options.
 * Maps to the DEDUCT-TAX-STATUS field and associated conditions in DEDUCFILE.cpy.
 * Determines whether a deduction is applied before or after tax calculations.
 */
public enum TaxStatus {
    PRE_TAX("P", "Pre-Tax"),
    POST_TAX("A", "Post-Tax");
    
    private final String code;
    private final String description;
    
    TaxStatus(String code, String description) {
        this.code = code;
        this.description = description;
    }
    
    public String getCode() {
        return code;
    }
    
    public String getDescription() {
        return description;
    }
    
    /**
     * Determines if this is a pre-tax deduction.
     * Pre-tax deductions reduce taxable income before tax calculations.
     * 
     * @return true if this is a pre-tax deduction
     */
    public boolean isPreTax() {
        return this == PRE_TAX;
    }
    
    /**
     * Determines if this is a post-tax deduction.
     * Post-tax deductions are applied after tax calculations.
     * 
     * @return true if this is a post-tax deduction
     */
    public boolean isPostTax() {
        return this == POST_TAX;
    }
    
    /**
     * Finds a TaxStatus enum by its code.
     * 
     * @param code The code to look up
     * @return The matching TaxStatus or null if not found
     */
    public static TaxStatus fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (TaxStatus status : TaxStatus.values()) {
            if (status.getCode().equals(code)) {
                return status;
            }
        }
        
        return null;
    }
}
