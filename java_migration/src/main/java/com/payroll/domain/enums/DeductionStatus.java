package com.payroll.domain.enums;

/**
 * Enum representing deduction status options.
 * Maps to the DEDUCT-STATUS field and associated conditions in DEDUCFILE.cpy.
 * Determines whether a deduction type is currently active and usable.
 */
public enum DeductionStatus {
    ACTIVE("A", "Active"),
    INACTIVE("I", "Inactive"),
    PENDING("P", "Pending"),
    EXPIRED("E", "Expired");
    
    private final String code;
    private final String description;
    
    DeductionStatus(String code, String description) {
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
     * Determines if this status indicates the deduction is currently active.
     * 
     * @return true if the deduction should be considered active
     */
    public boolean isActive() {
        return this == ACTIVE;
    }
    
    /**
     * Determines if this status indicates the deduction is usable on the given date.
     * 
     * @param currentDate The date to check against
     * @param effectiveDate The date when the deduction becomes effective
     * @param expirationDate The date when the deduction expires
     * @return true if the deduction is usable on the given date
     */
    public boolean isUsableOn(java.time.LocalDate currentDate, 
                             java.time.LocalDate effectiveDate,
                             java.time.LocalDate expirationDate) {
        // Only active deductions are usable
        if (this != ACTIVE) {
            return false;
        }
        
        // Check if current date is within effective and expiration dates
        boolean afterEffective = effectiveDate == null || 
                               !currentDate.isBefore(effectiveDate);
        boolean beforeExpiration = expirationDate == null || 
                                 !currentDate.isAfter(expirationDate);
        
        return afterEffective && beforeExpiration;
    }
    
    /**
     * Finds a DeductionStatus enum by its code.
     * 
     * @param code The code to look up
     * @return The matching DeductionStatus or null if not found
     */
    public static DeductionStatus fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (DeductionStatus status : DeductionStatus.values()) {
            if (status.getCode().equals(code)) {
                return status;
            }
        }
        
        return null;
    }
}
