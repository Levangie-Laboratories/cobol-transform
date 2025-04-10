package com.payroll.domain.enums;

/**
 * Enum representing tax filing status options.
 * Maps to the EMP-FEDERAL-FILING-STATUS and EMP-STATE-FILING-STATUS fields
 * and associated conditions in EMPFILE.cpy. Also used in tax rate tables.
 * This determines which tax brackets apply for tax calculations.
 */
public enum FilingStatus {
    SINGLE("S", "Single"),
    MARRIED("M", "Married Filing Jointly"),
    HEAD_OF_HOUSEHOLD("H", "Head of Household");
    
    private final String code;
    private final String description;
    
    FilingStatus(String code, String description) {
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
     * Finds a FilingStatus enum by its code.
     * 
     * @param code The code to look up
     * @return The matching FilingStatus or null if not found
     */
    public static FilingStatus fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (FilingStatus status : FilingStatus.values()) {
            if (status.getCode().equals(code)) {
                return status;
            }
        }
        
        return null;
    }
}
