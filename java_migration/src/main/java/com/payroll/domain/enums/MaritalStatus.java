package com.payroll.domain.enums;

/**
 * Enum representing employee marital status options.
 * Maps to the EMP-MARITAL-STATUS field and associated conditions in EMPFILE.cpy.
 */
public enum MaritalStatus {
    SINGLE("S", "Single"),
    MARRIED("M", "Married"),
    DIVORCED("D", "Divorced"),
    WIDOWED("W", "Widowed");
    
    private final String code;
    private final String description;
    
    MaritalStatus(String code, String description) {
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
     * Finds a MaritalStatus enum by its code.
     * 
     * @param code The code to look up
     * @return The matching MaritalStatus or null if not found
     */
    public static MaritalStatus fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (MaritalStatus status : MaritalStatus.values()) {
            if (status.getCode().equals(code)) {
                return status;
            }
        }
        
        return null;
    }
}
