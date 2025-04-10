package com.payroll.domain.enums;

/**
 * Enum representing employee employment status options.
 * Maps to the EMP-STATUS field and associated conditions in EMPFILE.cpy.
 */
public enum EmploymentStatus {
    ACTIVE("A", "Active"),
    TERMINATED("T", "Terminated"),
    LEAVE("L", "On Leave"),
    RETIRED("R", "Retired");
    
    private final String code;
    private final String description;
    
    EmploymentStatus(String code, String description) {
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
     * Finds an EmploymentStatus enum by its code.
     * 
     * @param code The code to look up
     * @return The matching EmploymentStatus or null if not found
     */
    public static EmploymentStatus fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (EmploymentStatus status : EmploymentStatus.values()) {
            if (status.getCode().equals(code)) {
                return status;
            }
        }
        
        return null;
    }
}
