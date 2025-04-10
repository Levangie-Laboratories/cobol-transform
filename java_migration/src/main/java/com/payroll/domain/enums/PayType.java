package com.payroll.domain.enums;

/**
 * Enum representing employee pay type options.
 * Maps to the EMP-PAY-TYPE field and associated conditions in EMPFILE.cpy.
 * This determines whether an employee is paid hourly or receives a salary.
 */
public enum PayType {
    HOURLY("H", "Hourly"),
    SALARY("S", "Salary");
    
    private final String code;
    private final String description;
    
    PayType(String code, String description) {
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
     * Finds a PayType enum by its code.
     * 
     * @param code The code to look up
     * @return The matching PayType or null if not found
     */
    public static PayType fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (PayType payType : PayType.values()) {
            if (payType.getCode().equals(code)) {
                return payType;
            }
        }
        
        return null;
    }
}
