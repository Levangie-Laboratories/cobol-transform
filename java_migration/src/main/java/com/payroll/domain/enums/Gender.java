package com.payroll.domain.enums;

/**
 * Enum representing employee gender options.
 * Maps to the EMP-GENDER field and associated conditions in EMPFILE.cpy.
 */
public enum Gender {
    MALE("M", "Male"),
    FEMALE("F", "Female"),
    OTHER("O", "Other");
    
    private final String code;
    private final String description;
    
    Gender(String code, String description) {
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
     * Finds a Gender enum by its code.
     * 
     * @param code The code to look up
     * @return The matching Gender or null if not found
     */
    public static Gender fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (Gender gender : Gender.values()) {
            if (gender.getCode().equals(code)) {
                return gender;
            }
        }
        
        return null;
    }
}
