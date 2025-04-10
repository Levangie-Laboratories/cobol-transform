package com.payroll.domain.enums;

/**
 * Enum representing deduction calculation methods.
 * Maps to the DEDUCT-CALC-METHOD field and associated conditions in DEDUCFILE.cpy.
 */
public enum CalculationMethod {
    FLAT_AMOUNT("F", "Flat Amount"),
    PERCENTAGE("P", "Percentage of Gross Pay"),
    HOURLY_RATE("H", "Hourly Rate"),
    GRADUATED("G", "Graduated Scale");
    
    private final String code;
    private final String description;
    
    CalculationMethod(String code, String description) {
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
     * Determines if this calculation method uses the flat amount field.
     * 
     * @return true if this method uses the flat amount field
     */
    public boolean usesFlatAmount() {
        return this == FLAT_AMOUNT;
    }
    
    /**
     * Determines if this calculation method uses the percentage field.
     * 
     * @return true if this method uses the percentage field
     */
    public boolean usesPercentage() {
        return this == PERCENTAGE;
    }
    
    /**
     * Determines if this calculation method uses the hourly rate field.
     * 
     * @return true if this method uses the hourly rate field
     */
    public boolean usesHourlyRate() {
        return this == HOURLY_RATE;
    }
    
    /**
     * Determines if this calculation method uses graduated ranges.
     * 
     * @return true if this method uses graduated ranges
     */
    public boolean usesGraduatedRanges() {
        return this == GRADUATED;
    }
    
    /**
     * Finds a CalculationMethod enum by its code.
     * 
     * @param code The code to look up
     * @return The matching CalculationMethod or null if not found
     */
    public static CalculationMethod fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (CalculationMethod method : CalculationMethod.values()) {
            if (method.getCode().equals(code)) {
                return method;
            }
        }
        
        return null;
    }
}
