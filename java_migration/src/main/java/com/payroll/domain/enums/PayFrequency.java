package com.payroll.domain.enums;

/**
 * Enum representing pay frequency options.
 * Maps to the EMP-PAY-FREQUENCY field and associated conditions in EMPFILE.cpy.
 * This determines how often an employee is paid, which affects salary calculations.
 */
public enum PayFrequency {
    WEEKLY("W", "Weekly", 52),
    BIWEEKLY("B", "Bi-weekly", 26),
    SEMI_MONTHLY("S", "Semi-monthly", 24),
    MONTHLY("M", "Monthly", 12);
    
    private final String code;
    private final String description;
    private final int periodsPerYear;
    
    PayFrequency(String code, String description, int periodsPerYear) {
        this.code = code;
        this.description = description;
        this.periodsPerYear = periodsPerYear;
    }
    
    public String getCode() {
        return code;
    }
    
    public String getDescription() {
        return description;
    }
    
    /**
     * Returns the number of pay periods per year for this frequency.
     * Useful for salary calculations when prorating annual amounts.
     * 
     * @return Number of pay periods per year
     */
    public int getPeriodsPerYear() {
        return periodsPerYear;
    }
    
    /**
     * Finds a PayFrequency enum by its code.
     * 
     * @param code The code to look up
     * @return The matching PayFrequency or null if not found
     */
    public static PayFrequency fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (PayFrequency frequency : PayFrequency.values()) {
            if (frequency.getCode().equals(code)) {
                return frequency;
            }
        }
        
        return null;
    }
}
