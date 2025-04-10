package com.payroll.domain.enums;

/**
 * Enum representing deduction frequency options.
 * Maps to the DEDUCT-FREQUENCY field and associated conditions in DEDUCFILE.cpy.
 * Determines when a deduction is applied during the payroll process.
 */
public enum DeductionFrequency {
    EVERY_PAY_PERIOD("E", "Every Pay Period"),
    FIRST_PAY_PERIOD_OF_MONTH("F", "First Pay Period of Month"),
    LAST_PAY_PERIOD_OF_MONTH("L", "Last Pay Period of Month"),
    BIWEEKLY("B", "Bi-Weekly"),
    MONTHLY("M", "Monthly"),
    QUARTERLY("Q", "Quarterly"),
    ANNUAL("A", "Annual"),
    ONE_TIME("O", "One-Time");
    
    private final String code;
    private final String description;
    
    DeductionFrequency(String code, String description) {
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
     * Determines if this deduction should be applied in the current pay period.
     * 
     * @param payPeriodStartDate The start date of the current pay period
     * @param payPeriodEndDate The end date of the current pay period
     * @param payFrequency The frequency of the pay periods (weekly, biweekly, etc.)
     * @return true if the deduction should be applied in this pay period
     */
    public boolean shouldApplyInPayPeriod(java.time.LocalDate payPeriodStartDate, 
                                         java.time.LocalDate payPeriodEndDate, 
                                         PayFrequency payFrequency) {
        // Every pay period always applies
        if (this == EVERY_PAY_PERIOD) {
            return true;
        }
        
        // One-time deductions are handled separately (typically manually flagged)
        if (this == ONE_TIME) {
            return false; // Would need a separate flag to indicate when to apply
        }
        
        // First pay period of month logic
        if (this == FIRST_PAY_PERIOD_OF_MONTH) {
            // Check if this is the first pay period that starts in this month
            java.time.LocalDate firstOfMonth = 
                java.time.LocalDate.of(payPeriodStartDate.getYear(), 
                                     payPeriodStartDate.getMonth(), 1);
            return payPeriodStartDate.isEqual(firstOfMonth) || 
                   (payPeriodStartDate.isAfter(firstOfMonth) && 
                    payPeriodStartDate.getDayOfMonth() <= 15);
        }
        
        // Last pay period of month logic
        if (this == LAST_PAY_PERIOD_OF_MONTH) {
            // Check if this pay period ends in the last day of the month or close to it
            java.time.LocalDate lastOfMonth = 
                payPeriodEndDate.withDayOfMonth(
                    payPeriodEndDate.getMonth().length(
                        java.time.Year.isLeap(payPeriodEndDate.getYear())));
            return payPeriodEndDate.isEqual(lastOfMonth) || 
                   (payPeriodEndDate.getDayOfMonth() >= 15 && 
                    payPeriodEndDate.getMonth() == lastOfMonth.getMonth());
        }
        
        // Other frequencies would need more complex logic based on pay period dates
        // and would typically be handled by specific deduction processing code
        
        return false;
    }
    
    /**
     * Finds a DeductionFrequency enum by its code.
     * 
     * @param code The code to look up
     * @return The matching DeductionFrequency or null if not found
     */
    public static DeductionFrequency fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (DeductionFrequency frequency : DeductionFrequency.values()) {
            if (frequency.getCode().equals(code)) {
                return frequency;
            }
        }
        
        return null;
    }
}
