package com.payroll.domain.enums;

/**
 * Enum representing deduction categories.
 * Maps to the DEDUCT-CATEGORY field and associated conditions in DEDUCFILE.cpy.
 */
public enum DeductionCategory {
    HEALTH_INSURANCE("HI", "Health Insurance"),
    DENTAL_INSURANCE("DI", "Dental Insurance"),
    VISION_INSURANCE("VI", "Vision Insurance"),
    RETIREMENT("RT", "Retirement"),
    LOAN_REPAYMENT("LR", "Loan Repayment"),
    GARNISHMENT("GN", "Garnishment"),
    CHARITY("CH", "Charitable Contribution"),
    UNION_DUES("UD", "Union Dues"),
    SAVINGS_PLAN("SP", "Savings Plan"),
    LIFE_INSURANCE("LI", "Life Insurance"),
    DISABILITY_INSURANCE("DY", "Disability Insurance"),
    MISCELLANEOUS("MS", "Miscellaneous");
    
    private final String code;
    private final String description;
    
    DeductionCategory(String code, String description) {
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
     * Finds a DeductionCategory enum by its code.
     * 
     * @param code The code to look up
     * @return The matching DeductionCategory or null if not found
     */
    public static DeductionCategory fromCode(String code) {
        if (code == null) {
            return null;
        }
        
        for (DeductionCategory category : DeductionCategory.values()) {
            if (category.getCode().equals(code)) {
                return category;
            }
        }
        
        return null;
    }
}
