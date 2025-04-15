package com.payroll.domain;

import com.payroll.domain.enums.*;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Entity representing an Employee record.
 * Maps to the EMPLOYEE-RECORD structure in EMPFILE.cpy.
 */
@Entity
@Table(name = "employees")
@Getter
@Setter
public class Employee {

    // Default constructor required by JPA
    public Employee() {
        // No initialization needed
    }

    @Id
    @Column(name = "employee_id", length = 6)
    private String employeeId; // Maps to EMP-ID
    
    // Personal Information
    @Column(name = "last_name", length = 20, nullable = false)
    private String lastName; // Maps to EMP-LAST-NAME
    
    @Column(name = "first_name", length = 15, nullable = false)
    private String firstName; // Maps to EMP-FIRST-NAME
    
    @Column(name = "middle_initial", length = 1)
    private String middleInitial; // Maps to EMP-MIDDLE-INIT
    
    @Enumerated(EnumType.STRING)
    @Column(name = "gender", length = 1)
    private Gender gender; // Maps to EMP-GENDER
    
    @Column(name = "birth_date")
    private LocalDate birthDate; // Maps to EMP-BIRTH-DATE
    
    @Column(name = "ssn", length = 9)
    private String socialSecurityNumber; // Maps to EMP-SSN
    
    @Enumerated(EnumType.STRING)
    @Column(name = "marital_status", length = 1)
    private MaritalStatus maritalStatus; // Maps to EMP-MARITAL-STATUS
    
    // Contact Information
    @Column(name = "address_line_1", length = 30)
    private String addressLine1; // Maps to EMP-ADDRESS-LINE-1
    
    @Column(name = "address_line_2", length = 30)
    private String addressLine2; // Maps to EMP-ADDRESS-LINE-2
    
    @Column(name = "city", length = 20)
    private String city; // Maps to EMP-CITY
    
    @Column(name = "state", length = 2)
    private String state; // Maps to EMP-STATE
    
    @Column(name = "zip_code", length = 10)
    private String zipCode; // Maps to EMP-ZIP
    
    @Column(name = "phone", length = 15)
    private String phone; // Maps to EMP-PHONE
    
    @Column(name = "email", length = 50)
    private String email; // Maps to EMP-EMAIL
    
    // Employment Information
    @Column(name = "hire_date")
    private LocalDate hireDate; // Maps to EMP-HIRE-DATE
    
    @Column(name = "department", length = 4)
    private String department; // Maps to EMP-DEPARTMENT
    
    @Column(name = "position", length = 20)
    private String position; // Maps to EMP-POSITION
    
    @Enumerated(EnumType.STRING)
    @Column(name = "status", length = 1)
    private EmploymentStatus status; // Maps to EMP-STATUS
    
    @Column(name = "termination_date")
    private LocalDate terminationDate; // Maps to EMP-TERM-DATE
    
    // Pay Information
    @Enumerated(EnumType.STRING)
    @Column(name = "pay_type", length = 1)
    private PayType payType; // Maps to EMP-PAY-TYPE
    
    @Enumerated(EnumType.STRING)
    @Column(name = "pay_frequency", length = 1)
    private PayFrequency payFrequency; // Maps to EMP-PAY-FREQUENCY
    
    @Column(name = "hourly_rate", precision = 9, scale = 2)
    private BigDecimal hourlyRate; // Maps to EMP-HOURLY-RATE
    
    @Column(name = "salary_amount", precision = 12, scale = 2)
    private BigDecimal salaryAmount; // Maps to EMP-SALARY-AMOUNT
    
    @Column(name = "standard_hours", precision = 5, scale = 2)
    private BigDecimal standardHours; // Maps to EMP-STANDARD-HOURS
    
    @Column(name = "overtime_rate", precision = 3, scale = 2)
    private BigDecimal overtimeRate; // Maps to EMP-OVERTIME-RATE
    
    @Column(name = "last_pay_date")
    private LocalDate lastPayDate; // Maps to EMP-LAST-PAY-DATE
    
    @Column(name = "direct_deposit_enabled")
    private boolean directDepositEnabled; // Maps to EMP-DIRECT-DEPOSIT-IND
    
    @Column(name = "bank_account_info", length = 30)
    private String bankAccountInfo; // Maps to EMP-BANK-ACCOUNT-INFO
    
    // Tax Information
    @Enumerated(EnumType.STRING)
    @Column(name = "federal_filing_status", length = 1)
    private FilingStatus federalFilingStatus; // Maps to EMP-FEDERAL-FILING-STATUS
    
    @Enumerated(EnumType.STRING)
    @Column(name = "state_filing_status", length = 1)
    private FilingStatus stateFilingStatus; // Maps to EMP-STATE-FILING-STATUS
    
    @Column(name = "federal_allowances")
    private Integer federalAllowances; // Maps to EMP-FEDERAL-ALLOWANCES
    
    @Column(name = "state_allowances")
    private Integer stateAllowances; // Maps to EMP-STATE-ALLOWANCES
    
    @Column(name = "additional_federal_tax", precision = 7, scale = 2)
    private BigDecimal additionalFederalTax; // Maps to EMP-ADDITIONAL-FIT
    
    @Column(name = "additional_state_tax", precision = 7, scale = 2)
    private BigDecimal additionalStateTax; // Maps to EMP-ADDITIONAL-SIT
    
    @Column(name = "tax_blocked")
    private boolean taxBlocked; // Maps to EMP-TAX-BLOCKED-IND
    
    // Deduction Information
    @Column(name = "health_plan_code", length = 3)
    private String healthPlanCode; // Maps to EMP-HEALTH-PLAN-CODE
    
    @Column(name = "health_deduction", precision = 7, scale = 2)
    private BigDecimal healthDeduction; // Maps to EMP-HEALTH-DEDUCTION
    
    @Column(name = "dental_plan_code", length = 3)
    private String dentalPlanCode; // Maps to EMP-DENTAL-PLAN-CODE
    
    @Column(name = "dental_deduction", precision = 7, scale = 2)
    private BigDecimal dentalDeduction; // Maps to EMP-DENTAL-DEDUCTION
    
    @Column(name = "vision_plan_code", length = 3)
    private String visionPlanCode; // Maps to EMP-VISION-PLAN-CODE
    
    @Column(name = "vision_deduction", precision = 7, scale = 2)
    private BigDecimal visionDeduction; // Maps to EMP-VISION-DEDUCTION
    
    @Column(name = "retirement_401k_enabled")
    private boolean retirement401kEnabled; // Maps to EMP-401K-IND
    
    @Column(name = "retirement_401k_percent", precision = 4, scale = 2)
    private BigDecimal retirement401kPercent; // Maps to EMP-401K-PERCENT
    
    @Column(name = "loan_deduction", precision = 7, scale = 2)
    private BigDecimal loanDeduction; // Maps to EMP-LOAN-DEDUCTION
    
    @Column(name = "garnish_deduction", precision = 7, scale = 2)
    private BigDecimal garnishDeduction; // Maps to EMP-GARNISH-DEDUCTION
    
    @Column(name = "charity_deduction", precision = 7, scale = 2)
    private BigDecimal charityDeduction; // Maps to EMP-CHARITY-DEDUCTION
    
    @Column(name = "union_dues", precision = 7, scale = 2)
    private BigDecimal unionDues; // Maps to EMP-UNION-DUES
    
    // Additional Deductions
    @ElementCollection
    @CollectionTable(
        name = "employee_additional_deductions",
        joinColumns = @JoinColumn(name = "employee_id")
    )
    private List<AdditionalDeduction> additionalDeductions = new ArrayList<>(); // Maps to EMP-ADDITIONAL-DEDUCTIONS
    
    // YTD Amounts
    @Column(name = "ytd_gross", precision = 10, scale = 2)
    private BigDecimal ytdGross; // Maps to EMP-YTD-GROSS
    
    @Column(name = "ytd_federal_tax", precision = 9, scale = 2)
    private BigDecimal ytdFederalTax; // Maps to EMP-YTD-FEDERAL-TAX
    
    @Column(name = "ytd_state_tax", precision = 9, scale = 2)
    private BigDecimal ytdStateTax; // Maps to EMP-YTD-STATE-TAX
    
    @Column(name = "ytd_local_tax", precision = 9, scale = 2)
    private BigDecimal ytdLocalTax; // Maps to EMP-YTD-LOCAL-TAX
    
    @Column(name = "ytd_social_security", precision = 9, scale = 2)
    private BigDecimal ytdSocialSecurity; // Maps to EMP-YTD-SOCIAL-SEC
    
    @Column(name = "ytd_medicare", precision = 9, scale = 2)
    private BigDecimal ytdMedicare; // Maps to EMP-YTD-MEDICARE
    
    @Column(name = "ytd_401k", precision = 9, scale = 2)
    private BigDecimal ytd401k; // Maps to EMP-YTD-401K
    
    @Column(name = "ytd_health_deduction", precision = 9, scale = 2)
    private BigDecimal ytdHealthDeduction; // Maps to EMP-YTD-HEALTH-DEDUCT
    
    @Column(name = "ytd_dental_deduction", precision = 9, scale = 2)
    private BigDecimal ytdDentalDeduction; // Maps to EMP-YTD-DENTAL-DEDUCT
    
    @Column(name = "ytd_vision_deduction", precision = 9, scale = 2)
    private BigDecimal ytdVisionDeduction; // Maps to EMP-YTD-VISION-DEDUCT
    
    @Column(name = "ytd_other_deduction", precision = 9, scale = 2)
    private BigDecimal ytdOtherDeduction; // Maps to EMP-YTD-OTHER-DEDUCT
    
    @Column(name = "ytd_net_pay", precision = 10, scale = 2)
    private BigDecimal ytdNetPay; // Maps to EMP-YTD-NET-PAY
    
    /**
     * Converts a date in YYYYMMDD format to LocalDate.
     * Used for converting dates from COBOL format during data migration.
     * 
     * @param dateValue The date value in YYYYMMDD format
     * @return The corresponding LocalDate or null if the input is invalid
     */
    public static LocalDate convertCobolDate(String dateValue) {
        if (dateValue == null || dateValue.length() != 8 || "00000000".equals(dateValue)) {
            return null;
        }
        
        try {
            int year = Integer.parseInt(dateValue.substring(0, 4));
            int month = Integer.parseInt(dateValue.substring(4, 6));
            int day = Integer.parseInt(dateValue.substring(6, 8));
            return LocalDate.of(year, month, day);
        } catch (Exception e) {
            return null;
        }
    }
    
    /**
     * Formats a LocalDate as a YYYYMMDD string for compatibility with COBOL programs.
     * 
     * @param date The LocalDate to format
     * @return The formatted date string or "00000000" if the date is null
     */
    public static String formatDateForCobol(LocalDate date) {
        if (date == null) {
            return "00000000";
        }
        return String.format("%04d%02d%02d", date.getYear(), date.getMonthValue(), date.getDayOfMonth());
    }
    
    // Manual getter methods to resolve compilation issues with Lombok
    public PayType getPayType() {
        return payType;
    }
    public String getLastName() {
        return lastName;
    }
    
    public String getFirstName() {
        return firstName;
    }
    
    public String getMiddleInitial() {
        return middleInitial;
    }
    
    public String getEmployeeId() {
        return employeeId;
    }
    
    public List<AdditionalDeduction> getAdditionalDeductions() {
        return additionalDeductions;
    }
    
    public BigDecimal getDentalDeduction() {
        return dentalDeduction;
    }
    
    public String getVisionPlanCode() {
        return visionPlanCode;
    }
    
    public BigDecimal getVisionDeduction() {
        return visionDeduction;
    }
    
    public boolean isRetirement401kEnabled() {
        return retirement401kEnabled;
    }
    
    public BigDecimal getRetirement401kPercent() {
        return retirement401kPercent;
    }
    
    public BigDecimal getYtdHealthDeduction() {
        return ytdHealthDeduction;
    }
    
    public String getHealthPlanCode() {
        return healthPlanCode;
    }
    
    // Manual setter method to resolve compilation issues with Lombok
    public void setMiddleInitial(String middleInitial) {
        this.middleInitial = middleInitial;
    }
}
