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
 * Entity representing a deduction type.
 * Maps to the DEDUCTION-TYPE-RECORD structure in DEDUCFILE.cpy.
 */
@Entity
@Table(name = "deduction_types")
@Getter
@Setter
public class DeductionType {

    // Default constructor required by JPA
    public DeductionType() {
        // No initialization needed
    }

    @Id
    @Column(name = "deduction_code", length = 3)
    private String deductionCode; // Maps to DEDUCT-CODE
    
    @Column(name = "deduction_name", length = 30, nullable = false)
    private String deductionName; // Maps to DEDUCT-NAME
    
    @Column(name = "description", length = 100)
    private String description; // Maps to DEDUCT-DESCRIPTION
    
    @Enumerated(EnumType.STRING)
    @Column(name = "category", length = 2, nullable = false)
    private DeductionCategory category; // Maps to DEDUCT-CATEGORY
    
    @Enumerated(EnumType.STRING)
    @Column(name = "tax_status", length = 1, nullable = false)
    private TaxStatus taxStatus; // Maps to DEDUCT-TAX-STATUS
    
    @Enumerated(EnumType.STRING)
    @Column(name = "calculation_method", length = 1, nullable = false)
    private CalculationMethod calculationMethod; // Maps to DEDUCT-CALC-METHOD
    
    // Calculation parameters
    @Column(name = "flat_amount", precision = 9, scale = 2)
    private BigDecimal flatAmount; // Maps to DEDUCT-FLAT-AMOUNT
    
    @Column(name = "percentage_rate", precision = 5, scale = 2)
    private BigDecimal percentageRate; // Maps to DEDUCT-PERCENTAGE
    
    @Column(name = "hourly_rate", precision = 5, scale = 2)
    private BigDecimal hourlyRate; // Maps to DEDUCT-HOURLY-RATE
    
    // Graduated ranges - stored as a collection of embedded objects
    @ElementCollection
    @CollectionTable(
        name = "deduction_graduated_ranges",
        joinColumns = @JoinColumn(name = "deduction_code")
    )
    private List<GraduatedRange> graduatedRanges = new ArrayList<>(); // Maps to DEDUCT-GRAD-RANGES
    
    // Limit parameters
    @Column(name = "max_amount_per_pay", precision = 9, scale = 2)
    private BigDecimal maxAmountPerPay; // Maps to DEDUCT-MAX-AMT-PER-PAY
    
    @Column(name = "annual_max_amount", precision = 11, scale = 2)
    private BigDecimal annualMaxAmount; // Maps to DEDUCT-ANNUAL-MAX-AMT
    
    @Column(name = "min_amount_per_pay", precision = 9, scale = 2)
    private BigDecimal minAmountPerPay; // Maps to DEDUCT-MIN-AMT-PER-PAY
    
    @Column(name = "max_percentage", precision = 5, scale = 2)
    private BigDecimal maxPercentage; // Maps to DEDUCT-MAX-PERCENTAGE
    
    @Enumerated(EnumType.STRING)
    @Column(name = "frequency", length = 1, nullable = false)
    private DeductionFrequency frequency; // Maps to DEDUCT-FREQUENCY
    
    @Column(name = "priority")
    private Integer priority; // Maps to DEDUCT-PRIORITY
    
    // Vendor information
    @Column(name = "vendor_id", length = 10)
    private String vendorId; // Maps to DEDUCT-VENDOR-ID
    
    @Column(name = "vendor_name", length = 30)
    private String vendorName; // Maps to DEDUCT-VENDOR-NAME
    
    @Column(name = "vendor_account", length = 20)
    private String vendorAccount; // Maps to DEDUCT-VENDOR-ACCOUNT
    
    // Effective dates and status
    @Column(name = "effective_date")
    private LocalDate effectiveDate; // Maps to DEDUCT-EFFECTIVE-DATE
    
    @Column(name = "expiration_date")
    private LocalDate expirationDate; // Maps to DEDUCT-EXPIRATION-DATE
    
    @Enumerated(EnumType.STRING)
    @Column(name = "status", length = 1, nullable = false)
    private DeductionStatus status; // Maps to DEDUCT-STATUS
    
    // Flags
    @Column(name = "required")
    private boolean required; // Maps to DEDUCT-REQUIRED-FLAG
    
    @Column(name = "employer_match")
    private boolean employerMatch; // Maps to DEDUCT-EMPLOYER-MATCH-FLAG
    
    @Column(name = "employer_match_rate", precision = 5, scale = 2)
    private BigDecimal employerMatchRate; // Maps to DEDUCT-EMPLOYER-MATCH-RATE
    
    @Column(name = "employer_match_max", precision = 9, scale = 2)
    private BigDecimal employerMatchMax; // Maps to DEDUCT-EMPLOYER-MATCH-MAX
    
    @Column(name = "special_processing")
    private boolean specialProcessing; // Maps to DEDUCT-SPECIAL-PROC-FLAG
    
    /**
     * Calculates the deduction amount based on the deduction type parameters.
     * 
     * @param grossPay The gross pay amount
     * @param hours The hours worked (for hourly rate calculations)
     * @param ytdDeduction The year-to-date deduction amount (for annual limits)
     * @return The calculated deduction amount
     */
    public BigDecimal calculateDeduction(BigDecimal grossPay, BigDecimal hours, BigDecimal ytdDeduction) {
        // If not active or not applicable, return zero
        if (status != DeductionStatus.ACTIVE || grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        BigDecimal calculatedAmount = BigDecimal.ZERO;
        
        // Calculate based on method
        switch (calculationMethod) {
            case FLAT_AMOUNT:
                if (flatAmount != null) {
                    calculatedAmount = flatAmount;
                }
                break;
                
            case PERCENTAGE:
                if (percentageRate != null) {
                    calculatedAmount = grossPay.multiply(
                        percentageRate.divide(new BigDecimal("100")));
                }
                break;
                
            case HOURLY_RATE:
                if (hourlyRate != null && hours != null) {
                    calculatedAmount = hourlyRate.multiply(hours);
                }
                break;
                
            case GRADUATED:
                // Find the appropriate graduated range
                for (GraduatedRange range : graduatedRanges) {
                    if (range.appliesTo(grossPay)) {
                        calculatedAmount = range.calculateDeduction(grossPay, true);
                        break;
                    }
                }
                break;
        }
        
        // Apply minimum per pay if set
        if (minAmountPerPay != null && calculatedAmount.compareTo(minAmountPerPay) < 0 
                && calculatedAmount.compareTo(BigDecimal.ZERO) > 0) {
            calculatedAmount = minAmountPerPay;
        }
        
        // Apply maximum per pay if set
        if (maxAmountPerPay != null && calculatedAmount.compareTo(maxAmountPerPay) > 0) {
            calculatedAmount = maxAmountPerPay;
        }
        
        // Apply annual maximum if set
        if (annualMaxAmount != null && ytdDeduction != null) {
            BigDecimal remainingAnnual = annualMaxAmount.subtract(ytdDeduction);
            if (remainingAnnual.compareTo(BigDecimal.ZERO) <= 0) {
                calculatedAmount = BigDecimal.ZERO;
            } else if (calculatedAmount.compareTo(remainingAnnual) > 0) {
                calculatedAmount = remainingAnnual;
            }
        }
        
        // Apply maximum percentage if set
        if (maxPercentage != null) {
            BigDecimal maxAmount = grossPay.multiply(
                maxPercentage.divide(new BigDecimal("100")));
            if (calculatedAmount.compareTo(maxAmount) > 0) {
                calculatedAmount = maxAmount;
            }
        }
        
        return calculatedAmount;
    }
    
    /**
     * Determines if this deduction should be applied in the current pay period.
     * 
     * @param currentDate The current date
     * @param payPeriodStartDate The pay period start date
     * @param payPeriodEndDate The pay period end date
     * @param payFrequency The pay frequency
     * @return true if the deduction should be applied
     */
    public boolean isApplicable(LocalDate currentDate, 
                               LocalDate payPeriodStartDate, 
                               LocalDate payPeriodEndDate, 
                               PayFrequency payFrequency) {
        // Check if deduction is active and within effective/expiration dates
        if (!status.isUsableOn(currentDate, effectiveDate, expirationDate)) {
            return false;
        }
        
        // Check if deduction should be applied based on frequency
        return frequency.shouldApplyInPayPeriod(payPeriodStartDate, payPeriodEndDate, payFrequency);
    }
    
    @Override
    public String toString() {
        return "DeductionType{" +
                "deductionCode='" + deductionCode + '\'' +
                ", deductionName='" + deductionName + '\'' +
                ", category=" + category +
                ", taxStatus=" + taxStatus +
                ", calculationMethod=" + calculationMethod +
                ", status=" + status +
                ", required=" + required +
                '}';
    }
    
    // Manual getter methods to resolve compilation issues with Lombok
    public String getDeductionCode() {
        return deductionCode;
    }
    
    public TaxStatus getTaxStatus() {
        return taxStatus;
    }
    
    public CalculationMethod getCalculationMethod() {
        return calculationMethod;
    }
    
    public BigDecimal getFlatAmount() {
        return flatAmount;
    }
    
    public BigDecimal getMaxAmountPerPay() {
        return maxAmountPerPay;
    }
    
    public DeductionStatus getStatus() {
        return status;
    }
}
