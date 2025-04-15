package com.payroll.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Entity representing tax rates and brackets.
 * Maps to the TAX-RATES-TABLE structure in TAXRATES.cpy.
 */
@Entity
@Table(name = "tax_rates")
@Getter
@Setter
public class TaxRate {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    /**
     * Default constructor
     */
    public TaxRate() {
        this.federalTaxBrackets = new ArrayList<>();
    }
    
    @Column(name = "tax_year", nullable = false)
    private Integer taxYear; // Maps to TAX-YEAR
    
    @Column(name = "effective_date", nullable = false)
    private LocalDate effectiveDate; // Maps to TAX-EFFECTIVE-DATE
    
    @Column(name = "expiration_date", nullable = false)
    private LocalDate expirationDate; // Maps to TAX-EXPIRATION-DATE
    
    // Federal tax brackets
    @OneToMany(mappedBy = "taxRate", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<FederalTaxBracket> federalTaxBrackets = new ArrayList<>();
    
    // State tax tables - to be implemented later with additional entities
    // Would map to STATE-TAX-TABLE
    
    // FICA taxes
    @Column(name = "social_security_rate", precision = 4, scale = 2, nullable = false)
    private BigDecimal socialSecurityRate; // Maps to SOC-SEC-RATE
    
    @Column(name = "social_security_wage_base", precision = 10, scale = 2, nullable = false)
    private BigDecimal socialSecurityWageBase; // Maps to SOC-SEC-WAGE-BASE
    
    @Column(name = "social_security_max_tax", precision = 10, scale = 2, nullable = false)
    private BigDecimal socialSecurityMaxTax; // Maps to SOC-SEC-MAX-TAX
    
    @Column(name = "medicare_rate", precision = 4, scale = 2, nullable = false)
    private BigDecimal medicareRate; // Maps to MEDICARE-RATE
    
    @Column(name = "medicare_additional_rate", precision = 4, scale = 2, nullable = false)
    private BigDecimal medicareAdditionalRate; // Maps to MEDICARE-ADDL-RATE
    
    @Column(name = "medicare_additional_threshold", precision = 10, scale = 2, nullable = false)
    private BigDecimal medicareAdditionalThreshold; // Maps to MEDICARE-ADDL-THRESHOLD
    
    // Local tax tables - to be implemented later with additional entities
    // Would map to LOCAL-TAX-TABLE
    
    // Unemployment taxes
    @Column(name = "futa_rate", precision = 4, scale = 2, nullable = false)
    private BigDecimal futaRate; // Maps to FUTA-RATE
    
    @Column(name = "futa_wage_base", precision = 10, scale = 2, nullable = false)
    private BigDecimal futaWageBase; // Maps to FUTA-WAGE-BASE
    
    // State unemployment tax rates - to be implemented later with additional entities
    // Would map to SUTA-TABLE
    
    // Tax constants
    @Column(name = "standard_deduction_single", precision = 8, scale = 2, nullable = false)
    private BigDecimal standardDeductionSingle; // Maps to STANDARD-DEDUCTION-SINGLE
    
    @Column(name = "standard_deduction_married", precision = 8, scale = 2, nullable = false)
    private BigDecimal standardDeductionMarried; // Maps to STANDARD-DEDUCTION-MARRIED
    
    @Column(name = "standard_deduction_head", precision = 8, scale = 2, nullable = false)
    private BigDecimal standardDeductionHead; // Maps to STANDARD-DEDUCTION-HEAD
    
    @Column(name = "personal_exemption_amount", precision = 8, scale = 2, nullable = false)
    private BigDecimal personalExemptionAmount; // Maps to PERSONAL-EXEMPTION-AMOUNT
    
    @Column(name = "personal_exemption_phaseout", precision = 10, scale = 2, nullable = false)
    private BigDecimal personalExemptionPhaseout; // Maps to PERSONAL-EXEMPTION-PHASEOUT
    
    @Column(name = "tax_credit_rate", precision = 4, scale = 2, nullable = false)
    private BigDecimal taxCreditRate; // Maps to TAX-CREDIT-RATE
    
    /**
     * Find tax rates applicable for a specific date.
     * This can be used as a named query by the repository.
     * 
     * @param year The tax year to find
     * @param date The date to check against effective and expiration dates
     * @return TaxRate for the specified year and date range
     */
    public static String findByYearAndDate() {
        return "SELECT t FROM TaxRate t WHERE t.taxYear = :year " +
               "AND t.effectiveDate <= :date AND t.expirationDate >= :date";
    }
    
    /**
     * Adds a federal tax bracket to this tax rate.
     * Helper method to maintain both sides of the bidirectional relationship.
     * 
     * @param bracket The bracket to add
     */
    public void addFederalTaxBracket(FederalTaxBracket bracket) {
        federalTaxBrackets.add(bracket);
        bracket.setTaxRate(this);
    }
    
    /**
     * Removes a federal tax bracket from this tax rate.
     * Helper method to maintain both sides of the bidirectional relationship.
     * 
     * @param bracket The bracket to remove
     */
    public void removeFederalTaxBracket(FederalTaxBracket bracket) {
        federalTaxBrackets.remove(bracket);
        bracket.setTaxRate(null);
    }
    
    /**
     * Gets the standard deduction amount for a given filing status.
     * 
     * @param filingStatus The filing status
     * @return The standard deduction amount
     */
    public BigDecimal getStandardDeduction(com.payroll.domain.enums.FilingStatus filingStatus) {
        switch (filingStatus) {
            case SINGLE:
                return standardDeductionSingle;
            case MARRIED:
                return standardDeductionMarried;
            case HEAD_OF_HOUSEHOLD:
                return standardDeductionHead;
            default:
                return BigDecimal.ZERO;
        }
    }
    
    // Getter methods - previously generated by Lombok
    public Long getId() {
        return id;
    }
    
    public Integer getTaxYear() {
        return taxYear;
    }
    
    public LocalDate getEffectiveDate() {
        return effectiveDate;
    }
    
    public LocalDate getExpirationDate() {
        return expirationDate;
    }
    
    public List<FederalTaxBracket> getFederalTaxBrackets() {
        return federalTaxBrackets;
    }
    
    public BigDecimal getSocialSecurityRate() {
        return socialSecurityRate;
    }
    
    public BigDecimal getSocialSecurityWageBase() {
        return socialSecurityWageBase;
    }
    
    public BigDecimal getSocialSecurityMaxTax() {
        return socialSecurityMaxTax;
    }
    
    public BigDecimal getMedicareRate() {
        return medicareRate;
    }
    
    public BigDecimal getMedicareAdditionalRate() {
        return medicareAdditionalRate;
    }
    
    public BigDecimal getMedicareAdditionalThreshold() {
        return medicareAdditionalThreshold;
    }
    
    public BigDecimal getFutaRate() {
        return futaRate;
    }
    
    public BigDecimal getFutaWageBase() {
        return futaWageBase;
    }
    
    public BigDecimal getStandardDeductionSingle() {
        return standardDeductionSingle;
    }
    
    public BigDecimal getStandardDeductionMarried() {
        return standardDeductionMarried;
    }
    
    public BigDecimal getStandardDeductionHead() {
        return standardDeductionHead;
    }
    
    public BigDecimal getPersonalExemptionAmount() {
        return personalExemptionAmount;
    }
    
    public BigDecimal getPersonalExemptionPhaseout() {
        return personalExemptionPhaseout;
    }
    
    public BigDecimal getTaxCreditRate() {
        return taxCreditRate;
    }
    
    /**
     * Sets the federal tax brackets for this tax rate while maintaining bidirectional relationships.
     * 
     * @param brackets The list of federal tax brackets
     */
    public void setFederalTaxBrackets(List<FederalTaxBracket> brackets) {
        this.federalTaxBrackets.clear();
        if (brackets != null) {
            for (FederalTaxBracket bracket : brackets) {
                addFederalTaxBracket(bracket);
            }
        }
    }
}
