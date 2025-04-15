package com.payroll.domain;

import com.payroll.domain.enums.FilingStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.math.BigDecimal;

/**
 * Entity representing a federal tax bracket.
 * Maps to the FED-TAX-BRACKET structure in TAXRATES.cpy.
 */
@Entity
@Table(name = "federal_tax_brackets")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FederalTaxBracket {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tax_rate_id", nullable = false)
    private TaxRate taxRate;
    
    @Column(name = "bracket_index", nullable = false)
    private Integer bracketIndex; // Position in the bracket array (1-7)
    
    @Enumerated(EnumType.STRING)
    @Column(name = "filing_status", nullable = false)
    private FilingStatus filingStatus; // Maps to FED-FILING-STATUS
    
    @Column(name = "bracket_floor", precision = 10, scale = 2, nullable = false)
    private BigDecimal bracketFloor; // Maps to FED-BRACKET-FLOOR
    
    @Column(name = "bracket_ceiling", precision = 10, scale = 2, nullable = false)
    private BigDecimal bracketCeiling; // Maps to FED-BRACKET-CEILING
    
    @Column(name = "bracket_rate", precision = 4, scale = 2, nullable = false)
    private BigDecimal bracketRate; // Maps to FED-BRACKET-RATE
    
    @Column(name = "base_tax", precision = 10, scale = 2, nullable = false)
    private BigDecimal baseTax; // Maps to FED-BRACKET-BASE-TAX
    
    /**
     * Checks if a given income amount falls into this tax bracket.
     * 
     * @param taxableIncome The income amount to check
     * @return true if the income is within this bracket's range
     */
    public boolean appliesTo(BigDecimal taxableIncome) {
        if (taxableIncome.compareTo(bracketFloor) < 0) {
            return false;
        }
        
        // If ceiling is zero or income is less than ceiling, this bracket applies
        return bracketCeiling.compareTo(BigDecimal.ZERO) == 0 || 
               taxableIncome.compareTo(bracketCeiling) < 0;
    }
    
    /**
     * Calculates the tax for the portion of income that falls within this bracket.
     * 
     * @param taxableIncome The total taxable income
     * @return The tax amount for the portion within this bracket
     */
    public BigDecimal calculateTax(BigDecimal taxableIncome) {
        if (!appliesTo(taxableIncome)) {
            return BigDecimal.ZERO;
        }
        
        // Calculate tax: base tax + rate * (income - floor)
        BigDecimal incomeInBracket = taxableIncome.subtract(bracketFloor);
        BigDecimal bracketTax = incomeInBracket.multiply(bracketRate.divide(new BigDecimal("100")));
        
        return baseTax.add(bracketTax);
    }
    
    // Manual setter method to resolve compilation issues with Lombok
    public void setTaxRate(TaxRate taxRate) {
        this.taxRate = taxRate;
    }
    
    // Convenience methods to match test method names
    public void setFloor(BigDecimal floor) {
        this.bracketFloor = floor;
    }
    
    public void setCeiling(BigDecimal ceiling) {
        this.bracketCeiling = ceiling;
    }
    
    public void setRate(BigDecimal rate) {
        this.bracketRate = rate;
    }
}
