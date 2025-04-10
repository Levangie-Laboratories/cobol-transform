package com.payroll.service.impl;

import com.payroll.domain.Employee;
import com.payroll.domain.FederalTaxBracket;
import com.payroll.domain.TaxRate;
import com.payroll.domain.enums.FilingStatus;
import com.payroll.repository.TaxRateRepository;
import com.payroll.service.TaxCalculationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * Implementation of the TaxCalculationService.
 * This service calculates various tax amounts based on employee information and pay amounts.
 * It mirrors the logic from the original TAXCALC.cbl COBOL program.
 */
@Service
public class TaxCalculationServiceImpl implements TaxCalculationService {

    private static final Logger logger = LoggerFactory.getLogger(TaxCalculationServiceImpl.class);
    
    private final TaxRateRepository taxRateRepository;
    
    /**
     * Constructor with dependency injection.
     */
    @Autowired
    public TaxCalculationServiceImpl(TaxRateRepository taxRateRepository) {
        this.taxRateRepository = taxRateRepository;
    }
    
    /**
     * Calculate all taxes for an employee based on the provided gross pay and other parameters.
     */
    @Override
    public TaxResult calculateTaxes(Employee employee, BigDecimal grossPay, BigDecimal ytdGross,
                                 BigDecimal adjustments, int taxYear) {
        logger.debug("Calculating taxes for employee {} with gross pay {}", employee.getEmployeeId(), grossPay);
        
        // Skip tax calculation if employee has tax blocked flag
        if (employee.isTaxBlocked()) {
            logger.info("Tax calculation skipped for employee {} due to tax blocked flag", employee.getEmployeeId());
            return createZeroTaxResult();
        }
        
        TaxResult result = new TaxResult();
        
        // Calculate federal tax
        BigDecimal federalTax = calculateFederalTax(
            grossPay, 
            employee.getFederalFilingStatus(), 
            employee.getFederalAllowances(), 
            employee.getAdditionalFederalTax(),
            taxYear
        );
        result.setFederalTax(federalTax);
        
        // Calculate state tax
        String stateCode = employee.getState();
        BigDecimal stateTax = calculateStateTax(
            grossPay,
            stateCode,
            employee.getStateFilingStatus(),
            employee.getStateAllowances(),
            employee.getAdditionalStateTax(),
            taxYear
        );
        result.setStateTax(stateTax);
        
        // Calculate local tax (if applicable)
        // For simplicity, we'll assume no local tax if no local code is specified
        BigDecimal localTax = BigDecimal.ZERO;
        if (stateCode != null && !stateCode.isEmpty()) {
            localTax = calculateLocalTax(grossPay, stateCode, taxYear);
        }
        result.setLocalTax(localTax);
        
        // Calculate FICA taxes
        BigDecimal socialSecurityTax = calculateSocialSecurityTax(grossPay, ytdGross, taxYear);
        BigDecimal medicareTax = calculateMedicareTax(grossPay, ytdGross, taxYear);
        
        result.setSocialSecurityTax(socialSecurityTax);
        result.setMedicareTax(medicareTax);
        
        // Apply any manual adjustments
        if (adjustments != null && adjustments.compareTo(BigDecimal.ZERO) != 0) {
            // Adjustments are applied to federal tax for simplicity
            federalTax = federalTax.add(adjustments);
            result.setFederalTax(federalTax);
        }
        
        // Calculate the total tax
        result.calculateTotal();
        
        logger.debug("Tax calculation completed for employee {}: {}", employee.getEmployeeId(), result.getTotalTax());
        return result;
    }

    /**
     * Create a zero tax result object for cases where no taxes should be calculated.
     */
    private TaxResult createZeroTaxResult() {
        TaxResult result = new TaxResult();
        result.setFederalTax(BigDecimal.ZERO);
        result.setStateTax(BigDecimal.ZERO);
        result.setLocalTax(BigDecimal.ZERO);
        result.setSocialSecurityTax(BigDecimal.ZERO);
        result.setMedicareTax(BigDecimal.ZERO);
        result.setTotalTax(BigDecimal.ZERO);
        return result;
    }

    /**
     * Calculate federal income tax based on the provided parameters.
     */
    @Override
    public BigDecimal calculateFederalTax(BigDecimal grossPay, FilingStatus filingStatus,
                                        int allowances, BigDecimal additionalWithholding,
                                        int taxYear) {
        logger.debug("Calculating federal tax: grossPay={}, filingStatus={}, allowances={}, taxYear={}",
                   grossPay, filingStatus, allowances, taxYear);
        
        if (grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // If no filing status, default to single
        if (filingStatus == null) {
            filingStatus = FilingStatus.SINGLE;
        }
        
        // Get tax rates for the current year
        Optional<TaxRate> taxRateOpt = taxRateRepository.findByYearAndDate(taxYear, LocalDate.now());
        
        if (!taxRateOpt.isPresent()) {
            logger.warn("No tax rate found for year {}. Using default calculations.", taxYear);
            // Use a simplified default calculation
            return calculateDefaultFederalTax(grossPay, filingStatus, allowances);
        }
        
        TaxRate taxRate = taxRateOpt.get();
        
        // Get standard deduction based on filing status
        BigDecimal standardDeduction = taxRate.getStandardDeduction(filingStatus);
        
        // Calculate allowance value
        BigDecimal allowanceValue = taxRate.getPersonalExemptionAmount();
        BigDecimal totalAllowances = allowanceValue.multiply(new BigDecimal(allowances));
        
        // Calculate taxable income
        BigDecimal taxableIncome = grossPay
            .subtract(standardDeduction)
            .subtract(totalAllowances);
        
        // If taxable income is negative or zero, no tax is due
        if (taxableIncome.compareTo(BigDecimal.ZERO) <= 0) {
            return additionalWithholding != null ? additionalWithholding : BigDecimal.ZERO;
        }
        
        // Find the applicable tax bracket and calculate tax
        BigDecimal calculatedTax = BigDecimal.ZERO;
        boolean bracketFound = false;
        
        // Filter brackets for the correct filing status
        List<FederalTaxBracket> brackets = taxRate.getFederalTaxBrackets();
        for (FederalTaxBracket bracket : brackets) {
            if (bracket.getFilingStatus() == filingStatus && bracket.appliesTo(taxableIncome)) {
                calculatedTax = bracket.calculateTax(taxableIncome);
                bracketFound = true;
                break;
            }
        }
        
        if (!bracketFound) {
            logger.warn("No applicable tax bracket found for filing status {} and income {}. Using default calculation.",
                       filingStatus, taxableIncome);
            return calculateDefaultFederalTax(grossPay, filingStatus, allowances);
        }
        
        // Add any additional withholding
        if (additionalWithholding != null && additionalWithholding.compareTo(BigDecimal.ZERO) > 0) {
            calculatedTax = calculatedTax.add(additionalWithholding);
        }
        
        // Round to 2 decimal places
        calculatedTax = calculatedTax.setScale(2, RoundingMode.HALF_UP);
        
        logger.debug("Federal tax calculated: {}", calculatedTax);
        return calculatedTax;
    }

    /**
     * Calculate a default federal tax when no tax rates are available.
     * This is a simplified calculation used as a fallback only.
     */
    private BigDecimal calculateDefaultFederalTax(BigDecimal grossPay, FilingStatus filingStatus, int allowances) {
        // Default flat tax rate of 15%
        BigDecimal taxRate = new BigDecimal("0.15");
        
        // Default standard deduction
        BigDecimal standardDeduction;
        switch (filingStatus) {
            case MARRIED:
                standardDeduction = new BigDecimal("24000");
                break;
            case HEAD_OF_HOUSEHOLD:
                standardDeduction = new BigDecimal("18000");
                break;
            case SINGLE:
            default:
                standardDeduction = new BigDecimal("12000");
                break;
        }
        
        // Default allowance amount
        BigDecimal allowanceAmount = new BigDecimal("4050").multiply(new BigDecimal(allowances));
        
        // Calculate taxable income
        BigDecimal taxableIncome = grossPay
            .subtract(standardDeduction.divide(new BigDecimal("26"), 2, RoundingMode.HALF_UP))
            .subtract(allowanceAmount.divide(new BigDecimal("26"), 2, RoundingMode.HALF_UP));
        
        if (taxableIncome.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Calculate tax
        return taxableIncome.multiply(taxRate).setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate state income tax based on the provided parameters.
     */
    @Override
    public BigDecimal calculateStateTax(BigDecimal grossPay, String stateCode,
                                     FilingStatus filingStatus, int allowances,
                                     BigDecimal additionalWithholding, int taxYear) {
        logger.debug("Calculating state tax: grossPay={}, stateCode={}, filingStatus={}, taxYear={}",
                   grossPay, stateCode, filingStatus, taxYear);
        
        if (grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0 || stateCode == null || stateCode.isEmpty()) {
            return BigDecimal.ZERO;
        }
        
        // In a full implementation, this would look up state tax rates and brackets from the database
        // For now, we'll use a simplified calculation based on state code
        
        // Default state tax rate (simplified for migration example)
        BigDecimal taxRate;
        switch (stateCode) {
            case "CA":
                taxRate = new BigDecimal("0.06"); // 6%
                break;
            case "NY":
                taxRate = new BigDecimal("0.055"); // 5.5%
                break;
            case "TX":
                taxRate = BigDecimal.ZERO; // No state income tax
                break;
            case "FL":
                taxRate = BigDecimal.ZERO; // No state income tax
                break;
            default:
                taxRate = new BigDecimal("0.05"); // 5% default
                break;
        }
        
        // Calculate state tax
        BigDecimal stateTax = grossPay.multiply(taxRate).setScale(2, RoundingMode.HALF_UP);
        
        // Add any additional withholding
        if (additionalWithholding != null && additionalWithholding.compareTo(BigDecimal.ZERO) > 0) {
            stateTax = stateTax.add(additionalWithholding);
        }
        
        logger.debug("State tax calculated: {}", stateTax);
        return stateTax;
    }

    /**
     * Calculate local tax based on the provided parameters.
     */
    @Override
    public BigDecimal calculateLocalTax(BigDecimal grossPay, String localCode, int taxYear) {
        logger.debug("Calculating local tax: grossPay={}, localCode={}, taxYear={}",
                   grossPay, localCode, taxYear);
        
        if (grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0 || localCode == null || localCode.isEmpty()) {
            return BigDecimal.ZERO;
        }
        
        // In a full implementation, this would look up local tax rates from the database
        // For now, we'll use a simplified calculation
        
        // Default local tax rate (simplified for migration example)
        BigDecimal taxRate = new BigDecimal("0.01"); // 1% default local tax
        
        // Calculate local tax
        BigDecimal localTax = grossPay.multiply(taxRate).setScale(2, RoundingMode.HALF_UP);
        
        logger.debug("Local tax calculated: {}", localTax);
        return localTax;
    }

    /**
     * Calculate Social Security tax based on the provided parameters.
     */
    @Override
    public BigDecimal calculateSocialSecurityTax(BigDecimal grossPay, BigDecimal ytdGross, int taxYear) {
        logger.debug("Calculating Social Security tax: grossPay={}, ytdGross={}, taxYear={}",
                   grossPay, ytdGross, taxYear);
        
        if (grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Get tax rates for the current year
        Optional<TaxRate> taxRateOpt = taxRateRepository.findByYearAndDate(taxYear, LocalDate.now());
        
        // Default values in case no tax rate is found
        BigDecimal socialSecurityRate = new BigDecimal("0.062"); // 6.2%
        BigDecimal wageBase = new BigDecimal("142800"); // 2021 wage base
        
        if (taxRateOpt.isPresent()) {
            TaxRate taxRate = taxRateOpt.get();
            socialSecurityRate = taxRate.getSocialSecurityRate().divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
            wageBase = taxRate.getSocialSecurityWageBase();
        }
        
        // If YTD gross is null, assume it's zero
        if (ytdGross == null) {
            ytdGross = BigDecimal.ZERO;
        }
        
        // Check if employee has already reached the wage base
        if (ytdGross.compareTo(wageBase) >= 0) {
            return BigDecimal.ZERO;
        }
        
        // Calculate the remaining amount subject to Social Security tax
        BigDecimal remainingWageBase = wageBase.subtract(ytdGross);
        
        // Calculate the amount of current gross pay subject to Social Security tax
        BigDecimal taxableAmount;
        if (grossPay.compareTo(remainingWageBase) <= 0) {
            taxableAmount = grossPay;
        } else {
            taxableAmount = remainingWageBase;
        }
        
        // Calculate Social Security tax
        BigDecimal socialSecurityTax = taxableAmount.multiply(socialSecurityRate)
            .setScale(2, RoundingMode.HALF_UP);
        
        logger.debug("Social Security tax calculated: {}", socialSecurityTax);
        return socialSecurityTax;
    }

    /**
     * Calculate Medicare tax based on the provided parameters.
     */
    @Override
    public BigDecimal calculateMedicareTax(BigDecimal grossPay, BigDecimal ytdGross, int taxYear) {
        logger.debug("Calculating Medicare tax: grossPay={}, ytdGross={}, taxYear={}",
                   grossPay, ytdGross, taxYear);
        
        if (grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Get tax rates for the current year
        Optional<TaxRate> taxRateOpt = taxRateRepository.findByYearAndDate(taxYear, LocalDate.now());
        
        // Default values in case no tax rate is found
        BigDecimal medicareRate = new BigDecimal("0.0145"); // 1.45%
        BigDecimal additionalMedicareRate = new BigDecimal("0.009"); // 0.9%
        BigDecimal additionalMedicareThreshold = new BigDecimal("200000"); // $200,000
        
        if (taxRateOpt.isPresent()) {
            TaxRate taxRate = taxRateOpt.get();
            medicareRate = taxRate.getMedicareRate().divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
            additionalMedicareRate = taxRate.getMedicareAdditionalRate().divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
            additionalMedicareThreshold = taxRate.getMedicareAdditionalThreshold();
        }
        
        // If YTD gross is null, assume it's zero
        if (ytdGross == null) {
            ytdGross = BigDecimal.ZERO;
        }
        
        // Calculate regular Medicare tax (no wage base)
        BigDecimal regularMedicareTax = grossPay.multiply(medicareRate);
        
        // Calculate additional Medicare tax if applicable
        BigDecimal additionalMedicareTax = BigDecimal.ZERO;
        
        if (ytdGross.compareTo(additionalMedicareThreshold) >= 0) {
            // All of the current gross pay is subject to additional Medicare tax
            additionalMedicareTax = grossPay.multiply(additionalMedicareRate);
        } else if (ytdGross.add(grossPay).compareTo(additionalMedicareThreshold) > 0) {
            // Part of the current gross pay is subject to additional Medicare tax
            BigDecimal amountOverThreshold = ytdGross.add(grossPay).subtract(additionalMedicareThreshold);
            additionalMedicareTax = amountOverThreshold.multiply(additionalMedicareRate);
        }
        
        // Total Medicare tax
        BigDecimal medicareTax = regularMedicareTax.add(additionalMedicareTax)
            .setScale(2, RoundingMode.HALF_UP);
        
        logger.debug("Medicare tax calculated: {}", medicareTax);
        return medicareTax;
    }
}
