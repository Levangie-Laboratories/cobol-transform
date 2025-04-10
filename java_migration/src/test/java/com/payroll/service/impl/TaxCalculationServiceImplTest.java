package com.payroll.service.impl;

import com.payroll.domain.Employee;
import com.payroll.domain.FederalTaxBracket;
import com.payroll.domain.TaxRate;
import com.payroll.domain.enums.FilingStatus;
import com.payroll.repository.TaxRateRepository;
import com.payroll.service.TaxCalculationService.TaxResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

/**
 * Unit tests for TaxCalculationServiceImpl.
 * These tests verify that the Java implementation correctly reproduces the
 * tax calculation logic from the original COBOL program.
 */
@ExtendWith(MockitoExtension.class)
public class TaxCalculationServiceImplTest {

    @Mock
    private TaxRateRepository taxRateRepository;

    @InjectMocks
    private TaxCalculationServiceImpl taxCalculationService;

    private Employee testEmployee;
    private TaxRate testTaxRate;
    private final int TAX_YEAR = 2025;

    @BeforeEach
    void setUp() {
        // Set up test employee
        testEmployee = new Employee();
        testEmployee.setEmployeeId("EMP001");
        testEmployee.setFederalFilingStatus(FilingStatus.SINGLE);
        testEmployee.setFederalAllowances(2);
        testEmployee.setAdditionalFederalTax(new BigDecimal("50.00"));
        testEmployee.setState("CA");
        testEmployee.setStateFilingStatus(FilingStatus.SINGLE);
        testEmployee.setStateAllowances(2);
        testEmployee.setAdditionalStateTax(new BigDecimal("25.00"));
        testEmployee.setTaxBlocked(false);

        // Set up test tax rate with federal brackets
        testTaxRate = new TaxRate();
        testTaxRate.setTaxYear(TAX_YEAR);
        testTaxRate.setEffectiveDate(LocalDate.of(TAX_YEAR, 1, 1));
        testTaxRate.setExpirationDate(LocalDate.of(TAX_YEAR, 12, 31));
        
        // Social Security and Medicare rates
        testTaxRate.setSocialSecurityRate(new BigDecimal("6.20"));
        testTaxRate.setSocialSecurityWageBase(new BigDecimal("142800.00"));
        testTaxRate.setSocialSecurityMaxTax(new BigDecimal("8853.60"));
        testTaxRate.setMedicareRate(new BigDecimal("1.45"));
        testTaxRate.setMedicareAdditionalRate(new BigDecimal("0.90"));
        testTaxRate.setMedicareAdditionalThreshold(new BigDecimal("200000.00"));
        
        // Standard deductions
        testTaxRate.setStandardDeductionSingle(new BigDecimal("12950.00"));
        testTaxRate.setStandardDeductionMarried(new BigDecimal("25900.00"));
        testTaxRate.setStandardDeductionHead(new BigDecimal("19400.00"));
        testTaxRate.setPersonalExemptionAmount(new BigDecimal("4050.00"));
        
        // Set up federal tax brackets for SINGLE filing status
        List<FederalTaxBracket> brackets = new ArrayList<>();
        
        // Bracket 1: 10% up to $10,275
        FederalTaxBracket bracket1 = new FederalTaxBracket();
        bracket1.setFilingStatus(FilingStatus.SINGLE);
        bracket1.setFloor(BigDecimal.ZERO);
        bracket1.setCeiling(new BigDecimal("10275.00"));
        bracket1.setRate(new BigDecimal("10.00"));
        bracket1.setBaseTax(BigDecimal.ZERO);
        brackets.add(bracket1);
        
        // Bracket 2: 12% $10,276 to $41,775
        FederalTaxBracket bracket2 = new FederalTaxBracket();
        bracket2.setFilingStatus(FilingStatus.SINGLE);
        bracket2.setFloor(new BigDecimal("10275.01"));
        bracket2.setCeiling(new BigDecimal("41775.00"));
        bracket2.setRate(new BigDecimal("12.00"));
        bracket2.setBaseTax(new BigDecimal("1027.50"));
        brackets.add(bracket2);
        
        // Bracket 3: 22% $41,776 to $89,075
        FederalTaxBracket bracket3 = new FederalTaxBracket();
        bracket3.setFilingStatus(FilingStatus.SINGLE);
        bracket3.setFloor(new BigDecimal("41775.01"));
        bracket3.setCeiling(new BigDecimal("89075.00"));
        bracket3.setRate(new BigDecimal("22.00"));
        bracket3.setBaseTax(new BigDecimal("4807.50"));
        brackets.add(bracket3);
        
        testTaxRate.setFederalTaxBrackets(brackets);
        
        // Configure mock repository behavior
        when(taxRateRepository.findByYearAndDate(eq(TAX_YEAR), any(LocalDate.class)))
            .thenReturn(Optional.of(testTaxRate));
    }

    @Test
    @DisplayName("Test employee with tax blocked should have zero taxes")
    void testEmployeeWithTaxBlockedShouldHaveZeroTaxes() {
        // Set tax blocked
        testEmployee.setTaxBlocked(true);
        
        // Calculate taxes
        BigDecimal grossPay = new BigDecimal("2000.00");
        BigDecimal ytdGross = new BigDecimal("10000.00");
        TaxResult result = taxCalculationService.calculateTaxes(
            testEmployee, grossPay, ytdGross, null, TAX_YEAR);
        
        // Verify all taxes are zero
        assertEquals(BigDecimal.ZERO, result.getFederalTax());
        assertEquals(BigDecimal.ZERO, result.getStateTax());
        assertEquals(BigDecimal.ZERO, result.getLocalTax());
        assertEquals(BigDecimal.ZERO, result.getSocialSecurityTax());
        assertEquals(BigDecimal.ZERO, result.getMedicareTax());
        assertEquals(BigDecimal.ZERO, result.getTotalTax());
    }

    @Test
    @DisplayName("Test federal tax calculation for standard case")
    void testFederalTaxCalculation() {
        // Test with standard biweekly gross pay of $2000
        BigDecimal grossPay = new BigDecimal("2000.00");
        
        // Calculate federal tax only
        BigDecimal federalTax = taxCalculationService.calculateFederalTax(
            grossPay, 
            testEmployee.getFederalFilingStatus(), 
            testEmployee.getFederalAllowances(),
            testEmployee.getAdditionalFederalTax(),
            TAX_YEAR);
            
        // Expected calculation:
        // Biweekly standard deduction: $12950/26 = $498.08
        // Allowances: 2 * $4050/26 = $311.54
        // Taxable income: $2000 - $498.08 - $311.54 = $1190.38
        // Tax bracket 1: 10% of $1190.38 = $119.04
        // Additional withholding: $50.00
        // Total federal tax: $119.04 + $50.00 = $169.04
        
        // Allow for small rounding differences due to division by pay periods
        BigDecimal expectedTax = new BigDecimal("169.04");
        BigDecimal tolerance = new BigDecimal("1.00");
        
        assertTrue(expectedTax.subtract(federalTax).abs().compareTo(tolerance) <= 0,
            "Expected federal tax around " + expectedTax + " but got " + federalTax);
    }

    @Test
    @DisplayName("Test Social Security tax calculation")
    void testSocialSecurityTaxCalculation() {
        // Test with gross pay of $2000 and YTD gross of $10000
        BigDecimal grossPay = new BigDecimal("2000.00");
        BigDecimal ytdGross = new BigDecimal("10000.00");
        
        // Calculate Social Security tax
        BigDecimal ssTax = taxCalculationService.calculateSocialSecurityTax(
            grossPay, ytdGross, TAX_YEAR);
        
        // Expected: $2000 * 6.2% = $124.00
        BigDecimal expectedTax = new BigDecimal("124.00");
        assertEquals(expectedTax, ssTax, "Social Security tax should be " + expectedTax);
        
        // Test with YTD gross exceeding wage base
        ytdGross = new BigDecimal("142800.00"); // Exactly at wage base
        ssTax = taxCalculationService.calculateSocialSecurityTax(
            grossPay, ytdGross, TAX_YEAR);
        assertEquals(BigDecimal.ZERO, ssTax, "Social Security tax should be zero when YTD exceeds wage base");
        
        // Test with YTD gross approaching wage base
        ytdGross = new BigDecimal("141800.00"); // $1000 below wage base
        ssTax = taxCalculationService.calculateSocialSecurityTax(
            grossPay, ytdGross, TAX_YEAR);
        expectedTax = new BigDecimal("62.00"); // 6.2% of $1000 (the remaining amount under the wage base)
        assertEquals(expectedTax, ssTax, "Social Security tax should be limited to remaining wage base");
    }

    @Test
    @DisplayName("Test Medicare tax calculation")
    void testMedicareTaxCalculation() {
        // Test with gross pay of $2000 and YTD gross of $10000
        BigDecimal grossPay = new BigDecimal("2000.00");
        BigDecimal ytdGross = new BigDecimal("10000.00");
        
        // Calculate Medicare tax
        BigDecimal medicareTax = taxCalculationService.calculateMedicareTax(
            grossPay, ytdGross, TAX_YEAR);
        
        // Expected: $2000 * 1.45% = $29.00
        BigDecimal expectedTax = new BigDecimal("29.00");
        assertEquals(expectedTax, medicareTax, "Medicare tax should be " + expectedTax);
        
        // Test with YTD gross exceeding additional Medicare threshold
        ytdGross = new BigDecimal("200000.00"); // Exactly at threshold
        medicareTax = taxCalculationService.calculateMedicareTax(
            grossPay, ytdGross, TAX_YEAR);
        expectedTax = new BigDecimal("47.00"); // 1.45% + 0.9% additional = 2.35% of $2000
        assertEquals(expectedTax, medicareTax, "Medicare tax should include additional rate above threshold");
        
        // Test with YTD gross approaching additional Medicare threshold
        ytdGross = new BigDecimal("199000.00"); // $1000 below threshold
        grossPay = new BigDecimal("2000.00");
        medicareTax = taxCalculationService.calculateMedicareTax(
            grossPay, ytdGross, TAX_YEAR);
        
        // Regular rate on first $1000: $1000 * 1.45% = $14.50
        // Combined rate on remaining $1000: $1000 * 2.35% = $23.50
        // Total: $38.00
        expectedTax = new BigDecimal("38.00");
        assertEquals(expectedTax, medicareTax, "Medicare tax should handle threshold crossing correctly");
    }

    @Test
    @DisplayName("Test complete tax calculation")
    void testCompleteTaxCalculation() {
        // Test with typical biweekly pay and YTD amounts
        BigDecimal grossPay = new BigDecimal("2000.00");
        BigDecimal ytdGross = new BigDecimal("10000.00");
        
        // Calculate complete taxes
        TaxResult result = taxCalculationService.calculateTaxes(
            testEmployee, grossPay, ytdGross, null, TAX_YEAR);
        
        // Verify that all tax components are calculated
        assertNotNull(result.getFederalTax(), "Federal tax should be calculated");
        assertNotNull(result.getStateTax(), "State tax should be calculated");
        assertNotNull(result.getSocialSecurityTax(), "Social Security tax should be calculated");
        assertNotNull(result.getMedicareTax(), "Medicare tax should be calculated");
        assertNotNull(result.getTotalTax(), "Total tax should be calculated");
        
        // Verify that total tax equals sum of components
        BigDecimal expectedTotal = result.getFederalTax()
            .add(result.getStateTax())
            .add(result.getLocalTax() != null ? result.getLocalTax() : BigDecimal.ZERO)
            .add(result.getSocialSecurityTax())
            .add(result.getMedicareTax());
        
        assertEquals(expectedTotal, result.getTotalTax(), "Total tax should equal sum of components");
    }

    @Test
    @DisplayName("Test tax calculation with manual adjustment")
    void testTaxCalculationWithAdjustment() {
        // Test with standard amounts and a manual adjustment
        BigDecimal grossPay = new BigDecimal("2000.00");
        BigDecimal ytdGross = new BigDecimal("10000.00");
        BigDecimal adjustment = new BigDecimal("100.00");
        
        // Calculate taxes with adjustment
        TaxResult result = taxCalculationService.calculateTaxes(
            testEmployee, grossPay, ytdGross, adjustment, TAX_YEAR);
        
        // Calculate taxes without adjustment for comparison
        TaxResult baseResult = taxCalculationService.calculateTaxes(
            testEmployee, grossPay, ytdGross, null, TAX_YEAR);
        
        // Verify adjustment is applied (should be to federal tax in current implementation)
        BigDecimal expectedFederalWithAdjustment = baseResult.getFederalTax().add(adjustment);
        assertEquals(expectedFederalWithAdjustment, result.getFederalTax(), 
            "Federal tax should include manual adjustment");
        
        // Total tax should also reflect the adjustment
        BigDecimal expectedTotalWithAdjustment = baseResult.getTotalTax().add(adjustment);
        assertEquals(expectedTotalWithAdjustment, result.getTotalTax(), 
            "Total tax should reflect the manual adjustment");
    }

    @Test
    @DisplayName("Test edge case with zero gross pay")
    void testZeroGrossPay() {
        // Test with zero gross pay
        BigDecimal grossPay = BigDecimal.ZERO;
        BigDecimal ytdGross = new BigDecimal("10000.00");
        
        // Calculate taxes
        TaxResult result = taxCalculationService.calculateTaxes(
            testEmployee, grossPay, ytdGross, null, TAX_YEAR);
        
        // Verify all taxes are zero
        assertEquals(BigDecimal.ZERO, result.getFederalTax());
        assertEquals(BigDecimal.ZERO, result.getStateTax());
        assertEquals(BigDecimal.ZERO, result.getSocialSecurityTax());
        assertEquals(BigDecimal.ZERO, result.getMedicareTax());
        assertEquals(BigDecimal.ZERO, result.getTotalTax());
    }
}
