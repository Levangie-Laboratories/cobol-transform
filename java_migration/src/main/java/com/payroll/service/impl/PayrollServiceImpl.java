package com.payroll.service.impl;

import com.payroll.domain.Employee;
import com.payroll.domain.PayrollData;
import com.payroll.domain.enums.EmploymentStatus;
import com.payroll.domain.enums.PayType;
import com.payroll.repository.EmployeeRepository;
import com.payroll.repository.PayrollDataRepository;
import com.payroll.repository.DeductionTypeRepository;
import com.payroll.repository.TaxRateRepository;
import com.payroll.service.DeductionCalculationService;
import com.payroll.service.DeductionCalculationService.DeductionResult;
import com.payroll.service.PayStubService;
import com.payroll.service.PayStubService.PayStub;
import com.payroll.service.PayrollService;
import com.payroll.service.TaxCalculationService;
import com.payroll.service.TaxCalculationService.TaxResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.Year;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of the PayrollService.
 * This service orchestrates the entire payroll process and coordinates between other services.
 * It mirrors the logic from the original PAYCALC.cbl COBOL program.
 */
@Service
public class PayrollServiceImpl implements PayrollService {

    private static final Logger logger = LoggerFactory.getLogger(PayrollServiceImpl.class);
    
    private final EmployeeRepository employeeRepository;
    private final PayrollDataRepository payrollDataRepository;
    private final DeductionTypeRepository deductionTypeRepository;
    private final TaxRateRepository taxRateRepository;
    private final TaxCalculationService taxCalculationService;
    private final DeductionCalculationService deductionCalculationService;
    private final PayStubService payStubService;
    
    /**
     * Constructor with dependency injection.
     */
    @Autowired
    public PayrollServiceImpl(EmployeeRepository employeeRepository,
                             PayrollDataRepository payrollDataRepository,
                             DeductionTypeRepository deductionTypeRepository,
                             TaxRateRepository taxRateRepository,
                             TaxCalculationService taxCalculationService,
                             DeductionCalculationService deductionCalculationService,
                             PayStubService payStubService) {
        this.employeeRepository = employeeRepository;
        this.payrollDataRepository = payrollDataRepository;
        this.deductionTypeRepository = deductionTypeRepository;
        this.taxRateRepository = taxRateRepository;
        this.taxCalculationService = taxCalculationService;
        this.deductionCalculationService = deductionCalculationService;
        this.payStubService = payStubService;
    }
    
    /**
     * Process payroll for a specific pay period.
     * This is the main entry point for payroll processing.
     */
    @Override
    @Transactional
    public PayrollSummary processPayroll(LocalDate payPeriodStartDate, LocalDate payPeriodEndDate) {
        logger.info("Starting payroll processing for period from {} to {}", 
                  payPeriodStartDate, payPeriodEndDate);
        
        // Initialize payroll summary
        PayrollSummary summary = new PayrollSummary();
        summary.setPayPeriodStartDate(payPeriodStartDate);
        summary.setPayPeriodEndDate(payPeriodEndDate);
        summary.setProcessDate(LocalDate.now());
        
        // Find all payroll data records for this pay period
        List<PayrollData> payrollDataRecords = payrollDataRepository
            .findByPayPeriodStartDateAndPayPeriodEndDate(payPeriodStartDate, payPeriodEndDate);
        
        logger.info("Found {} payroll records to process", payrollDataRecords.size());
        
        // Process each employee's payroll record
        List<String> processedEmployees = new ArrayList<>();
        List<String> failedEmployees = new ArrayList<>();
        
        for (PayrollData payrollData : payrollDataRecords) {
            String employeeId = payrollData.getEmployeeId();
            
            try {
                // Find employee record
                Employee employee = employeeRepository.findById(employeeId)
                    .orElseThrow(() -> new RuntimeException("Employee not found: " + employeeId));
                
                // Process employee payroll
                PayStub payStub = processEmployeePayroll(employee, payrollData);
                
                // Update summary with employee results
                updatePayrollSummary(summary, payrollData, payStub);
                
                processedEmployees.add(employeeId);
                logger.debug("Successfully processed payroll for employee {}", employeeId);
                
            } catch (Exception e) {
                logger.error("Error processing payroll for employee {}: {}", employeeId, e.getMessage(), e);
                failedEmployees.add(employeeId);
            }
        }
        
        logger.info("Payroll processing completed. Processed: {}, Failed: {}", 
                  processedEmployees.size(), failedEmployees.size());
        
        return summary;
    }
    
    /**
     * Update payroll summary with employee results.
     */
    private void updatePayrollSummary(PayrollSummary summary, PayrollData payrollData, PayStub payStub) {
        // Extract values from pay stub
        BigDecimal grossPay = payStub.getGrossPay();
        BigDecimal regularPay = payStub.getRegularPay();
        BigDecimal overtimePay = payStub.getOvertimePay();
        BigDecimal otherPay = payStub.getOtherPay();
        
        // Create a tax result object from pay stub
        TaxResult taxResult = new TaxResult();
        taxResult.setFederalTax(payStub.getFederalTax());
        taxResult.setStateTax(payStub.getStateTax());
        taxResult.setLocalTax(payStub.getLocalTax());
        taxResult.setSocialSecurityTax(payStub.getSocialSecurityTax());
        taxResult.setMedicareTax(payStub.getMedicareTax());
        taxResult.setTotalTax(payStub.getTotalTaxes());
        
        // Create a deduction result object from pay stub
        DeductionResult deductionResult = new DeductionResult();
        deductionResult.setTotalDeductions(payStub.getTotalDeductions());
        
        // Add to summary
        summary.addEmployeeResults(
            grossPay, regularPay, overtimePay, otherPay,
            taxResult, deductionResult, payStub.getNetPay());
    }

    /**
     * Process payroll for a specific employee in a specific pay period.
     */
    @Override
    @Transactional
    public PayStub processEmployeePayroll(Employee employee, PayrollData payrollData) {
        String employeeId = employee.getEmployeeId();
        logger.debug("Processing payroll for employee {}", employeeId);
        
        // Validate employee status
        if (employee.getStatus() != EmploymentStatus.ACTIVE) {
            logger.warn("Employee {} is not active. Status: {}", employeeId, employee.getStatus());
            throw new IllegalStateException("Employee is not active: " + employeeId);
        }
        
        // Calculate gross pay
        PayCalculationResult payCalculation = calculateGrossPay(employee, payrollData);
        BigDecimal grossPay = payCalculation.getGrossPay();
        BigDecimal regularPay = payCalculation.getRegularPay();
        BigDecimal overtimePay = payCalculation.getOvertimePay();
        BigDecimal otherPay = payCalculation.getOtherPay();
        
        logger.debug("Calculated gross pay for employee {}: {}", employeeId, grossPay);
        
        // Calculate taxes
        int taxYear = getPayYear(payrollData.getPayPeriodEndDate());
        BigDecimal ytdGross = employee.getYtdGross() != null ? employee.getYtdGross() : BigDecimal.ZERO;
        BigDecimal taxAdjustment = payrollData.getManualTaxAdjustment();
        
        TaxResult taxResult = taxCalculationService.calculateTaxes(
            employee, grossPay, ytdGross, taxAdjustment, taxYear);
        
        logger.debug("Calculated taxes for employee {}: {}", employeeId, taxResult.getTotalTax());
        
        // Calculate deductions
        BigDecimal deductionAdjustment = payrollData.getManualDeductionAdjustment();
        BigDecimal hours = payrollData.getRegularHours();
        
        DeductionResult deductionResult = deductionCalculationService.calculateDeductions(
            employee, grossPay, hours, deductionTypeRepository.findAllActive(), deductionAdjustment);
        
        logger.debug("Calculated deductions for employee {}: {}", 
                   employeeId, deductionResult.getTotalDeductions());
        
        // Calculate net pay
        BigDecimal netPay = calculateNetPay(grossPay, taxResult, deductionResult);
        logger.debug("Calculated net pay for employee {}: {}", employeeId, netPay);
        
        // Update employee YTD totals
        employee = updateEmployeeYtdTotals(
            employee, grossPay, taxResult, deductionResult, netPay);
        
        // Save updated employee record
        employeeRepository.save(employee);
        
        // Generate pay stub
        PayStub payStub = payStubService.generatePayStub(
            employee, payrollData, grossPay, regularPay, overtimePay, otherPay,
            taxResult, deductionResult, netPay);
        
        // Save pay stub
        payStubService.savePayStub(payStub);
        
        logger.info("Successfully processed payroll for employee {}", employeeId);
        return payStub;
    }

    /**
     * Calculate gross pay for an employee.
     */
    @Override
    public PayCalculationResult calculateGrossPay(Employee employee, PayrollData payrollData) {
        logger.debug("Calculating gross pay for employee {}", employee.getEmployeeId());
        
        PayCalculationResult result = new PayCalculationResult();
        
        // Initialize amounts
        BigDecimal regularPay = BigDecimal.ZERO;
        BigDecimal overtimePay = BigDecimal.ZERO;
        BigDecimal otherPay = BigDecimal.ZERO;
        
        // Calculate based on pay type
        if (employee.getPayType() == PayType.HOURLY) {
            // Hourly employee: pay = hours * rate
            BigDecimal hourlyRate = employee.getHourlyRate();
            BigDecimal overtimeRate = employee.getOvertimeRate();
            BigDecimal regularHours = payrollData.getRegularHours();
            BigDecimal overtimeHours = payrollData.getOvertimeHours();
            
            if (hourlyRate != null && regularHours != null) {
                regularPay = hourlyRate.multiply(regularHours)
                    .setScale(2, RoundingMode.HALF_UP);
            }
            
            if (hourlyRate != null && overtimeRate != null && overtimeHours != null) {
                // Overtime pay = overtime hours * hourly rate * overtime rate
                overtimePay = overtimeHours.multiply(hourlyRate).multiply(overtimeRate)
                    .setScale(2, RoundingMode.HALF_UP);
            }
            
        } else if (employee.getPayType() == PayType.SALARY) {
            // Salaried employee: pay = salary amount based on frequency
            BigDecimal salaryAmount = employee.getSalaryAmount();
            
            if (salaryAmount != null) {
                switch (employee.getPayFrequency()) {
                    case WEEKLY:
                        regularPay = salaryAmount.divide(new BigDecimal("52"), 2, RoundingMode.HALF_UP);
                        break;
                    case BIWEEKLY:
                        regularPay = salaryAmount.divide(new BigDecimal("26"), 2, RoundingMode.HALF_UP);
                        break;
                    case MONTHLY:
                        regularPay = salaryAmount.divide(new BigDecimal("12"), 2, RoundingMode.HALF_UP);
                        break;
                    case SEMI_MONTHLY:
                        regularPay = salaryAmount.divide(new BigDecimal("24"), 2, RoundingMode.HALF_UP);
                        break;
                    default:
                        regularPay = salaryAmount.divide(new BigDecimal("26"), 2, RoundingMode.HALF_UP); // Default to bi-weekly
                }
            }
        }
        
        // Add bonuses, commissions, etc.
        if (payrollData.getBonusAmount() != null) {
            otherPay = otherPay.add(payrollData.getBonusAmount());
        }
        
        if (payrollData.getCommissionAmount() != null) {
            otherPay = otherPay.add(payrollData.getCommissionAmount());
        } else if (payrollData.getCommissionRate() != null && payrollData.getCommissionSales() != null) {
            // Calculate commission based on sales and rate
            BigDecimal commission = payrollData.getCommissionSales()
                .multiply(payrollData.getCommissionRate().divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP))
                .setScale(2, RoundingMode.HALF_UP);
            otherPay = otherPay.add(commission);
        }
        
        if (payrollData.getRetroPayAmount() != null) {
            otherPay = otherPay.add(payrollData.getRetroPayAmount());
        }
        
        // Apply rate override if specified
        if (payrollData.isOverrideRate() && payrollData.getOverrideRateAmount() != null) {
            regularPay = payrollData.getOverrideRateAmount();
            // With override, ignore calculated regular pay
        }
        
        // Set results
        result.setRegularPay(regularPay);
        result.setOvertimePay(overtimePay);
        result.setOtherPay(otherPay);
        
        // Calculate gross pay
        BigDecimal grossPay = regularPay.add(overtimePay).add(otherPay)
            .setScale(2, RoundingMode.HALF_UP);
        result.setGrossPay(grossPay);
        
        logger.debug("Gross pay calculation for employee {}: regular={}, overtime={}, other={}, gross={}",
                   employee.getEmployeeId(), regularPay, overtimePay, otherPay, grossPay);
        
        return result;
    }
    
    /**
     * Calculate net pay based on gross pay, taxes, and deductions.
     */
    private BigDecimal calculateNetPay(BigDecimal grossPay, TaxResult taxResult, DeductionResult deductionResult) {
        BigDecimal totalTaxes = taxResult.getTotalTax() != null ? 
            taxResult.getTotalTax() : BigDecimal.ZERO;
        
        BigDecimal totalDeductions = deductionResult.getTotalDeductions() != null ?
            deductionResult.getTotalDeductions() : BigDecimal.ZERO;
        
        BigDecimal netPay = grossPay.subtract(totalTaxes).subtract(totalDeductions)
            .setScale(2, RoundingMode.HALF_UP);
        
        // Net pay cannot be negative
        if (netPay.compareTo(BigDecimal.ZERO) < 0) {
            logger.warn("Calculated negative net pay - setting to zero. Gross: {}, Taxes: {}, Deductions: {}",
                       grossPay, totalTaxes, totalDeductions);
            netPay = BigDecimal.ZERO;
        }
        
        return netPay;
    }

    /**
     * Update an employee's year-to-date totals after processing a pay period.
     */
    @Override
    public Employee updateEmployeeYtdTotals(Employee employee, BigDecimal grossPay,
                                          TaxResult taxResult, DeductionResult deductionResult,
                                          BigDecimal netPay) {
        logger.debug("Updating YTD totals for employee {}", employee.getEmployeeId());
        
        // Update YTD gross pay
        BigDecimal ytdGross = employee.getYtdGross() != null ? 
            employee.getYtdGross() : BigDecimal.ZERO;
        ytdGross = ytdGross.add(grossPay).setScale(2, RoundingMode.HALF_UP);
        employee.setYtdGross(ytdGross);
        
        // Update YTD tax amounts
        if (taxResult != null) {
            // Federal tax
            if (taxResult.getFederalTax() != null) {
                BigDecimal ytdFederal = employee.getYtdFederalTax() != null ?
                    employee.getYtdFederalTax() : BigDecimal.ZERO;
                ytdFederal = ytdFederal.add(taxResult.getFederalTax()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdFederalTax(ytdFederal);
            }
            
            // State tax
            if (taxResult.getStateTax() != null) {
                BigDecimal ytdState = employee.getYtdStateTax() != null ?
                    employee.getYtdStateTax() : BigDecimal.ZERO;
                ytdState = ytdState.add(taxResult.getStateTax()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdStateTax(ytdState);
            }
            
            // Social Security tax
            if (taxResult.getSocialSecurityTax() != null) {
                BigDecimal ytdSs = employee.getYtdSocialSecurity() != null ?
                    employee.getYtdSocialSecurity() : BigDecimal.ZERO;
                ytdSs = ytdSs.add(taxResult.getSocialSecurityTax()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdSocialSecurity(ytdSs);
            }
            
            // Medicare tax
            if (taxResult.getMedicareTax() != null) {
                BigDecimal ytdMedicare = employee.getYtdMedicare() != null ?
                    employee.getYtdMedicare() : BigDecimal.ZERO;
                ytdMedicare = ytdMedicare.add(taxResult.getMedicareTax()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdMedicare(ytdMedicare);
            }
        }
        
        // Update YTD deduction amounts
        if (deductionResult != null) {
            // 401k
            if (deductionResult.getRetirement401k() != null) {
                BigDecimal ytd401k = employee.getYtd401k() != null ?
                    employee.getYtd401k() : BigDecimal.ZERO;
                ytd401k = ytd401k.add(deductionResult.getRetirement401k()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtd401k(ytd401k);
            }
            
            // Health
            if (deductionResult.getHealthInsurance() != null) {
                BigDecimal ytdHealth = employee.getYtdHealthDeduction() != null ?
                    employee.getYtdHealthDeduction() : BigDecimal.ZERO;
                ytdHealth = ytdHealth.add(deductionResult.getHealthInsurance()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdHealthDeduction(ytdHealth);
            }
            
            // Dental
            if (deductionResult.getDentalInsurance() != null) {
                BigDecimal ytdDental = employee.getYtdDentalDeduction() != null ?
                    employee.getYtdDentalDeduction() : BigDecimal.ZERO;
                ytdDental = ytdDental.add(deductionResult.getDentalInsurance()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdDentalDeduction(ytdDental);
            }
            
            // Vision
            if (deductionResult.getVisionInsurance() != null) {
                BigDecimal ytdVision = employee.getYtdVisionDeduction() != null ?
                    employee.getYtdVisionDeduction() : BigDecimal.ZERO;
                ytdVision = ytdVision.add(deductionResult.getVisionInsurance()).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdVisionDeduction(ytdVision);
            }
            
            // Other deductions
            BigDecimal otherDeductions = BigDecimal.ZERO;
            if (deductionResult.getLoanRepayment() != null) {
                otherDeductions = otherDeductions.add(deductionResult.getLoanRepayment());
            }
            if (deductionResult.getGarnishment() != null) {
                otherDeductions = otherDeductions.add(deductionResult.getGarnishment());
            }
            if (deductionResult.getCharityContribution() != null) {
                otherDeductions = otherDeductions.add(deductionResult.getCharityContribution());
            }
            if (deductionResult.getUnionDues() != null) {
                otherDeductions = otherDeductions.add(deductionResult.getUnionDues());
            }
            
            if (otherDeductions.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal ytdOther = employee.getYtdOtherDeduction() != null ?
                    employee.getYtdOtherDeduction() : BigDecimal.ZERO;
                ytdOther = ytdOther.add(otherDeductions).setScale(2, RoundingMode.HALF_UP);
                employee.setYtdOtherDeduction(ytdOther);
            }
        }
        
        // Update YTD net pay
        BigDecimal ytdNet = employee.getYtdNetPay() != null ?
            employee.getYtdNetPay() : BigDecimal.ZERO;
        ytdNet = ytdNet.add(netPay).setScale(2, RoundingMode.HALF_UP);
        employee.setYtdNetPay(ytdNet);
        
        // Update last pay date
        employee.setLastPayDate(LocalDate.now());
        
        logger.debug("Updated YTD totals for employee {}: gross={}, net={}", 
                   employee.getEmployeeId(), ytdGross, ytdNet);
        
        return employee;
    }

    /**
     * Generate a detailed payroll report for a specific pay period.
     */
    @Override
    public String generatePayrollReport(LocalDate payPeriodStartDate, LocalDate payPeriodEndDate) {
        logger.info("Generating payroll report for period from {} to {}", 
                  payPeriodStartDate, payPeriodEndDate);
        
        // This would generate a detailed report for the pay period
        // For now, it's a simplified implementation
        
        StringBuilder report = new StringBuilder();
        report.append("=======================================================\n");
        report.append("                  PAYROLL REPORT\n");
        report.append("=======================================================\n");
        report.append("Pay Period: ").append(payPeriodStartDate).append(" to ").append(payPeriodEndDate).append("\n");
        report.append("Generated on: ").append(LocalDate.now()).append("\n");
        report.append("=======================================================\n");
        
        // Find all payroll data records for this pay period
        List<PayrollData> payrollDataRecords = payrollDataRepository
            .findByPayPeriodStartDateAndPayPeriodEndDate(payPeriodStartDate, payPeriodEndDate);
        
        report.append("Total Records: ").append(payrollDataRecords.size()).append("\n\n");
        
        // In a real implementation, would process the records and generate a detailed report
        report.append("Detailed employee information would be included here.\n");
        
        return report.toString();
    }

    /**
     * Generate a summary payroll report for a specific pay period.
     */
    @Override
    public String generatePayrollSummaryReport(PayrollSummary payrollSummary) {
        logger.info("Generating payroll summary report");
        
        StringBuilder report = new StringBuilder();
        report.append("=======================================================\n");
        report.append("               PAYROLL SUMMARY REPORT\n");
        report.append("=======================================================\n");
        report.append("Pay Period: ").append(payrollSummary.getPayPeriodStartDate())
              .append(" to ").append(payrollSummary.getPayPeriodEndDate()).append("\n");
        report.append("Process Date: ").append(payrollSummary.getProcessDate()).append("\n");
        report.append("=======================================================\n\n");
        
        report.append("SUMMARY TOTALS:\n");
        report.append("----------------\n");
        report.append(String.format("%-30s %12d\n", "Number of Employees:", payrollSummary.getEmployeeCount()));
        report.append(String.format("%-30s %12.2f\n", "Total Gross Pay:", getValue(payrollSummary.getTotalGrossPay())));
        report.append(String.format("%-30s %12.2f\n", "Total Regular Pay:", getValue(payrollSummary.getTotalRegularPay())));
        report.append(String.format("%-30s %12.2f\n", "Total Overtime Pay:", getValue(payrollSummary.getTotalOvertimePay())));
        report.append(String.format("%-30s %12.2f\n", "Total Other Pay:", getValue(payrollSummary.getTotalOtherPay())));
        report.append("\n");
        
        report.append("TAX TOTALS:\n");
        report.append("-----------\n");
        report.append(String.format("%-30s %12.2f\n", "Total Federal Tax:", getValue(payrollSummary.getTotalFederalTax())));
        report.append(String.format("%-30s %12.2f\n", "Total State Tax:", getValue(payrollSummary.getTotalStateTax())));
        report.append(String.format("%-30s %12.2f\n", "Total Local Tax:", getValue(payrollSummary.getTotalLocalTax())));
        report.append(String.format("%-30s %12.2f\n", "Total Social Security Tax:", getValue(payrollSummary.getTotalSocialSecurityTax())));
        report.append(String.format("%-30s %12.2f\n", "Total Medicare Tax:", getValue(payrollSummary.getTotalMedicareTax())));
        report.append(String.format("%-30s %12.2f\n", "Total Taxes:", getValue(payrollSummary.getTotalTaxes())));
        report.append("\n");
        
        report.append("DEDUCTION TOTALS:\n");
        report.append("----------------\n");
        report.append(String.format("%-30s %12.2f\n", "Total Deductions:", getValue(payrollSummary.getTotalDeductions())));
        report.append("\n");
        
        report.append("FINAL TOTALS:\n");
        report.append("-------------\n");
        report.append(String.format("%-30s %12.2f\n", "Total Net Pay:", getValue(payrollSummary.getTotalNetPay())));
        
        return report.toString();
    }
    
    /**
     * Get the year for a given date.
     */
    private int getPayYear(LocalDate date) {
        return date != null ? date.getYear() : Year.now().getValue();
    }
    
    /**
     * Get the BigDecimal value or return 0 if null.
     */
    private double getValue(BigDecimal value) {
        return value != null ? value.doubleValue() : 0.0;
    }
}
