package com.payroll.service.impl;

import com.payroll.domain.Employee;
import com.payroll.domain.PayrollData;
import com.payroll.service.DeductionCalculationService.DeductionResult;
import com.payroll.service.PayStubService;
import com.payroll.service.TaxCalculationService.TaxResult;
import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * Implementation of the PayStubService.
 * This service generates pay stubs for employees based on payroll data and calculation results.
 * It mirrors the logic from the original PAYSTUB.cbl COBOL program.
 */
@Service
public class PayStubServiceImpl implements PayStubService {

    private static final Logger logger = LoggerFactory.getLogger(PayStubServiceImpl.class);
    
    @PersistenceContext
    private EntityManager entityManager;
    
    // Company information for pay stubs
    private static final String COMPANY_NAME = "ABC Corporation";
    private static final String COMPANY_ADDRESS = "123 Main Street, Anytown, USA 12345";
    
    /**
     * Constructor with dependency injection.
     */
    @Autowired
    public PayStubServiceImpl() {
        // Constructor injection could be added here if needed
    }
    
    /**
     * Generate a pay stub for an employee.
     */
    @Override
    public PayStub generatePayStub(Employee employee, PayrollData payrollData,
                                  BigDecimal grossPay, BigDecimal regularPay,
                                  BigDecimal overtimePay, BigDecimal otherPay,
                                  TaxResult taxResult, DeductionResult deductionResult,
                                  BigDecimal netPay) {
        logger.debug("Generating pay stub for employee {}", employee.getEmployeeId());
        
        // Create a new pay stub object
        PayStub payStub = new PayStub();
        
        // Generate a unique pay stub ID
        String payStubId = generatePayStubId(employee.getEmployeeId(), payrollData.getPayPeriodId());
        payStub.setPayStubId(payStubId);
        
        // Set pay period information
        payStub.setPayDate(payrollData.getCheckDate());
        payStub.setPayPeriodId(payrollData.getPayPeriodId());
        payStub.setPayPeriodStartDate(payrollData.getPayPeriodStartDate());
        payStub.setPayPeriodEndDate(payrollData.getPayPeriodEndDate());
        
        // Set employee information
        payStub.setEmployeeId(employee.getEmployeeId());
        payStub.setEmployeeName(formatEmployeeName(employee));
        payStub.setDepartment(employee.getDepartment());
        payStub.setPosition(employee.getPosition());
        
        // Set current pay information
        payStub.setRegularHours(payrollData.getRegularHours());
        payStub.setOvertimeHours(payrollData.getOvertimeHours());
        
        // Set pay rates based on employee type
        if (employee.getPayType() == com.payroll.domain.enums.PayType.HOURLY) {
            payStub.setRegularRate(employee.getHourlyRate());
            payStub.setOvertimeRate(employee.getOvertimeRate());
        } else {
            // For salaried employees, calculate an effective hourly rate if hours > 0
            if (payrollData.getRegularHours() != null && payrollData.getRegularHours().compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal effectiveRate = regularPay.divide(payrollData.getRegularHours(), 2, java.math.RoundingMode.HALF_UP);
                payStub.setRegularRate(effectiveRate);
            }
        }
        
        // Set pay amounts
        payStub.setRegularPay(regularPay);
        payStub.setOvertimePay(overtimePay);
        payStub.setOtherPay(otherPay);
        payStub.setGrossPay(grossPay);
        
        // Set tax information
        if (taxResult != null) {
            payStub.setFederalTax(taxResult.getFederalTax());
            payStub.setStateTax(taxResult.getStateTax());
            payStub.setLocalTax(taxResult.getLocalTax());
            payStub.setSocialSecurityTax(taxResult.getSocialSecurityTax());
            payStub.setMedicareTax(taxResult.getMedicareTax());
            payStub.setTotalTaxes(taxResult.getTotalTax());
        }
        
        // Set deduction information
        if (deductionResult != null) {
            payStub.setHealthInsurance(deductionResult.getHealthInsurance());
            payStub.setDentalInsurance(deductionResult.getDentalInsurance());
            payStub.setVisionInsurance(deductionResult.getVisionInsurance());
            payStub.setRetirement401k(deductionResult.getRetirement401k());
            
            // Set additional deductions
            payStub.setDetailedDeductions(deductionResult.getAdditionalDeductions());
            
            // Calculate other deductions total
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
            if (deductionResult.getAdditionalDeductions() != null) {
                for (BigDecimal amount : deductionResult.getAdditionalDeductions().values()) {
                    otherDeductions = otherDeductions.add(amount);
                }
            }
            payStub.setOtherDeductions(otherDeductions);
            
            payStub.setTotalDeductions(deductionResult.getTotalDeductions());
        }
        
        // Set net pay
        payStub.setNetPay(netPay);
        
        // Set YTD information
        payStub.setYtdGross(employee.getYtdGross());
        payStub.setYtdFederalTax(employee.getYtdFederalTax());
        payStub.setYtdStateTax(employee.getYtdStateTax());
        payStub.setYtdSocialSecurityTax(employee.getYtdSocialSecurity());
        payStub.setYtdMedicareTax(employee.getYtdMedicare());
        payStub.setYtdRetirement(employee.getYtd401k());
        
        BigDecimal ytdTotalDeductions = BigDecimal.ZERO;
        if (employee.getYtdHealthDeduction() != null) {
            ytdTotalDeductions = ytdTotalDeductions.add(employee.getYtdHealthDeduction());
        }
        if (employee.getYtdDentalDeduction() != null) {
            ytdTotalDeductions = ytdTotalDeductions.add(employee.getYtdDentalDeduction());
        }
        if (employee.getYtdVisionDeduction() != null) {
            ytdTotalDeductions = ytdTotalDeductions.add(employee.getYtdVisionDeduction());
        }
        if (employee.getYtd401k() != null) {
            ytdTotalDeductions = ytdTotalDeductions.add(employee.getYtd401k());
        }
        if (employee.getYtdOtherDeduction() != null) {
            ytdTotalDeductions = ytdTotalDeductions.add(employee.getYtdOtherDeduction());
        }
        payStub.setYtdTotalDeductions(ytdTotalDeductions);
        
        payStub.setYtdNetPay(employee.getYtdNetPay());
        
        // Set company information
        payStub.setCompanyName(COMPANY_NAME);
        payStub.setCompanyAddress(COMPANY_ADDRESS);
        
        logger.debug("Pay stub generated for employee {}", employee.getEmployeeId());
        return payStub;
    }

    /**
     * Format a pay stub for display or printing.
     */
    @Override
    public String formatPayStub(PayStub payStub, String format) {
        logger.debug("Formatting pay stub {} in {} format", payStub.getPayStubId(), format);
        
        if ("TEXT".equalsIgnoreCase(format)) {
            return formatPayStubAsText(payStub);
        } else if ("HTML".equalsIgnoreCase(format)) {
            return formatPayStubAsHtml(payStub);
        } else {
            logger.warn("Unsupported format requested: {}", format);
            return formatPayStubAsText(payStub); // Default to text
        }
    }

    /**
     * Save a pay stub to the database for historical records.
     */
    @Override
    public PayStub savePayStub(PayStub payStub) {
        logger.debug("Saving pay stub {} to database", payStub.getPayStubId());
        
        // In a real implementation, this would persist the pay stub to a database
        // For this sample, we'll just return the input object
        logger.info("Pay stub {} saved (simulated)", payStub.getPayStubId());
        return payStub;
    }

    /**
     * Retrieve a previously generated pay stub.
     */
    @Override
    public PayStub getPayStub(String payStubId) {
        logger.debug("Retrieving pay stub {}", payStubId);
        
        // In a real implementation, this would query the database
        // For this sample, we'll return null (not found)
        logger.warn("Pay stub {} not found (retrieval not implemented)", payStubId);
        return null;
    }

    /**
     * Retrieve all pay stubs for a specific employee.
     */
    @Override
    public List<PayStub> getPayStubsForEmployee(String employeeId) {
        logger.debug("Retrieving pay stubs for employee {}", employeeId);
        
        // In a real implementation, this would query the database
        // For this sample, we'll return an empty list
        logger.warn("No pay stubs found for employee {} (retrieval not implemented)", employeeId);
        return new ArrayList<>();
    }

    /**
     * Generate a PDF document from a pay stub.
     */
    @Override
    public byte[] generatePdfPayStub(PayStub payStub) {
        logger.debug("Generating PDF pay stub for {}", payStub.getPayStubId());
        
        try {
            // In a real implementation, this would use JasperReports to generate a PDF
            // For this sample, we'll create a simplified implementation
            
            // Create a collection for the report data source
            List<PayStub> payStubList = Collections.singletonList(payStub);
            JRBeanCollectionDataSource dataSource = new JRBeanCollectionDataSource(payStubList);
            
            // Prepare parameters for the report
            Map<String, Object> parameters = new HashMap<>();
            parameters.put("companyName", payStub.getCompanyName());
            parameters.put("companyAddress", payStub.getCompanyAddress());
            parameters.put("reportDate", new Date());
            
            // Load the compiled report template
            // In a real implementation, this would reference an actual .jasper file
            // JasperReport report = (JasperReport) JRLoader.loadObject(getClass().getResourceAsStream("/reports/paystub_template.jasper"));
            
            // Fill the report
            // JasperPrint jasperPrint = JasperFillManager.fillReport(report, parameters, dataSource);
            
            // Export to PDF
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            // JasperExportManager.exportReportToPdfStream(jasperPrint, baos);
            
            // For this example, we'll just create a placeholder PDF content
            baos.write("PDF Pay Stub Content - Placeholder".getBytes());
            
            logger.info("PDF pay stub generated for {}", payStub.getPayStubId());
            return baos.toByteArray();
            
        } catch (Exception e) {
            logger.error("Error generating PDF pay stub: {}", e.getMessage(), e);
            return new byte[0];
        }
    }
    
    /**
     * Generate a unique pay stub ID.
     */
    private String generatePayStubId(String employeeId, int payPeriodId) {
        // Format: EMPID-PERIOD-TIMESTAMP
        String timestamp = String.valueOf(System.currentTimeMillis());
        return String.format("%s-%d-%s", employeeId, payPeriodId, timestamp);
    }
    
    /**
     * Format employee name as "Last, First M."
     */
    private String formatEmployeeName(Employee employee) {
        StringBuilder nameBuilder = new StringBuilder();
        
        if (employee.getLastName() != null) {
            nameBuilder.append(employee.getLastName());
        }
        
        if (employee.getFirstName() != null) {
            if (nameBuilder.length() > 0) {
                nameBuilder.append(", ");
            }
            nameBuilder.append(employee.getFirstName());
        }
        
        if (employee.getMiddleInitial() != null && !employee.getMiddleInitial().isEmpty()) {
            nameBuilder.append(" ").append(employee.getMiddleInitial()).append(".");
        }
        
        return nameBuilder.toString();
    }
    
    /**
     * Format pay stub as plain text.
     */
    private String formatPayStubAsText(PayStub payStub) {
        StringBuilder sb = new StringBuilder();
        
        // Company header
        sb.append(payStub.getCompanyName()).append("\n");
        sb.append(payStub.getCompanyAddress()).append("\n");
        sb.append("\n");
        
        // Pay stub header
        sb.append("PAY STUB").append("\n");
        sb.append("==========").append("\n");
        sb.append("\n");
        
        // Employee information
        sb.append("Employee: ").append(payStub.getEmployeeName())
          .append(" (ID: ").append(payStub.getEmployeeId()).append(")\n");
        sb.append("Department: ").append(payStub.getDepartment())
          .append("   Position: ").append(payStub.getPosition()).append("\n");
        sb.append("\n");
        
        // Pay period information
        sb.append("Pay Period: ").append(formatDate(payStub.getPayPeriodStartDate()))
          .append(" to ").append(formatDate(payStub.getPayPeriodEndDate())).append("\n");
        sb.append("Pay Date: ").append(formatDate(payStub.getPayDate())).append("\n");
        sb.append("\n");
        
        // Current pay information
        sb.append("EARNINGS\n");
        sb.append("---------------------------\n");
        sb.append(String.format("%-20s %8s %8s %10s\n", "Description", "Hours", "Rate", "Amount"));
        sb.append(String.format("%-20s %8.2f %8.2f %10.2f\n", "Regular", 
                             getValue(payStub.getRegularHours()), 
                             getValue(payStub.getRegularRate()), 
                             getValue(payStub.getRegularPay())));
        sb.append(String.format("%-20s %8.2f %8.2f %10.2f\n", "Overtime", 
                             getValue(payStub.getOvertimeHours()), 
                             getValue(payStub.getOvertimeRate()), 
                             getValue(payStub.getOvertimePay())));
        if (payStub.getOtherPay() != null && payStub.getOtherPay().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-20s %8s %8s %10.2f\n", "Other", 
                                 "", "", getValue(payStub.getOtherPay())));
        }
        sb.append(String.format("%-38s %10.2f\n", "Gross Pay:", getValue(payStub.getGrossPay())));
        sb.append("\n");
        
        // Tax information
        sb.append("TAXES\n");
        sb.append("---------------------------\n");
        sb.append(String.format("%-28s %10s %10s\n", "Description", "Current", "YTD"));
        sb.append(String.format("%-28s %10.2f %10.2f\n", "Federal Income Tax", 
                             getValue(payStub.getFederalTax()), 
                             getValue(payStub.getYtdFederalTax())));
        sb.append(String.format("%-28s %10.2f %10.2f\n", "State Income Tax", 
                             getValue(payStub.getStateTax()), 
                             getValue(payStub.getYtdStateTax())));
        sb.append(String.format("%-28s %10.2f %10.2f\n", "Social Security Tax", 
                             getValue(payStub.getSocialSecurityTax()), 
                             getValue(payStub.getYtdSocialSecurityTax())));
        sb.append(String.format("%-28s %10.2f %10.2f\n", "Medicare Tax", 
                             getValue(payStub.getMedicareTax()), 
                             getValue(payStub.getYtdMedicareTax())));
        if (payStub.getLocalTax() != null && payStub.getLocalTax().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-28s %10.2f %10s\n", "Local Tax", 
                                 getValue(payStub.getLocalTax()), 
                                 ""));
        }
        sb.append(String.format("%-28s %10.2f %10s\n", "Total Taxes:", 
                             getValue(payStub.getTotalTaxes()), 
                             ""));
        sb.append("\n");
        
        // Deduction information
        sb.append("DEDUCTIONS\n");
        sb.append("---------------------------\n");
        sb.append(String.format("%-28s %10s %10s\n", "Description", "Current", "YTD"));
        if (payStub.getHealthInsurance() != null && payStub.getHealthInsurance().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-28s %10.2f %10s\n", "Health Insurance", 
                                 getValue(payStub.getHealthInsurance()), 
                                 ""));
        }
        if (payStub.getDentalInsurance() != null && payStub.getDentalInsurance().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-28s %10.2f %10s\n", "Dental Insurance", 
                                 getValue(payStub.getDentalInsurance()), 
                                 ""));
        }
        if (payStub.getVisionInsurance() != null && payStub.getVisionInsurance().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-28s %10.2f %10s\n", "Vision Insurance", 
                                 getValue(payStub.getVisionInsurance()), 
                                 ""));
        }
        if (payStub.getRetirement401k() != null && payStub.getRetirement401k().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-28s %10.2f %10.2f\n", "401(k) Retirement", 
                                 getValue(payStub.getRetirement401k()), 
                                 getValue(payStub.getYtdRetirement())));
        }
        if (payStub.getOtherDeductions() != null && payStub.getOtherDeductions().compareTo(BigDecimal.ZERO) > 0) {
            sb.append(String.format("%-28s %10.2f %10s\n", "Other Deductions", 
                                 getValue(payStub.getOtherDeductions()), 
                                 ""));
        }
        sb.append(String.format("%-28s %10.2f %10.2f\n", "Total Deductions:", 
                             getValue(payStub.getTotalDeductions()), 
                             getValue(payStub.getYtdTotalDeductions())));
        sb.append("\n");
        
        // Net pay
        sb.append(String.format("%-28s %10.2f %10.2f\n", "NET PAY:", 
                             getValue(payStub.getNetPay()), 
                             getValue(payStub.getYtdNetPay())));
        
        return sb.toString();
    }
    
    /**
     * Format pay stub as HTML.
     */
    private String formatPayStubAsHtml(PayStub payStub) {
        StringBuilder sb = new StringBuilder();
        
        sb.append("<!DOCTYPE html>\n");
        sb.append("<html>\n");
        sb.append("<head>\n");
        sb.append("  <title>Pay Stub</title>\n");
        sb.append("  <style>\n");
        sb.append("    body { font-family: Arial, sans-serif; margin: 20px; }\n");
        sb.append("    .header { text-align: center; margin-bottom: 20px; }\n");
        sb.append("    .employee-info { margin-bottom: 20px; }\n");
        sb.append("    table { width: 100%; border-collapse: collapse; margin-bottom: 20px; }\n");
        sb.append("    th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }\n");
        sb.append("    th { background-color: #f2f2f2; }\n");
        sb.append("    .amount { text-align: right; }\n");
        sb.append("    .total { font-weight: bold; }\n");
        sb.append("    .section-header { background-color: #e0e0e0; font-weight: bold; }\n");
        sb.append("  </style>\n");
        sb.append("</head>\n");
        sb.append("<body>\n");
        
        // Company header
        sb.append("  <div class=\"header\">\n");
        sb.append("    <h2>").append(payStub.getCompanyName()).append("</h2>\n");
        sb.append("    <p>").append(payStub.getCompanyAddress()).append("</p>\n");
        sb.append("    <h1>PAY STUB</h1>\n");
        sb.append("  </div>\n");
        
        // Employee information
        sb.append("  <div class=\"employee-info\">\n");
        sb.append("    <p><strong>Employee:</strong> ").append(payStub.getEmployeeName())
          .append(" (ID: ").append(payStub.getEmployeeId()).append(")</p>\n");
        sb.append("    <p><strong>Department:</strong> ").append(payStub.getDepartment())
          .append("   <strong>Position:</strong> ").append(payStub.getPosition()).append("</p>\n");
        sb.append("    <p><strong>Pay Period:</strong> ").append(formatDate(payStub.getPayPeriodStartDate()))
          .append(" to ").append(formatDate(payStub.getPayPeriodEndDate()))
          .append("   <strong>Pay Date:</strong> ").append(formatDate(payStub.getPayDate())).append("</p>\n");
        sb.append("  </div>\n");
        
        // Current pay information
        sb.append("  <h3>EARNINGS</h3>\n");
        sb.append("  <table>\n");
        sb.append("    <tr>\n");
        sb.append("      <th>Description</th>\n");
        sb.append("      <th>Hours</th>\n");
        sb.append("      <th>Rate</th>\n");
        sb.append("      <th class=\"amount\">Amount</th>\n");
        sb.append("    </tr>\n");
        sb.append("    <tr>\n");
        sb.append("      <td>Regular</td>\n");
        sb.append("      <td>").append(formatValue(payStub.getRegularHours())).append("</td>\n");
        sb.append("      <td>").append(formatValue(payStub.getRegularRate())).append("</td>\n");
        sb.append("      <td class=\"amount\">").append(formatValue(payStub.getRegularPay())).append("</td>\n");
        sb.append("    </tr>\n");
        sb.append("    <tr>\n");
        sb.append("      <td>Overtime</td>\n");
        sb.append("      <td>").append(formatValue(payStub.getOvertimeHours())).append("</td>\n");
        sb.append("      <td>").append(formatValue(payStub.getOvertimeRate())).append("</td>\n");
        sb.append("      <td class=\"amount\">").append(formatValue(payStub.getOvertimePay())).append("</td>\n");
        sb.append("    </tr>\n");
        if (payStub.getOtherPay() != null && payStub.getOtherPay().compareTo(BigDecimal.ZERO) > 0) {
            sb.append("    <tr>\n");
            sb.append("      <td>Other</td>\n");
            sb.append("      <td></td>\n");
            sb.append("      <td></td>\n");
            sb.append("      <td class=\"amount\">").append(formatValue(payStub.getOtherPay())).append("</td>\n");
            sb.append("    </tr>\n");
        }
        sb.append("    <tr class=\"total\">\n");
        sb.append("      <td colspan=\"3\">Gross Pay:</td>\n");
        sb.append("      <td class=\"amount\">").append(formatValue(payStub.getGrossPay())).append("</td>\n");
        sb.append("    </tr>\n");
        sb.append("  </table>\n");
        
        // HTML for other sections (taxes, deductions, net pay) would follow the same pattern
        // Abbreviated for brevity
        
        sb.append("</body>\n");
        sb.append("</html>\n");
        
        return sb.toString();
    }
    
    /**
     * Format date as MM/DD/YYYY.
     */
    private String formatDate(LocalDate date) {
        if (date == null) {
            return "";
        }
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy");
        return date.format(formatter);
    }
    
    /**
     * Get the BigDecimal value or return 0 if null.
     */
    private double getValue(BigDecimal value) {
        return value != null ? value.doubleValue() : 0.0;
    }
    
    /**
     * Format BigDecimal value as string with 2 decimal places.
     */
    private String formatValue(BigDecimal value) {
        if (value == null) {
            return "0.00";
        }
        return String.format("%.2f", value);
    }
}
