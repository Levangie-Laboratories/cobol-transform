package com.payroll.migration;

import com.payroll.domain.AdditionalDeduction;
import com.payroll.domain.Employee;
import com.payroll.domain.enums.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Reader for COBOL EMPFILE.dat file.
 * Parses employee records according to the EMPFILE.cpy copybook structure.
 */
@Component
public class EmployeeFileReader extends CobolFileReader<Employee> {

    private static final Logger logger = LoggerFactory.getLogger(EmployeeFileReader.class);
    
    /**
     * Parse a line from the employee file into an Employee object.
     * Field positions are based on the EMPFILE.cpy copybook.
     */
    @Override
    protected Employee parseLine(String line) {
        if (line == null || line.trim().isEmpty()) {
            return null;
        }
        
        try {
            Employee employee = new Employee();
            
            // Employee ID (positions 1-6)
            employee.setEmployeeId(extractString(line, 0, 6));
            
            // Personal information
            employee.setLastName(extractString(line, 6, 20));
            employee.setFirstName(extractString(line, 26, 15));
            employee.setMiddleInitial(extractString(line, 41, 1));
            
            // Gender (position 43)
            String genderCode = extractString(line, 42, 1);
            if (!genderCode.isEmpty()) {
                employee.setGender(Gender.fromCode(genderCode));
            }
            
            // Birth date (positions 44-51)
            employee.setBirthDate(extractDate(line, 43, 8));
            
            // SSN (positions 52-60)
            employee.setSocialSecurityNumber(extractString(line, 51, 9));
            
            // Marital status (position 61)
            String maritalCode = extractString(line, 60, 1);
            if (!maritalCode.isEmpty()) {
                employee.setMaritalStatus(MaritalStatus.fromCode(maritalCode));
            }
            
            // Address information (positions 62-171)
            employee.setAddressLine1(extractString(line, 61, 30));
            employee.setAddressLine2(extractString(line, 91, 30));
            employee.setCity(extractString(line, 121, 20));
            employee.setState(extractString(line, 141, 2));
            employee.setZipCode(extractString(line, 143, 10));
            employee.setPhone(extractString(line, 153, 15));
            employee.setEmail(extractString(line, 168, 50));
            
            // Employment information
            employee.setHireDate(extractDate(line, 218, 8));
            employee.setDepartment(extractString(line, 226, 4));
            employee.setPosition(extractString(line, 230, 20));
            
            // Employment status (position 251)
            String statusCode = extractString(line, 250, 1);
            if (!statusCode.isEmpty()) {
                employee.setStatus(EmploymentStatus.fromCode(statusCode));
            }
            
            employee.setTerminationDate(extractDate(line, 251, 8));
            
            // Pay information
            String payTypeCode = extractString(line, 259, 1);
            if (!payTypeCode.isEmpty()) {
                employee.setPayType(PayType.fromCode(payTypeCode));
            }
            
            String payFreqCode = extractString(line, 260, 1);
            if (!payFreqCode.isEmpty()) {
                employee.setPayFrequency(PayFrequency.fromCode(payFreqCode));
            }
            
            employee.setHourlyRate(extractDecimal(line, 261, 6, 2));
            employee.setSalaryAmount(extractDecimal(line, 267, 9, 2));
            employee.setStandardHours(extractDecimal(line, 276, 5, 2));
            employee.setOvertimeRate(extractDecimal(line, 281, 3, 2));
            employee.setLastPayDate(extractDate(line, 284, 8));
            employee.setDirectDepositEnabled(extractBoolean(line, 292));
            employee.setBankAccountInfo(extractString(line, 293, 30));
            
            // Tax information
            String federalFilingCode = extractString(line, 323, 1);
            if (!federalFilingCode.isEmpty()) {
                employee.setFederalFilingStatus(FilingStatus.fromCode(federalFilingCode));
            }
            
            String stateFilingCode = extractString(line, 324, 1);
            if (!stateFilingCode.isEmpty()) {
                employee.setStateFilingStatus(FilingStatus.fromCode(stateFilingCode));
            }
            
            employee.setFederalAllowances(extractInteger(line, 325, 2));
            employee.setStateAllowances(extractInteger(line, 327, 2));
            employee.setAdditionalFederalTax(extractDecimal(line, 329, 7, 2));
            employee.setAdditionalStateTax(extractDecimal(line, 336, 7, 2));
            employee.setTaxBlocked(extractBoolean(line, 343));
            
            // Deduction information
            employee.setHealthPlanCode(extractString(line, 344, 3));
            employee.setHealthDeduction(extractDecimal(line, 347, 7, 2));
            employee.setDentalPlanCode(extractString(line, 354, 3));
            employee.setDentalDeduction(extractDecimal(line, 357, 7, 2));
            employee.setVisionPlanCode(extractString(line, 364, 3));
            employee.setVisionDeduction(extractDecimal(line, 367, 7, 2));
            employee.setRetirement401kEnabled(extractBoolean(line, 374));
            employee.setRetirement401kPercent(extractDecimal(line, 375, 5, 2));
            employee.setLoanDeduction(extractDecimal(line, 380, 7, 2));
            employee.setGarnishDeduction(extractDecimal(line, 387, 7, 2));
            employee.setCharityDeduction(extractDecimal(line, 394, 7, 2));
            employee.setUnionDues(extractDecimal(line, 401, 7, 2));
            
            // Additional deductions (may be in a separate section or file)
            List<AdditionalDeduction> additionalDeductions = parseAdditionalDeductions(line, 408);
            if (!additionalDeductions.isEmpty()) {
                employee.setAdditionalDeductions(additionalDeductions);
            }
            
            // YTD amounts
            employee.setYtdGross(extractDecimal(line, 408, 10, 2));
            employee.setYtdFederalTax(extractDecimal(line, 418, 9, 2));
            employee.setYtdStateTax(extractDecimal(line, 427, 9, 2));
            employee.setYtdLocalTax(extractDecimal(line, 436, 9, 2));
            employee.setYtdSocialSecurity(extractDecimal(line, 445, 9, 2));
            employee.setYtdMedicare(extractDecimal(line, 454, 9, 2));
            employee.setYtd401k(extractDecimal(line, 463, 9, 2));
            employee.setYtdHealthDeduction(extractDecimal(line, 472, 9, 2));
            employee.setYtdDentalDeduction(extractDecimal(line, 481, 9, 2));
            employee.setYtdVisionDeduction(extractDecimal(line, 490, 9, 2));
            employee.setYtdOtherDeduction(extractDecimal(line, 499, 9, 2));
            employee.setYtdNetPay(extractDecimal(line, 508, 10, 2));
            
            return employee;
            
        } catch (Exception e) {
            logger.error("Error parsing employee record: {}", e.getMessage(), e);
            return null;
        }
    }
    
    /**
     * Parse additional deductions from the employee record.
     * In a real implementation, this might read from a separate section or file.
     */
    private List<AdditionalDeduction> parseAdditionalDeductions(String line, int startPos) {
        // This is a simplified implementation
        // In the actual COBOL file, additional deductions might be in a separate section
        // or even a separate file with a link to the employee ID
        List<AdditionalDeduction> deductions = new ArrayList<>();
        
        // For demonstration purposes, we'll assume no additional deductions in this example
        // A real implementation would loop through deduction records if they exist
        
        return deductions;
    }
}
