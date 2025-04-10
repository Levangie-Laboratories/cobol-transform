package com.payroll.service.impl;

import com.payroll.domain.DeductionType;
import com.payroll.domain.Employee;
import com.payroll.domain.AdditionalDeduction;
import com.payroll.domain.GraduatedRange;
import com.payroll.domain.enums.*;
import com.payroll.repository.DeductionTypeRepository;
import com.payroll.service.DeductionCalculationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Implementation of the DeductionCalculationService.
 * This service calculates various deduction amounts based on employee information and pay amounts.
 * It mirrors the logic from the original DEDCALC.cbl COBOL program.
 */
@Service
public class DeductionCalculationServiceImpl implements DeductionCalculationService {

    private static final Logger logger = LoggerFactory.getLogger(DeductionCalculationServiceImpl.class);
    
    private final DeductionTypeRepository deductionTypeRepository;
    
    /**
     * Constructor with dependency injection.
     */
    @Autowired
    public DeductionCalculationServiceImpl(DeductionTypeRepository deductionTypeRepository) {
        this.deductionTypeRepository = deductionTypeRepository;
    }
    
    /**
     * Calculate all deductions for an employee based on the provided gross pay and other parameters.
     */
    @Override
    public DeductionResult calculateDeductions(Employee employee, BigDecimal grossPay, 
                                     BigDecimal hours, List<DeductionType> deductionTypes,
                                     BigDecimal manualAdjustment) {
        logger.debug("Calculating deductions for employee {} with gross pay {}", employee.getEmployeeId(), grossPay);
        
        // Skip deduction calculation if gross pay is zero
        if (grossPay == null || grossPay.compareTo(BigDecimal.ZERO) <= 0) {
            logger.info("Deduction calculation skipped for employee {} due to zero gross pay", employee.getEmployeeId());
            return createZeroDeductionResult();
        }
        
        DeductionResult result = new DeductionResult();
        Map<String, BigDecimal> additionalDeductions = new HashMap<>();
        
        // If no deduction types are provided, retrieve active ones from repository
        if (deductionTypes == null || deductionTypes.isEmpty()) {
            deductionTypes = deductionTypeRepository.findAllActive();
        }
        
        // Calculate standard deductions
        BigDecimal healthInsurance = calculateHealthInsurance(employee, deductionTypes);
        BigDecimal dentalInsurance = calculateDentalInsurance(employee, deductionTypes);
        BigDecimal visionInsurance = calculateVisionInsurance(employee, deductionTypes);
        BigDecimal retirement = calculateRetirement(employee, grossPay, deductionTypes);
        BigDecimal loanRepayment = calculateLoanRepayment(employee, deductionTypes);
        BigDecimal garnishment = calculateGarnishment(employee, grossPay, deductionTypes, null);
        BigDecimal charity = calculateCharity(employee, grossPay, deductionTypes);
        BigDecimal unionDues = calculateUnionDues(employee, grossPay, deductionTypes);
        
        // Set calculated deductions to result
        result.setHealthInsurance(healthInsurance);
        result.setDentalInsurance(dentalInsurance);
        result.setVisionInsurance(visionInsurance);
        result.setRetirement401k(retirement);
        result.setLoanRepayment(loanRepayment);
        result.setGarnishment(garnishment);
        result.setCharityContribution(charity);
        result.setUnionDues(unionDues);
        
        // Process additional deductions from employee record
        if (employee.getAdditionalDeductions() != null && !employee.getAdditionalDeductions().isEmpty()) {
            for (AdditionalDeduction addDeduction : employee.getAdditionalDeductions()) {
                if (addDeduction.isActive()) {
                    additionalDeductions.put(addDeduction.getDeductionCode(), addDeduction.getDeductionAmount());
                }
            }
        }
        
        result.setAdditionalDeductions(additionalDeductions);
        
        // Apply any manual adjustments
        if (manualAdjustment != null && manualAdjustment.compareTo(BigDecimal.ZERO) != 0) {
            // For simplicity, we'll apply adjustment to the highest deduction
            BigDecimal highestDeduction = BigDecimal.ZERO;
            String highestType = null;
            
            if (healthInsurance.compareTo(highestDeduction) > 0) {
                highestDeduction = healthInsurance;
                highestType = "health";
            }
            if (retirement.compareTo(highestDeduction) > 0) {
                highestDeduction = retirement;
                highestType = "retirement";
            }
            
            // Apply adjustment
            if (highestType != null) {
                if ("health".equals(highestType)) {
                    healthInsurance = healthInsurance.add(manualAdjustment);
                    result.setHealthInsurance(healthInsurance);
                } else if ("retirement".equals(highestType)) {
                    retirement = retirement.add(manualAdjustment);
                    result.setRetirement401k(retirement);
                }
            }
        }
        
        // Separate pre-tax and post-tax deductions
        BigDecimal totalPreTaxDeductions = BigDecimal.ZERO;
        BigDecimal totalPostTaxDeductions = BigDecimal.ZERO;
        
        // Group deductions by tax status
        for (DeductionType deductionType : deductionTypes) {
            String deductionCode = deductionType.getDeductionCode();
            BigDecimal amount = BigDecimal.ZERO;
            
            // Determine amount based on deduction code
            if ("HI".equals(deductionCode) && healthInsurance != null) {
                amount = healthInsurance;
            } else if ("DI".equals(deductionCode) && dentalInsurance != null) {
                amount = dentalInsurance;
            } else if ("VI".equals(deductionCode) && visionInsurance != null) {
                amount = visionInsurance;
            } else if ("401K".equals(deductionCode) && retirement != null) {
                amount = retirement;
            } else if ("LOAN".equals(deductionCode) && loanRepayment != null) {
                amount = loanRepayment;
            } else if ("GARN".equals(deductionCode) && garnishment != null) {
                amount = garnishment;
            } else if ("CHAR".equals(deductionCode) && charity != null) {
                amount = charity;
            } else if ("UNION".equals(deductionCode) && unionDues != null) {
                amount = unionDues;
            } else if (additionalDeductions.containsKey(deductionCode)) {
                amount = additionalDeductions.get(deductionCode);
            }
            
            // Skip if zero
            if (amount.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            
            // Add to appropriate total based on tax status
            if (deductionType.getTaxStatus() == TaxStatus.PRE_TAX) {
                totalPreTaxDeductions = totalPreTaxDeductions.add(amount);
            } else {
                totalPostTaxDeductions = totalPostTaxDeductions.add(amount);
            }
        }
        
        result.setTotalPreTaxDeductions(totalPreTaxDeductions);
        result.setTotalPostTaxDeductions(totalPostTaxDeductions);
        
        // Calculate total deductions
        result.calculateTotal();
        
        logger.debug("Deduction calculation completed for employee {}: {}", 
                   employee.getEmployeeId(), result.getTotalDeductions());
        return result;
    }
    
    /**
     * Create a zero deduction result object for cases where no deductions should be calculated.
     */
    private DeductionResult createZeroDeductionResult() {
        DeductionResult result = new DeductionResult();
        result.setHealthInsurance(BigDecimal.ZERO);
        result.setDentalInsurance(BigDecimal.ZERO);
        result.setVisionInsurance(BigDecimal.ZERO);
        result.setRetirement401k(BigDecimal.ZERO);
        result.setLoanRepayment(BigDecimal.ZERO);
        result.setGarnishment(BigDecimal.ZERO);
        result.setCharityContribution(BigDecimal.ZERO);
        result.setUnionDues(BigDecimal.ZERO);
        result.setAdditionalDeductions(new HashMap<>());
        result.setTotalPreTaxDeductions(BigDecimal.ZERO);
        result.setTotalPostTaxDeductions(BigDecimal.ZERO);
        result.setTotalDeductions(BigDecimal.ZERO);
        return result;
    }

    /**
     * Calculate health insurance deduction for an employee.
     */
    @Override
    public BigDecimal calculateHealthInsurance(Employee employee, List<DeductionType> deductionTypes) {
        logger.debug("Calculating health insurance deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has a health plan code
        String healthPlanCode = employee.getHealthPlanCode();
        if (healthPlanCode == null || healthPlanCode.isEmpty()) {
            return BigDecimal.ZERO;
        }
        
        // Check if employee already has a fixed health deduction amount
        if (employee.getHealthDeduction() != null && employee.getHealthDeduction().compareTo(BigDecimal.ZERO) > 0) {
            return employee.getHealthDeduction();
        }
        
        // Find health insurance deduction type
        DeductionType healthDeduction = findDeductionByCodeAndCategory(deductionTypes, 
                                                               healthPlanCode, 
                                                               DeductionCategory.HEALTH_INSURANCE);
        
        if (healthDeduction == null) {
            logger.warn("No health insurance deduction type found for plan code: {}", healthPlanCode);
            return BigDecimal.ZERO;
        }
        
        // Use flat amount for health insurance
        if (healthDeduction.getCalculationMethod() == CalculationMethod.FLAT_AMOUNT && 
            healthDeduction.getFlatAmount() != null) {
            return healthDeduction.getFlatAmount();
        }
        
        // Default return zero if no calculation method matched
        return BigDecimal.ZERO;
    }

    /**
     * Calculate dental insurance deduction for an employee.
     */
    @Override
    public BigDecimal calculateDentalInsurance(Employee employee, List<DeductionType> deductionTypes) {
        logger.debug("Calculating dental insurance deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has a dental plan code
        String dentalPlanCode = employee.getDentalPlanCode();
        if (dentalPlanCode == null || dentalPlanCode.isEmpty()) {
            return BigDecimal.ZERO;
        }
        
        // Check if employee already has a fixed dental deduction amount
        if (employee.getDentalDeduction() != null && employee.getDentalDeduction().compareTo(BigDecimal.ZERO) > 0) {
            return employee.getDentalDeduction();
        }
        
        // Find dental insurance deduction type
        DeductionType dentalDeduction = findDeductionByCodeAndCategory(deductionTypes, 
                                                                dentalPlanCode, 
                                                                DeductionCategory.DENTAL_INSURANCE);
        
        if (dentalDeduction == null) {
            logger.warn("No dental insurance deduction type found for plan code: {}", dentalPlanCode);
            return BigDecimal.ZERO;
        }
        
        // Use flat amount for dental insurance
        if (dentalDeduction.getCalculationMethod() == CalculationMethod.FLAT_AMOUNT && 
            dentalDeduction.getFlatAmount() != null) {
            return dentalDeduction.getFlatAmount();
        }
        
        // Default return zero if no calculation method matched
        return BigDecimal.ZERO;
    }

    /**
     * Calculate vision insurance deduction for an employee.
     */
    @Override
    public BigDecimal calculateVisionInsurance(Employee employee, List<DeductionType> deductionTypes) {
        logger.debug("Calculating vision insurance deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has a vision plan code
        String visionPlanCode = employee.getVisionPlanCode();
        if (visionPlanCode == null || visionPlanCode.isEmpty()) {
            return BigDecimal.ZERO;
        }
        
        // Check if employee already has a fixed vision deduction amount
        if (employee.getVisionDeduction() != null && employee.getVisionDeduction().compareTo(BigDecimal.ZERO) > 0) {
            return employee.getVisionDeduction();
        }
        
        // Find vision insurance deduction type
        DeductionType visionDeduction = findDeductionByCodeAndCategory(deductionTypes, 
                                                                visionPlanCode, 
                                                                DeductionCategory.VISION_INSURANCE);
        
        if (visionDeduction == null) {
            logger.warn("No vision insurance deduction type found for plan code: {}", visionPlanCode);
            return BigDecimal.ZERO;
        }
        
        // Use flat amount for vision insurance
        if (visionDeduction.getCalculationMethod() == CalculationMethod.FLAT_AMOUNT && 
            visionDeduction.getFlatAmount() != null) {
            return visionDeduction.getFlatAmount();
        }
        
        // Default return zero if no calculation method matched
        return BigDecimal.ZERO;
    }

    /**
     * Calculate retirement plan (401k) deduction for an employee.
     */
    @Override
    public BigDecimal calculateRetirement(Employee employee, BigDecimal grossPay, List<DeductionType> deductionTypes) {
        logger.debug("Calculating retirement deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has 401k enabled
        if (!employee.isRetirement401kEnabled()) {
            return BigDecimal.ZERO;
        }
        
        // Check if employee has a retirement percentage
        BigDecimal retirementPercent = employee.getRetirement401kPercent();
        if (retirementPercent == null || retirementPercent.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Find retirement deduction type
        DeductionType retirementDeduction = findDeductionByCategory(deductionTypes, DeductionCategory.RETIREMENT);
        
        if (retirementDeduction == null) {
            logger.warn("No retirement deduction type found");
            // Use basic calculation if no deduction type found
            return calculateBasicRetirement(grossPay, retirementPercent);
        }
        
        // Calculate retirement deduction amount
        BigDecimal retirementAmount = grossPay.multiply(retirementPercent.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP));
        
        // Apply maximum per pay limit if defined
        if (retirementDeduction.getMaxAmountPerPay() != null && 
            retirementAmount.compareTo(retirementDeduction.getMaxAmountPerPay()) > 0) {
            retirementAmount = retirementDeduction.getMaxAmountPerPay();
        }
        
        // Apply annual maximum if defined (would need YTD data to implement properly)
        
        return retirementAmount.setScale(2, RoundingMode.HALF_UP);
    }
    
    /**
     * Calculate basic retirement amount without deduction type constraints.
     */
    private BigDecimal calculateBasicRetirement(BigDecimal grossPay, BigDecimal retirementPercent) {
        return grossPay.multiply(retirementPercent.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP))
            .setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate loan repayment deduction for an employee.
     */
    @Override
    public BigDecimal calculateLoanRepayment(Employee employee, List<DeductionType> deductionTypes) {
        logger.debug("Calculating loan repayment deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has a loan deduction amount
        if (employee.getLoanDeduction() == null || employee.getLoanDeduction().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Return the fixed loan deduction amount
        return employee.getLoanDeduction();
    }

    /**
     * Calculate garnishment deduction for an employee.
     */
    @Override
    public BigDecimal calculateGarnishment(Employee employee, BigDecimal grossPay,
                                         List<DeductionType> deductionTypes,
                                         BigDecimal garnishmentOverride) {
        logger.debug("Calculating garnishment deduction for employee {}", employee.getEmployeeId());
        
        // If override is provided, use it
        if (garnishmentOverride != null && garnishmentOverride.compareTo(BigDecimal.ZERO) > 0) {
            return garnishmentOverride;
        }
        
        // Check if employee has a garnishment deduction amount
        if (employee.getGarnishDeduction() == null || employee.getGarnishDeduction().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Find garnishment deduction type for limits
        DeductionType garnishmentDeduction = findDeductionByCategory(deductionTypes, DeductionCategory.GARNISHMENT);
        
        BigDecimal garnishmentAmount = employee.getGarnishDeduction();
        
        // Apply maximum percentage of gross pay if defined
        if (garnishmentDeduction != null && garnishmentDeduction.getMaxPercentage() != null) {
            BigDecimal maxGarnishment = grossPay.multiply(
                garnishmentDeduction.getMaxPercentage().divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP));
            
            if (garnishmentAmount.compareTo(maxGarnishment) > 0) {
                garnishmentAmount = maxGarnishment;
            }
        }
        
        return garnishmentAmount.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate charity contribution deduction for an employee.
     */
    @Override
    public BigDecimal calculateCharity(Employee employee, BigDecimal grossPay,
                                     List<DeductionType> deductionTypes) {
        logger.debug("Calculating charity deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has a charity deduction amount
        if (employee.getCharityDeduction() == null || employee.getCharityDeduction().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Return the fixed charity deduction amount
        return employee.getCharityDeduction();
    }

    /**
     * Calculate union dues deduction for an employee.
     */
    @Override
    public BigDecimal calculateUnionDues(Employee employee, BigDecimal grossPay,
                                       List<DeductionType> deductionTypes) {
        logger.debug("Calculating union dues deduction for employee {}", employee.getEmployeeId());
        
        // Check if employee has union dues amount
        if (employee.getUnionDues() == null || employee.getUnionDues().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        // Find union dues deduction type
        DeductionType unionDuesDeduction = findDeductionByCategory(deductionTypes, DeductionCategory.UNION_DUES);
        
        BigDecimal unionDuesAmount = employee.getUnionDues();
        
        // If union dues are percentage-based, calculate from gross pay
        if (unionDuesDeduction != null && unionDuesDeduction.getCalculationMethod() == CalculationMethod.PERCENTAGE) {
            BigDecimal percentage = unionDuesDeduction.getPercentageRate();
            if (percentage != null && percentage.compareTo(BigDecimal.ZERO) > 0) {
                unionDuesAmount = grossPay.multiply(
                    percentage.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP));
            }
        }
        
        return unionDuesAmount.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate employer-matching contributions for applicable deductions.
     */
    @Override
    public Map<String, BigDecimal> calculateEmployerContributions(Employee employee, 
                                                              BigDecimal grossPay,
                                                              DeductionResult employeeDeductions,
                                                              List<DeductionType> deductionTypes) {
        logger.debug("Calculating employer contributions for employee {}", employee.getEmployeeId());
        
        Map<String, BigDecimal> employerContributions = new HashMap<>();
        
        // Find deduction types with employer matching
        List<DeductionType> matchingDeductions = deductionTypes.stream()
            .filter(dt -> dt.isEmployerMatch())
            .collect(Collectors.toList());
        
        // Process each matching deduction type
        for (DeductionType deduction : matchingDeductions) {
            String code = deduction.getDeductionCode();
            BigDecimal employeeAmount = BigDecimal.ZERO;
            
            // Determine employee amount based on deduction code
            if ("401K".equals(code) && employeeDeductions.getRetirement401k() != null) {
                employeeAmount = employeeDeductions.getRetirement401k();
            } else {
                // For other deduction types, would need to look up in additional deductions
                continue;
            }
            
            // Skip if employee amount is zero
            if (employeeAmount.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            
            // Calculate employer match
            BigDecimal matchRate = deduction.getEmployerMatchRate();
            BigDecimal matchMax = deduction.getEmployerMatchMax();
            
            if (matchRate == null || matchRate.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            
            // Calculate match amount
            BigDecimal matchAmount = employeeAmount.multiply(
                matchRate.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP));
            
            // Apply maximum if defined
            if (matchMax != null && matchAmount.compareTo(matchMax) > 0) {
                matchAmount = matchMax;
            }
            
            // Add to contributions map
            employerContributions.put(code, matchAmount.setScale(2, RoundingMode.HALF_UP));
        }
        
        return employerContributions;
    }
    
    /**
     * Helper method to find a deduction type by its code and category.
     */
    private DeductionType findDeductionByCodeAndCategory(List<DeductionType> deductionTypes, 
                                                     String code, 
                                                     DeductionCategory category) {
        if (deductionTypes == null || code == null) {
            return null;
        }
        
        return deductionTypes.stream()
            .filter(dt -> dt.getStatus() == DeductionStatus.ACTIVE)
            .filter(dt -> dt.getCategory() == category)
            .filter(dt -> code.equals(dt.getDeductionCode()))
            .findFirst()
            .orElse(null);
    }
    
    /**
     * Helper method to find a deduction type by its category.
     */
    private DeductionType findDeductionByCategory(List<DeductionType> deductionTypes, DeductionCategory category) {
        if (deductionTypes == null || category == null) {
            return null;
        }
        
        return deductionTypes.stream()
            .filter(dt -> dt.getStatus() == DeductionStatus.ACTIVE)
            .filter(dt -> dt.getCategory() == category)
            .findFirst()
            .orElse(null);
    }
}
