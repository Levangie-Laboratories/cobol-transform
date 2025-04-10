package com.payroll.migration;

import com.payroll.domain.DeductionType;
import com.payroll.domain.Employee;
import com.payroll.domain.PayrollData;
import com.payroll.domain.TaxRate;
import com.payroll.repository.DeductionTypeRepository;
import com.payroll.repository.EmployeeRepository;
import com.payroll.repository.PayrollDataRepository;
import com.payroll.repository.TaxRateRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * Service for migrating data from COBOL data files to the Java/database system.
 * This service coordinates the migration process for all data types.
 */
@Service
public class DataMigrationService {

    private static final Logger logger = LoggerFactory.getLogger(DataMigrationService.class);
    
    private final EmployeeRepository employeeRepository;
    private final TaxRateRepository taxRateRepository;
    private final DeductionTypeRepository deductionTypeRepository;
    private final PayrollDataRepository payrollDataRepository;
    
    private final EmployeeFileReader employeeFileReader;
    private final ResourceLoader resourceLoader;
    
    // Migration result tracking
    private int totalRecordsProcessed = 0;
    private int successfulRecords = 0;
    private int failedRecords = 0;
    
    /**
     * Constructor with dependency injection.
     */
    @Autowired
    public DataMigrationService(EmployeeRepository employeeRepository,
                              TaxRateRepository taxRateRepository,
                              DeductionTypeRepository deductionTypeRepository,
                              PayrollDataRepository payrollDataRepository,
                              EmployeeFileReader employeeFileReader,
                              ResourceLoader resourceLoader) {
        this.employeeRepository = employeeRepository;
        this.taxRateRepository = taxRateRepository;
        this.deductionTypeRepository = deductionTypeRepository;
        this.payrollDataRepository = payrollDataRepository;
        this.employeeFileReader = employeeFileReader;
        this.resourceLoader = resourceLoader;
    }
    
    /**
     * Migrate all data from COBOL files to the database.
     * This is the main entry point for the migration process.
     * 
     * @param dataDir Directory containing the COBOL data files
     * @return A summary of the migration process
     */
    public MigrationResult migrateAllData(String dataDir) {
        logger.info("Starting full data migration from {}", dataDir);
        
        // Reset counters
        resetCounters();
        
        // Migrate each data type
        // The order is important due to dependencies between types
        MigrationResult employeesResult = migrateEmployees(dataDir + "/EMPFILE.dat");
        MigrationResult taxRatesResult = migrateTaxRates(dataDir + "/TAXRATES.dat");
        MigrationResult deductionTypesResult = migrateDeductionTypes(dataDir + "/DEDUCFILE.dat");
        MigrationResult payrollDataResult = migratePayrollData(dataDir + "/PAYDATA.dat");
        
        // Combine results
        MigrationResult finalResult = new MigrationResult();
        finalResult.setTotalRecords(employeesResult.getTotalRecords() + 
                                 taxRatesResult.getTotalRecords() + 
                                 deductionTypesResult.getTotalRecords() + 
                                 payrollDataResult.getTotalRecords());
        finalResult.setSuccessfulRecords(employeesResult.getSuccessfulRecords() + 
                                      taxRatesResult.getSuccessfulRecords() + 
                                      deductionTypesResult.getSuccessfulRecords() + 
                                      payrollDataResult.getSuccessfulRecords());
        finalResult.setFailedRecords(employeesResult.getFailedRecords() + 
                                  taxRatesResult.getFailedRecords() + 
                                  deductionTypesResult.getFailedRecords() + 
                                  payrollDataResult.getFailedRecords());
        
        logger.info("Full data migration completed: {}", finalResult);
        
        return finalResult;
    }
    
    /**
     * Migrate employee data from COBOL file to the database.
     * 
     * @param filePath Path to the EMPFILE.dat file
     * @return A summary of the migration process
     */
    @Transactional
    public MigrationResult migrateEmployees(String filePath) {
        logger.info("Starting employee data migration from {}", filePath);
        
        // Reset counters for this migration
        resetCounters();
        
        try (InputStream inputStream = openFile(filePath)) {
            // Read employee records from the file
            List<Employee> employees = employeeFileReader.readFile(inputStream);
            totalRecordsProcessed = employees.size();
            
            // Save each employee to the database
            for (Employee employee : employees) {
                try {
                    employeeRepository.save(employee);
                    successfulRecords++;
                } catch (Exception e) {
                    logger.error("Error saving employee {}: {}", employee.getEmployeeId(), e.getMessage());
                    failedRecords++;
                }
            }
            
            logger.info("Employee migration completed: {} total, {} successful, {} failed",
                       totalRecordsProcessed, successfulRecords, failedRecords);
            
            return createResult();
            
        } catch (IOException e) {
            logger.error("Error reading employee file: {}", e.getMessage(), e);
            failedRecords = totalRecordsProcessed;
            return createResult();
        }
    }
    
    /**
     * Migrate tax rate data from COBOL file to the database.
     * 
     * @param filePath Path to the TAXRATES.dat file
     * @return A summary of the migration process
     */
    @Transactional
    public MigrationResult migrateTaxRates(String filePath) {
        logger.info("Starting tax rate data migration from {}", filePath);
        
        // Reset counters for this migration
        resetCounters();
        
        // In a complete implementation, this would use a TaxRateFileReader
        // Similar to the EmployeeFileReader
        logger.info("Tax rate migration not yet implemented");
        
        return createResult();
    }
    
    /**
     * Migrate deduction type data from COBOL file to the database.
     * 
     * @param filePath Path to the DEDUCFILE.dat file
     * @return A summary of the migration process
     */
    @Transactional
    public MigrationResult migrateDeductionTypes(String filePath) {
        logger.info("Starting deduction type data migration from {}", filePath);
        
        // Reset counters for this migration
        resetCounters();
        
        // In a complete implementation, this would use a DeductionTypeFileReader
        // Similar to the EmployeeFileReader
        logger.info("Deduction type migration not yet implemented");
        
        return createResult();
    }
    
    /**
     * Migrate payroll data from COBOL file to the database.
     * 
     * @param filePath Path to the PAYDATA.dat file
     * @return A summary of the migration process
     */
    @Transactional
    public MigrationResult migratePayrollData(String filePath) {
        logger.info("Starting payroll data migration from {}", filePath);
        
        // Reset counters for this migration
        resetCounters();
        
        // In a complete implementation, this would use a PayrollDataFileReader
        // Similar to the EmployeeFileReader
        logger.info("Payroll data migration not yet implemented");
        
        return createResult();
    }
    
    /**
     * Open a file for reading, handling class path and file system paths.
     */
    private InputStream openFile(String filePath) throws IOException {
        try {
            // Try as a class path resource first
            Resource resource = resourceLoader.getResource("classpath:" + filePath);
            if (resource.exists()) {
                return resource.getInputStream();
            }
            
            // If not a class path resource, try as a file system path
            Path path = Paths.get(filePath);
            if (Files.exists(path)) {
                return new FileInputStream(path.toFile());
            }
            
            throw new IOException("File not found: " + filePath);
            
        } catch (Exception e) {
            throw new IOException("Error opening file: " + filePath, e);
        }
    }
    
    /**
     * Reset migration counters.
     */
    private void resetCounters() {
        totalRecordsProcessed = 0;
        successfulRecords = 0;
        failedRecords = 0;
    }
    
    /**
     * Create a migration result object with the current counters.
     */
    private MigrationResult createResult() {
        MigrationResult result = new MigrationResult();
        result.setTotalRecords(totalRecordsProcessed);
        result.setSuccessfulRecords(successfulRecords);
        result.setFailedRecords(failedRecords);
        return result;
    }
    
    /**
     * Class for tracking migration results.
     */
    public static class MigrationResult {
        private int totalRecords;
        private int successfulRecords;
        private int failedRecords;
        
        public int getTotalRecords() {
            return totalRecords;
        }
        
        public void setTotalRecords(int totalRecords) {
            this.totalRecords = totalRecords;
        }
        
        public int getSuccessfulRecords() {
            return successfulRecords;
        }
        
        public void setSuccessfulRecords(int successfulRecords) {
            this.successfulRecords = successfulRecords;
        }
        
        public int getFailedRecords() {
            return failedRecords;
        }
        
        public void setFailedRecords(int failedRecords) {
            this.failedRecords = failedRecords;
        }
        
        @Override
        public String toString() {
            return String.format("MigrationResult{totalRecords=%d, successfulRecords=%d, failedRecords=%d}",
                               totalRecords, successfulRecords, failedRecords);
        }
    }
}
