package com.payroll.repository;

import com.payroll.domain.PayrollData;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;

/**
 * Repository interface for PayrollData entities.
 * Provides methods for accessing and manipulating payroll period data.
 */
@Repository
public interface PayrollDataRepository extends JpaRepository<PayrollData, Long> {

    /**
     * Find all payroll data for a specific employee.
     *
     * @param employeeId The employee ID to search for
     * @return A list of payroll data records for the specified employee
     */
    List<PayrollData> findByEmployeeId(String employeeId);
    
    /**
     * Find all payroll data for a specific pay period.
     *
     * @param payPeriodId The pay period ID to search for
     * @return A list of payroll data records for the specified pay period
     */
    List<PayrollData> findByPayPeriodId(Integer payPeriodId);
    
    /**
     * Find payroll data for a specific employee in a specific pay period.
     *
     * @param employeeId The employee ID to search for
     * @param payPeriodId The pay period ID to search for
     * @return The payroll data record for the specified employee and pay period, if any
     */
    PayrollData findByEmployeeIdAndPayPeriodId(String employeeId, Integer payPeriodId);
    
    /**
     * Find all payroll data for a specific pay period date range.
     *
     * @param startDate The start date of the pay period
     * @param endDate The end date of the pay period
     * @return A list of payroll data records for the specified pay period date range
     */
    List<PayrollData> findByPayPeriodStartDateAndPayPeriodEndDate(
        LocalDate startDate, LocalDate endDate);
    
    /**
     * Find all payroll data with a specific record status.
     *
     * @param recordStatus The record status to search for
     * @return A list of payroll data records with the specified status
     */
    List<PayrollData> findByRecordStatus(String recordStatus);
    
    /**
     * Find all payroll data with a non-zero bonus amount.
     *
     * @return A list of payroll data records with bonuses
     */
    @Query("SELECT pd FROM PayrollData pd WHERE pd.bonusAmount > 0")
    List<PayrollData> findAllWithBonuses();
    
    /**
     * Find all payroll data with a specific error code.
     *
     * @param errorCode The error code to search for
     * @return A list of payroll data records with the specified error code
     */
    List<PayrollData> findByErrorCode(String errorCode);
    
    /**
     * Find all payroll data with check dates in a specific range.
     *
     * @param startDate The start date of the check date range
     * @param endDate The end date of the check date range
     * @return A list of payroll data records with check dates in the specified range
     */
    List<PayrollData> findByCheckDateBetween(LocalDate startDate, LocalDate endDate);
}
