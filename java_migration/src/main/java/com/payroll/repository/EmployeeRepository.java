package com.payroll.repository;

import com.payroll.domain.Employee;
import com.payroll.domain.enums.EmploymentStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;

/**
 * Repository interface for Employee entities.
 * Provides methods for accessing and manipulating employee data.
 */
@Repository
public interface EmployeeRepository extends JpaRepository<Employee, String> {

    /**
     * Find all employees with a specific employment status.
     *
     * @param status The employment status to search for
     * @return A list of employees with the specified status
     */
    List<Employee> findByStatus(EmploymentStatus status);
    
    /**
     * Find all employees in a specific department.
     *
     * @param department The department code to search for
     * @return A list of employees in the specified department
     */
    List<Employee> findByDepartment(String department);
    
    /**
     * Find all employees with a specific pay type.
     *
     * @param payType The pay type code to search for
     * @return A list of employees with the specified pay type
     */
    List<Employee> findByPayType(com.payroll.domain.enums.PayType payType);
    
    /**
     * Find employees hired between the specified dates.
     *
     * @param startDate The start date of the hire date range
     * @param endDate The end date of the hire date range
     * @return A list of employees hired within the specified date range
     */
    List<Employee> findByHireDateBetween(LocalDate startDate, LocalDate endDate);
    
    /**
     * Find employees whose last name starts with the specified prefix.
     *
     * @param prefix The last name prefix to search for
     * @return A list of employees whose last name starts with the specified prefix
     */
    List<Employee> findByLastNameStartingWith(String prefix);
    
    /**
     * Find active employees eligible for a specific deduction.
     *
     * @param status The employment status (usually ACTIVE)
     * @param healthPlanCode The health plan code to search for
     * @return A list of active employees with the specified health plan
     */
    List<Employee> findByStatusAndHealthPlanCodeNot(EmploymentStatus status, String healthPlanCode);
}
