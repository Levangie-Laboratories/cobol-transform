package com.payroll.repository;

import com.payroll.domain.DeductionType;
import com.payroll.domain.enums.DeductionCategory;
import com.payroll.domain.enums.DeductionStatus;
import com.payroll.domain.enums.TaxStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;

/**
 * Repository interface for DeductionType entities.
 * Provides methods for accessing deduction types and parameters.
 */
@Repository
public interface DeductionTypeRepository extends JpaRepository<DeductionType, String> {

    /**
     * Find all deduction types with a specific category.
     *
     * @param category The deduction category to search for
     * @return A list of deduction types in the specified category
     */
    List<DeductionType> findByCategory(DeductionCategory category);
    
    /**
     * Find all deduction types with a specific status.
     *
     * @param status The deduction status to search for
     * @return A list of deduction types with the specified status
     */
    List<DeductionType> findByStatus(DeductionStatus status);
    
    /**
     * Find all deduction types with a specific tax status.
     *
     * @param taxStatus The tax status to search for
     * @return A list of deduction types with the specified tax status
     */
    List<DeductionType> findByTaxStatus(TaxStatus taxStatus);
    
    /**
     * Find all active deduction types.
     *
     * @return A list of active deduction types
     */
    default List<DeductionType> findAllActive() {
        return findByStatus(DeductionStatus.ACTIVE);
    }
    
    /**
     * Find all active pre-tax deduction types.
     *
     * @return A list of active pre-tax deduction types
     */
    List<DeductionType> findByStatusAndTaxStatus(DeductionStatus status, TaxStatus taxStatus);
    
    /**
     * Find active deduction types that are effective on a specific date.
     *
     * @param status Usually ACTIVE
     * @param date The date to check against effective and expiration dates
     * @return A list of deduction types that are effective on the specified date
     */
    @Query("SELECT d FROM DeductionType d WHERE d.status = :status " +
           "AND (d.effectiveDate IS NULL OR d.effectiveDate <= :date) " +
           "AND (d.expirationDate IS NULL OR d.expirationDate >= :date)")
    List<DeductionType> findActiveAndEffectiveOn(
        @Param("status") DeductionStatus status, @Param("date") LocalDate date);
    
    /**
     * Find all required deduction types.
     *
     * @return A list of required deduction types
     */
    List<DeductionType> findByRequiredTrue();
    
    /**
     * Find all deduction types with employer matching.
     *
     * @return A list of deduction types with employer matching
     */
    List<DeductionType> findByEmployerMatchTrue();
    
    /**
     * Find deduction types by a specific vendor.
     *
     * @param vendorId The vendor ID to search for
     * @return A list of deduction types for the specified vendor
     */
    List<DeductionType> findByVendorId(String vendorId);
    
    /**
     * Find active deduction types in a specific category.
     *
     * @param status Usually ACTIVE
     * @param category The deduction category to search for
     * @return A list of active deduction types in the specified category
     */
    List<DeductionType> findByStatusAndCategory(DeductionStatus status, DeductionCategory category);
}
