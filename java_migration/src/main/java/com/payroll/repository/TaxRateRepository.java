package com.payroll.repository;

import com.payroll.domain.TaxRate;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * Repository interface for TaxRate entities.
 * Provides methods for accessing tax rates and related tax calculation parameters.
 */
@Repository
public interface TaxRateRepository extends JpaRepository<TaxRate, Long> {

    /**
     * Find all tax rates for a specific tax year.
     *
     * @param taxYear The tax year to search for
     * @return A list of tax rates for the specified year
     */
    List<TaxRate> findByTaxYear(Integer taxYear);
    
    /**
     * Find tax rates effective on a specific date.
     *
     * @param date The date to check against effective and expiration dates
     * @return A list of tax rates effective on the specified date
     */
    List<TaxRate> findByEffectiveDateLessThanEqualAndExpirationDateGreaterThanEqual(
        LocalDate date, LocalDate sameDate);
    
    /**
     * Find a tax rate for a specific year that is effective on a specific date.
     * Uses the named query defined in the TaxRate entity.
     *
     * @param year The tax year to find
     * @param date The date to check against effective and expiration dates
     * @return The tax rate for the specified year and date, if any
     */
    @Query("SELECT t FROM TaxRate t WHERE t.taxYear = :year AND t.effectiveDate <= :date AND t.expirationDate >= :date")
    Optional<TaxRate> findByYearAndDate(@Param("year") Integer year, @Param("date") LocalDate date);
    
    /**
     * Find the most recent tax rate as of a specific date.
     *
     * @param date The reference date
     * @return The most recent tax rate as of the specified date
     */
    @Query("SELECT t FROM TaxRate t WHERE t.effectiveDate <= :date " +
           "ORDER BY t.effectiveDate DESC LIMIT 1")
    Optional<TaxRate> findMostRecentAsOf(@Param("date") LocalDate date);
    
    /**
     * Find the tax rate for the current year as of today.
     *
     * @return The tax rate for the current year, if any
     */
    default Optional<TaxRate> findCurrentTaxRate() {
        LocalDate today = LocalDate.now();
        int currentYear = today.getYear();
        return findByYearAndDate(currentYear, today);
    }
}
