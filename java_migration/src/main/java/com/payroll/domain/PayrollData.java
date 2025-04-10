package com.payroll.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Entity representing payroll data for a specific pay period.
 * Maps to the PAYROLL-DATA-RECORD structure in PAYDATA.cpy.
 */
@Entity
@Table(name = "payroll_data")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PayrollData {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "employee_id", length = 6, nullable = false)
    private String employeeId; // Maps to PAY-EMPLOYEE-ID
    
    // Pay Period Information
    @Column(name = "pay_period_id", nullable = false)
    private Integer payPeriodId; // Maps to PAY-PERIOD-ID
    
    @Column(name = "pay_period_start_date", nullable = false)
    private LocalDate payPeriodStartDate; // Maps to PAY-PERIOD-START-DATE
    
    @Column(name = "pay_period_end_date", nullable = false)
    private LocalDate payPeriodEndDate; // Maps to PAY-PERIOD-END-DATE
    
    @Column(name = "check_date", nullable = false)
    private LocalDate checkDate; // Maps to PAY-CHECK-DATE
    
    // Work Hours
    @Column(name = "regular_hours", precision = 5, scale = 2)
    private BigDecimal regularHours; // Maps to PAY-REGULAR-HOURS
    
    @Column(name = "overtime_hours", precision = 5, scale = 2)
    private BigDecimal overtimeHours; // Maps to PAY-OVERTIME-HOURS
    
    @Column(name = "doubletime_hours", precision = 5, scale = 2)
    private BigDecimal doubletimeHours; // Maps to PAY-DOUBLETIME-HOURS
    
    @Column(name = "shift_diff_hours", precision = 5, scale = 2)
    private BigDecimal shiftDiffHours; // Maps to PAY-SHIFT-DIFF-HOURS
    
    @Column(name = "shift_diff_rate", precision = 4, scale = 2)
    private BigDecimal shiftDiffRate; // Maps to PAY-SHIFT-DIFF-RATE
    
    @Column(name = "on_call_hours", precision = 5, scale = 2)
    private BigDecimal onCallHours; // Maps to PAY-ON-CALL-HOURS
    
    @Column(name = "on_call_rate", precision = 4, scale = 2)
    private BigDecimal onCallRate; // Maps to PAY-ON-CALL-RATE
    
    @Column(name = "holiday_hours", precision = 5, scale = 2)
    private BigDecimal holidayHours; // Maps to PAY-HOLIDAY-HOURS
    
    @Column(name = "holiday_rate", precision = 4, scale = 2)
    private BigDecimal holidayRate; // Maps to PAY-HOLIDAY-RATE
    
    // Additional Amounts
    @Column(name = "bonus_amount", precision = 9, scale = 2)
    private BigDecimal bonusAmount; // Maps to PAY-BONUS-AMOUNT
    
    @Column(name = "commission_amount", precision = 9, scale = 2)
    private BigDecimal commissionAmount; // Maps to PAY-COMMISSION-AMOUNT
    
    @Column(name = "commission_rate", precision = 4, scale = 2)
    private BigDecimal commissionRate; // Maps to PAY-COMMISSION-RATE
    
    @Column(name = "commission_sales", precision = 11, scale = 2)
    private BigDecimal commissionSales; // Maps to PAY-COMMISSION-SALES
    
    @Column(name = "tips_amount", precision = 9, scale = 2)
    private BigDecimal tipsAmount; // Maps to PAY-TIPS-AMOUNT
    
    @Column(name = "allowance_amount", precision = 9, scale = 2)
    private BigDecimal allowanceAmount; // Maps to PAY-ALLOWANCE-AMOUNT
    
    @Column(name = "reimbursement_amount", precision = 9, scale = 2)
    private BigDecimal reimbursementAmount; // Maps to PAY-REIMBURSEMENT-AMT
    
    @Column(name = "other_earnings", precision = 9, scale = 2)
    private BigDecimal otherEarnings; // Maps to PAY-OTHER-EARNINGS
    
    @Column(name = "other_earnings_description", length = 20)
    private String otherEarningsDescription; // Maps to PAY-OTHER-EARNINGS-DESC
    
    // Leave Time
    @Column(name = "vacation_hours", precision = 5, scale = 2)
    private BigDecimal vacationHours; // Maps to PAY-VACATION-HOURS
    
    @Column(name = "sick_hours", precision = 5, scale = 2)
    private BigDecimal sickHours; // Maps to PAY-SICK-HOURS
    
    @Column(name = "personal_hours", precision = 5, scale = 2)
    private BigDecimal personalHours; // Maps to PAY-PERSONAL-HOURS
    
    @Column(name = "bereavement_hours", precision = 5, scale = 2)
    private BigDecimal bereavementHours; // Maps to PAY-BEREAVEMENT-HOURS
    
    @Column(name = "jury_duty_hours", precision = 5, scale = 2)
    private BigDecimal juryDutyHours; // Maps to PAY-JURY-DUTY-HOURS
    
    @Column(name = "fmla_hours", precision = 5, scale = 2)
    private BigDecimal fmlaHours; // Maps to PAY-FMLA-HOURS
    
    @Column(name = "military_hours", precision = 5, scale = 2)
    private BigDecimal militaryHours; // Maps to PAY-MILITARY-HOURS
    
    @Column(name = "other_leave_hours", precision = 5, scale = 2)
    private BigDecimal otherLeaveHours; // Maps to PAY-OTHER-LEAVE-HOURS
    
    @Column(name = "other_leave_description", length = 20)
    private String otherLeaveDescription; // Maps to PAY-OTHER-LEAVE-DESC
    
    // Adjustments
    @Column(name = "manual_tax_adjustment", precision = 9, scale = 2)
    private BigDecimal manualTaxAdjustment; // Maps to PAY-MANUAL-TAX-ADJUST
    
    @Column(name = "manual_deduction_adjustment", precision = 9, scale = 2)
    private BigDecimal manualDeductionAdjustment; // Maps to PAY-MANUAL-DEDUCT-ADJ
    
    @Column(name = "retro_pay_amount", precision = 9, scale = 2)
    private BigDecimal retroPayAmount; // Maps to PAY-RETRO-PAY-AMOUNT
    
    @Column(name = "advance_amount", precision = 9, scale = 2)
    private BigDecimal advanceAmount; // Maps to PAY-ADVANCE-AMOUNT
    
    @Column(name = "garnish_override", precision = 9, scale = 2)
    private BigDecimal garnishOverride; // Maps to PAY-GARNISH-OVERRIDE
    
    @Column(name = "other_adjustment_amount", precision = 9, scale = 2)
    private BigDecimal otherAdjustmentAmount; // Maps to PAY-OTHER-ADJUST-AMT
    
    @Column(name = "other_adjustment_description", length = 20)
    private String otherAdjustmentDescription; // Maps to PAY-OTHER-ADJUST-DESC
    
    // Override Flags
    @Column(name = "override_rate")
    private boolean overrideRate; // Maps to PAY-OVERRIDE-RATE-FLAG
    
    @Column(name = "override_rate_amount", precision = 6, scale = 2)
    private BigDecimal overrideRateAmount; // Maps to PAY-OVERRIDE-RATE
    
    @Column(name = "skip_tax")
    private boolean skipTax; // Maps to PAY-SKIP-TAX-FLAG
    
    @Column(name = "skip_deduction")
    private boolean skipDeduction; // Maps to PAY-SKIP-DEDUCT-FLAG
    
    @Column(name = "special_calculation")
    private boolean specialCalculation; // Maps to PAY-SPECIAL-CALC-FLAG
    
    // Status
    @Column(name = "record_status", length = 1)
    private String recordStatus; // Maps to PAY-RECORD-STATUS
    
    @Column(name = "error_code", length = 4)
    private String errorCode; // Maps to PAY-ERROR-CODE
    
    @Column(name = "error_description", length = 50)
    private String errorDescription; // Maps to PAY-ERROR-DESC
    
    // Audit Fields
    @Column(name = "created_timestamp", length = 20)
    private String createdTimestamp; // Maps to PAY-CREATED-TIMESTAMP
    
    @Column(name = "created_user", length = 15)
    private String createdUser; // Maps to PAY-CREATED-USER
    
    @Column(name = "updated_timestamp", length = 20)
    private String updatedTimestamp; // Maps to PAY-UPDATED-TIMESTAMP
    
    @Column(name = "updated_user", length = 15)
    private String updatedUser; // Maps to PAY-UPDATED-USER
    
    // Relationship to Employee
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "employee_id", referencedColumnName = "employee_id", insertable = false, updatable = false)
    private Employee employee;
    
    /**
     * Calculates the total hours worked in this pay period.
     * 
     * @return The sum of regular, overtime, doubletime, and other hour types
     */
    public BigDecimal getTotalHours() {
        BigDecimal total = BigDecimal.ZERO;
        
        if (regularHours != null) total = total.add(regularHours);
        if (overtimeHours != null) total = total.add(overtimeHours);
        if (doubletimeHours != null) total = total.add(doubletimeHours);
        if (shiftDiffHours != null) total = total.add(shiftDiffHours);
        if (onCallHours != null) total = total.add(onCallHours);
        if (holidayHours != null) total = total.add(holidayHours);
        
        return total;
    }
    
    /**
     * Calculates the total leave hours in this pay period.
     * 
     * @return The sum of all leave hour types
     */
    public BigDecimal getTotalLeaveHours() {
        BigDecimal total = BigDecimal.ZERO;
        
        if (vacationHours != null) total = total.add(vacationHours);
        if (sickHours != null) total = total.add(sickHours);
        if (personalHours != null) total = total.add(personalHours);
        if (bereavementHours != null) total = total.add(bereavementHours);
        if (juryDutyHours != null) total = total.add(juryDutyHours);
        if (fmlaHours != null) total = total.add(fmlaHours);
        if (militaryHours != null) total = total.add(militaryHours);
        if (otherLeaveHours != null) total = total.add(otherLeaveHours);
        
        return total;
    }
}
