# Technical Architecture

## System Overview

The Payroll System Java Migration is a comprehensive modernization of a legacy COBOL-based payroll processing system. The migration preserves all business logic and functionality while leveraging modern Java technologies and architectural patterns.

### High-Level Architecture

The system follows a layered architecture pattern:

```
┌─────────────────────────┐
│      Presentation       │  (Future - REST API/Web UI)
└───────────┬─────────────┘
            │
┌───────────▼─────────────┐
│      Service Layer      │  (Business Logic)
└───────────┬─────────────┘
            │
┌───────────▼─────────────┐
│    Data Access Layer    │  (Repositories)
└───────────┬─────────────┘
            │
┌───────────▼─────────────┐
│      Domain Model       │  (Entities)
└───────────┬─────────────┘
            │
┌───────────▼─────────────┐
│        Database         │  (PostgreSQL)
└─────────────────────────┘
```

### Key Components

1. **Domain Model**: Java entity classes representing the business objects, mapped from COBOL copybooks
2. **Repositories**: Spring Data JPA interfaces for database access
3. **Services**: Business logic components that implement the core functionality
4. **Migration Utilities**: Tools for converting COBOL data files to the Java/database format

### Technology Stack

- **Framework**: Spring Boot 2.7.x
- **ORM**: Hibernate/JPA
- **Database**: PostgreSQL
- **Build Tool**: Maven
- **Java Version**: 11+
- **Reporting**: JasperReports

## Domain Model

### Entity Classes

The domain model consists of Java entity classes that map directly to the COBOL copybook structures:

| COBOL Copybook | Java Entity Class | Description |
|----------------|-------------------|-------------|
| EMPFILE.cpy    | Employee.java     | Employee master data |
| TAXRATES.cpy   | TaxRate.java      | Tax rates and brackets |
| DEDUCFILE.cpy  | DeductionType.java| Deduction definitions |
| PAYDATA.cpy    | PayrollData.java  | Pay period data |

### Domain Model Diagram

```
┌────────────┐       ┌────────────┐
│  Employee  │       │ PayrollData│
└─────┬──────┘       └──────┬─────┘
      │                     │
      │ uses               │ references
      │                    │
┌─────▼──────┐      ┌──────▼─────┐
│DeductionType│      │  TaxRate   │
└─────┬──────┘      └────────────┘
      │
      │ contains
      │
┌─────▼──────┐
│GraduatedRange│
└────────────┘
```

### Data Type Mapping

COBOL data types have been mapped to appropriate Java types:

| COBOL Data Type | Java Data Type |
|-----------------|----------------|
| PIC X(n)        | String         |
| PIC 9(n)        | Integer        |
| PIC 9(n)V9(m)   | BigDecimal     |
| PIC X (Y/N)     | boolean        |
| YYYYMMDD date   | LocalDate      |

## Data Access Layer

### Repository Interfaces

The data access layer uses Spring Data JPA repositories, which provide methods for CRUD operations and custom queries:

```java
@Repository
public interface EmployeeRepository extends JpaRepository<Employee, String> {
    List<Employee> findByStatus(EmploymentStatus status);
    List<Employee> findByDepartment(String department);
}
```

### Transaction Management

Transactions are managed through Spring's declarative transaction management using `@Transactional` annotations. Service methods that modify data are annotated to ensure data consistency:

```java
@Service
public class PayrollServiceImpl implements PayrollService {

    @Transactional
    public PayStub processEmployeePayroll(Employee employee, PayrollData payrollData) {
        // Method body
    }
}
```

## Service Layer

### Service Components

The service layer consists of interfaces and implementations that encapsulate the business logic:

| COBOL Program | Java Service Interface | Java Service Implementation |
|---------------|----------------------|---------------------------|
| PAYCALC.cbl   | PayrollService      | PayrollServiceImpl        |
| TAXCALC.cbl   | TaxCalculationService | TaxCalculationServiceImpl |
| DEDCALC.cbl   | DeductionCalculationService | DeductionCalculationServiceImpl |
| PAYSTUB.cbl   | PayStubService      | PayStubServiceImpl        |

### Service Interactions

The services interact in a way that mirrors the COBOL program flow:

```
                   ┌─────────────────┐
                   │  PayrollService │
                   └────────┬────────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
┌─────────────▼─────┐ ┌─────▼───────┐ ┌──▼───────────┐
│TaxCalculationService│ │PayStubService│ │DeductionService│
└───────────────────┘ └─────────────┘ └────────────────┘
```

1. `PayrollService` orchestrates the overall payroll process
2. `TaxCalculationService` calculates various taxes
3. `DeductionCalculationService` calculates employee deductions
4. `PayStubService` generates pay stubs with calculated values

### Business Logic Implementation

The business logic in each service implementation follows the same rules and calculations as the original COBOL programs. For example:

- **Gross Pay Calculation**: The same formulas for calculating regular, overtime, and other pay based on employee type (hourly vs. salaried)
- **Tax Calculation**: The same tax bracket logic and withholding calculations
- **Deduction Processing**: The same rules for applying various deductions based on deduction types and employee elections

## Migration Utilities

### Data Migration Components

Migration utilities include:

1. **CobolFileReader**: Abstract base class for reading COBOL fixed-width data files
2. **EmployeeFileReader**: Reads EMPFILE.dat and converts records to Employee entities
3. **TaxRateFileReader**: Reads TAXRATES.dat and converts records to TaxRate entities
4. **DeductionTypeFileReader**: Reads DEDUCFILE.dat and converts records to DeductionType entities
5. **PayrollDataFileReader**: Reads PAYDATA.dat and converts records to PayrollData entities
6. **DataMigrationService**: Coordinates the overall migration process

### Migration Process Flow

```
┌────────────────┐    ┌───────────┐    ┌──────────────┐
│ COBOL Data File│───>│File Reader│───>│ Java Entities │
└────────────────┘    └───────────┘    └──────┬───────┘
                                             │
                                             ▼
┌────────────────┐    ┌───────────┐    ┌──────────────┐
│   Database     │<───│Repository │<───│DataMigration │
└────────────────┘    └───────────┘    └──────────────┘
```

## Deployment Architecture

### System Requirements

- **Java**: JDK 11 or higher
- **Memory**: Minimum 4GB RAM
- **Disk Space**: Minimum 1GB for application and database
- **Database**: PostgreSQL 12 or higher

### Deployment Options

1. **Standalone Application**: JAR file with embedded Tomcat server
2. **Containerized**: Docker image for easy deployment
3. **Application Server**: Deployable as WAR file to Tomcat, JBoss, etc.

### Configuration

Configuration is managed through Spring Boot's property system:

- **application.properties**: Core configuration for database, logging, etc.
- **Environment Variables**: Override properties for different environments
- **Profiles**: Different configurations for development, test, production

## Security Considerations

### Data Protection

- Sensitive employee data is encrypted in the database
- Personal identifiable information (PII) is protected according to data privacy regulations
- Database access is restricted through authentication and authorization

### Application Security

- Input validation to prevent SQL injection and other attacks
- Output encoding to prevent cross-site scripting
- Proper error handling to avoid information disclosure

## Performance Considerations

### Database Optimization

- Appropriate indexes for common query patterns
- Connection pooling for efficient database access
- Query optimization for complex operations

### Caching

- Reference data (tax rates, deduction types) is cached for performance
- Employee data is cached when appropriate

## Testing Strategy

### Test Types

- **Unit Tests**: For individual components
- **Integration Tests**: For component interactions
- **System Tests**: For end-to-end functionality
- **Performance Tests**: For system under load

### Test Coverage

- Business logic is thoroughly tested for correctness
- Edge cases are covered to ensure robustness
- Boundary conditions are tested to validate constraints

## Conclusion

The Java migration of the COBOL payroll system preserves all the original functionality while leveraging modern technologies and practices. The architecture follows standard patterns for maintainability, extensibility, and performance. The system is designed to be a drop-in replacement for the legacy system, providing the same business capabilities with improved technical foundations.
