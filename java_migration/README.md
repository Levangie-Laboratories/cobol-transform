# Payroll System - Java Migration

This project is a Java migration of a legacy COBOL payroll processing system. It preserves all the business logic and functionality of the original system while leveraging modern Java technologies and approaches.

## System Overview

The Payroll System is designed to calculate employee salaries, taxes, deductions, and generate pay stubs. It processes employee and payroll data for each pay period, performs various calculations, and produces output files including pay stubs and summary reports.

## Architecture

The application follows a multi-tiered architecture:
- **Presentation Layer**: REST API and report generation
- **Service Layer**: Core business logic implementation
- **Data Access Layer**: Database interaction components
- **Domain Model**: Java entity classes representing the business objects

## Technology Stack

- Java 11
- Spring Boot 2.7.x
- Spring Data JPA
- PostgreSQL Database
- JasperReports for report generation
- Maven for dependency management and build
- JUnit and Mockito for testing

## System Requirements

- JDK 11 or later
- Maven 3.6 or later
- PostgreSQL 12 or later

## Installation and Setup

### Database Setup

1. Create a PostgreSQL database named `payroll`:
   ```sql
   CREATE DATABASE payroll;
   ```

2. Configure database connection in `src/main/resources/application.properties` if needed

### Building the Application

```bash
mvn clean install
```

### Running the Application

```bash
mvn spring-boot:run
```

Or after building:

```bash
java -jar target/payroll-system-1.0.0-SNAPSHOT.jar
```

## Data Migration

The data migration process converts legacy COBOL data files to the new database:

1. Place COBOL data files in the `/data` directory
2. Run the data migration utility:
   ```bash
java -jar target/payroll-system-1.0.0-SNAPSHOT.jar --migrate
   ```

## Testing

### Running Tests

```bash
mvn test
```

### Test Coverage Report

```bash
mvn jacoco:report
```

The coverage report will be available at `target/site/jacoco/index.html`

## Main Components

### Domain Model

The domain model classes correspond to the original COBOL copybooks:
- `Employee` - maps to EMPFILE.cpy
- `TaxRate` - maps to TAXRATES.cpy
- `DeductionType` - maps to DEDUCFILE.cpy
- `PayrollData` - maps to PAYDATA.cpy

### Services

Service components implement the business logic from the COBOL modules:
- `PayrollService` - corresponds to PAYCALC.cbl
- `TaxCalculationService` - corresponds to TAXCALC.cbl
- `DeductionCalculationService` - corresponds to DEDCALC.cbl
- `PayStubService` - corresponds to PAYSTUB.cbl

## Documentation

- Javadoc documentation is available in the `doc` directory after building with `mvn javadoc:javadoc`
- Additional technical documentation is available in the `docs` directory

## Contribution Guidelines

1. Follow the existing code style and patterns
2. Write unit tests for new functionality
3. Update documentation as needed
4. Submit pull requests for code review

## License

This project is proprietary and confidential.

## Support

For support and issues, please contact the development team.
