package com.payroll.migration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Abstract base class for COBOL file readers.
 * Provides common functionality for reading and parsing COBOL data files.
 */
public abstract class CobolFileReader<T> {

    private static final Logger logger = LoggerFactory.getLogger(CobolFileReader.class);
    
    /**
     * Read data from a COBOL file and convert it to a list of Java objects.
     * 
     * @param inputStream The input stream for the COBOL data file
     * @return A list of Java objects representing the COBOL records
     * @throws IOException If an I/O error occurs
     */
    public List<T> readFile(InputStream inputStream) throws IOException {
        List<T> results = new ArrayList<>();
        int lineNumber = 0;
        
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lineNumber++;
                try {
                    T record = parseLine(line);
                    if (record != null) {
                        results.add(record);
                    }
                } catch (Exception e) {
                    logger.error("Error parsing line {}: {}", lineNumber, e.getMessage(), e);
                    logger.error("Line content: {}", line);
                }
            }
        }
        
        logger.info("Read {} records from file", results.size());
        return results;
    }
    
    /**
     * Parse a line from the COBOL file into a Java object.
     * This method must be implemented by subclasses for each specific file type.
     * 
     * @param line The line from the COBOL file
     * @return A Java object representing the COBOL record
     */
    protected abstract T parseLine(String line);
    
    /**
     * Extract a substring from a line and trim it.
     * This handles the fixed-width nature of COBOL records.
     * 
     * @param line The line from the COBOL file
     * @param start The starting index (0-based)
     * @param length The length of the field
     * @return The extracted and trimmed string
     */
    protected String extractString(String line, int start, int length) {
        // Ensure line is long enough
        if (line.length() < start + length) {
            return "";
        }
        
        return line.substring(start, start + length).trim();
    }
    
    /**
     * Extract an integer from a line.
     * 
     * @param line The line from the COBOL file
     * @param start The starting index (0-based)
     * @param length The length of the field
     * @return The extracted integer, or null if not a valid integer
     */
    protected Integer extractInteger(String line, int start, int length) {
        String value = extractString(line, start, length);
        if (value.isEmpty()) {
            return null;
        }
        
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            logger.warn("Invalid integer value: {}", value);
            return null;
        }
    }
    
    /**
     * Extract a decimal from a line.
     * 
     * @param line The line from the COBOL file
     * @param start The starting index (0-based)
     * @param length The length of the field
     * @param scale The number of decimal places
     * @return The extracted decimal, or null if not a valid decimal
     */
    protected BigDecimal extractDecimal(String line, int start, int length, int scale) {
        String value = extractString(line, start, length);
        if (value.isEmpty()) {
            return null;
        }
        
        try {
            // COBOL stores decimal values as whole numbers, with implied decimal point
            BigDecimal amount = new BigDecimal(value);
            if (scale > 0) {
                amount = amount.movePointLeft(scale);
            }
            return amount;
        } catch (NumberFormatException e) {
            logger.warn("Invalid decimal value: {}", value);
            return null;
        }
    }
    
    /**
     * Extract a date from a line in YYYYMMDD format.
     * 
     * @param line The line from the COBOL file
     * @param start The starting index (0-based)
     * @param length The length of the field (typically 8 for YYYYMMDD)
     * @return The extracted date, or null if not a valid date
     */
    protected LocalDate extractDate(String line, int start, int length) {
        String value = extractString(line, start, length);
        if (value.isEmpty() || value.equals("00000000")) {
            return null;
        }
        
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd");
            return LocalDate.parse(value, formatter);
        } catch (Exception e) {
            logger.warn("Invalid date value: {}", value);
            return null;
        }
    }
    
    /**
     * Extract a boolean from a line, typically represented as 'Y' or 'N' in COBOL.
     * 
     * @param line The line from the COBOL file
     * @param start The starting index (0-based)
     * @return The extracted boolean
     */
    protected boolean extractBoolean(String line, int start) {
        if (line.length() <= start) {
            return false;
        }
        
        char value = line.charAt(start);
        return value == 'Y' || value == 'y' || value == '1' || value == 'T' || value == 't';
    }
}
