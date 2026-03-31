#!/usr/bin/env python3
"""
Generate FHIR FSH SQL Mappings - Updates SanteonCarePlan.fsh
Generates mappings for all SQL files in queries/ folder.
Extracts SQL comments and includes them in the mapping output.
"""

from pathlib import Path
import re


class SQLMappingGenerator:
    """Generate table/column mappings from SQL files"""
    
    def __init__(self, sql_path: str):
        self.sql_path = Path(sql_path)
        self.sql_filename = self.sql_path.name
        # Create valid FHIR id from filename (replace dots and underscores with hyphens)
        self.fhir_id = self.sql_filename.replace('.', '-').replace('_', '-').lower()
        with open(sql_path, 'r') as f:
            self.sql_content = f.read()
        self.sql_comments = self._extract_sql_comments()
        self.alias_to_tables = self._extract_source_tables()
        
    def _extract_sql_comments(self) -> dict:
        """Extract SQL comments and associate them with column aliases.
        
        Strategy: Look for lines with 'AS column_alias' and then scan backwards
        to collect all consecutive comment lines (starting with '--').
        Only captures actual comment lines, stops at first non-comment line.
        
        Returns a dict: {column_alias: [comment_lines]}
        """
        comments = {}
        
        # Split SQL into lines for processing
        lines = self.sql_content.split('\n')
        
        for i, line in enumerate(lines):
            # Look for lines with AS column_alias pattern
            alias_match = re.search(r'\sAS\s+(\w+)', line, re.IGNORECASE)
            if alias_match:
                column_alias = alias_match.group(1).lower()
                
                # Look backwards for consecutive comment lines (no empty lines allowed)
                comment_lines = []
                j = i - 1
                
                # First skip any empty lines immediately before AS
                while j >= 0 and not lines[j].strip():
                    j -= 1
                
                # Now collect consecutive comment-only lines
                while j >= 0:
                    prev_line = lines[j].strip()
                    if prev_line.startswith('--'):
                        # Extract comment text, removing the '--' prefix
                        comment_text = prev_line[2:].strip()
                        if comment_text:
                            comment_lines.insert(0, comment_text)
                        j -= 1
                    else:
                        # Stop at first non-comment line
                        break
                
                if comment_lines:
                    comments[column_alias] = comment_lines
        
        return comments
    
    def _extract_source_tables(self) -> dict:
        """Extract actual source table.column references for each SQL alias.
        
        Maps SQL aliases (like careplanidentifier) to their source columns
        (like luscii_patients.Patient_Number, luscii_programs.name, etc.)
        
        Returns a dict: {alias: [table.column, table.column, ...]}
        """
        alias_to_sources = {}
        
        # Manually define mappings based on SQL query structure
        # This is specific to the LUSCII and PJ data sources
        if "luscii_careplan" in self.sql_filename:
            alias_to_sources = {
                'careplanidentifier': [
                    'luscii_patients.Patient_Number',
                    'luscii_programs.name',
                    'luscii_patientsprogramshistory.createdAt'
                ],
                'careplancategorycodingsystem': [],  # Fixed empty value
                'careplancategorycodingcode': ['luscii_programs.name'],
                'careplanperiodstart': ['luscii_patientsprogramshistory.createdAt'],
                'careplanperiodend': [
                    'luscii_usersStatusChangeReasons.processedAt',
                    'luscii_patientsprogramshistory.createdAt'
                ],
            }
        elif "pj_careplan" in self.sql_filename:
            alias_to_sources = {
                'careplanidentifier': [
                    'pj_patients.patient_ref',
                    'pj_programmes.programme_code',
                    'pj_enrolments.enrolment_date'
                ],
                'careplancategorycodingsystem': [],  # Fixed value
                'careplancategorycodingcode': ['pj_programmes.programme_code'],
                'careplanperiodstart': ['pj_enrolments.enrolment_date'],
                'careplanperiodend': ['pj_enrolments.discharge_date'],
            }
        
        return alias_to_sources
        
    def generate_query_section(self) -> str:
        """Generate the Source Query section with SQL comments"""
        lines = []
        lines.append("// ============================================================")
        lines.append(f"// Source Query: {self.sql_filename}")
        lines.append("// ============================================================")
        
        # Add SQL query as comments
        for sql_line in self.sql_content.split('\n'):
            lines.append(f"// {sql_line}")
        
        lines.append("// ============================================================")
        return "\n".join(lines)
    
    def generate_table_mapping_section(self) -> str:
        """Generate the table[column] mapping section with SQL comments"""
        lines = []
        lines.append("// ============================================================")
        lines.append(f"// SQL Column to Table Mapping ({self.sql_filename})")
        lines.append("// ============================================================")
        
        # Extract only actual SQL column aliases (AS followed by word at end of line or before comma)
        # This regex is more specific to avoid false positives
        for alias_match in re.finditer(r'\bAS\s+(\w+)\s*(?:,\s*$|$)', self.sql_content, re.MULTILINE | re.IGNORECASE):
            column_alias = alias_match.group(1).lower()
            
            # Get associated comments
            comment_lines = self.sql_comments.get(column_alias, [])
            if comment_lines:
                # Format with comments
                for comment in comment_lines:
                    lines.append(f"// {column_alias:<40} — {comment}")
            else:
                lines.append(f"// {column_alias:<40}")
        
        lines.append("// ============================================================")
        return "\n".join(lines)
    
    def generate_fsh_mappings(self, mapping_index: int = 0) -> str:
        """Generate FSH ^mapping statements for each FHIR element with source table.column names
        
        Args:
            mapping_index: The index for element-level mappings (0 for first SQL source, 1 for second, etc.)
        """
        lines = []
        
        # Profile-level mapping header (use valid FHIR id)
        lines.append(f"* ^mapping[+].identity = \"{self.fhir_id}\"")
        lines.append(f"* ^mapping[=].name = \"SQL Data Mapping ({self.sql_filename})\"")
        lines.append(f"* ^mapping[=].uri = \"urn:santeon:mapping:{self.fhir_id}\"")
        lines.append(f"* ^mapping[=].comment = \"Maps CarePlan elements to {self.sql_filename}\"\n")
        
        # Map FHIR elements to SQL aliases
        fhir_to_sql = {
            'identifier': ['careplanidentifier'],
            'category': ['careplancategorycodingcode'],
            'category.coding': ['careplancategorycodingcode'],
            'category.coding.code': ['careplancategorycodingcode'],
            'category.coding.system': ['careplancategorycodingsystem'],
            'subject': ['careplanidentifier'],
            'period': ['careplanperiodstart', 'careplanperiodend'],
            'period.start': ['careplanperiodstart'],
            'period.end': ['careplanperiodend'],
        }
        
        # Generate element-level mappings (only for elements with actual source columns)
        for fhir_element, sql_aliases in fhir_to_sql.items():
            # Collect actual source table.column names and comments
            source_columns = []
            comments_list = []
            
            for alias in sql_aliases:
                # Get actual source tables for this alias
                tables = self.alias_to_tables.get(alias, [])
                if tables:  # Only add if there are actual source columns
                    source_columns.extend(tables)
                    # Get comments for this alias
                    alias_comments = self.sql_comments.get(alias, [])
                    if alias_comments:
                        comments_list.append(' '.join(alias_comments))
            
            # Only output mapping if we have source columns
            if source_columns:
                # Build map value with line breaks between columns (no brackets, just table.column)
                formatted_columns = []
                for col in source_columns:
                    # Keep original table.column format, just add to list for line-break joining
                    formatted_columns.append(col)
                
                # Join with \n for line breaks in the map value
                map_value = "\\n".join(formatted_columns)
                
                # Build comment from collected comments
                comment_value = " | ".join(comments_list) if comments_list else ""
                
                # Output FSH mapping statements with the correct mapping index
                lines.append(f"* {fhir_element} ^mapping[{mapping_index}].identity = \"{self.fhir_id}\"")
                lines.append(f"* {fhir_element} ^mapping[{mapping_index}].map = \"{map_value}\"")
                if comment_value:
                    lines.append(f"* {fhir_element} ^mapping[{mapping_index}].comment = \"{comment_value}\"")
                lines.append("")
        
        return "\n".join(lines)


class ProfileUpdater:
    """Updates the FHIR profile with mappings from all SQL files"""
    
    def __init__(self, profile_path: str, queries_dir: str):
        self.profile_path = Path(profile_path)
        self.queries_dir = Path(queries_dir)
        with open(profile_path, 'r') as f:
            self.profile_content = f.read()
    
    def find_all_sql_files(self) -> list:
        """Find all .sql files in the queries directory"""
        return sorted(self.queries_dir.glob('*.sql'))
    
    def generate_all_mappings(self) -> str:
        """Generate FSH element mappings for all SQL files"""
        sql_files = self.find_all_sql_files()
        
        if not sql_files:
            print("Warning: No SQL files found in queries directory")
            return ""
        
        # Generate FSH mappings for each file with the correct mapping index
        mapping_sections = []
        for mapping_index, sql_file in enumerate(sql_files):
            print(f"Processing: {sql_file.name}")
            generator = SQLMappingGenerator(str(sql_file))
            mapping_sections.append(generator.generate_fsh_mappings(mapping_index))
        
        return "\n".join(mapping_sections).rstrip()
    
    def update_profile(self) -> bool:
        """Update the profile FSH file with mapping statements only"""
        
        # Find and replace the entire mapping section (from first mapping to before suppress comments)
        # Pattern: from start of ^mapping statements to before "// Suppress meta"
        mapping_section_pattern = r'\* \^mapping\[.*?(?=// ---|\n// ---)'
        
        new_mapping_entries = self.generate_all_mappings()
        
        # Replace the mapping section
        updated_content = re.sub(
            mapping_section_pattern,
            f'{new_mapping_entries.rstrip()}\n',
            self.profile_content,
            flags=re.DOTALL
        )
        
        # Write updated content back to file
        with open(self.profile_path, 'w') as f:
            f.write(updated_content)
        
        return True



def main():
    """Main entry point"""
    queries_dir = Path(__file__).parent / "queries"
    profile_path = Path(__file__).parent.parent / "fsh" / "SanteonCarePlan.fsh"
    
    if not queries_dir.exists():
        print(f"Error: Queries directory not found at {queries_dir}")
        return 1
    
    if not profile_path.exists():
        print(f"Error: Profile file not found at {profile_path}")
        return 1
    
    print(f"Queries directory: {queries_dir}")
    print(f"Profile path: {profile_path}\n")
    
    updater = ProfileUpdater(str(profile_path), str(queries_dir))
    
    # Generate and display mappings
    print("=== GENERATED MAPPINGS ===\n")
    all_mappings = updater.generate_all_mappings()
    print(all_mappings)
    
    # Update the profile file
    if updater.update_profile():
        print(f"\n* Profile updated: {profile_path}")
        return 0
    else:
        print(f"\n! Failed to update profile")
        return 1


if __name__ == "__main__":
    exit(main())
