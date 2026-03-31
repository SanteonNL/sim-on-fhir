# Development Notes

## Python Setup

**Issue**: Python not available directly in terminal (aliased in Windows)

**Solution**: Use one of these aliases/methods:
- `py -3` — Recommended Windows Python launcher
- `py` — Python 3 (if configured as default)
- Check if PowerShell alias exists: `Get-Alias py`

**Example**:
```powershell
cd c:\Users\t.hetterscheid\Repo\sim-on-fhir\input\static
py generate_fhir_sql_mappings.py
```

## Projects

### SIM-ON-FHIR (Santeon CarePlan IG)
- **Location**: `c:\Users\t.hetterscheid\Repo\sim-on-fhir`
- **SQL Mappings**: `input/static/queries/careplan.sql` → FHIR CarePlan profile
- **FSH Profile**: `input/fsh/SanteonCarePlan.fsh`
- **Mapping Generator**: `input/static/generate_fhir_sql_mappings.py`

**Key Files**:
- LUSCII SQL tables:
  - `luscii_patients` (t1) — Patient data
  - `luscii_patientsprogramshistory` (t2) — Enrollment timeline
  - `luscii_programs` (t3) — Program/category info
  - `luscii_usersStatusChangeReasons` — Status change tracking

**Documentation**:
- `input/static/FHIR-to-SQL-Mapping.md` — Detailed element mapping
- `input/static/CarePlan-DataFlow.md` — Data flow diagrams
- `input/static/careplan-fhir-sql-mapping.csv` — Quick reference table

**Auto-Generated FSH Mappings**:
- `input/static/generate_fhir_sql_mappings.py` — Python script that generates FHIR mappings with tables & columns
- Output: `input/static/careplan_fhir_mappings.fsh` — Auto-generated FSH mapping definitions
- **Usage**: `py generate_fhir_sql_mappings.py` from `input/static/` directory
- Lists only tables and columns used (no transformation logic)
- Uses SQL filename (careplan.sql) for the mapping identity

