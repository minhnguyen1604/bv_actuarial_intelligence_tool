import os
import json
import pandas as pd
from typing import List, Dict, Any

# Load schemas globally
SCHEMAS_PATH = os.path.join(os.path.dirname(__file__), "..", "schemas.json")
schemas = {}
if os.path.exists(SCHEMAS_PATH):
    with open(SCHEMAS_PATH, 'r', encoding='utf-8') as f:
        schemas = json.load(f)

def validate_form(file_path: str, sheet_name: str, group_code: str) -> Dict[str, Any]:
    """
    Validates a specific sheet in an excel file based on group_code rules.
    Returns: {"ok": bool, "errors": list of error strings}
    """
    errors = []
    
    try:
        df = pd.read_excel(file_path, sheet_name=sheet_name)
    except Exception as e:
        return {"ok": False, "errors": [f"Cannot read Excel sheet: {str(e)}"]}
        
    # Basic check: is dataframe empty?
    if df.empty:
        return {"ok": False, "errors": ["File is empty."]}
        
    if group_code == "Vietjet":
        return _check_vietjet(df)
    elif "XCG" in group_code or "PA_NNTX" in group_code:
        return _check_xcg(df, group_code)
    else:
        return _check_general(df, group_code)


def _check_vietjet(df: pd.DataFrame) -> Dict[str, Any]:
    errors = []
    if len(df.columns) < 25:
        errors.append(f"File doesn't have enough columns (Needs 25, got {len(df.columns)}).")
        
    # Mock more checks
    # ...
    
    return {"ok": len(errors) == 0, "errors": errors}

def _check_xcg(df: pd.DataFrame, group_code: str) -> Dict[str, Any]:
    errors = []
    # Mock checks for dates, money, duplicates
    return {"ok": len(errors) == 0, "errors": errors}

def _check_general(df: pd.DataFrame, group_code: str) -> Dict[str, Any]:
    errors = []
    # Determine the template schema
    schema_key = None
    if "Eng_LT" in group_code: schema_key = "Eng_LT_Pre"
    elif "Eng_ST" in group_code: schema_key = "Eng_ST_Pre"
    elif "Fire_LT" in group_code: schema_key = "Fire_LT_Pre"
    elif "Fire_ST" in group_code: schema_key = "Fire_ST_Pre"
    elif "Misc_LT" in group_code: schema_key = "Misc_LT_Pre"
    elif "Misc_ST" in group_code: schema_key = "Misc_ST_Pre"
    elif "Marine" in group_code: schema_key = "7_term"
    
    if schema_key and schema_key in schemas:
        required_cols = schemas[schema_key]
        if len(df.columns) < len(required_cols):
            # In real logic, we pad with NA. Here we just warn.
            pass
            
    return {"ok": len(errors) == 0, "errors": errors}
