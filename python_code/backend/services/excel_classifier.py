import re
import unicodedata
from typing import Optional

GROUP_MAPPING = {
    "Eng": ["engineering", "eng", "donkt", "kt"],
    "Marine": ["hang hai", "marine"],
    "Misc": ["misc", "tnrrhh"],
    "Fire": ["fire", "tai san", "ts"],
    "PA_TTTBVV": ["chet", "tpd"],
    "Vietjet": ["vietjet"],
    "PA": ["bhcn", "pa"],
    "XCG": ["xcg"],
    "PA_NNTX": ["ntx"],
    "XCG_CWVN": ["cwvn"],
    "Travel_BHTT": ["bhtt"],
    "Travel_CTTV": ["cttv"],
    "Kcare": ["kcare"]
}

TERM_MAPPING = {
    "LT": ["lt", "dai ky", "dai ki", "longterm", "dk", "365"],
    "ST": ["st", "ngan ky", "shortterm", "nk", "364", "ngan ki"]
}

def remove_accents(input_str: str) -> str:
    if not input_str:
        return ""
    nfkd_form = unicodedata.normalize('NFKD', input_str)
    return "".join([c for c in nfkd_form if not unicodedata.combining(c)])

def get_group_code(file_name: str, sheet_name: str) -> Optional[str]:
    """
    Classifies the Excel file into a Group Code (e.g., Eng_LT, Vietjet, etc.)
    based on the file name and sheet name, mirroring the legacy R logic.
    """
    if not file_name:
        file_name = ""
    if not sheet_name:
        sheet_name = ""
        
    file_name = remove_accents(file_name).lower()
    sheet_name = remove_accents(sheet_name).lower()
    
    group = None
    term = None
    
    # 1. Find group in sheet_name first
    for g, keywords in GROUP_MAPPING.items():
        if any(re.search(kw, sheet_name, re.IGNORECASE) for kw in keywords):
            group = g
            break
            
    # 2. If not found, find group in file_name
    if not group:
        for g, keywords in GROUP_MAPPING.items():
            if any(re.search(kw, file_name, re.IGNORECASE) for kw in keywords):
                group = g
                break
                
    # 3. Special groups that don't need LT/ST term suffix
    if group in ["PA_TTTBVV", "Travel_BHTT", "Vietjet", "Travel_CTTV"]:
        return group
        
    # 4. Find term in sheet_name first
    for t, keywords in TERM_MAPPING.items():
        if any(re.search(kw, sheet_name, re.IGNORECASE) for kw in keywords):
            term = t
            break
            
    # 5. If not found, find term in file_name
    if not term:
        for t, keywords in TERM_MAPPING.items():
            if any(re.search(kw, file_name, re.IGNORECASE) for kw in keywords):
                term = t
                break
                
    # 6. Combine group and term
    if group and term:
        return f"{group}_{term}"
        
    return None
