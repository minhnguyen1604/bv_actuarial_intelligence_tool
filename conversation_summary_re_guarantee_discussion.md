# Summary of Technical & Research Discussions

## 1. Executive Dashboard UI & Header Modernization (`VAS Analysis`)
- **Scientific 4-Block Layout**: Re-architected `#vas-exec-tab` into 4 distinct analytical blocks:
  1. Integrated Executive Command Header.
  2. Executive KPI Ribbon (`Lines Summary & Financial KPIs`).
  3. 100% Full-Width Visual Analytics Canvas (24 Quarters from 2020Q1 to 2026Q1 with -30° X-axis tick labels).
  4. Deep-Dive Analytics Sub-Tabs (`Premium History & YoY Matrix`, `Quarterly Metric Comparison`, `Type Breakdown & Structure`).
- **Single Integrated Card Header**: Combined navigation tabs, action buttons (`Upload & Calculate`, `Export Excel`), and a 5-filter control row (`Mode`, `Period`, `Type`, `Metric`, `From`) into a single glassmorphic card spanning 100% width with zero horizontal scroll overflow.
- **BaoViet Corporate Palette**:
  - `Written`: `#0056A3` (BaoViet Corporate Blue)
  - `Earned`: `#0284C7` (Cyan Blue)
  - `Paid`: `#6366F1` (Indigo Purple)
  - `Incurred`: `#F59E0B` (Amber Orange)
- **Emoji Clean-up**: Removed `📊` emoji icons from empty state banners and KPI card titles for a clean, corporate aesthetic.

---

## 2. `start.bat` Script Optimization & Port Clearance
- **Issue**: Port 8000 conflicts and silent crashes due to missing working directory flags and fragile CMD regex parsing.
- **Fixes**:
  - Replaced CMD `findstr` loops with a PowerShell command:
    `powershell -NoProfile -Command "Get-NetTCPConnection -LocalPort 8000 -ErrorAction SilentlyContinue | Select-Object -ExpandProperty OwningProcess | Stop-Process -Force -ErrorAction SilentlyContinue"`
  - Added working directory flag `/d "%~dp0python_code"` to the `start` command for uvicorn.
  - Replaced non-interactive `timeout` with fail-safe `ping 127.0.0.1 -n 5 >nul` delays.

---

## 3. Thesis & Survey Document Revisions ("Tái bảo lãnh" vs. "Bảo lãnh đối ứng")

### Context & Academic Scope
- **Thesis Topic**: *Factors Affecting Corporate Customers' Decision to Choose Bank Guarantee Products at Vietnam Joint Stock Commercial Bank for Industry and Trade (VietinBank)* (Dang Tu Linh, MDE31, NEU).
- **Domain Logic**: "Tái bảo lãnh" (Re-guarantee) targets Financial Institutions (FIs), whereas the study focuses on Corporate enterprises. "Bảo lãnh đối ứng" (Counter-guarantee) is governed by credit laws and is retained under variable `NET` (`Global Correspondent Net`).

### Document Audit & Edits
1. **`DTL_Thesis_Design_NEU_MDE_Final.docx`**:
   - Confirmed **0 occurrences** of "Tái bảo lãnh" (Re-guarantee).
   - "Counter Guarantee" is preserved under variable `NET` (Table 1 & P31).
2. **`Phieu_Khao_Sat_Chinh_Thuc_VietinBank_revised.docx`**:
   - **Paragraph P30**: Removed `/ Tái bảo lãnh (RG)`, updating option to `[  ] Bảo lãnh bảo hành (Warranty Guarantee)`.
   - **Table 0 (Likert Statements)**: Removed the empty first column (formerly variable codes `COST1`, `COL1`...) left by manual deletion, restructuring Table 0 into a clean 6-column format (`Statement | 1 | 2 | 3 | 4 | 5`).
   - **Section Headers**: Fixed missing titles for Section II (`II. Biện pháp Bảo đảm & Tỷ lệ Ký quỹ`) and Section VI (`VI. Mối quan hệ Gắn kết với Ngân hàng`), and corrected typo in Section VIII (`VIII. Thiết kế Giải pháp Bảo lãnh theo yêu cầu`).

### Econometric Model Design Clarification
- Product category data is collected in **Section I (General Info)** of the survey.
- In the primary econometric formula:
  $$DEC = \beta_0 + \beta_1 COST + \beta_2 COL + \beta_3 SPE + \beta_4 REP + \beta_5 STA + \beta_6 REL + \beta_7 DIG + \beta_8 CUS + \beta_9 RSK + \beta_{10} NET + \varepsilon$$
  Product type variables are evaluated via **Sub-group Analysis / Segmented Regressions** or **Control Dummy Variables** in Chapter 4, avoiding Likert item inflation while providing actionable product-specific recommendations.
