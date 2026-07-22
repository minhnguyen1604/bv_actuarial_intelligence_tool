process_next_file <- function() {
  if (length(vals$queue) == 0) {
    vals$processing <- NULL
    return()
  }else{
  
  # Lấy file tiếp theo
  file_name <- vals$queue[1]
  selected_files = vals$queue
  vals$queue <- vals$queue[-1]
  vals$processing <- file_name

  req(input$dpnv_ngay ,input$dpnv_thang, input$dpnv_nam )
# Ví dụ input từ Shiny
dpnv_ngay  <- input$dpnv_ngay   # số
dpnv_thang <- input$dpnv_thang  # số
dpnv_nam   <- input$dpnv_nam 



if (is.null(selected_files) || length(selected_files) == 0) {
  showNotification("⚠️ Bạn chưa chọn file nào!", type = "warning")
  return()
}

dir.create("www/output_excel", showWarnings = FALSE)

# Hàm tạo tên cột cho từng kỳ phí
generate_fee_cols <- function(ky) {
  prefix <- paste0("Ky_phi_", ky)
  c(
    paste0(prefix, "_So_tien_VND"),
    paste0(prefix, "_So_tien_USD"),
    paste0(prefix, "_So_tien_EUR"),
    paste0(prefix, "_Tu_Ngay"),
    paste0(prefix, "_Tu_Thang"),
    paste0(prefix, "_Tu_Nam"),
    paste0(prefix, "_Den_Ngay"),
    paste0(prefix, "_Den_Thang"),
    paste0(prefix, "_Den_Nam"),
    paste0(prefix, "_Ghi_Ngay"),
    paste0(prefix, "_Ghi_Thang"),
    paste0(prefix, "_Ghi_Nam")
  )
}
ty_gia <- readRDS("ty_gia.rds")
#____________________________________________________________________________________Longterm
    if(file_name %in% vals$long_files){
      while(sink.number() > 0) sink(NULL) 
      future({
        tryCatch({
          
    rds_path <- file.path(paste0("www/cur_data_",date_now()), paste0(file_name, ".rds"))
    
    if (!file.exists(rds_path)) {
      showNotification(paste("❌ Không tìm thấy file:", file_name), type = "error")
      next
    }
    
    df <- readRDS(rds_path)
    if (!is.data.frame(df)) {
      showNotification(paste("❌ File", file_name, "không phải là data.frame"), type = "error")
      next
    }
    
    if (ncol(df) < 24) {
      showNotification(paste("❗ File", file_name, "không đủ 24 cột cố định"), type = "error")
      next
    }
    out_file <- file.path("www/output_excel", paste0(file_name, ".xlsx"))
    
    wb <- openxlsx::createWorkbook()
    
    
    vlookup_range <- paste0("Tygia!$A$1:$C$", nrow(ty_gia)+1)
    
    fallback_cell <- paste0("Tygia!$A$", nrow(ty_gia)+1 )
    
    addWorksheet(wb, "Tygia")
    writeDataTable(
      wb,
      sheet = "Tygia",
      x = ty_gia,
      tableName = "ty_gia"
    )
    
    # Sau khi đã thêm các sheet Ky_phi, tạo sheet "Result"
    addWorksheet(wb, "Result")
    
    
    
    # Lấy danh sách kỳ phí thực sự có mặt
    ky_available <- unique(gsub("Ky_phi_([0-9]+)_.*", "\\1",
                                grep("^Ky_phi_\\d+_", colnames(df), value = TRUE)))
    ky_available <- sort(as.integer(ky_available))
    
    if (length(ky_available) == 0) {
      showNotification(paste("⚠️ File", file_name, "không có kỳ phí nào hợp lệ."), type = "warning")
      next
    }

    
    sheet <- paste0("Ky_phi", ky_available)
    
    # Các chỉ tiêu cần tính
    columns_to_sum <- c(
      "Phi_bao_hiem_sau_dong",
      "Phi_bao_hiem_giu_lai",
      "Phi_tai_bao_hiem",
      "Phi_bao_hiem_giu_lai_duoc_huong",
      "Phi_tai_bao_hiem_duoc_huong",
      "Phi_bao_hiem_giu_lai_chua_huong",
      "Phi_tai_bao_hiem_chua_huong"
    )
    four_last_quarters <- as.vector(ty_gia[(nrow(ty_gia) -3) :nrow(ty_gia),1])
    four_last_quarters <- c("Số dùng để tính",four_last_quarters)
    
    
    result_data <- expand.grid(
      Ky_phi = sheet,
      Quy = four_last_quarters,
      stringsAsFactors = FALSE
    )
    
    # Thêm các cột công thức, khởi tạo là NA_character_
    for (col in columns_to_sum) {
      result_data[[col]] <- NA_character_
    }
    
    # Gán tên cột tương ứng
    colnames(result_data) <- c("Ky_phi", "Quy", columns_to_sum)
    
    
    for (ky in ky_available) {
      showNotification(
        paste0("📂 Đang xử lý file: ", file_name, " | Kỳ phí: ", ky),
        type = "message",
        duration = 10
      )
      
      fee_cols <- generate_fee_cols(ky)
      fee_cols_exist <- fee_cols[fee_cols %in% colnames(df)]
      base_cols <- colnames(df)[1:min(24, ncol(df))]
      sheet_data <- df[, c(base_cols, fee_cols_exist), drop = FALSE]
      
      
      cols_to_add <- c(
        "Quy_ghi_nhan_doanh_thu", "Quy_ghi_nhan_doanh_thu_2024", "Quy_ghi_nhan_doanh_thu_2025", 
        "Thoi_diem_ghi_nhan_doanh_thu", "Check_01", "Check_02", "Check_03", "Check_04", "Check_05", 
        "Check_06", "Check_07", "Tổng hợp các tiêu chí", 
        "Thoi_diem_tinh_DPNV_Ngay", "Thoi_diem_tinh_DPNV_Thang", "Thoi_diem_tinh_DPNV_Nam", 
        "Thu_tu_Quy_DPNV", "Mau_so", "Tu_so_huong_cu", "Tu_so_chua_huong", 
        "Tu_so_huong_sau_dieu_chinh", "Tu_so_chua_huong_dieu_chinh", 
        "TS_chua_huong_SĐC_final", "MS_SĐC_final", 
        "Phi_bao_hiem_sau_dong", "Phi_bao_hiem_giu_lai", "Phi_tai_bao_hiem", 
        "Phi_bao_hiem_giu_lai_duoc_huong", "Phi_tai_bao_hiem_duoc_huong", 
        "Phi_bao_hiem_giu_lai_chua_huong", "Phi_tai_bao_hiem_chua_huong", 
        "Check_Phi_bao_hiem_giu_lai_chua_huong", "Check_Phi_tai_bao_hiem_chua_huong"
      )
      
      for (col in cols_to_add) {
        if (!col %in% colnames(sheet_data)) {
          sheet_data[[col]] <- NA
        }
      }
      
      # Ngày tháng
      sheet_data$Thoi_diem_tinh_DPNV_Ngay <- dpnv_ngay
      sheet_data$Thoi_diem_tinh_DPNV_Thang <- dpnv_thang
      sheet_data$Thoi_diem_tinh_DPNV_Nam <- dpnv_nam

      # Tên sheet
      sheet_name <- paste0("Ky_phi", ky)
      addWorksheet(wb, sheet_name)
      
      # Ghi công thức sử dụng wb_formula()
      n <- nrow(sheet_data)
      ghi_thang <- paste0("Ky_phi_", ky, "_Ghi_Thang")
      ghi_nam   <- paste0("Ky_phi_", ky, "_Ghi_Nam")
      den_ngay  <- paste0("Ky_phi_", ky, "_Den_Ngay")
      den_thang <- paste0("Ky_phi_", ky, "_Den_Thang")
      den_nam   <- paste0("Ky_phi_", ky, "_Den_Nam")
      
      header <- colnames(sheet_data)
      
      col_pos <- function(col_name) which(header == col_name)
      
      openxlsx::writeData(wb, sheet = sheet_name, x = sheet_data, withFilter = TRUE)
      #addTable(wb, sheet_name, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium2", tableName = sheet_name)
      
      #___________quý ghi nhận doanh thu
      # Lấy vị trí cột (số) rồi chuyển sang chữ
      col_thang <- int2col(col_pos(ghi_thang))   # ghi letter
      col_nam   <- int2col(col_pos(ghi_nam))
      col_out   <- col_pos("Quy_ghi_nhan_doanh_thu")  # ghi ra số thứ tự cột
      
      # Tạo công thức cho từng hàng
      rows <- 2:(n + 1)
      
      formulas <- paste0(
        '=IF(', col_thang, rows, '="",0,INT((', col_thang, rows, '-1)/3)+1)'
      )

      # Ghi công thức vào sheet
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_out,
        startRow = 2
      )
      
      #___________quý ghi nhận doanh thu 2024
      
      # Vị trí cột đầu ra
      col_out_2024 <- col_pos("Quy_ghi_nhan_doanh_thu_2024")
      col_quy      <- int2col(col_pos("Quy_ghi_nhan_doanh_thu"))
      col_nam      <- int2col(col_pos(ghi_nam))
      
      # Tạo công thức theo từng dòng
      formulas_quy_2024 <- paste0(
        '=IF(VALUE(', col_nam, rows, ')=2024,', col_quy, rows, ',0)'
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas_quy_2024,
        startCol = col_out_2024,
        startRow = 2
      )
      
      #___________quý ghi nhận doanh thu 2025
      
      # Vị trí cột đầu ra
      col_out_2025 <- col_pos("Quy_ghi_nhan_doanh_thu_2025")
      col_quy      <- int2col(col_pos("Quy_ghi_nhan_doanh_thu"))
      col_nam      <- int2col(col_pos(ghi_nam))
      
      # Tạo công thức theo từng dòng
      formulas_quy_2025 <- paste0(
        '=IF(VALUE(', col_nam, rows, ')=2025,', col_quy, rows, ',0)'
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas_quy_2025,
        startCol = col_out_2025,
        startRow = 2
      )
     
      #___________ thời điểm ghi nhận doanh thu
      
      col_quy <- int2col(col_pos("Quy_ghi_nhan_doanh_thu"))
      col_nam <- int2col(col_pos(ghi_nam))
      
      
      formulas <- paste0(
        '=CONCATENATE("Q",',
        col_quy, rows, ',"/",',
        col_nam, rows, ')'
      )
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Thoi_diem_ghi_nhan_doanh_thu"),
        startRow = 2
      )
     
      #___________ check_01
      
      
      # Lấy tên cột thành chữ Excel
      col_check <- col_pos("Check_01")
      col_ngay  <- int2col(col_pos(den_ngay))
      col_thang <- int2col(col_pos(den_thang))
      col_nam   <- int2col(col_pos(den_nam))
      
      col_dpnv_ngay  <- int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay"))
      col_dpnv_thang <- int2col(col_pos("Thoi_diem_tinh_DPNV_Thang"))
      col_dpnv_nam   <- int2col(col_pos("Thoi_diem_tinh_DPNV_Nam"))
      
      # Viết công thức từng dòng
      formulas_check01 <- paste0(
        '=IFERROR(IF(DATE(', col_dpnv_nam, rows, ',', col_dpnv_thang, rows, ',', col_dpnv_ngay, rows, ') - ',
        'DATE(', col_nam, rows, ',', col_thang, rows, ',', col_ngay, rows, ') >= 0, 0, 1), 0)'
      )
      
      # Ghi công thức vào file
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas_check01,
        startCol = col_check,
        startRow = 2
      )
    
      
      #_______________________ check 02
      col_check02 <- col_pos("Check_02")
      
      tu_ngay   <- int2col(col_pos("Thoi_han_bao_hiem_Tu_Ngay"))
      tu_thang  <- int2col(col_pos("Thoi_han_bao_hiem_Tu_Thang"))
      tu_nam    <- int2col(col_pos("Thoi_han_bao_hiem_Tu_Nam"))
      
      den_ngay  <- int2col(col_pos("Thoi_han_bao_hiem_Den_Ngay"))
      den_thang <- int2col(col_pos("Thoi_han_bao_hiem_Den_Thang"))
      den_nam   <- int2col(col_pos("Thoi_han_bao_hiem_Den_Nam"))
      
      formulas_check02 <- paste0(
        '=IF(OR(',
        tu_ngay, rows, '="",',
        tu_thang, rows, '="",',
        tu_nam, rows, '="",',
        den_ngay, rows, '="",',
        den_thang, rows, '="",',
        den_nam, rows, '=""),0,',
        'IF(DATE(', den_nam, rows, ',', den_thang, rows, ',', den_ngay, rows, ') - ',
        'DATE(', tu_nam, rows, ',', tu_thang, rows, ',', tu_ngay, rows, ') <= 365,0,1))'
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas_check02,
        startCol = col_check02,
        startRow = 2
      )

      #_______________________ check 03
      
      col_vnd <- int2col(col_pos(paste0("Ky_phi_",ky,"_So_tien_VND")))
      col_usd <- int2col(col_pos(paste0("Ky_phi_",ky,"_So_tien_USD")))
      col_eur <- int2col(col_pos(paste0("Ky_phi_",ky,"_So_tien_EUR")))
      
      formulas <- paste0(
        '=IF(AND(',
        col_vnd, rows, '="",',
        col_usd, rows, '="",',
        col_eur, rows, '=""),0,1)'
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_03"),
        startRow = 2
      )

      #_______________________ check 04
      tu_nam   <- int2col(col_pos(paste0("Ky_phi_", ky, "_Tu_Nam")))
      tu_thang <- int2col(col_pos(paste0("Ky_phi_", ky, "_Tu_Thang")))
      tu_ngay  <- int2col(col_pos(paste0("Ky_phi_", ky, "_Tu_Ngay")))
      
      den_nam   <- int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Nam")))
      den_thang <- int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Thang")))
      den_ngay  <- int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Ngay")))
      
      formulas <- paste0(
        '=IFERROR(IF(DATE(', den_nam, rows, ",", den_thang, rows, ",", den_ngay, rows,
        ')-DATE(', tu_nam, rows, ",", tu_thang, rows, ",", tu_ngay, rows,
        ")>0,1,0),0)"
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_04"),
        startRow = 2
      )
     
      #_______________________ check 05
      
      formulas <- paste0(
        '=IF(AND(', int2col(col_pos(ghi_thang)), rows, '="",', int2col(col_pos(ghi_nam)), rows, '=""), 0,1)'
      )
 
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_05"),
        startRow = 2
      )
    
      #_______________________ check 06
      formulas <- paste0(
        '=IFERROR(IF(DATE(',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Nam")), rows, ',',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Thang")), rows, ',',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay")), rows, ') - DATE(',
        int2col(col_pos("Thoi_han_bao_hiem_Tu_Nam")), rows, ',',
        int2col(col_pos("Thoi_han_bao_hiem_Tu_Thang")), rows, ',',
        int2col(col_pos("Thoi_han_bao_hiem_Tu_Ngay")), rows, ')>=0,1,0),0)'
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_06"),
        startRow = 2
      )
    
      #_______________________ check 07
      
      formulas <- paste0(
        '=IFERROR(IF(DATE(',
        int2col(col_pos(paste0("Ky_phi_",ky,"_Ghi_Nam"))), rows, ',',
        int2col(col_pos(paste0("Ky_phi_",ky,"_Ghi_Thang"))), rows, ',',
        int2col(col_pos(paste0("Ky_phi_",ky,"_Ghi_Ngay"))), rows, ') - DATE(',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Nam")), rows, ',',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Thang")), rows, ',',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay")), rows, ') > 0, 0, 1), 0)'
      )
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_07"),
        startRow = 2
      )
    
      #_______________________ tổng hợp các tiêu chí
      
      formulas <- paste0(
        '=IF(OR(',
        int2col(col_pos("Check_01")), rows, '=0, ',
        int2col(col_pos("Check_02")), rows, '=0, ',
        int2col(col_pos("Check_03")), rows, '=0, ',
        int2col(col_pos("Check_04")), rows, '=0, ',
        int2col(col_pos("Check_05")), rows, '=0, ',
        int2col(col_pos("Check_06")), rows, '=0, ',
        int2col(col_pos("Check_07")), rows, '=0), 0, 1)'
      )
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Tổng hợp các tiêu chí"),
        startRow = 2
      )
    
      #_______________________ tổng hợp các tiêu chí
      
      formulas <- paste0(
        '=IF(', int2col(col_pos("Tổng hợp các tiêu chí")), rows, '=0, 0, ',
        'IFERROR((',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Nam")), rows, ' - ',
        int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Nam"))), rows, ') * 4 + ',
        'INT((', int2col(col_pos("Thoi_diem_tinh_DPNV_Thang")), rows, ' - 1) / 3) + 1 - ',
        '(INT((', int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Thang"))), rows, ' - 1) / 3) + 1 )+1, 0))'
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Thu_tu_Quy_DPNV"),
        startRow = 2
      )
     
      #_______________________ mau so
      
      formulas <- paste0(
        '=IFERROR((',
        '(DATE(', 
        int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Nam"))), rows, ",",
        int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Thang"))), rows, ",",
        int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Ngay"))), rows, ') - ',
        'DATE(',
        int2col(col_pos(paste0("Ky_phi_", ky, "_Tu_Nam"))), rows, ",",
        int2col(col_pos(paste0("Ky_phi_", ky, "_Tu_Thang"))), rows, ",",
        int2col(col_pos(paste0("Ky_phi_", ky, "_Tu_Ngay"))), rows, ') + 1) / 365) * 8, 0)'
      )
      
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Mau_so"),
        startRow = 2
      )
   
      
      #_______________________ Tu_so_huong_cu
      
      
      formulas <- paste0(
        '=IF(', int2col(col_pos("Tổng hợp các tiêu chí")), rows, '=0, ',
        int2col(col_pos("Mau_so")), rows, ', ',
        'IF(', int2col(col_pos("Thu_tu_Quy_DPNV")), rows, '<=0, 0, ',
        int2col(col_pos("Thu_tu_Quy_DPNV")), rows, '*2-1))'
      )
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Tu_so_huong_cu"),
        startRow = 2
      )

      #_______________________ Tu_so_chua_huong
      
      
      formulas <- paste0(
        '=', int2col(col_pos("Mau_so")), rows, ' - ', 
        int2col(col_pos("Tu_so_huong_cu")), rows
      )
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Tu_so_chua_huong"),
        startRow = 2
      )

      #_______________________ Tu_so_huong_sau_dieu_chinh
      
      
      formulas <- paste0(
        '=', int2col(col_pos("Mau_so")), rows, ' - ', 
        int2col(col_pos("Tu_so_chua_huong_dieu_chinh")), rows
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Tu_so_huong_sau_dieu_chinh"),
        startRow = 2
      )
     
      #_______________________ Tu_so_chua_huong_dieu_chinh
      
      formulas <- paste0(
        '=IFERROR(IF(',
        int2col(col_pos("Tu_so_chua_huong")), rows, '>=0, ',
        int2col(col_pos("Tu_so_chua_huong")), rows, ', ((',
        'DATE(',
        int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Nam"))), rows, ',',
        int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Thang"))), rows, ',',
        int2col(col_pos(paste0("Ky_phi_", ky, "_Den_Ngay"))), rows, ') - ',
        'DATE(',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Nam")), rows, ',',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Thang")), rows, ',',
        int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay")), rows, ')',
        ') / 365) * 8)',
        ',0)'
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Tu_so_chua_huong_dieu_chinh"),
        startRow = 2
      )

      #_______________________ TS_chua_huong_SĐC_final
      formulas <- paste0(
        int2col(col_pos("Tu_so_chua_huong_dieu_chinh")), rows
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("TS_chua_huong_SĐC_final"),
        startRow = 2
      )
      
      #_______________________ MS_SĐC_final
      formulas <- paste0(
        int2col(col_pos("Mau_so")), rows
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("MS_SĐC_final"),
        startRow = 2
      )
     
      
      #_______________________ Phi_bao_hiem_sau_dong
      formulas <- paste0(
        "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_VND"))), rows, ") + ",
        "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_USD"))), rows, ") * IFERROR(VALUE(VLOOKUP(",
        int2col(col_pos("Thoi_diem_ghi_nhan_doanh_thu")), rows, ", ",
        vlookup_range, ", 2, 0)), VALUE(VLOOKUP(", fallback_cell, ", ",
        vlookup_range, ", 2, 0))) + ",
        "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_EUR"))), rows, ") * IFERROR(VALUE(VLOOKUP(",
        int2col(col_pos("Thoi_diem_ghi_nhan_doanh_thu")), rows, ", ",
        vlookup_range, ", 3, 0)), VALUE(VLOOKUP(", fallback_cell, ", ",
        vlookup_range, ", 3, 0)))"
      )
  
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_bao_hiem_sau_dong"),
        startRow = 2
      )
  
      #_______________________ Phi_bao_hiem_giu_lai
      
      col_name <- if ("Ty_le_giu_lai_cua_BHBV" %in% colnames(sheet_data)) {
        "Ty_le_giu_lai_cua_BHBV"
      } else if ("Ty_le_giu_lai_cua_BHBV_checked" %in% colnames(sheet_data)) {
        "Ty_le_giu_lai_cua_BHBV_checked"
      } else {
        stop("Không tìm thấy cột giữ lại")
      }
      
      hi <- int2col(col_pos(col_name))
      phi_col <- int2col(col_pos("Phi_bao_hiem_sau_dong"))
      
      # formulas <- paste0(
      #   'IF(', phi_col, rows, '="", 0, VALUE(', phi_col, rows, ') * IF(',
      #   hi, rows, '="", 1, IF(VALUE(SUBSTITUTE(', hi, rows, ',".",",")) > 1,',
      #   'VALUE(SUBSTITUTE(', hi, rows, ',".",",")) / 100,',
      #   'VALUE(SUBSTITUTE(', hi, rows, ',".",","))',
      #   ')))'
      # )
      formulas <- paste0(
        'IF(', phi_col, rows, '="", 0, VALUE(', phi_col, rows, ') * IF(OR(',
        hi, rows, '="",',hi, rows, '=0), 1, IF(VALUE(', hi, rows, ') > 1,',
        'VALUE(', hi, rows, ') / 100, VALUE(', hi, rows, '))))'
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_bao_hiem_giu_lai"),
        startRow = 2
      )
      #_______________________ Phi_tai_bao_hiem
      formulas <- paste0(
        "IF(",
        int2col(col_pos("Phi_bao_hiem_sau_dong")), rows, '="", 0, VALUE(',
        int2col(col_pos("Phi_bao_hiem_sau_dong")), rows, ") - VALUE(",
        int2col(col_pos("Phi_bao_hiem_giu_lai")), rows, "))"
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_tai_bao_hiem"),
        startRow = 2
      )
      #_______________________ Phi_bao_hiem_giu_lai_chua_huong
      formulas <- paste0(
        "IF(",
        "VALUE(", int2col(col_pos("MS_SĐC_final")), rows, ")=0, 0, ",
        "VALUE(", int2col(col_pos("Phi_bao_hiem_giu_lai")), rows, ") * ",
        "VALUE(", int2col(col_pos("TS_chua_huong_SĐC_final")), rows, ") / ",
        "VALUE(", int2col(col_pos("MS_SĐC_final")), rows, ")",
        ")"
      )
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_bao_hiem_giu_lai_chua_huong"),
        startRow = 2
      )
      #_______________________ Phi_bao_hiem_giu_lai_duoc_huong

      formulas <- paste0(
        "VALUE(", int2col(col_pos("Phi_bao_hiem_giu_lai")), rows, ") - ",
        "VALUE(", int2col(col_pos("Phi_bao_hiem_giu_lai_chua_huong")), rows, ")"
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_bao_hiem_giu_lai_duoc_huong"),
        startRow = 2
      )
      
      
      #_______________________ Phi_tai_bao_hiem_duoc_huong
      formulas <- paste0(
        "VALUE(", int2col(col_pos("Phi_tai_bao_hiem")), rows, ") - ",
        "VALUE(", int2col(col_pos("Phi_tai_bao_hiem_chua_huong")), rows, ")"
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_tai_bao_hiem_duoc_huong"),
        startRow = 2
      )
      
      #_______________________ Phi_tai_bao_hiem_chua_huong
      formulas <- paste0(
        "IF(",
        "VALUE(", int2col(col_pos("MS_SĐC_final")), rows, ")=0, 0, ",
        "VALUE(", int2col(col_pos("Phi_tai_bao_hiem")), rows, ") * ",
        "VALUE(", int2col(col_pos("TS_chua_huong_SĐC_final")), rows, ") / ",
        "VALUE(", int2col(col_pos("MS_SĐC_final")), rows, ")",
        ")"
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Phi_tai_bao_hiem_chua_huong"),
        startRow = 2
      )
      #_______________________ Check_Phi_bao_hiem_giu_lai_chua_huong
      formulas <- paste0(
        '=IF(',
        'VALUE(', int2col(col_pos("MS_SĐC_final")), rows, ') = 0, 0, ',
        'VALUE(', int2col(col_pos("Phi_bao_hiem_giu_lai")), rows, ') * (',
        'VALUE(', int2col(col_pos("Tu_so_huong_sau_dieu_chinh")), rows, ') / ',
        'VALUE(', int2col(col_pos("MS_SĐC_final")), rows, ')) - ',
        'VALUE(', int2col(col_pos("Phi_bao_hiem_giu_lai_duoc_huong")), rows, '))'
      )
      
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_Phi_bao_hiem_giu_lai_chua_huong"),
        startRow = 2
      )
      
      #_______________________ Check_Phi_tai_bao_hiem_chua_huong
      formulas <- paste0(
        'IF(',
        'VALUE(', int2col(col_pos("MS_SĐC_final")), rows, ') = 0, 0, ',
        'VALUE(', int2col(col_pos("Phi_tai_bao_hiem")), rows, ') * (',
        'VALUE(', int2col(col_pos("Tu_so_huong_sau_dieu_chinh")), rows, ') / ',
        'VALUE(', int2col(col_pos("MS_SĐC_final")), rows, ')) - ',
        'VALUE(', int2col(col_pos("Phi_tai_bao_hiem_duoc_huong")), rows, '))'
      )
      
      writeFormula(
        wb,
        sheet = sheet_name,
        x = formulas,
        startCol = col_pos("Check_Phi_tai_bao_hiem_chua_huong"),
        startRow = 2
      )
      
    }
    
    start_row <- 2
    end_row <- start_row + n - 1

    for (col in columns_to_sum) {
      result_data[[col]] <- mapply(function(sheet1, quy1) {
        col_range <- paste0(sheet1, "!", int2col(col_pos(col)), start_row, ":", int2col(col_pos(col)), end_row)
        
        criteria_range <- paste0(sheet1, "!", int2col(col_pos("Tổng hợp các tiêu chí")), start_row, ":", int2col(col_pos("Tổng hợp các tiêu chí")), end_row)
        
        quy_range <- paste0(sheet1, "!", int2col(col_pos("Thoi_diem_ghi_nhan_doanh_thu")), start_row, ":", int2col(col_pos("Thoi_diem_ghi_nhan_doanh_thu")), end_row)
        if (quy1 != "Số dùng để tính"){
        formula_text <- paste0('SUMIFS(', col_range, ',', criteria_range, ',"1",', quy_range, ',"', quy1, '")')
        }else{
        formula_text <- paste0('SUMIFS(', col_range, ',', criteria_range, ',"1")')
        }
        return(formula_text)
      },
      result_data$Ky_phi,
      result_data$Quy
      )
    }


    writeData(wb, sheet = "Result", x = "SUBTOTAL", startCol = 1, startRow = 1)
    for (j in seq_along(columns_to_sum)) {
      col_index <- j + 2  # bắt đầu từ cột 3
      
      # Tính số dòng dữ liệu
      num_rows <- nrow(result_data)
      
      # Tạo công thức SUBTOTAL, ví dụ =SUBTOTAL(9, C3:C{num_rows + 2})
      col_letter <- int2col(col_index)
      subtotal_formula <- paste0("=SUBTOTAL(9,", col_letter, "3:", col_letter, num_rows + 2, ")")
      
      writeFormula(
        wb, sheet = "Result",
        x = subtotal_formula,
        startCol = col_index,
        startRow = 1
      )
    }
    
    # Ghi header theo hàng ngang
    writeData(wb, sheet = "Result", x = t(as.data.frame(names(result_data))), startCol = 1, startRow = 2, colNames = FALSE, withFilter = TRUE)
    

    # Ghi dữ liệu từng dòng
    for (i in seq_len(nrow(result_data))) {
      # Ghi Ky_phi và Quy
      writeData(wb, sheet = "Result", x = result_data$Ky_phi[i], startCol = 1, startRow = i + 2)
      writeData(wb, sheet = "Result", x = result_data$Quy[i], startCol = 2, startRow = i + 2)
      
      # Ghi công thức cho từng cột cần tính
      for (j in seq_along(columns_to_sum)) {
        formula_text <- result_data[[columns_to_sum[j]]][i]
        writeFormula(
          wb, sheet = "Result",
          x = paste0("=", formula_text),
          startCol = j + 2,  # cột 1 là Ky_phi, 2 là Quy, nên bắt đầu từ cột 3
          startRow = i + 2
        )
      }
    }
    
    
    saveWorkbook(wb, out_file, overwrite = TRUE)
    # 🔄 Cập nhật danh sách file sau khi lưu
    files_rv(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
    showNotification(paste("✅ Xuất Excel cho", file_name, "hoàn tất!"))
    #___________________hết long term
    

    }, error = function(e) {
      shiny::showNotification(
        paste("❌ Lỗi khi xử lý", file_name),
        type = "error"
      )
      return(NULL)
    })
    
    vals$done <- c(vals$done, file_name)
    vals$processing <- NULL    
    
    process_next_file()
    }, delay =0.1)
      
    }else if(grepl("Vietjet", file_name)){   #_________________________________________________________________________________ Vietjet
      while(sink.number() > 0) sink(NULL) 
      future({
        tryCatch({
          
          rds_path <- file.path(paste0("www/cur_data_",date_now()), paste0(file_name, ".rds"))
          
          if (!file.exists(rds_path)) {
            showNotification(paste("❌ Không tìm thấy file:", file_name), type = "error")
            next
          }
          
          sheet_data <- readRDS(rds_path)
          
          if (!is.data.frame(sheet_data)) {
            showNotification(paste("❌ File", file_name, "không phải là data.frame"), type = "error")
            next
          }
        
          
          out_file <- file.path("www/output_excel", paste0(file_name, ".xlsx"))
          
          wb <- openxlsx::createWorkbook()
          
          ty_gia <- readRDS("ty_gia.rds")
          
          
          # Các chỉ tiêu cần tính
          columns_to_sum <- c(
            "Phi_bao_hiem_goc",
            "Phi_bao_hiem_giu_lai",
            "Giam_phi_bao_hiem_goc",
            "Giam_phi_bao_hiem_giu_lai",
            "Giam_phi_bao_hiem_tai"
          )
          four_last_quarters <- as.vector(ty_gia[(nrow(ty_gia) -3) :nrow(ty_gia),1])
          
          result_data <- expand.grid(
            Quy = four_last_quarters,
            stringsAsFactors = FALSE
          )
          
          # Thêm các cột công thức, khởi tạo là NA_character_
          for (col in columns_to_sum) {
            result_data[[col]] <- NA_character_
          }
          
          # Gán tên cột tương ứng
          colnames(result_data) <- c( "Quy", columns_to_sum)
          

            showNotification(
              paste0("📂 Đang xử lý file: ", file_name),
              type = "message",
              duration = 3
            )
            
           
            
            
            cols_to_add <- c(
              "Quy_phat_sinh_doanh_thu", "Quy_Nam", "Phi_bao_hiem_giu_lai"
            )
            
            for (col in cols_to_add) {
              if (!col %in% colnames(sheet_data)) {
                sheet_data[[col]] <- NA
              }
            }
            
            
            # Tên sheet
            sheet_name <- "Vietjet"
            addWorksheet(wb, sheet_name)
            
         
            addWorksheet(wb, "Result")
            # Ghi công thức sử dụng wb_formula()
            n <- nrow(sheet_data)
            
            header <- colnames(sheet_data)
            col_pos <- function(col_name) which(header == col_name)
            openxlsx::writeData(wb, sheet = sheet_name, x = sheet_data, withFilter = TRUE) 
            #___________quý phát sinh doanh thu
            # Lấy vị trí cột (số) rồi chuyển sang chữ
            col_thang <- int2col(col_pos("Thang_phat_sinh_doanh_thu"))   # ghi letter
            col_out   <- col_pos("Quy_phat_sinh_doanh_thu")  # ghi ra số thứ tự cột

            # Tạo công thức cho từng hàng
            rows <- 2:(n + 1)
            
            formulas <- paste0(
              '=IF(', col_thang, rows, '="",0,IF(VALUE(', col_thang, rows, ')=3,2, INT((', col_thang, rows, '-1)/3)+1))'
            )
            
            # Ghi công thức vào sheet
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_out,
              startRow = 2
            )
    
            
            #___________ thời điểm ghi nhận doanh thu
            
            col_quy <- int2col(col_pos("Quy_phat_sinh_doanh_thu"))
            col_nam <- int2col(col_pos("Nam_phat_sinh_doanh_thu"))
            
            
            formulas <- paste0(
              '=CONCATENATE("Q",',
              col_quy, rows, ',"/",',
              col_nam, rows, ')'
            )
            
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_pos("Quy_Nam"),
              startRow = 2
            )
            #___________ Phi_bao_hiem_giu_lai
          
            goc <- int2col(col_pos("Phi_bao_hiem_goc"))
            tai <- int2col(col_pos("Phi_bao_hiem_tai"))
            
         
            formulas <- paste0(
              '=',  goc, rows, '-', tai, rows
            )
            
            # print(formulas)
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_pos("Phi_bao_hiem_giu_lai"),
              startRow = 2
            )
          
          start_row <- 2
          end_row <- start_row + n - 1
          
          for (col in columns_to_sum[1:2]) {
            result_data[[col]] <- mapply(function( quy1) {
              col_range <- paste0( "Vietjet!", int2col(col_pos(col)), start_row, ":", int2col(col_pos(col)), end_row)
              
              quy_range <- paste0( "Vietjet!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
              
              
              formula_text <- paste0('SUMIFS(', col_range, ',', quy_range,',"', quy1, '")')
              return(formula_text)
            },
            result_data$Quy
            )
          }
          
          col = "Giam_phi_bao_hiem_goc"
          result_data[[col]] <- mapply(function(sheet1, quy1) {
            col_range <- paste0(sheet1, "!", int2col(col_pos("Phi_bao_hiem_goc")), start_row, ":", int2col(col_pos("Phi_bao_hiem_goc")), end_row)
            hieu_luc_range <- paste0(sheet1, "!", int2col(col_pos("hieu_luc")), start_row, ":", int2col(col_pos("hieu_luc")), end_row)
            quy_range <- paste0(sheet1, "!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
            formula_text <- paste0(
              'SUMIFS(',
              col_range, ',',
              hieu_luc_range, ',"het",',
              quy_range, ',"', quy1, '")'
            )
            
            
            return(formula_text)
          },
          "Vietjet",
          result_data$Quy
          )
       
          col = "Giam_phi_bao_hiem_giu_lai"
          result_data[[col]] <- mapply(function(sheet1, quy1) {
            col_range <- paste0(sheet1, "!", int2col(col_pos("Phi_bao_hiem_giu_lai")), start_row, ":", int2col(col_pos("Phi_bao_hiem_giu_lai")), end_row)
  
            hieu_luc_range <- paste0(sheet1, "!", int2col(col_pos("hieu_luc")), start_row, ":", int2col(col_pos("hieu_luc")), end_row)
            quy_range <- paste0(sheet1, "!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
            formula_text <- paste0(
              'SUMIFS(',
              col_range, ',',
              hieu_luc_range, ',"het",',
              quy_range, ',"', quy1, '")'
            )
            
            return(formula_text)
          },
          "Vietjet",
          result_data$Quy
          )
    
          
          col <- "Giam_phi_bao_hiem_tai"
          
          goc_col <- "D"
          giu_col <- "E"
          start_row <- 3
          end_row <- start_row + nrow(result_data) - 1
          
          
          # Gán công thức từng dòng
          result_data[[col]] <- mapply(function(row) {
            paste0(goc_col, row, " - ", giu_col, row)
          }, row = start_row:end_row)
          
       
          
          
          writeData(wb, sheet = "Result", x = "SUBTOTAL", startCol = 1, startRow = 1)
          for (j in seq_along(columns_to_sum)) {
            col_index <- j + 1  # bắt đầu từ cột 3
            
            # Tính số dòng dữ liệu
            num_rows <- nrow(result_data)
            
            # Tạo công thức SUBTOTAL, ví dụ =SUBTOTAL(9, C3:C{num_rows + 2})
            col_letter <- int2col(col_index)
            subtotal_formula <- paste0("=SUBTOTAL(9,", col_letter, "3:", col_letter, num_rows + 2, ")")
            
            writeFormula(
              wb, sheet = "Result",
              x = subtotal_formula,
              startCol = col_index,
              startRow = 1
            )
          }
          
          # 
          # Ghi header theo hàng ngang
          writeData(wb, sheet = "Result", x = t(as.data.frame(names(result_data))), startCol = 1, startRow = 2, colNames = FALSE, withFilter = TRUE)
          
          
          # Ghi dữ liệu từng dòng
          for (i in seq_len(nrow(result_data))) {
            writeData(wb, sheet = "Result", x = result_data$Quy[i], startCol = 1, startRow = i + 2)
            
            # Ghi công thức cho từng cột cần tính
            for (j in seq_along(columns_to_sum)) {
              formula_text <- result_data[[columns_to_sum[j]]][i]
              writeFormula(
                wb, sheet = "Result",
                x = paste0("=", formula_text),
                startCol = j + 1,  # cột 1 là Ky_phi, 2 là Quy, nên bắt đầu từ cột 3
                startRow = i + 2
              )
            }
          }
     
          
          saveWorkbook(wb, out_file, overwrite = TRUE)
          # 🔄 Cập nhật danh sách file sau khi lưu
          files_rv(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
          showNotification(paste("✅ Xuất Excel cho", file_name, "hoàn tất!"))
          #___________________hết long term
          
          
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Lỗi khi xử lý", file_name),
            type = "error"
          )
          return(NULL)
        })
        
        vals$done <- c(vals$done, file_name)
        vals$processing <- NULL    
        
        process_next_file()
      }, delay =0.1)
      
    } else if(grepl("TTTBVV", file_name)){   #_________________________________________________________________________________TTTBVV
     #source("TTTBVV.R", local = TRUE)
      while(sink.number() > 0) sink(NULL) 
      future({
        tryCatch({
          
          rds_path <- file.path(paste0("www/cur_data_",date_now()), paste0(file_name, ".rds"))
          
          if (!file.exists(rds_path)) {
            showNotification(paste("❌ Không tìm thấy file:", file_name), type = "error")
            next
          }
          
          df <- readRDS(rds_path)
          if (!is.data.frame(df)) {
            showNotification(paste("❌ File", file_name, "không phải là data.frame"), type = "error")
            next
          }
          
          if (ncol(df) < 24) {
            showNotification(paste("❗ File", file_name, "không đủ 24 cột cố định"), type = "error")
            next
          }
          
          out_file <- file.path("www/output_excel", paste0(file_name, ".xlsx"))
          
          wb <- openxlsx::createWorkbook()
          
          ty_gia <- readRDS("ty_gia.rds")
          vlookup_range <- paste0("Tygia!$A$1:$C$", nrow(ty_gia)+1)
          
          fallback_cell <- paste0("Tygia!$A$", nrow(ty_gia)+1 )
          
          addWorksheet(wb, "Tygia")
          writeDataTable(
            wb,
            sheet = "Tygia",
            x = ty_gia,
            tableName = "ty_gia"
          )
          
          # Sau khi đã thêm các sheet Ky_phi, tạo sheet "Result"
          addWorksheet(wb, "Result")
          
          
          
          # Lấy danh sách kỳ phí thực sự có mặt
          ky_available <- unique(gsub("Ky_phi_([0-9]+)_.*", "\\1",
                                      grep("^Ky_phi_\\d+_", colnames(df), value = TRUE)))
          ky_available <- sort(as.integer(ky_available))
          
          if (length(ky_available) == 0) {
            showNotification(paste("⚠️ File", file_name, "không có kỳ phí nào hợp lệ."), type = "warning")
            next
          }
          
          
          sheet <- paste0("Ky_phi", ky_available)
          
          # Các chỉ tiêu cần tính
          columns_to_sum <- c(
            "Phi_bao_hiem_goc",
            "Phi_bao_hiem_giu_lai",
            "Du_phong_bao_hiem_goc",
            "Du_phong_bao_hiem_giu_lai",
            "Du_phong_bao_hiem_tai"
            
          )
          four_last_quarters <- as.vector(ty_gia[(nrow(ty_gia) -3) :nrow(ty_gia),1])
          
          result_data <- expand.grid(
            Ky_phi =sheet,    # c("Tổng",sheet),
            Quy = four_last_quarters,
            stringsAsFactors = FALSE
          )
          
          # Thêm các cột công thức, khởi tạo là NA_character_
          for (col in columns_to_sum) {
            result_data[[col]] <- NA_character_
          }
          
          # Gán tên cột tương ứng
          colnames(result_data) <- c("Ky_phi", "Quy", columns_to_sum)
          
          
          for (ky in ky_available) {
            showNotification(
              paste0("📂 Đang xử lý file: ", file_name, " | Kỳ phí: ", ky),
              type = "message",
              duration = 3
            )
            
            fee_cols <- generate_fee_cols(ky)
            fee_cols_exist <- fee_cols[fee_cols %in% colnames(df)]
            base_cols <- colnames(df)[1:min(24, ncol(df))]
            sheet_data <- df[, c(base_cols, fee_cols_exist), drop = FALSE]
            
            
            cols_to_add <- c(
              "Thoi_diem_tinh_DPNV_Ngay", "Thoi_diem_tinh_DPNV_Thang","Thoi_diem_tinh_DPNV_Nam",
              "Quy_ghi_doanh_thu", "Nam_ghi_doanh_thu", "Quy_Nam","Phi_bao_hiem_goc","Ty_le_giu_lai_BHBV",
              "Phi_bao_hiem_giu_lai","Tong_so_ngay","So_ngay_da_qua","So_ngay_con_lai","Du_phong_bao_hiem_goc","Du_phong_bao_hiem_giu_lai"
            )
            
            for (col in cols_to_add) {
              if (!col %in% colnames(sheet_data)) {
                sheet_data[[col]] <- NA
              }
            }
            
            # Ngày tháng
            sheet_data$Thoi_diem_tinh_DPNV_Ngay <- dpnv_ngay
            sheet_data$Thoi_diem_tinh_DPNV_Thang <- dpnv_thang
            sheet_data$Thoi_diem_tinh_DPNV_Nam <- dpnv_nam
            
            # Tên sheet
            sheet_name <- paste0("Ky_phi", ky)
            addWorksheet(wb, sheet_name)
            # Ghi công thức sử dụng wb_formula()
            n <- nrow(sheet_data)
            ghi_thang <- paste0("Ky_phi_", ky, "_Ghi_Thang")
            ghi_nam   <- paste0("Ky_phi_", ky, "_Ghi_Nam")
            den_ngay  <- paste0("Ky_phi_", ky, "_Den_Ngay")
            den_thang <- paste0("Ky_phi_", ky, "_Den_Thang")
            den_nam   <- paste0("Ky_phi_", ky, "_Den_Nam")
            
            header <- colnames(sheet_data)
            col_pos <- function(col_name) which(header == col_name)
            
            openxlsx::writeData(wb, sheet = sheet_name, x = sheet_data, withFilter = TRUE)
            #addTable(wb, sheet_name, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium2", tableName = sheet_name)
            
            #___________quý ghi nhận doanh thu
            # Lấy vị trí cột (số) rồi chuyển sang chữ
            col_thang <- int2col(col_pos(ghi_thang))   # ghi letter
            col_nam   <- int2col(col_pos(ghi_nam))
            col_out   <- col_pos("Quy_ghi_doanh_thu")  # ghi ra số thứ tự cột
            
            # Tạo công thức cho từng hàng
            rows <- 2:(n + 1)
            
            formulas <- paste0(
              '=IF(', col_thang, rows, '="",0,INT((', col_thang, rows, '-1)/3)+1)'
            )
            #print(formulas)
            # Ghi công thức vào sheet
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_out,
              startRow = 2
            )
            
            #___________năm ghi nhận doanh thu
            col_quy      <- int2col(col_pos("Nam_ghi_doanh_thu"))
            col_nam      <- int2col(col_pos(ghi_nam))
            
            # Tạo công thức theo từng dòng
            formulas_quy_2024 <- paste0(
              '=VALUE(', col_nam, rows, ')'  )
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas_quy_2024,
              startCol = col_quy,
              startRow = 2
            )
            
            #___________ thời điểm ghi nhận doanh thu
            
            col_quy <- int2col(col_pos("Quy_ghi_doanh_thu"))
            col_nam <- int2col(col_pos("Nam_ghi_doanh_thu"))
            
            
            formulas <- paste0(
              '=CONCATENATE("Q",',
              col_quy, rows, ',"/",',
              col_nam, rows, ')'
            )
            
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_pos("Quy_Nam"),
              startRow = 2
            )
            
            #_______________________ Phi_bao_hiem_goc
            formulas <- paste0(
              "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_VND"))), rows, ") + ",
              "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_USD"))), rows, ") * IFERROR(VALUE(VLOOKUP(",
              int2col(col_pos("Quy_Nam")), rows, ", ",
              vlookup_range, ", 2, 0)), VALUE(VLOOKUP(", fallback_cell, ", ",
              vlookup_range, ", 2, 0))) + ",
              "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_EUR"))), rows, ") * IFERROR(VALUE(VLOOKUP(",
              int2col(col_pos("Quy_Nam")), rows, ", ",
              vlookup_range, ", 3, 0)), VALUE(VLOOKUP(", fallback_cell, ", ",
              vlookup_range, ", 3, 0)))"
            )
            #print(formulas)
            
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_pos("Phi_bao_hiem_goc"),
              startRow = 2
            )
            #_______________________ Ty_le_giu_lai_BHBV
            col_name <- if ("Ty_le_giu_lai_cua_BHBV" %in% colnames(sheet_data)) {
              "Ty_le_giu_lai_cua_BHBV"
            } else if ("Ty_le_giu_lai_cua_BHBV_checked" %in% colnames(sheet_data)) {
              "Ty_le_giu_lai_cua_BHBV_checked"
            } else {
              stop("Không tìm thấy cột giữ lại")
            }
            
            hi <- int2col(col_pos(col_name))
            
            # formulas <- paste0(
            #   'VALUE(IF(',
            #   hi, rows, '="", "100%", ',
            #   'IF(VALUE(SUBSTITUTE(', hi, rows, ',".",","))>1,',
            #   'VALUE(SUBSTITUTE(', hi, rows, ',".",","))/100,',
            #   'VALUE(SUBSTITUTE(', hi, rows, ',".",","))',
            #   ')',
            #   '))'
            # )
            
            formulas <- paste0(
              'IF(OR(',
              hi, rows, '="",',hi, rows, '=0), 1, IF(VALUE(', hi, rows, ') > 1,',
              'VALUE(', hi, rows, ') / 100, VALUE(', hi, rows, ')))'
            )
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_pos("Ty_le_giu_lai_BHBV"),
              startRow = 2
            )
            
            #_______________________ Phi_bao_hiem_giu_lai
            formulas <- paste0(
              "IF(",
              int2col(col_pos("Phi_bao_hiem_goc")), rows, '="", 0, VALUE(',
              int2col(col_pos("Phi_bao_hiem_goc")), rows, ") * ",
              int2col(col_pos("Ty_le_giu_lai_BHBV")), rows, ")"
            )
            
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas,
              startCol = col_pos("Phi_bao_hiem_giu_lai"),
              startRow = 2
            )
            #_______________________ Tong_so_ngay
            
            tu_ngay   <- int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Ngay")))
            tu_thang  <- int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Thang")))
            tu_nam    <- int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Nam")))
            
            den_ngay  <- int2col(col_pos(paste0("Ky_phi_",ky,"_Den_Ngay")))
            den_thang <- int2col(col_pos(paste0("Ky_phi_",ky,"_Den_Thang")))
            den_nam   <- int2col(col_pos(paste0("Ky_phi_",ky,"_Den_Nam")))
            
            formulas_check02 <- paste0(
              'MAX(0,DATE(', den_nam, rows, ',', den_thang, rows, ',', den_ngay, rows, ') - ',
              'DATE(', tu_nam, rows, ',', tu_thang, rows, ',', tu_ngay, rows, ')+1 ) '
            )
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas_check02,
              startCol = col_pos("Tong_so_ngay"),
              startRow = 2
            )
            
            #_______________________ So_ngay_da_qua
            den_ngay   <- int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay"))
            den_thang  <- int2col(col_pos("Thoi_diem_tinh_DPNV_Thang"))
            den_nam    <- int2col(col_pos("Thoi_diem_tinh_DPNV_Nam"))
            
            tu_ngay   <- int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Ngay")))
            tu_thang  <- int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Thang")))
            tu_nam    <- int2col(col_pos(paste0("Ky_phi_",ky,"_Tu_Nam")))
            
            formulas_check02 <- paste0(
              'DATE(', den_nam, rows, ',', den_thang, rows, ',', den_ngay, rows, ') - ',
              'DATE(', tu_nam, rows, ',', tu_thang, rows, ',', tu_ngay, rows, ')+1  '
            )
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas_check02,
              startCol = col_pos("So_ngay_da_qua"),
              startRow = 2
            )
            
            
            #_______________________ So_ngay_con_lai
            a   <- int2col(col_pos("Tong_so_ngay"))
            b  <- int2col(col_pos("So_ngay_da_qua"))
            
            
            formulas_check02 <- paste0(
              'IF( ',b, rows,'<0,', a,rows,',IF(',b,rows,'>',a,rows, ',0,', a,rows,'-',b,rows,'))'
            )
      
            
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas_check02,
              startCol = col_pos("So_ngay_con_lai"),
              startRow = 2
            )
            
            #_______________________ Du_phong_bao_hiem_goc
            a   <- int2col(col_pos("Tong_so_ngay"))
            b  <- int2col(col_pos("So_ngay_con_lai"))
            c  <- int2col(col_pos("Phi_bao_hiem_goc"))
            
            formulas_check02 <- paste0(
              b, rows, '/', a, rows, '*', c, rows
            )
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas_check02,
              startCol = col_pos("Du_phong_bao_hiem_goc"),
              startRow = 2
            )
           
            
            #_______________________ Du_phong_bao_hiem_giu_lai
            a   <- int2col(col_pos("Tong_so_ngay"))
            b  <- int2col(col_pos("So_ngay_con_lai"))
            c  <- int2col(col_pos("Phi_bao_hiem_giu_lai"))
            
            formulas_check02 <- paste0(
              b, rows, '/', a, rows, '*', c, rows
            )
            writeFormula(
              wb,
              sheet = sheet_name,
              x = formulas_check02,
              startCol = col_pos("Du_phong_bao_hiem_giu_lai"),
              startRow = 2
            )
            
          }
         
          start_row <- 2
          end_row <- start_row + n - 1
          
          for (col in columns_to_sum[1:4]) {
            result_data[[col]] <- mapply(function(sheet1, quy1) {
              col_range <- paste0(sheet1, "!", int2col(col_pos(col)), start_row, ":", int2col(col_pos(col)), end_row)
              
              quy_range <- paste0(sheet1, "!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
              
              
              formula_text <- paste0('SUMIFS(', col_range, ',', quy_range,',"', quy1, '")')
              return(formula_text)
            },
            result_data$Ky_phi,
            result_data$Quy
            )
          }
          
          
          
          col <- "Du_phong_bao_hiem_tai"
          
          goc_col <- "E"
          giu_col <- "F"
          start_row <- 3
          end_row <- start_row + nrow(result_data) - 1
          
          
          # Gán công thức từng dòng
          result_data[[col]] <- mapply(function(row) {
            paste0(goc_col, row, " - ", giu_col, row)
          }, row = start_row:end_row)
          
          
          
          
          writeData(wb, sheet = "Result", x = "SUBTOTAL", startCol = 1, startRow = 1)
          for (j in seq_along(columns_to_sum)) {
            col_index <- j + 2  # bắt đầu từ cột 3
            
            # Tính số dòng dữ liệu
            num_rows <- nrow(result_data)
            
            # Tạo công thức SUBTOTAL, ví dụ =SUBTOTAL(9, C3:C{num_rows + 2})
            col_letter <- int2col(col_index)
            subtotal_formula <- paste0("=SUBTOTAL(9,", col_letter, "3:", col_letter, num_rows + 2, ")")
            
            writeFormula(
              wb, sheet = "Result",
              x = subtotal_formula,
              startCol = col_index,
              startRow = 1
            )
          }
          # 
          # Ghi header theo hàng ngang
          writeData(wb, sheet = "Result", x = t(as.data.frame(names(result_data))), startCol = 1, startRow = 2, colNames = FALSE, withFilter = TRUE)
          
          
          # Ghi dữ liệu từng dòng
          for (i in seq_len(nrow(result_data))) {
            # Ghi Ky_phi và Quy
            writeData(wb, sheet = "Result", x = result_data$Ky_phi[i], startCol = 1, startRow = i + 2)
            writeData(wb, sheet = "Result", x = result_data$Quy[i], startCol = 2, startRow = i + 2)
            
            # Ghi công thức cho từng cột cần tính
            for (j in seq_along(columns_to_sum)) {
              formula_text <- result_data[[columns_to_sum[j]]][i]
              writeFormula(
                wb, sheet = "Result",
                x = paste0("=", formula_text),
                startCol = j + 2,  # cột 1 là Ky_phi, 2 là Quy, nên bắt đầu từ cột 3
                startRow = i + 2
              )
            }
          }
          
          
          saveWorkbook(wb, out_file, overwrite = TRUE)
          # 🔄 Cập nhật danh sách file sau khi lưu
          files_rv(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
          showNotification(paste("✅ Xuất Excel cho", file_name, "hoàn tất!"))
          #___________________hết long term
          
          
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Lỗi khi xử lý", file_name),
            type = "error"
          )
          return(NULL)
        })
        
        vals$done <- c(vals$done, file_name)
        vals$processing <- NULL    
        
        process_next_file()
      }, delay =0.1)
      
    } else if(file_name %in% vals$short_files){  #_________________________________________________________________________________short_term
      while(sink.number() > 0) sink(NULL) 
      future({
    tryCatch({
      
      rds_path <- file.path(paste0("www/cur_data_",date_now()), paste0(file_name, ".rds"))
      
      if (!file.exists(rds_path)) {
        showNotification(paste("❌ Không tìm thấy file:", file_name), type = "error")
        next
      }
      
      df <- readRDS(rds_path)
      if (!is.data.frame(df)) {
        showNotification(paste("❌ File", file_name, "không phải là data.frame"), type = "error")
        next
      }
      
      if (ncol(df) < 24) {
        showNotification(paste("❗ File", file_name, "không đủ 24 cột cố định"), type = "error")
        next
      }
      out_file <- file.path("www/output_excel", paste0(file_name, ".xlsx"))
      
      wb <- openxlsx::createWorkbook()
      
      ty_gia <- readRDS("ty_gia.rds")
      vlookup_range <- paste0("Tygia!$A$1:$C$", nrow(ty_gia)+1)
      
      fallback_cell <- paste0("Tygia!$A$", nrow(ty_gia)+1 )
      
      addWorksheet(wb, "Tygia")
      writeDataTable(
        wb,
        sheet = "Tygia",
        x = ty_gia,
        tableName = "ty_gia"
      )
      
      # Sau khi đã thêm các sheet Ky_phi, tạo sheet "Result"
      addWorksheet(wb, "Result")
      
      
      
      # Lấy danh sách kỳ phí thực sự có mặt
      ky_available <- unique(gsub("Ky_phi_([0-9]+)_.*", "\\1",
                                  grep("^Ky_phi_\\d+_", colnames(df), value = TRUE)))
      ky_available <- sort(as.integer(ky_available))
      
      if (length(ky_available) == 0) {
        showNotification(paste("⚠️ File", file_name, "không có kỳ phí nào hợp lệ."), type = "warning")
        next
      }
      
      
      sheet <- paste0("Ky_phi", ky_available)
      
      # Các chỉ tiêu cần tính
      columns_to_sum <- c(
        "Phi_bao_hiem_goc",
        "Phi_bao_hiem_giu_lai",
        "Giam_phi_bao_hiem_goc",
        "Giam_phi_bao_hiem_giu_lai",
        "Giam_phi_bao_hiem_tai"
        
      )
      four_last_quarters <- as.vector(ty_gia[(nrow(ty_gia) -3) :nrow(ty_gia),1])

      result_data <- expand.grid(
        Ky_phi =sheet,    # c("Tổng",sheet),
        Quy = four_last_quarters,
        stringsAsFactors = FALSE
      )

      # Thêm các cột công thức, khởi tạo là NA_character_
      for (col in columns_to_sum) {
        result_data[[col]] <- NA_character_
      }

      # Gán tên cột tương ứng
      colnames(result_data) <- c("Ky_phi", "Quy", columns_to_sum)
      
      
      for (ky in ky_available) {
        showNotification(
          paste0("📂 Đang xử lý file: ", file_name, " | Kỳ phí: ", ky),
          type = "message",
          duration = 3
        )
        
        fee_cols <- generate_fee_cols(ky)
        fee_cols_exist <- fee_cols[fee_cols %in% colnames(df)]
        base_cols <- colnames(df)[1:min(24, ncol(df))]
        sheet_data <- df[, c(base_cols, fee_cols_exist), drop = FALSE]
        
        
        cols_to_add <- c(
          "Thoi_diem_tinh_DPNV_Ngay", "Thoi_diem_tinh_DPNV_Thang","Thoi_diem_tinh_DPNV_Nam",
          "Quy_ghi_doanh_thu", "Nam_ghi_doanh_thu", "Quy_Nam","Phi_bao_hiem_goc","Ty_le_giu_lai_BHBV",
          "Phi_bao_hiem_giu_lai","Dem_ngay","Het_hieu_luc"
        )
        
        for (col in cols_to_add) {
          if (!col %in% colnames(sheet_data)) {
            sheet_data[[col]] <- NA
          }
        }
        
        # Ngày tháng
        sheet_data$Thoi_diem_tinh_DPNV_Ngay <- dpnv_ngay
        sheet_data$Thoi_diem_tinh_DPNV_Thang <- dpnv_thang
        sheet_data$Thoi_diem_tinh_DPNV_Nam <- dpnv_nam
        
        # Tên sheet
        sheet_name <- paste0("Ky_phi", ky)
        addWorksheet(wb, sheet_name)
        # Ghi công thức sử dụng wb_formula()
        n <- nrow(sheet_data)
        ghi_thang <- paste0("Ky_phi_", ky, "_Ghi_Thang")
        ghi_nam   <- paste0("Ky_phi_", ky, "_Ghi_Nam")
        den_ngay  <- paste0("Ky_phi_", ky, "_Den_Ngay")
        den_thang <- paste0("Ky_phi_", ky, "_Den_Thang")
        den_nam   <- paste0("Ky_phi_", ky, "_Den_Nam")
        
        header <- colnames(sheet_data)
        col_pos <- function(col_name) which(header == col_name)
        
        openxlsx::writeData(wb, sheet = sheet_name, x = sheet_data, withFilter = TRUE)
        #addTable(wb, sheet_name, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium2", tableName = sheet_name)
        
        #___________quý ghi nhận doanh thu
        # Lấy vị trí cột (số) rồi chuyển sang chữ
        col_thang <- int2col(col_pos(ghi_thang))   # ghi letter
        col_nam   <- int2col(col_pos(ghi_nam))
        col_out   <- col_pos("Quy_ghi_doanh_thu")  # ghi ra số thứ tự cột
        
        # Tạo công thức cho từng hàng
        rows <- 2:(n + 1)
        
        formulas <- paste0(
          '=IF(', col_thang, rows, '="",0,INT((', col_thang, rows, '-1)/3)+1)'
        )

        # Ghi công thức vào sheet
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas,
          startCol = col_out,
          startRow = 2
        )

        #___________năm ghi nhận doanh thu
        col_quy      <- int2col(col_pos("Nam_ghi_doanh_thu"))
        col_nam      <- int2col(col_pos(ghi_nam))

        # Tạo công thức theo từng dòng
        formulas_quy_2024 <- paste0(
          '=VALUE(', col_nam, rows, ')'  )
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas_quy_2024,
          startCol = col_quy,
          startRow = 2
        )
        
        #___________ thời điểm ghi nhận doanh thu
        
        col_quy <- int2col(col_pos("Quy_ghi_doanh_thu"))
        col_nam <- int2col(col_pos("Nam_ghi_doanh_thu"))
        
        
        formulas <- paste0(
          '=CONCATENATE("Q",',
          col_quy, rows, ',"/",',
          col_nam, rows, ')'
        )
        
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas,
          startCol = col_pos("Quy_Nam"),
          startRow = 2
        )
        
        #_______________________ Phi_bao_hiem_goc
        formulas <- paste0(
          "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_VND"))), rows, ") + ",
          "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_USD"))), rows, ") * IFERROR(VALUE(VLOOKUP(",
          int2col(col_pos("Quy_Nam")), rows, ", ",
          vlookup_range, ", 2, 0)), VALUE(VLOOKUP(", fallback_cell, ", ",
          vlookup_range, ", 2, 0))) + ",
          "VALUE(", int2col(col_pos(paste0("Ky_phi_", ky, "_So_tien_EUR"))), rows, ") * IFERROR(VALUE(VLOOKUP(",
          int2col(col_pos("Quy_Nam")), rows, ", ",
          vlookup_range, ", 3, 0)), VALUE(VLOOKUP(", fallback_cell, ", ",
          vlookup_range, ", 3, 0)))"
        )
        #print(formulas)
        
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas,
          startCol = col_pos("Phi_bao_hiem_goc"),
          startRow = 2
        )
        #_______________________ Ty_le_giu_lai_BHBV
        col_name <- if ("Ty_le_giu_lai_cua_BHBV" %in% colnames(sheet_data)) {
          "Ty_le_giu_lai_cua_BHBV"
        } else if ("Ty_le_giu_lai_cua_BHBV_checked" %in% colnames(sheet_data)) {
          "Ty_le_giu_lai_cua_BHBV_checked"
        } else {
          stop("Không tìm thấy cột giữ lại")
        }
        
        hi <- int2col(col_pos(col_name))
        
        # formulas <- paste0(
        #   'VALUE(IF(',
        #   hi, rows, '="", "100%", ',
        #   'IF(VALUE(SUBSTITUTE(', hi, rows, ',".",","))>1,',
        #   'VALUE(SUBSTITUTE(', hi, rows, ',".",","))/100,',
        #   'VALUE(SUBSTITUTE(', hi, rows, ',".",","))',
        #   ')',
        #   '))'
        # )
        formulas <- paste0(
          'IF(OR(',
          hi, rows, '="",',hi, rows, '=0), 1, IF(VALUE(', hi, rows, ') > 1,',
          'VALUE(', hi, rows, ') / 100, VALUE(', hi, rows, ')))'
        )
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas,
          startCol = col_pos("Ty_le_giu_lai_BHBV"),
          startRow = 2
        )
        
        #_______________________ Phi_bao_hiem_giu_lai
        formulas <- paste0(
          "IF(",
          int2col(col_pos("Phi_bao_hiem_goc")), rows, '="", 0, VALUE(',
          int2col(col_pos("Phi_bao_hiem_goc")), rows, ") * ",
          int2col(col_pos("Ty_le_giu_lai_BHBV")), rows, ")"
        )
        
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas,
          startCol = col_pos("Phi_bao_hiem_giu_lai"),
          startRow = 2
        )
        #_______________________ Dem ngay
        
        tu_ngay   <- int2col(col_pos("Thoi_han_bao_hiem_Tu_Ngay"))
        tu_thang  <- int2col(col_pos("Thoi_han_bao_hiem_Tu_Thang"))
        tu_nam    <- int2col(col_pos("Thoi_han_bao_hiem_Tu_Nam"))
        
        den_ngay  <- int2col(col_pos("Thoi_han_bao_hiem_Den_Ngay"))
        den_thang <- int2col(col_pos("Thoi_han_bao_hiem_Den_Thang"))
        den_nam   <- int2col(col_pos("Thoi_han_bao_hiem_Den_Nam"))
        
        formulas_check02 <- paste0(
          '=IF(OR(',
          tu_ngay, rows, '="",',
          tu_thang, rows, '="",',
          tu_nam, rows, '="",',
          den_ngay, rows, '="",',
          den_thang, rows, '="",',
          den_nam, rows, '=""),0,',
          'IF(DATE(', den_nam, rows, ',', den_thang, rows, ',', den_ngay, rows, ') - ',
          'DATE(', tu_nam, rows, ',', tu_thang, rows, ',', tu_ngay, rows, ') < 365,1,0))'
        )
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas_check02,
          startCol = col_pos("Dem_ngay"),
          startRow = 2
        )
        
        #_______________________ Het_hieu_luc
        tu_ngay   <- int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay"))
        tu_thang  <- int2col(col_pos("Thoi_diem_tinh_DPNV_Thang"))
        tu_nam    <- int2col(col_pos("Thoi_diem_tinh_DPNV_Nam"))
        
        den_ngay  <- int2col(col_pos("Thoi_han_bao_hiem_Den_Ngay"))
        den_thang <- int2col(col_pos("Thoi_han_bao_hiem_Den_Thang"))
        den_nam   <- int2col(col_pos("Thoi_han_bao_hiem_Den_Nam"))

        formulas_check02 <- paste0(
          '=IF(OR(',
          tu_ngay, rows, '="",',
          tu_thang, rows, '="",',
          tu_nam, rows, '="",',
          den_ngay, rows, '="",',
          den_thang, rows, '="",',
          den_nam, rows, '=""),0,',
          'IF(DATE(', tu_nam, rows, ',', tu_thang, rows, ',', tu_ngay, rows, ') - ',
          'DATE(', den_nam, rows, ',', den_thang, rows, ',', den_ngay, rows, ') >=0,1))'
        )
        writeFormula(
          wb,
          sheet = sheet_name,
          x = formulas_check02,
          startCol = col_pos("Het_hieu_luc"),
          startRow = 2
        )
        
        
      }
      
      start_row <- 2
      end_row <- start_row + n - 1

      for (col in columns_to_sum[1:2]) {
        result_data[[col]] <- mapply(function(sheet1, quy1) {
          col_range <- paste0(sheet1, "!", int2col(col_pos(col)), start_row, ":", int2col(col_pos(col)), end_row)

          dem_ngay_range <- paste0(sheet1, "!", int2col(col_pos("Dem_ngay")), start_row, ":", int2col(col_pos("Dem_ngay")), end_row)
          hieu_luc_range <- paste0(sheet1, "!", int2col(col_pos("Het_hieu_luc")), start_row, ":", int2col(col_pos("Het_hieu_luc")), end_row)
          quy_range <- paste0(sheet1, "!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
          # if (sheet1 == "Tổng"){
          #   formula_text <- paste0('SUMIFS(', col_range, ',', criteria_range, ',"1",', quy_range, ',"', quy1, '")')
          # }else{
          #   formula_text <- paste0('SUMIFS(', col_range, ',', quy_range,',"', quy1, '")')
          # }
          
          formula_text <- paste0('SUMIFS(', col_range, ',', quy_range,',"', quy1, '")')
          return(formula_text)
        },
        result_data$Ky_phi,
        result_data$Quy
        )
      }
      
      col = "Giam_phi_bao_hiem_goc"
      result_data[[col]] <- mapply(function(sheet1, quy1) {
        col_range <- paste0(sheet1, "!", int2col(col_pos("Phi_bao_hiem_goc")), start_row, ":", int2col(col_pos("Phi_bao_hiem_goc")), end_row)
        
        dem_ngay_range <- paste0(sheet1, "!", int2col(col_pos("Dem_ngay")), start_row, ":", int2col(col_pos("Dem_ngay")), end_row)
        hieu_luc_range <- paste0(sheet1, "!", int2col(col_pos("Het_hieu_luc")), start_row, ":", int2col(col_pos("Het_hieu_luc")), end_row)
        quy_range <- paste0(sheet1, "!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
        formula_text <- paste0(
          'SUMIFS(',
          col_range, ',',
          dem_ngay_range, ',"1",',
          hieu_luc_range, ',"1",',
          quy_range, ',"', quy1, '")'
        )
        

        return(formula_text)
      },
      result_data$Ky_phi,
      result_data$Quy
      )
      
      col = "Giam_phi_bao_hiem_giu_lai"
      result_data[[col]] <- mapply(function(sheet1, quy1) {
        col_range <- paste0(sheet1, "!", int2col(col_pos("Phi_bao_hiem_giu_lai")), start_row, ":", int2col(col_pos("Phi_bao_hiem_giu_lai")), end_row)
        
        dem_ngay_range <- paste0(sheet1, "!", int2col(col_pos("Dem_ngay")), start_row, ":", int2col(col_pos("Dem_ngay")), end_row)
        hieu_luc_range <- paste0(sheet1, "!", int2col(col_pos("Het_hieu_luc")), start_row, ":", int2col(col_pos("Het_hieu_luc")), end_row)
        quy_range <- paste0(sheet1, "!", int2col(col_pos("Quy_Nam")), start_row, ":", int2col(col_pos("Quy_Nam")), end_row)
        formula_text <- paste0(
          'SUMIFS(',
          col_range, ',',
          dem_ngay_range, ',"1",',
          hieu_luc_range, ',"1",',
          quy_range, ',"', quy1, '")'
        )
        
        
        return(formula_text)
      },
      result_data$Ky_phi,
      result_data$Quy
      )
      
      col <- "Giam_phi_bao_hiem_tai"
      
      goc_col <- "E"
      giu_col <- "F"
      start_row <- 3
      end_row <- start_row + nrow(result_data) - 1
      
      
      # Gán công thức từng dòng
      result_data[[col]] <- mapply(function(row) {
        paste0(goc_col, row, " - ", giu_col, row)
      }, row = start_row:end_row)
      
  
      
      
      writeData(wb, sheet = "Result", x = "SUBTOTAL", startCol = 1, startRow = 1)
      for (j in seq_along(columns_to_sum)) {
        col_index <- j + 2  # bắt đầu từ cột 3

        # Tính số dòng dữ liệu
        num_rows <- nrow(result_data)

        # Tạo công thức SUBTOTAL, ví dụ =SUBTOTAL(9, C3:C{num_rows + 2})
        col_letter <- int2col(col_index)
        subtotal_formula <- paste0("=SUBTOTAL(9,", col_letter, "3:", col_letter, num_rows + 2, ")")

        writeFormula(
          wb, sheet = "Result",
          x = subtotal_formula,
          startCol = col_index,
          startRow = 1
        )
      }
      # 
      # Ghi header theo hàng ngang
      writeData(wb, sheet = "Result", x = t(as.data.frame(names(result_data))), startCol = 1, startRow = 2, colNames = FALSE, withFilter = TRUE)


      # Ghi dữ liệu từng dòng
      for (i in seq_len(nrow(result_data))) {
        # Ghi Ky_phi và Quy
        writeData(wb, sheet = "Result", x = result_data$Ky_phi[i], startCol = 1, startRow = i + 2)
        writeData(wb, sheet = "Result", x = result_data$Quy[i], startCol = 2, startRow = i + 2)

        # Ghi công thức cho từng cột cần tính
        for (j in seq_along(columns_to_sum)) {
          formula_text <- result_data[[columns_to_sum[j]]][i]
          writeFormula(
            wb, sheet = "Result",
            x = paste0("=", formula_text),
            startCol = j + 2,  # cột 1 là Ky_phi, 2 là Quy, nên bắt đầu từ cột 3
            startRow = i + 2
          )
        }
      }
      
      
      saveWorkbook(wb, out_file, overwrite = TRUE)
      # 🔄 Cập nhật danh sách file sau khi lưu
      files_rv(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
      showNotification(paste("✅ Xuất Excel cho", file_name, "hoàn tất!"))
      #___________________hết long term
      
      
    }, error = function(e) {
      shiny::showNotification(
        paste("❌ Lỗi khi xử lý", file_name,": ",e),
        type = "error"
      )
      return(NULL)
    })
    
    vals$done <- c(vals$done, file_name)
    vals$processing <- NULL    
    
    process_next_file()
  }, delay =0.1)
  
    }


}
}





