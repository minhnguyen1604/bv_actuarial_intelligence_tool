#_____________________ FUNCTION
# Hàm tạo tên cột cho kỳ phí

# Reactive để chứa dữ liệu ghép hoặc excel gốc
merged_group_code2 <- reactiveVal(NULL)


remove_accents2 <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII")
}

# Mapping các nhóm chính
group_mapping2 <- list(
  XCG = c("xcg"),
  PA_NNTX = c("ntx")
)

# Mapping term (LT/ST)
term_mapping2 <- list(
  LT = c("lt", "dai ky","dai ki", "longterm","dk","365"),
  ST = c("st", "ngan ky", "shortterm","nk","364")
)

# Hàm kiểm tra file thuộc nhóm nào
get_group_code2 <- function(file_name, sheet_name) {
  file_name <- tolower(remove_accents2(file_name))
  sheet_name <- tolower(remove_accents2(sheet_name))
  # print(paste("Cleaned file name:", file_name))
  # print(paste("Cleaned sheet name:", sheet_name))
  
  group <- NA
  term <- NA
  
  for (g in names(group_mapping2)) {
    if (any(sapply(group_mapping2[[g]], function(keyword) grepl(keyword, file_name)))) {
      group <- g
      break
    }
  }
  for (t in names(term_mapping2)) {
        if (any(sapply(term_mapping2[[t]], function(keyword) grepl(keyword, sheet_name)))) {
          term <- t
          break
        }
      }
      
  if (is.na(term)){
        for (t in names(term_mapping2)) {
          if (any(sapply(term_mapping2[[t]], function(keyword) grepl(keyword, file_name)))) {
            term <- t
            break
          }
        }
      }
      
      if (!is.na(group) && !is.na(term)) {
        return(paste0(group, "_", term))
      } else {
        return(NA)
      }
}




#________________________________
# Reactive lấy tên sheet
sheet_names2 <- reactive({
  req(input$file2)
  getSheetNames(input$file2$datapath)
})

# UI chọn sheet
output$sheet_selector2 <- renderUI({
  req(sheet_names2())
  selectInput("sheet2", "Chọn sheet:", choices = sheet_names2())
})

# Reactive đọc Excel
excel_data2 <- reactive({
  req(input$file2, input$sheet2)
  read.xlsx(input$file2$datapath, sheet = input$sheet2)
})

checked_df2 <- reactiveVal()

observeEvent(input$check2, {
  
  withProgress(message = "Đang kiểm tra dữ liệu...", value = 0, {
    
    incProgress(0.1, detail = "Đang đọc dữ liệu Excel...")
    req(excel_data2())
    req( input$file2, input$sheet2)
    
    clean_name <- tolower(remove_accents2(tools::file_path_sans_ext(basename(input$file2$name))))
    clean_sheet <- tolower(remove_accents2(tools::file_path_sans_ext(basename(input$sheet2))))
    code <- get_group_code2(clean_name, clean_sheet)
    merged_group_code2(code)
    
    df_raw <- excel_data2()
    df = df_raw
    
    
    
    # Tạo thêm tên cột cho kỳ phí 7 đến 10
    extra_cols <- unlist(lapply(7:10, generate_fee_cols))
    base_cols <- colnames(readRDS("pre_data/Eng_LT_Pre.rds"))
    # Tổng hợp 144 tên cột
    all_cols <- c(base_cols, extra_cols)
    
    # Tạo dataframe rỗng với 0 dòng và 144 cột
    df_blank <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = length(all_cols)))
    colnames(df_blank) <- all_cols
    
    df_blank$STT = df$...1
    df_blank$Ten_cong_ty_Ten_ban = df$CONG_TY
    df_blank$So_don_Ma_nghiep_vu = df$SO_DON
    df_blank$Thoi_han_bao_hiem_Tu_Ngay = df$NGAY_HIEU_LUC_TU
    df_blank$Thoi_han_bao_hiem_Tu_Thang = df$THANG_HIEU_LUC_TU
    df_blank$Thoi_han_bao_hiem_Tu_Nam = df$NAM_HIEU_LUC_TU
    df_blank$Thoi_han_bao_hiem_Den_Ngay = df$NGAY_HIEU_LUC_DEN
    df_blank$Thoi_han_bao_hiem_Den_Thang = df$THANG_HIEU_LUC_DEN
    df_blank$Thoi_han_bao_hiem_Den_Nam = df$NAM_HIEU_LUC_DEN
    df_blank$So_tien_bao_hiem_So_tien = df$SO_TIEN_BH
    df_blank$So_tien_bao_hiem_Loai_tien = df$LOAI_TIEN_BH
    df_blank$Tong_phi_bao_hiem_khong_thue_So_tien = df$`SUM(PHI_BAO_HIEM)`
    df_blank$Tong_phi_bao_hiem_khong_thue_Loai_tien = df$BILLING_CURRENCY
    df_blank$Ty_le_giu_lai_cua_BHBV_checked = df$`Tỷ.lệ.BV.giữ.lại`
    
    print(colnames(df))
    for (i in 1:10) {
      # Tên gốc: KY1, KY2,...
      ky <- paste0("KY", i)
      ky_next <- paste0("KY", i + 1)
      
      # Các biến nguồn
      df_blank[[paste0("Ky_phi_", i, "_So_tien_VND")]] <- df[[paste0(ky, "_PHI_THUC_THU")]]
      df_blank[[paste0("Ky_phi_", i, "_Tu_Ngay")]]      <- df[[paste0(ky, "_DUE_DATE_DAY")]]
      df_blank[[paste0("Ky_phi_", i, "_Tu_Thang")]]    <- df[[paste0(ky, "_DUE_DATE_MONTH")]]
      df_blank[[paste0("Ky_phi_", i, "_Tu_Nam")]]      <- df[[paste0(ky, "_DUE_DATE_YEAR")]]
      
      # Cột "Đến Ngày/Tháng/Năm" phụ thuộc kỳ kế tiếp
      df_blank[[paste0("Ky_phi_", i, "_Den_Ngay")]] <- ifelse(
        !is.na(df[[paste0(ky, "_DUE_DATE_DAY")]]) & df[[paste0(ky, "_DUE_DATE_DAY")]] != 0 ,
        ifelse(df[[paste0(ky_next, "_PHI_THUC_THU")]] != 0,
               df[[paste0(ky_next, "_DUE_DATE_DAY")]],
               df$NGAY_HIEU_LUC_DEN),
        NA
      )
      
      df_blank[[paste0("Ky_phi_", i, "_Den_Thang")]] <- ifelse(
        !is.na(df[[paste0(ky, "_DUE_DATE_MONTH")]]) & df[[paste0(ky, "_DUE_DATE_MONTH")]] != 0,
        ifelse(df[[paste0(ky_next, "_PHI_THUC_THU")]] != 0,
               df[[paste0(ky_next, "_DUE_DATE_MONTH")]],
               df$THANG_HIEU_LUC_DEN),
        NA
      )
      
      df_blank[[paste0("Ky_phi_", i, "_Den_Nam")]] <- ifelse(
        !is.na(df[[paste0(ky, "_DUE_DATE_YEAR")]]) & df[[paste0(ky, "_DUE_DATE_YEAR")]] != 0,
        ifelse(df[[paste0(ky_next, "_PHI_THUC_THU")]] != 0,
               df[[paste0(ky_next, "_DUE_DATE_YEAR")]],
               df$NAM_HIEU_LUC_DEN),
        NA
      )
      
      # Ngày ghi nhận thực tế
      df_blank[[paste0("Ky_phi_", i, "_Ghi_Ngay")]]   <- df[[paste0(ky, "_DUE_DATE_REAL_DAY")]]
      df_blank[[paste0("Ky_phi_", i, "_Ghi_Thang")]] <- df[[paste0(ky, "_DUE_DATE_REAL_MONTH")]]
      df_blank[[paste0("Ky_phi_", i, "_Ghi_Nam")]]   <- df[[paste0(ky, "_DUE_DATE_REAL_YEAR")]]
    }
    df= df_blank
    
    # Hàm kiểm tra tính hợp lệ của ngày
    kiem_tra_ngay_hop_le <- function(ngay, thang, nam) {
      ngay <- as.integer(ngay)
      thang <- as.integer(thang)
      nam <- as.integer(nam)
      if (is.na(ngay) & is.na(thang) & is.na(nam)) return(TRUE)
      if (ngay==0 & thang ==0 & nam ==0) return(TRUE)
      d_str <- sprintf("%04d-%02d-%02d", nam, thang, ngay)
      parsed <- as.Date(d_str, format = "%Y-%m-%d")
      return(!is.na(parsed) && as.integer(format(parsed, "%d")) == ngay)
    }
    
    # Tìm các cột Ngày, Tháng, Năm
    cols_to_convert <- grep("_Ngay$|_Thang$|_Nam$", names(df), value = TRUE)
    df[cols_to_convert] <- lapply(df[cols_to_convert], as.integer)
    
    ngay_cols <- names(df)[str_detect(names(df), "_Ngay$")]
    for (ngay_col in ngay_cols) {
      prefix <- str_remove(ngay_col, "_Ngay$")
      thang_col <- paste0(prefix, "_Thang")
      nam_col <- paste0(prefix, "_Nam")
      new_col <- paste0(prefix, "_Hop_Le")
      
      if (thang_col %in% names(df) && nam_col %in% names(df)) {
        df[[new_col]] <- mapply(
          kiem_tra_ngay_hop_le,
          df[[ngay_col]],
          df[[thang_col]],
          df[[nam_col]]
        )
      }
    }
    
    # Tổng hợp kết quả kiểm tra
    hop_le_cols <- names(df)[str_detect(names(df), "_Hop_Le$")]
    df$Check_Ngay_Hop_Le <- apply(df[, hop_le_cols], 1, function(row) {
      if (all(row)) {
        return("Hợp lệ")
      } else {
        sai <- hop_le_cols[!row]
        return(paste("Sai:", paste(str_remove(sai, "_Hop_Le$"), collapse = ", ")))
      }
    })
    
    # Giữ lại cột kiểm tra, bỏ cột _Hop_Le
    df <- df[, !str_detect(names(df), "_Hop_Le$") | names(df) == "Check_Ngay_Hop_Le"]
    
    # Bỏ dòng có quá nhiều NA và dòng chứa "Tổng"
    df <- df[rowSums(is.na(df)) <= (ncol(df) - 4), ]
    df <- df[!grepl("Tổng", df[[1]], ignore.case = TRUE), ]
    
    # Gán reactive
    checked_df2(df)

    # Hiện modal
    showModal(modalDialog(
      title = "Kiểm tra file Excel",
      verbatimTextOutput("info2"),
      footer = tagList(
        downloadButton("download_checked_excel2", "📥 Tải file đã kiểm tra ngày"),
        actionButton("no_bind2", "Lưu"),
        modalButton("Đóng")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
})




# Hiển thị thông tin   

output$info2 <- renderPrint({
  req(checked_df2())
  req(input$file2)
  # Kiểm tra file tồn tại không
  cat("📄 Tên file Excel:", input$file2$name, "\n")
  cat("Excel:", nrow(excel_data2()), "dòng,", ncol(excel_data2()), "cột\n")
  cat("Số dòng sai ngày  :", nrow(checked_df2() %>% filter(Check_Ngay_Hop_Le != "Hợp lệ")), "dòng", "\n\n")
  # Kiểm tra file tồn tại không
  cat("📄 Data sau khi chuyển form:", merged_group_code2() , "\n")
  cat("Dataframe:", nrow(checked_df2()), "dòng,", ncol(checked_df2()), "cột\n")
  cat("Nghiệp vụ này không ghép với quý trước","\n")

})

# Tải file đã kiểm tra
output$download_checked_excel2 <- downloadHandler(
  filename = function() {
    paste0("Check_Ngay_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    df_out <- checked_df2()
    
    wb <- createWorkbook()
    addWorksheet(wb, "Check")
    writeData(wb, "Check", df_out)
    
    # Tô màu dòng sai
    sai_rows <- which(df_out$Check_Ngay_Hop_Le != "Hợp lệ") + 1
    yellow_style <- createStyle(fgFill = "#FFFF00")
    
    if (length(sai_rows) > 0) {
      addStyle(wb, "Check", style = yellow_style, rows = sai_rows,
               cols = 1:ncol(df_out), gridExpand = TRUE)
    }
    
    saveWorkbook(wb, file, overwrite = TRUE)
  }
)

#______________________________________________________no_bind


observeEvent(input$no_bind2, {
  removeModal()
  req(checked_df2(), input$file2, input$sheet2)
  excel <- checked_df2()
  
  # Xác định mã chuẩn từ tên file
  clean_name <- tolower(remove_accents2(tools::file_path_sans_ext(basename(input$file2$name))))
  clean_sheet <- tolower(remove_accents2(tools::file_path_sans_ext(basename(input$sheet2))))
  code <- get_group_code2(clean_name, clean_sheet)
  merged_group_code2(code)
  
  # Tạo tên và lưu file
  if (!is.na(code)) {
    rds_path <- file.path("cur_data", paste0(code, ".rds"))
    dir_created <- dir.create("cur_data", recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists("cur_data")) {
      showNotification("❌ Không thể tạo thư mục lưu file.", type = "error")
      return()
    }
    saveRDS(excel, rds_path)
    showNotification(paste("✅ Đã lưu file  vào", rds_path), type = "message")
  } else {
    showNotification("⚠️ Không xác định được mã chuẩn từ tên file.", type = "error")
  }
  
})







