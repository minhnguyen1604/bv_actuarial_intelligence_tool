#_____________________ FUNCTION



checked_df1 <- reactiveVal()
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
# Reactive để chứa dữ liệu ghép hoặc excel gốc
merged_data <- reactiveVal(NULL)
merged_group_code <- reactiveVal(NULL)


observeEvent(input$check2, {
  withProgress(message = "Đang kiểm tra dữ liệu...", value = 0, {
    
    incProgress(0.1, detail = "Đang đọc dữ liệu Excel...")
  req(excel_data())
  req( input$file1, input$sheet1)
  
  clean_name <- tolower(remove_accents(tools::file_path_sans_ext(basename(input$file1$name))))
  clean_sheet <- tolower(remove_accents(input$sheet1))

  code1 <- get_group_code(clean_name, clean_sheet)
  merged_group_code(code1)
  # print(code1)
  df_raw <- excel_data()
  df = df_raw
  
  if (code1 == "Vietjet") {
    
    # Kiểm tra cột 25 có tồn tại không
    if (ncol(df) >= 25) {
      chi_so <- which(grepl("phí bảo hiểm", df[[25]], ignore.case = TRUE))
      
      if (length(chi_so) >= 2) {
        s <- chi_so[1]
        e <- chi_so[2]
        
        # Giới hạn cột tối đa là 45 hoặc nhỏ hơn nếu df có ít hơn 45 cột
        max_col <- min(45, ncol(df))
        
        het_hieu_luc <- df[(s + 1):(e - 4), c(1:2, 25:max_col)]
        con_hieu_luc <- df[(e + 1):(nrow(df)-1), c(1:2, 25:max_col)]
        
        # Lọc bỏ dòng quá nhiều NA
        het_hieu_luc <- het_hieu_luc[rowSums(is.na(het_hieu_luc)) <= (ncol(het_hieu_luc) - 2), ]
        con_hieu_luc <- con_hieu_luc[rowSums(is.na(con_hieu_luc)) <= (ncol(con_hieu_luc) - 2), ]
        
        # Chỉ gán tên cột nếu số lượng cột khớp với vector 'cot'
        if (ncol(het_hieu_luc) == length(cot) && ncol(con_hieu_luc) == length(cot)) {
          colnames(het_hieu_luc) <- cot
          colnames(con_hieu_luc) <- cot
        } else {
          warning("Số cột của bảng không khớp với độ dài vector cot")
        }
        
        het_hieu_luc$hieu_luc <- "het"
        con_hieu_luc$hieu_luc <- "con"
        
        df <- dplyr::bind_rows(het_hieu_luc, con_hieu_luc)
        
        df <- df %>%
          pivot_longer(
            cols = -c(Nam_phat_sinh_doanh_thu, Thang_phat_sinh_doanh_thu,hieu_luc),
            names_to = "Chi_tieu",
            values_to = "Gia_tri"
          )
        df <- df %>%
          mutate(
            Dau_muc = str_extract(Chi_tieu, "_(Phi|Ty|Muc|Hach).*") %>% str_remove("^_"),
            Don_vi_lien_ket = str_remove(Chi_tieu, "_(Phi|Ty|Muc|Hach).*")
          )
        
        df <- df %>%
          pivot_wider(
            id_cols = c(Nam_phat_sinh_doanh_thu, Thang_phat_sinh_doanh_thu, hieu_luc, Don_vi_lien_ket),
            names_from = Dau_muc,
            values_from = Gia_tri
          )
        
        
        df <- df %>%
          rename(
            Phi_bao_hiem_goc = Phi_bao_hiem_sau_dong_bao_hiem,
            Phi_bao_hiem_tai = Phi_TBH
          )%>%
          mutate(
            Phi_bao_hiem_goc = suppressWarnings(as.numeric(Phi_bao_hiem_goc)),
            Phi_bao_hiem_tai = suppressWarnings(as.numeric(Phi_bao_hiem_tai))
          )
      } else {
        warning("Không đủ dòng chứa 'phí bảo hiểm' để tách dữ liệu")
      }
    } else {
      warning("Dữ liệu không có đủ 25 cột để xử lý Vietjet")
    }
    
  }else if(grepl("XCG_LT|XCG_ST|PA_NNTX", code1, ignore.case = TRUE )) {
    # Tạo thêm tên cột cho kỳ phí 7 đến 10
    extra_cols <- unlist(lapply(7:10, generate_fee_cols))
    base_cols <- colnames(readRDS("pre_data/Eng_LT_Pre.rds"))
    # Tổng hợp 144 tên cột
    all_cols <- c(base_cols, extra_cols)
    
    # Tạo dataframe rỗng với 0 dòng và 144 cột
    df_blank <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = length(all_cols)))
    colnames(df_blank) <- all_cols

    #df_blank$STT = df[,1]
    df_blank$STT <- df[[1]] 
    df_blank$Ten_cong_ty_Ten_ban = df$CONG_TY
    df_blank$So_don_Ma_nghiep_vu = df$SO_DON
    df_blank$So_don_Ma_hop_dong_Ma_SDBS =  df$LOAI_HINH_NAME
    df_blank$Ten_khach_hang = df$BEN_MUA_BAO_HIEM
    df_blank$So_InsureJ = df$BKS
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
    # Tìm tên cột khớp mẫu
    col_name <- grep("giữ[[:space:].]*lại|Retention", 
                     names(df), ignore.case = TRUE, value = TRUE)
    #Retention
    # Gán dữ liệu
    df_blank$Ty_le_giu_lai_cua_BHBV_checked <- df[[col_name]]
    # df_blank$Ty_le_giu_lai_cua_BHBV_checked = df$`Tỷ.lệ.BV.giữ.lại`
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
    df <- df[rowSums(is.na(df)) <= (ncol(df) - 4), ]
    df <- df[!grepl("Tổng", df[[1]], ignore.case = TRUE), ]
    # Giả sử bạn có các kỳ từ 1 đến 6
    all_fee_cols <- unlist(lapply(1:10, generate_fee_cols))
    
    # Gộp thêm các cột không theo kỳ nhưng vẫn cần chuyển
    other_cols <- c(
      "Thoi_han_bao_hiem_Tu_Ngay", "Thoi_han_bao_hiem_Tu_Thang", "Thoi_han_bao_hiem_Tu_Nam",
      "Thoi_han_bao_hiem_Den_Ngay", "Thoi_han_bao_hiem_Den_Thang", "Thoi_han_bao_hiem_Den_Nam",
      "So_tien_bao_hiem_So_tien", "Tong_phi_bao_hiem_khong_thue_So_tien",
      "Ty_le_dong_bao_hiem_coinsurance", "Ty_le_giu_lai_cua_BHBV_TBH_cung_cap",
      "Ty_le_giu_lai_cua_BHBV_checked", "Ty_le_giu_lai_cua_BHBV"
    )
    #print(colnames(df))
    # Danh sách tất cả các cột cần chuyển
    cols_to_numeric <- c(other_cols, all_fee_cols)
    
    # Lọc ra các cột thực sự có trong df (phòng khi thiếu cột)
    cols_exist <- intersect(cols_to_numeric, colnames(df))
    
    # Chuyển sang numeric
    df[cols_exist] <- lapply(df[cols_exist], function(x) as.numeric(as.character(x)))
    
    #______________________________kdh ntkjdhkjfdhjkhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
    
    }else{
  
  rds_name = paste0(code1,"_Pre")
  rds_path <- file.path("pre_data", paste0(rds_name, ".rds"))

  
  if (file.exists(rds_path)  & !grepl("fire_st", code1, ignore.case = TRUE)) {
  he <- colnames(readRDS(rds_path)) }else{
    he <- colnames(readRDS("pre_data/Eng_LT_Pre.rds"))
    if(!is.na(code1) & grepl("Marine", code1, ignore.case = TRUE)){
      he = colnames(readRDS("7_term.rds"))
    }else if(!is.na(code1) & grepl("kcare", code1, ignore.case = TRUE)){
      
      he = colnames(readRDS("www/cur_data_Q3_2025/XCG_LT.rds"))
      df= df[, -c(12,13,14)]
      # Vị trí muốn chèn
      insert_at <- 21
      
      # Tạo 3 cột toàn NA có độ dài bằng df
      new_cols <- data.frame(
        X = rep(NA, nrow(df)),
        Y = rep(NA, nrow(df)),
        Z = rep(NA, nrow(df))
      )
      
      # Chèn vào giữa
      df <- cbind(
        df[ , 1:insert_at],
        new_cols,
        df[ , (insert_at + 1):ncol(df)]
      )
      
    }else if(!is.na(code1) & grepl("CTTV|BHTT", code1, ignore.case = TRUE)){
      # Vị trí muốn chèn
      insert_at <- 5
      
      # Tạo 3 cột toàn NA có độ dài bằng df
      new_cols <- data.frame(
        X = rep(NA, nrow(df)),
        Y = rep(NA, nrow(df))
      )
      
      # Chèn vào giữa
      df <- cbind(
        df[ , 1:insert_at],
        new_cols,
        df[ , (insert_at + 1):ncol(df)]
      )
      # Vị trí muốn chèn
      insert_at <- 9
      
      # Tạo 3 cột toàn NA có độ dài bằng df
      new_cols <- data.frame(
        z = rep(NA, nrow(df)),
        t = rep(NA, nrow(df))
      )
      
      # Chèn vào giữa
      df <- cbind(
        df[ , 1:insert_at],
        new_cols,
        df[ , (insert_at + 1):ncol(df)]
      )
      
      he = colnames(readRDS("7_term.rds"))[1:36]
    }
  }
  
  if (ncol(df) >= 10 && any(grepl("^Ngày", df[[10]], ignore.case = TRUE))) {
    
    df <- cbind(
      df[,1:5],
      col6 = NA,
      col7 = NA,
      df[,6:ncol(df)]
    )
    
  }
  
  
  t <- integer(0)
  while (ncol(df) >= 12 && length(t) == 0) {
    t <- which(grepl("^Ngày", df[[12]], ignore.case = TRUE))
    if (length(t) == 0) {
      df <- df[, -1]  # Xoá cột đầu tiên
    }
  }

  
  # Nếu vẫn không tìm thấy sau khi xoá đến khi còn <12 cột
  if (length(t) == 0) {
    showNotification("Form không hợp lệ", type = "error")
    return(NULL)
  }
  

  t <- t[1] + 1  # Dòng dữ liệu thực sự
  # Cắt phần dữ liệu từ dòng t trở đi
  df <- df[t:nrow(df), ]
  
  # Xử lý số cột không khớp
  if (ncol(df) < length(he)) {
    warning("⚠ Dữ liệu có ÍT cột hơn chuẩn. Sẽ thêm NA.")
    # Thêm các cột NA cho đủ
    df[(ncol(df) + 1):length(he)] <- NA
  } else if (ncol(df) > length(he)) {
    warning("⚠ Dữ liệu có NHIỀU cột hơn chuẩn. Sẽ bị cắt bớt.")
    df <- df[, 1:length(he)]
  }
  
  colnames(df) <- he
  
  
  
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
  
  # Giả sử bạn có các kỳ từ 1 đến 6
  all_fee_cols <- unlist(lapply(1:10, generate_fee_cols))
  
  # Gộp thêm các cột không theo kỳ nhưng vẫn cần chuyển
  other_cols <- c(
    "Thoi_han_bao_hiem_Tu_Ngay", "Thoi_han_bao_hiem_Tu_Thang", "Thoi_han_bao_hiem_Tu_Nam",
    "Thoi_han_bao_hiem_Den_Ngay", "Thoi_han_bao_hiem_Den_Thang", "Thoi_han_bao_hiem_Den_Nam",
    "So_tien_bao_hiem_So_tien", "Tong_phi_bao_hiem_khong_thue_So_tien",
    "Ty_le_dong_bao_hiem_coinsurance", "Ty_le_giu_lai_cua_BHBV_TBH_cung_cap",
    "Ty_le_giu_lai_cua_BHBV_checked", "Ty_le_giu_lai_cua_BHBV"
  )
  
  # Danh sách tất cả các cột cần chuyển
  cols_to_numeric <- c(other_cols     )#, all_fee_cols)
  
  # Lọc ra các cột thực sự có trong df (phòng khi thiếu cột)
  cols_exist <- intersect(cols_to_numeric, colnames(df))
  
  # Chuyển sang numeric
  df[cols_exist] <- lapply(df[cols_exist], function(x) as.numeric(as.character(x)))

    }
  #_______________ ghép hoặc không ghép df
  
  if(!is.na(code1) ){                                                
  rds_name = paste0(code1,"_Pre")
  rds_path <- file.path("pre_data", paste0(rds_name, ".rds"))
  
  
  # Kiểm tra file tồn tại không
  if (!file.exists(rds_path)) {                               #_________________________________ chứa data lũy kế đến thời điểm 
    cur_path <- file.path(paste0("www/cur_data_",date_now()), paste0(code1, ".rds"))
    dir_created <- dir.create(paste0("www/cur_data_",date_now()), recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(paste0("www/cur_data_",date_now()))) {
      showNotification("❌ Không thể tạo thư mục lưu file.", type = "error")
      return()
    }
    cols_to_convert <- grep("(_So_tien_VND|_So_tien_USD)$", names(df), value = TRUE)
    df[cols_to_convert] <- lapply(df[cols_to_convert], convert_to_numeric)
   
    saveRDS(df, cur_path)
    
    merged_data(df)
    showNotification(paste("✅ Đã lưu file vào", cur_path), type = "message")
    #_________________________________
    
    # rds <- tryCatch(
    #   readRDS(paste0("www/cur_data_", pre_quarter(date_now()), "/", rds_name, ".rds")),
    #   error = function(e) {
    #     # Nếu lỗi "No such file or directory", thử với tên khác
    #     rds_name <- code1
    #     alt_path <- paste0("www/cur_data_", pre_quarter(date_now()), "/", rds_name, ".rds")
    #     readRDS(alt_path)
    #   }
    # )
    path <- paste0(
      "www/cur_data_",
      pre_quarter(date_now()),
      "/",
      rds_name,
      ".rds"
    )
    
    if (file.exists(path)) {
       rds <-readRDS(path)
    
    
   
    
    
    common_cols <- intersect(colnames(rds), colnames(df))   # Các cột giống tên
    # print(colnames(rds))
    # print(colnames(df))
    for (col in common_cols) {
      target_class <- class(df[[col]])
      
      if ("integer" %in% target_class) {
        rds[[col]] <- suppressWarnings(as.integer(rds[[col]]))
      } else if ("numeric" %in% target_class) {
        rds[[col]] <- suppressWarnings(as.numeric(rds[[col]]))
      } else if ("Date" %in% target_class) {
        rds[[col]] <- suppressWarnings(as.Date(rds[[col]]))
      } else {
        rds[[col]] <- as.character(rds[[col]])
      }
    }
    cols_to_convert <- grep("(_So_tien_VND|_So_tien_USD)$", names(rds), value = TRUE)
    rds[cols_to_convert] <- lapply(rds[cols_to_convert], convert_to_numeric)
    cols_to_convert <- grep("(_So_tien_VND|_So_tien_USD)$", names(df), value = TRUE)
    df[cols_to_convert] <- lapply(df[cols_to_convert], convert_to_numeric)

    
    cur_data = df[, common_cols]
    pre_data = rds[, common_cols]
    
    
    print(ngay_input())
    cols_cmp <- names(pre_data)[12:ncol(pre_data)]
    keys <- c("So_don_Ma_hop_dong_Ma_SDBS", "So_don_Nhom_nganh_nghe_kinh_doanh","So_don_Nhom_rui_ro", "So_InsureJ")
    pre2 <- pre_data %>%
      mutate(
        source = pre_quarter(date_now()),
        across(all_of(cols_cmp), as.character)
      ) %>%
      mutate(
        ngay_ket_thuc = make_date(
          year  = as.integer(Thoi_han_bao_hiem_Den_Nam),
          month = as.integer(Thoi_han_bao_hiem_Den_Thang),
          day   = as.integer(Thoi_han_bao_hiem_Den_Ngay)
        ) )%>%
      filter(ngay_ket_thuc > ngay_input() )
    
    
    cur2 <- cur_data %>%
      mutate(
        source = date_now(),
        across(all_of(cols_cmp), as.character)
      ) %>%
      mutate(
        ngay_ket_thuc = make_date(
          year  = as.integer(Thoi_han_bao_hiem_Den_Nam),
          month = as.integer(Thoi_han_bao_hiem_Den_Thang),
          day   = as.integer(Thoi_han_bao_hiem_Den_Ngay)
        ) ) %>%
      filter(ngay_ket_thuc > ngay_input() ) 
    
    all_data <- bind_rows(pre2, cur2)
    dong_moi <- cur2 %>%
      anti_join(pre2, by = keys) %>%
      mutate(
        Loai_dong = "Mới hoàn toàn",
        Cac_cot_thay_doi = NA_character_
      )
    

    dong_thay_doi <- all_data %>%
      group_by(across(all_of(keys))) %>%
      filter(n_distinct(source) >= 2) %>%
      mutate(
        # Các cột thay đổi
        Cac_cot_thay_doi = {
          tmp <- pick(all_of(cols_cmp))
          paste(
            names(tmp)[
              sapply(tmp, function(col) n_distinct(col, na.rm = TRUE) > 1)
            ],
            collapse = ", "
          )
        },
        
        # Có dòng rỗng hay không
        co_dong_rong = any(is.na(as.matrix(pick(all_of(cols_cmp)))))
      ) %>%
      filter(Cac_cot_thay_doi != "") %>%
      ungroup() %>%
      mutate(
        Loai_dong = if_else(
          co_dong_rong,
          "Thêm thông tin",
          "Thay đổi"
        )
      )
    
    
    dong_trung <-  all_data %>%
      group_by(across(all_of(c(keys, cols_cmp)))) %>%
      filter(
        n_distinct(source) >= 2
        #n() > 1
      ) %>% mutate(
        Loai_dong = "Trùng",
        Cac_cot_thay_doi = NA_character_
      )
    
    dong_bi_bo <- pre2 %>%
      anti_join(cur2, by = keys) %>%
      mutate(
        Loai_dong = "Bị bỏ",
        Cac_cot_thay_doi = NA_character_
      )
    
    
    ketqua = rbind(dong_moi, dong_thay_doi, dong_trung, dong_bi_bo)
    
    checked_df1(ketqua)

    
    
    #checked_df1(ketqua)
    #write_xlsx(ketqua, "C:\\Users\\tts.tranthidiemquynh\\OneDrive\\Desktop\\check.xlsx")
    
    }
    
    } else {                                                   #_________________________________ chỉ chứa data  quý này

    rds <- tryCatch(
        readRDS(paste0("www/cur_data_", pre_quarter(date_now()), "/", rds_name, ".rds")),
        error = function(e) {
          # Nếu lỗi "No such file or directory", thử với tên khác
          rds_name <- code1
          alt_path <- paste0("www/cur_data_", pre_quarter(date_now()), "/", rds_name, ".rds")
          readRDS(alt_path)
        }
      )
  
    common_cols <- intersect(colnames(rds), colnames(df))   # Các cột giống tên
    # print(colnames(rds))
    # print(colnames(df))
    for (col in common_cols) {
      target_class <- class(df[[col]])
      
      if ("integer" %in% target_class) {
        rds[[col]] <- suppressWarnings(as.integer(rds[[col]]))
      } else if ("numeric" %in% target_class) {
        rds[[col]] <- suppressWarnings(as.numeric(rds[[col]]))
      } else if ("Date" %in% target_class) {
        rds[[col]] <- suppressWarnings(as.Date(rds[[col]]))
      } else {
        rds[[col]] <- as.character(rds[[col]])
      }
    }
    cols_to_convert <- grep("(_So_tien_VND|_So_tien_USD)$", names(rds), value = TRUE)
    rds[cols_to_convert] <- lapply(rds[cols_to_convert], convert_to_numeric)
    cols_to_convert <- grep("(_So_tien_VND|_So_tien_USD)$", names(df), value = TRUE)
    df[cols_to_convert] <- lapply(df[cols_to_convert], convert_to_numeric)
    
    
    
    
    ###############################################
    #___________     check trùng   _______________#
    ###############################################
    
    
    cur_data = df[, common_cols]
    pre_data = rds[, common_cols]

    print(ngay_input())
    cols_cmp <- names(pre_data)[12:ncol(pre_data)]
    keys <- c("So_don_Ma_hop_dong_Ma_SDBS", "So_don_Nhom_nganh_nghe_kinh_doanh","So_don_Nhom_rui_ro", "So_InsureJ")
    pre2 <- pre_data %>%
      mutate(
        source = pre_quarter(date_now()),
        across(all_of(cols_cmp), as.character)
      ) %>%
      mutate(
        ngay_ket_thuc = make_date(
          year  = as.integer(Thoi_han_bao_hiem_Den_Nam),
          month = as.integer(Thoi_han_bao_hiem_Den_Thang),
          day   = as.integer(Thoi_han_bao_hiem_Den_Ngay)
        ) )%>%
      filter(ngay_ket_thuc > ngay_input() )


    cur2 <- cur_data %>%
      mutate(
        source = date_now(),
        across(all_of(cols_cmp), as.character)
      ) %>%
      mutate(
        ngay_ket_thuc = make_date(
          year  = as.integer(Thoi_han_bao_hiem_Den_Nam),
          month = as.integer(Thoi_han_bao_hiem_Den_Thang),
          day   = as.integer(Thoi_han_bao_hiem_Den_Ngay)
        ) ) %>%
      filter(ngay_ket_thuc > ngay_input() ) 
    
    all_data <- bind_rows(pre2, cur2)
    dong_moi <- cur2 %>%
      anti_join(pre2, by = keys) %>%
      mutate(
        Loai_dong = "Mới hoàn toàn",
        Cac_cot_thay_doi = NA_character_
      )
   
    
    dong_thay_doi <- all_data %>%
      group_by(across(all_of(keys))) %>%
      filter(n_distinct(source) >= 2) %>%
      mutate(
        Cac_cot_thay_doi = {
          tmp <- pick(all_of(cols_cmp))
          paste(
            names(tmp)[
              sapply(tmp, function(col) n_distinct(col) > 1)
            ],
            collapse = ", "
          )
        }
      ) %>%
      filter(Cac_cot_thay_doi != "") %>%
      ungroup() %>%
      mutate(Loai_dong = "Thay đổi")
    
    dong_trung <-  all_data %>%
      group_by(across(all_of(c(keys, cols_cmp)))) %>%
      filter(
        n_distinct(source) >= 2
        #n() > 1
      ) %>% mutate(
        Loai_dong = "Trùng",
        Cac_cot_thay_doi = NA_character_
      )
    
    ketqua = rbind(dong_moi, dong_thay_doi, dong_trung)
    checked_df1(ketqua)
    print("haha")
    #write_xlsx(ketqua, "C:\\Users\\tts.tranthidiemquynh\\OneDrive\\Desktop\\check.xlsx")
    
    
    #__________________________________________________________________________________
    merged <- bind_rows(
      rds[, common_cols],
      df[, common_cols]
    )
    cols_to_convert <- grep("(_So_tien_VND|_So_tien_USD)$", names(merged), value = TRUE)
    merged[cols_to_convert] <- lapply(merged[cols_to_convert], convert_to_numeric)
    
    
    cur_path <- file.path(paste0("www/cur_data_",date_now()), paste0(code1, ".rds"))
      dir_created <- dir.create(paste0("www/cur_data_",date_now()), recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(paste0("www/cur_data_",date_now()))) {
        showNotification("❌ Không thể tạo thư mục lưu file.", type = "error")
        return()
      }
      saveRDS(merged, cur_path)
      
      merged_data(merged)
      showNotification(paste("✅ Đã  lưu file vào", cur_path), type = "message")
      
  }
      
  showModal(modalDialog(
    title = "Đã xử lý xong",
    footer = tagList(
      downloadButton("download1", "Tải file sau xử lý, chưa tính"),
      downloadButton("download_che", "Tải file check trùng, thêm bớt"),
      actionButton("confirm_calc", "✅ Thực hiện tính toán"),
      modalButton("Đóng")
    ),
    size = "l",
    easyClose = TRUE
  ))
  
  }else{  
      showNotification("⚠️ Không xác định được mã chuẩn từ tên file.", type = "error")
    
  }
  
  })
})

  
output$download1 <- downloadHandler(
  filename = function() {
    paste0( merged_group_code(), "_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    df_to_export <- merged_data()
    if (is.null(df_to_export)) {
      showNotification("⚠️ Không có dữ liệu để xuất", type = "error")
      return(NULL)
    }
    openxlsx::write.xlsx(df_to_export, file, overwrite = TRUE)
  }
)
  
output$download_che <- downloadHandler(
  filename = function() {
    paste0( merged_group_code(), "_check_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    df_to_export <- checked_df1()
    if (is.null(df_to_export)) {
      showNotification("⚠️ Không có dữ liệu để xuất", type = "error")
      return(NULL)
    }
    openxlsx::write.xlsx(df_to_export, file, overwrite = TRUE)
  }
)  
