checked_df2 <- reactiveVal()
merged_group_code1 <- reactiveVal(NULL)
#________________________________
# Reactive lấy tên sheet
sheet_names <- reactive({
  req(input$file1)
  
  getSheetNames(input$file1$datapath)
})

# UI chọn sheet
output$sheet_selector1 <- renderUI({
  req(sheet_names())
  selectInput("sheet1", "Chọn sheet:", choices = sheet_names())
})

# Reactive đọc Excel
excel_data <- reactive({
  req(input$file1, input$sheet1)
  read_excel(
    input$file1$datapath,
    sheet = input$sheet1,
    col_types = "text"  # tất cả thành character
  )
  #read.xlsx(input$file1$datapath, sheet = input$sheet1,  colTypes = "text" ) # ép tất cả thành character
})
# Hàm kiểm tra tính hợp lệ của ngày
kiem_tra_ngay_hop_le <- function(ngay, thang, nam) {
  ngay  <- suppressWarnings(as.integer(ngay))
  thang <- suppressWarnings(as.integer(thang))
  nam   <- suppressWarnings(as.integer(nam))
  
  # Trường hợp bỏ trống hết
  if (all(is.na(c(ngay, thang, nam)))) {
    return(TRUE)
  }
  
  # Cả 3 bằng 0
  if (all(c(ngay, thang, nam) == 0, na.rm = TRUE) &&
      !any(is.na(c(ngay, thang, nam)))) {
    return(TRUE)
  }
  
  # Nếu thiếu 1 hoặc 2 thành phần => không hợp lệ
  if (any(is.na(c(ngay, thang, nam)))) {
    return(FALSE)
  }
  
  d_str <- sprintf("%04d-%02d-%02d", nam, thang, ngay)
  parsed <- as.Date(d_str, format = "%Y-%m-%d")
  return(!is.na(parsed) && as.integer(format(parsed, "%d")) == ngay)
}

remove_accents <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII")
}

# Mapping các nhóm chính
group_mapping <- list(
  Eng = c("engineering", "eng","donkt", "kt"),
  Marine = c("hang hai", "marine"),
  Misc = c("misc","tnrrhh"),
  Fire = c("fire","tai san","ts"),
  PA_TTTBVV = c("chet|tpd"),
  Vietjet = c("vietjet"),
  PA= c("bhcn","pa"),
  XCG = c("xcg"),
  PA_NNTX = c("ntx"),
  XCG_CWVN = c("cwvn"),
  Travel_BHTT = c("bhtt"),
  Travel_CTTV = c("cttv"),
  Kcare = c("kcare")
)

# Mapping term (LT/ST)
term_mapping <- list(
  LT = c("lt", "dai ky","dai ki", "longterm","dk","365"),
  ST = c("st", "ngan ky", "shortterm","nk","364", "ngan ki")
)

# Hàm kiểm tra file thuộc nhóm nào
get_group_code <- function(file_name, sheet_name) {
  file_name <- tolower(remove_accents(file_name))
  sheet_name <- tolower(remove_accents(sheet_name))
  # print(paste("Cleaned file name:", file_name))
  # print(paste("Cleaned sheet name:", sheet_name))
  
  group <- NA
  term <- NA
  
  for (g in names(group_mapping)) {
    if (any(sapply(group_mapping[[g]], function(keyword) grepl(keyword, sheet_name, ignore.case = TRUE)))) {
      group <- g
      break
    }
  }
  if (is.na(group)){
    for (g in names(group_mapping)) {
      if (any(sapply(group_mapping[[g]], function(keyword) grepl(keyword, file_name, ignore.case = TRUE)))) {
        group <- g
        break
      }
    }
  }
  # print(group)
  if (!is.na(group) & group %in% c("PA_TTTBVV","Travel_BHTT", "Vietjet", "Travel_CTTV" )){
    return(group)}else{
      for (t in names(term_mapping)) {
        if (any(sapply(term_mapping[[t]], function(keyword) grepl(keyword, sheet_name, ignore.case = TRUE)))) {
          term <- t
          break
        }
      }
      
      if (is.na(term)){
        for (t in names(term_mapping)) {
          if (any(sapply(term_mapping[[t]], function(keyword) grepl(keyword, file_name, ignore.case = TRUE)))) {
            term <- t
            break
          }
        }
      }
      
      # print(term)
      #print(paste("=> FINAL GROUP:", group, ", TERM:", term))
      if (!is.na(group) && !is.na(term)) {
        return(paste0(group, "_", term))
      } else {
        return(NA)
      }
    }
  
}
cot= c(
  "Nam_phat_sinh_doanh_thu",
  "Thang_phat_sinh_doanh_thu",
  
  # Vietjet MỞ RỘNG (3)
  "Vietjet_MO_RONG_Phi_bao_hiem_sau_dong_bao_hiem",
  "Vietjet_MO_RONG_Phi_BH_tinh_tai",
  "Vietjet_MO_RONG_Ty_le_tai_BH",
  "Vietjet_MO_RONG_Phi_TBH",
  "Vietjet_MO_RONG_Muc_trach_nhiem_BH",
  "Vietjet_MO_RONG_Hach_toan_BV_Vung_tau_100pct",
  
  # Vietjet NỘI ĐỊA (4)
  "Vietjet_NOI_DIA_Phi_bao_hiem_sau_dong_bao_hiem",
  "Vietjet_NOI_DIA_Ty_le_tai_BH",
  "Vietjet_NOI_DIA_Phi_TBH",
  "Vietjet_NOI_DIA_Muc_trach_nhiem_BH",
  "Vietjet_NOI_DIA_Hach_toan_BV_Phu_My_100pct",
  
  # Vietjet Staff (5)
  "Vietjet_Staff_Phi_bao_hiem_sau_dong_bao_hiem",
  "Vietjet_Staff_Ty_le_tai_BH",
  "Vietjet_Staff_Phi_TBH",
  "Vietjet_Staff_Muc_trach_nhiem_BH",
  
  # Vietjet Travel Safe (6)
  "Vietjet_Travel_Safe_Phi_bao_hiem_sau_dong_bao_hiem",
  "Vietjet_Travel_Safe_Phi_BH_tinh_tai",
  "Vietjet_Travel_Safe_Ty_le_tai_BH",
  "Vietjet_Travel_Safe_Phi_TBH",
  "Vietjet_Travel_Safe_Muc_trach_nhiem_BH",
  "Vietjet_Travel_Safe_Hach_toan_BV_Phu_My_20pct"
)
# ==== Hàm kiểm tra form Vietjet ====
check_form_vietjet <- function(df, cot) {
  # Kết quả mặc định
  res <- list(ok = TRUE, msg = NULL)
  
  # 1. Đủ số cột?
  if (ncol(df) < 25) {
    res$ok <- FALSE
    res$msg <- "❌ File không đủ 25 cột."
    return(res)
  }
  
  # 2. Có ít nhất 2 dòng 'phí bảo hiểm' trong cột 25?
  chi_so <- which(grepl("phí bảo hiểm", df[[25]], ignore.case = TRUE))
  if (length(chi_so) < 2) {
    res$ok <- FALSE
    res$msg <- "❌ Không tìm thấy đủ 2 dòng chứa 'phí bảo hiểm' trong cột 25."
    return(res)
  }
  
  s <- chi_so[1]
  e <- chi_so[2]
  
  # 3. Chỉ số dòng hợp lệ?
  if ((s + 1) > (e - 4) || (e + 1) > (nrow(df) - 1)) {
    res$ok <- FALSE
    res$msg <- "❌ Vị trí các dòng 'phí bảo hiểm' không hợp lệ."
    return(res)
  }
  
  # 4. Kiểm tra số cột của 2 bảng con
  max_col <- min(45, ncol(df))
  het_hieu_luc <- df[(s + 1):(e - 4), c(1:2, 25:max_col)]
  con_hieu_luc <- df[(e + 1):(nrow(df)-1), c(1:2, 25:max_col)]
  
  if (ncol(het_hieu_luc) != length(cot) || ncol(con_hieu_luc) != length(cot)) {
    res$ok <- FALSE
    res$msg <- "❌ Số cột của bảng không khớp với định dạng chuẩn."
    return(res)
  }
  
  # 5. Kiểm tra tên cột bắt buộc
  colnames(het_hieu_luc) <- cot
  colnames(con_hieu_luc) <- cot
  required_cols <- c("Nam_phat_sinh_doanh_thu", "Thang_phat_sinh_doanh_thu")
  if (!all(required_cols %in% colnames(het_hieu_luc))) {
    res$ok <- FALSE
    res$msg <- "❌ Thiếu cột bắt buộc: Năm, Tháng phát sinh doanh thu."
    return(res)
  }
  
  return(res)
}





observeEvent(input$check0, {
  withProgress(message = "Đang kiểm tra dữ liệu...", value = 0, {
    incProgress(0.1, detail = "Đang đọc dữ liệu Excel...")
    req(excel_data())
    req( input$file1, input$sheet1)
    
    clean_name <- tolower(remove_accents(tools::file_path_sans_ext(basename(input$file1$name))))
    clean_sheet <- tolower(remove_accents(input$sheet1))

    code1 <- get_group_code(clean_name, clean_sheet)
    print(clean_name)
    print(clean_sheet)
    print(code1)
    output_path =  "www/output_excel"
    # Lấy danh sách file trong folder
    files_in_folder <- list.files(output_path, full.names = FALSE)
    
    # Nếu code1 có trong tên file thì hiện modal
    if (any(grepl(code1, files_in_folder, ignore.case = TRUE))) {
      he= paste("Nghiệp vụ", code1, "đã được tính trước đó. Vẫn dùng file này, ấn 'Tiếp tục' ")
    }else{
      he = paste("Đã Upload file cho nghiệp vụ", code1)
    }
    
      
      showModal(modalDialog(
        title = "⚠️ Thông báo",
        he,
        easyClose = TRUE,
        footer = tagList(
          actionButton("check1","Tiếp tục"), 
          modalButton("Đóng")
        )
      ))
    
  })

    
})

observeEvent(input$check1, {
  withProgress(message = "Đang kiểm tra dữ liệu...", value = 0, {
    
    incProgress(0.1, detail = "Đang đọc dữ liệu Excel...")
    req(excel_data())
    req( input$file1, input$sheet1)
    
    clean_name <- tolower(remove_accents(tools::file_path_sans_ext(basename(input$file1$name))))
    clean_sheet <- tolower(remove_accents(input$sheet1))
    # print(clean_name)
    # print(clean_sheet)
    code1 <- get_group_code(clean_name, clean_sheet)
    # print(code1)
    
    
    df_raw <- excel_data()
    df = df_raw
    merged_group_code1(code1)
    #______________________________________________ xoa file trong output_excel
    output_path <- "www/output_excel"
    # Lấy danh sách file trong folder
    files_in_folder <- list.files(output_path, full.names = TRUE) # để có đường dẫn đầy đủ
    
    # Tìm file có chứa code1 trong tên
    file_to_delete <- files_in_folder[grepl(code1, basename(files_in_folder), ignore.case = TRUE)]
    
    # Nếu có file thì xóa
    if (length(file_to_delete) > 0) {
      file.remove(file_to_delete)
    }
    #______________________________________________ xoa file trong cur_data
    cur_path <- "www/cur_data"
    # Lấy danh sách file trong folder
    cur <- list.files(cur_path, full.names = TRUE) # để có đường dẫn đầy đủ
    
    # Tìm file có chứa code1 trong tên
    file_delete <- cur[grepl(code1, basename(cur), ignore.case = TRUE)]
    
    # Nếu có file thì xóa
    if (length(file_delete) > 0) {
      file.remove(file_delete)
    }
    
    #_______________________________________________
    if (code1 == "Vietjet") {
      # Kiểm tra form
      check_res <- check_form_vietjet(df, cot)
      if (!check_res$ok) {
        showModal(
          modalDialog(
            title = "Lỗi định dạng file",
            check_res$msg,
            easyClose = TRUE,
            footer = modalButton("Đóng")
          )
        )
        #return() # Dừng code
      }else{
        showModal(modalDialog(
          title = "File Vietjet đúng định dạng",
          footer = tagList(
            actionButton("check2","Tiếp tục với file này"), 
            modalButton("Đóng")
          ),
          size = "l",
          easyClose = TRUE
        ))
      }
    
      
    }else if(grepl("XCG_LT|XCG_ST|PA_NNTX", code1, ignore.case = TRUE )) {
      
      # Tìm các cột Ngày, Tháng, Năm
      cols_to_convert <- grep("Ngay_|Thang_|Nam_|_day|_month|_year", names(df), ignore.case = TRUE,value = TRUE)
      df[cols_to_convert] <- lapply(df[cols_to_convert], as.integer)
      
      ngay_cols <- names(df)[str_detect(names(df), "NGAY_|_DAY")]
      for (ngay_col in ngay_cols) {
        # Vị trí cột ngày trong data frame
        idx <- which(names(df) == ngay_col)
        
        # Lấy cột tháng và năm dựa theo vị trí
        if (idx + 2 <= ncol(df)) {
          thang_col <- names(df)[idx + 1]
          nam_col   <- names(df)[idx + 2]
          
          # Tạo tên cột kết quả
          new_col <- paste0(str_remove(ngay_col, "(Ngay_|_day)$"), "_Hop_Le")
          
          # Gọi hàm kiểm tra ngày hợp lệ
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
          return("Ngày hợp lệ")
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
      
      
      #____________________________________________ check_so_tien
      
      tien_cols <- grep("_So_tien|PHI_PS", names(df), value = TRUE)
      
      for(col in tien_cols){
        new_col <- paste0(col, "_check")
        
        df[[new_col]] <- sapply(df[[col]], kiem_tra_so_tien)
      }
      
      # Tổng hợp kết quả check tiền
      check_money_cols <- paste0(tien_cols, "_check")
      
      df$Check_So_Tien <- apply(df[, check_money_cols, drop = FALSE], 1, function(row){
        
        if(all(row == "Hợp lệ")){
          return("Số tiền hợp lệ")
        } else {
          sai <- check_money_cols[row != "Hợp lệ"]
          return(paste("Sai:", paste(str_remove(sai, "_check$"), collapse = ", ")))
        }
      })
      
      # bỏ cột check trung gian
      df <- df[, !str_detect(names(df), "_check$") | names(df) == "Check_So_Tien"]
      #_________________________________________________________________ check trùng
      
      # Cột chính
      check_cols <- c(
        "BKS",
        "LOAI_HINH_NAME","NGAY_HIEU_LUC_TU","THANG_HIEU_LUC_TU",
        "NAM_HIEU_LUC_TU","NGAY_HIEU_LUC_DEN","THANG_HIEU_LUC_DEN",
        "NAM_HIEU_LUC_DEN","SO_TIEN_BH","LOAI_TIEN_BH",
        "SUM(PHI_BAO_HIEM)","BILLING_CURRENCY"
      )
      
      # Cột cần check PHI_THUC_THU
      check_cols_ps <- colnames(df)[grepl("PHI_THUC_THU", colnames(df), ignore.case = TRUE)]
      
      # Tạo bản sao và bỏ qua 0
      df_for_check <- df
      df_for_check[check_cols_ps] <- lapply(df_for_check[check_cols_ps], function(x) {
        x <- suppressWarnings(as.numeric(as.character(x)))
        x[x == 0] <- NA
        x
      })
      
      # Kết quả tổng
      hehe <- data.frame()
      d <- 0
      
      for (i in check_cols_ps) {
        all_check_cols <- c(check_cols, i)
        
        # Nhóm theo all_check_cols ngoại trừ cột đang check
        df_grouped <- df_for_check %>%
          group_by(across(all_of(check_cols))) %>%
          filter(sum(!is.na(.data[[i]])) > 1) %>%  # chỉ giữ nhóm có >1 giá trị >0
          ungroup()
        
        dup_flag <- duplicated(df_grouped[all_check_cols]) | duplicated(df_grouped[all_check_cols], fromLast = TRUE)
        df_dups <- df_grouped[dup_flag, ]
        
        # if (nrow(df_dups) > 0) {
          if (d > 0) {
            hehe <- bind_rows(hehe, df_dups)
          } else {
            hehe <- df_dups
          }
          d <- d + 1
          
          # Bỏ các bản ghi đã trùng ra khỏi dataset để vòng sau không check lại
          df_for_check <- anti_join(df_for_check, df_dups, by = colnames(df_for_check))
        #}
      }
      # Quan trọng: join hehe với df_original thay vì df_for_check (NA đã được thay lại)
      hehe_full <- df %>%
        semi_join(hehe, by = check_cols) %>% 
        mutate(check_trung = "Trùng")
      
      # Ghép vào df gốc
      df <- df %>%
        left_join(hehe_full %>% select(all_of(colnames(df)), check_trung),
                  by = colnames(df)) %>%
        mutate(check_trung = ifelse(is.na(check_trung), 0, check_trung))
      
      #______________________________kdh ntkjdhkjfdhjkhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
      
    }else{

      rds_name = paste0(code1,"_Pre")
      rds_path <- file.path("pre_data", paste0(rds_name, ".rds"))
      # Đọc cột chuẩn
      if (file.exists(rds_path)  & !grepl("fire_st", code1, ignore.case = TRUE)) {
        he <- colnames(readRDS(rds_path)) 
        }else{
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
      # Bỏ dòng có quá nhiều NA và dòng chứa "Tổng"
      df <- df[rowSums(is.na(df)) <= (ncol(df) - 4), ]
      df <- df[!grepl("Tổng", df[[1]], ignore.case = TRUE), ]
      
      
      
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
          return("Ngày hợp lệ")
        } else {
          sai <- hop_le_cols[!row]
          return(paste("Sai:", paste(str_remove(sai, "_Hop_Le$"), collapse = ", ")))
        }
      })
      # Giữ lại cột kiểm tra, bỏ cột _Hop_Le
      df <- df[, !str_detect(names(df), "_Hop_Le$") | names(df) == "Check_Ngay_Hop_Le"]
      # saveRDS(df, "test.rds")
      #____________________________________________ check_so_tien
      
      tien_cols <- grep("_So_tien", names(df), value = TRUE)
      
      for(col in tien_cols){
        new_col <- paste0(col, "_check")
        
        df[[new_col]] <- sapply(df[[col]], kiem_tra_so_tien)
      }
      
      # Tổng hợp kết quả check tiền
      check_money_cols <- paste0(tien_cols, "_check")
      
      df$Check_So_Tien <- apply(df[, check_money_cols, drop = FALSE], 1, function(row){
        
        if(all(row == "Hợp lệ")){
          return("Số tiền hợp lệ")
        } else {
          sai <- check_money_cols[row != "Hợp lệ"]
          return(paste("Sai:", paste(str_remove(sai, "_check$"), collapse = ", ")))
        }
      })
      
      # bỏ cột check trung gian
      df <- df[, !str_detect(names(df), "_check$") | names(df) == "Check_So_Tien"]
      
      
      
      
      
      
      #____________________________________________ check_trùng
      # Cột cần check trùng mới
      check_cols<- c(
        "So_don_Ma_nghiep_vu",
        "So_don_Ma_hop_dong_Ma_SDBS",
        "So_InsureJ",
        "Doi_tuong_duoc_bao_hiem",
        "Dia_diem_bao_hiem",
        "Thoi_han_bao_hiem_Tu_Thang", "Thoi_han_bao_hiem_Tu_Nam", "Thoi_han_bao_hiem_Den_Ngay" ,"Thoi_han_bao_hiem_Den_Thang"  ,         
        "Thoi_han_bao_hiem_Den_Nam" ,
        "So_tien_bao_hiem_So_tien",
        "Tong_phi_bao_hiem_khong_thue_So_tien"
      )
      
      # Tìm bản ghi trùng theo cột mới
      df_dups <- df %>%
        filter(duplicated(across(all_of(check_cols))) |
                 duplicated(across(all_of(check_cols)), fromLast = TRUE)) %>%
        mutate(check_trung = "Trùng")
      # Ghép vào df gốc theo khóa check
      # Ghép cờ vào df, chỉ join theo check_cols
      df <- df %>%
        left_join(
          df_dups %>%
            select(all_of(check_cols)) %>%
            distinct() %>%           # loại lặp khóa
            mutate(check_trung = "Trùng"),
          by = check_cols
        ) %>%
        mutate(check_trung = ifelse(is.na(check_trung), 0, check_trung))
    }
    
    checked_df2(df)
    
    # Hiện modal
    if (code1 != "Vietjet"){
    showModal(modalDialog(
      title = "Kiểm tra file Excel",
      verbatimTextOutput("info2"),
      footer = tagList(
        downloadButton("download_checked_excel2", "Tải file check ngày"),
        actionButton("check2","Tiếp tục với file này"), 
        modalButton("Đóng")
      ),
      size = "l",
      easyClose = TRUE
    ))
    }
    
  })
})




# Hiển thị thông tin   

output$info2 <- renderPrint({
  req(checked_df2(), merged_group_code1())
  code1 <- merged_group_code1()
  df= checked_df2()
  #saveRDS(df, "test.rds")

  
  # Kiểm tra file tồn tại không
  
  cat("📄 Tên file Excel:", input$file1$name, "\n")
  cat("Nghiệp vụ:", code1, "\n")

  if (code1 != "Vietjet"){
    cat("⚠️ Số dòng sai ngày  :", nrow(checked_df2() %>% filter(Check_Ngay_Hop_Le != "Ngày hợp lệ")), "dòng", "\n")
    cat("💰 Số dòng sai số tiền:", nrow(checked_df2() %>% filter(Check_So_Tien != "Số tiền hợp lệ")), "dòng\n")
    cat("⚠️ Số dòng trùng  :", nrow(checked_df2() %>% filter(check_trung == "Trùng")), "dòng", "\n")
  }
  
})

# Tải file đã kiểm tra
output$download_checked_excel2 <- downloadHandler(
  filename = function() {
    paste0(merged_group_code1(), "_check_ngay_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    df_out <- checked_df2()
    
    wb <- createWorkbook()
    addWorksheet(wb, "Check")
    writeData(wb, "Check", df_out)
    
    # Tô màu dòng sai
    sai_rows <- which(df_out$Check_Ngay_Hop_Le != "Ngày hợp lệ") + 1
    trung <- which(df_out$check_trung == "Trùng") + 1
    sai_so <- which(df_out$Check_So_Tien != "Số tiền hợp lệ") + 1
    
    red_style    <- createStyle(fgFill = "#FF0000")  # sai số tiền
    yellow_style <- createStyle(fgFill = "#FFFF00")
    green_style <- createStyle(fgFill = "green")
    
    if (length(sai_rows) > 0) {
      addStyle(wb, "Check", style = yellow_style, rows = sai_rows,
               cols = 1:ncol(df_out), gridExpand = TRUE)
    }
    if (length(trung) > 0) {
      addStyle(wb, "Check", style = green_style, rows = trung,
               cols = 1:ncol(df_out), gridExpand = TRUE)
    }
    if (length(sai_so) > 0) {
      addStyle(
        wb, "Check",
        style = red_style,
        rows = sai_so,
        cols = 1:ncol(df_out),
        gridExpand = TRUE
      )
    }
    saveWorkbook(wb, file, overwrite = TRUE)
  }
)





