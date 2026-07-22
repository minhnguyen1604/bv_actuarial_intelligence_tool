hehe_eng <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
check_eng <- reactiveVal(NULL) 
folder_eng <- "www/ts_kt_tn"

observeEvent(input$run_eng, {
  req(input$file_eng1)
  req(input$file_eng2)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
    withProgress(message = "Đang xử lý dữ liệu...", {
      incProgress(0.2, detail = "Đang đọc các file Excel...")
      
      
      ty_gia <- readRDS("ty_gia.rds")
      usd_rate <- ty_gia %>%
        filter(Thoi_gian == paste0(input$quarter, "/", input$year)) %>%
        pull(USD) %>% .[1]
      print(usd_rate)
      
      
      
      # ----- File TRONG PC -----
      file_trong_pc <- input$file_eng1$datapath
      sheet_names <- excel_sheets(file_trong_pc)
      all_sheets <- lapply(sheet_names, function(sheet) {
        read_excel(file_trong_pc, sheet = sheet, col_types = "text")
      })
      names(all_sheets) <- sheet_names
      file <- grep("E_|P_|M_", names(all_sheets), ignore.case = TRUE)
      all_sheets[file] <- lapply(all_sheets[file], clean_df)
      all_data_trong <- bind_rows(lapply(names(all_sheets)[file], function(name) {
        df <- all_sheets[[name]]
        df <- mutate_all(df, as.character)
        df$source <- name
        df
      }))
      trong_pc <- all_data_trong %>%
       filter( !is.na(nv)) %>%
        mutate(file = "FILE TRONG PC")
      
      # ----- File TRÊN PC -----
      file_tren_pc <- input$file_eng2$datapath
      sheet_names <- excel_sheets(file_tren_pc)
      all_sheets <- lapply(sheet_names, function(sheet) {
        read_excel(file_tren_pc, sheet = sheet, col_types = "text")
      })
      names(all_sheets) <- sheet_names
      file <- grep("E_|P_|M_", names(all_sheets), ignore.case = TRUE)
      all_sheets[file] <- lapply(all_sheets[file], clean_df)
      all_data_tren <- bind_rows(lapply(names(all_sheets)[file], function(name) {
        df <- all_sheets[[name]]
        df <- mutate_all(df, as.character)
        df$source <- name
        df
      }))
      tren_pc <- all_data_tren %>%
        filter( !is.na(nv)) %>%
        mutate(file = "FILE TRÊN PC")
      df = bind_rows(trong_pc, tren_pc)
      # Kiểm tra ngày tháng năm
      #---------------------------- CHECK NAM
      s_nam <- 2000
      e_nam <- 2050
      
      df <- df %>%
        mutate(
          check_nam = paste(
            ifelse(
              !is.na(suppressWarnings(as.numeric(don_hieu_luc_tu_nam))) &
                (suppressWarnings(as.numeric(don_hieu_luc_tu_nam)) < s_nam |
                   suppressWarnings(as.numeric(don_hieu_luc_tu_nam)) > e_nam),
              "sai don hieu luc tu", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(don_hieu_luc_den_nam))) &
                (suppressWarnings(as.numeric(don_hieu_luc_den_nam)) < s_nam |
                   suppressWarnings(as.numeric(don_hieu_luc_den_nam)) > e_nam),
              "sai don hieu luc den", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_ton_that_nam))) &
                (suppressWarnings(as.numeric(ngay_ton_that_nam)) < s_nam |
                   suppressWarnings(as.numeric(ngay_ton_that_nam)) > e_nam),
              "sai ngay ton that", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_thong_bao_nam))) &
                (suppressWarnings(as.numeric(ngay_thong_bao_nam)) < s_nam |
                   suppressWarnings(as.numeric(ngay_thong_bao_nam)) > e_nam),
              "sai ngay thong bao", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_nam))) &
                (suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_nam)) < s_nam |
                   suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_nam)) > e_nam),
              "sai ngay thanh toan dang ky", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_dong_ho_so_nam))) &
                (suppressWarnings(as.numeric(ngay_dong_ho_so_nam)) < s_nam |
                   suppressWarnings(as.numeric(ngay_dong_ho_so_nam)) > e_nam),
              "sai ngay dong ho so", ""
            ),
            sep = "; "
          ) %>% gsub("(^[; ]+|[; ]+$)", "", .)
        )
      
      
      #---------------------------- CHECK THÁNG
      s_th <- 1
      e_th <- 12
      df <- df %>%
        mutate(
          check_thang = paste(
            ifelse(
              !is.na(suppressWarnings(as.numeric(don_hieu_luc_tu_th))) &
                (suppressWarnings(as.numeric(don_hieu_luc_tu_th)) < s_th |
                   suppressWarnings(as.numeric(don_hieu_luc_tu_th)) > e_th),
              "sai don hieu luc tu", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(don_hieu_luc_den_th))) &
                (suppressWarnings(as.numeric(don_hieu_luc_den_th)) < s_th |
                   suppressWarnings(as.numeric(don_hieu_luc_den_th)) > e_th),
              "sai don hieu luc den", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_ton_that_th))) &
                (suppressWarnings(as.numeric(ngay_ton_that_th)) < s_th |
                   suppressWarnings(as.numeric(ngay_ton_that_th)) > e_th),
              "sai ngay ton that", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_thong_bao_th))) &
                (suppressWarnings(as.numeric(ngay_thong_bao_th)) < s_th |
                   suppressWarnings(as.numeric(ngay_thong_bao_th)) > e_th),
              "sai ngay thong bao", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_th))) &
                (suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_th)) < s_th |
                   suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_th)) > e_th),
              "sai ngay thanh toan dang ky", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_dong_ho_so_th))) &
                (suppressWarnings(as.numeric(ngay_dong_ho_so_th)) < s_th |
                   suppressWarnings(as.numeric(ngay_dong_ho_so_th)) > e_th),
              "sai ngay dong ho so", ""
            ),
            sep = "; "
          ) %>% gsub("(^[; ]+|[; ]+$)", "", .)
        )
      
      #---------------------------- CHECK NGÀY
      s_ng <- 1
      e_ng <- 31
      
      df <- df %>%
        mutate(
          check_ngay = paste(
            ifelse(
              !(don_hieu_luc_tu_ng == "Khởi công" & don_hieu_luc_den_ng == "Hoàn thành") &
                !is.na(suppressWarnings(as.numeric(don_hieu_luc_tu_ng))) &
                (suppressWarnings(as.numeric(don_hieu_luc_tu_ng)) < s_ng |
                   suppressWarnings(as.numeric(don_hieu_luc_tu_ng)) > e_ng),
              "sai don hieu luc tu", ""
            ),
            ifelse(
              !(don_hieu_luc_tu_ng == "Khởi công" & don_hieu_luc_den_ng == "Hoàn thành") &
                !is.na(suppressWarnings(as.numeric(don_hieu_luc_den_ng))) &
                (suppressWarnings(as.numeric(don_hieu_luc_den_ng)) < s_ng |
                   suppressWarnings(as.numeric(don_hieu_luc_den_ng)) > e_ng),
              "sai don hieu luc den", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_ton_that_ng))) &
                (suppressWarnings(as.numeric(ngay_ton_that_ng)) < s_ng |
                   suppressWarnings(as.numeric(ngay_ton_that_ng)) > e_ng),
              "sai ngay ton that", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_thong_bao_ng))) &
                (suppressWarnings(as.numeric(ngay_thong_bao_ng)) < s_ng |
                   suppressWarnings(as.numeric(ngay_thong_bao_ng)) > e_ng),
              "sai ngay thong bao", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_ng))) &
                (suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_ng)) < s_ng |
                   suppressWarnings(as.numeric(ngay_thanh_toan_dang_ky_ng)) > e_ng),
              "sai ngay thanh toan dang ky", ""
            ),
            ifelse(
              !is.na(suppressWarnings(as.numeric(ngay_dong_ho_so_ng))) &
                (suppressWarnings(as.numeric(ngay_dong_ho_so_ng)) < s_ng |
                   suppressWarnings(as.numeric(ngay_dong_ho_so_ng)) > e_ng),
              "sai ngay dong ho so", ""
            ),
            sep = "; "
          ) %>% gsub("(^[; ]+|[; ]+$)", "", .)
        )
      df <- df %>%
        unite("check", check_nam, check_thang,  check_ngay , sep = "; ", na.rm = TRUE)

      df$check =  gsub("(^[; ]+|[; ]+$)", "", df$check )
      #______________________________ check thiếu tháng ngày
      df <- df %>%
        mutate(
          check_thieu_thang_ngay = paste(
            ifelse(
              !is.na(don_hieu_luc_tu_nam) & (is.na(don_hieu_luc_tu_th) | is.na(don_hieu_luc_tu_ng)),
              "thieu thang/ngay don hieu luc tu", ""
            ),
            ifelse(
              !is.na(don_hieu_luc_den_nam) & (is.na(don_hieu_luc_den_th) | is.na(don_hieu_luc_den_ng)),
              "thieu thang/ngay don hieu luc den", ""
            ),
            ifelse(
              !is.na(ngay_ton_that_nam) & (is.na(ngay_ton_that_th) | is.na(ngay_ton_that_ng)),
              "thieu thang/ngay ngay ton that", ""
            ),
            ifelse(
              !is.na(ngay_thong_bao_nam) & (is.na(ngay_thong_bao_th) | is.na(ngay_thong_bao_ng)),
              "thieu thang/ngay ngay thong bao", ""
            ),
            ifelse(
              !is.na(ngay_thanh_toan_dang_ky_nam) & (is.na(ngay_thanh_toan_dang_ky_th) | is.na(ngay_thanh_toan_dang_ky_ng)),
              "thieu thang/ngay thanh toan dang ky", ""
            ),
            ifelse(
              !is.na(ngay_dong_ho_so_nam) & (is.na(ngay_dong_ho_so_th) | is.na(ngay_dong_ho_so_ng)),
              "thieu thang/ngay dong ho so", ""
            ),
            sep = "; "
          ) %>% gsub("(^[; ]+|[; ]+$)", "", .)
        )
      df <- df %>%
        unite("check", check, check_thieu_thang_ngay, sep = "; ", na.rm = TRUE) %>%
        mutate(check = gsub("(^[; ]+|[; ]+$)", "", check))
      
      # Xử lý dữ liệu như bạn đã làm (rút gọn ví dụ)
      # cols <- c(
      #   "du_phong_boi_thuong_vnd", "du_phong_boi_thuong_usd",
      #   "boi_thuong_da_tra_vnd", "boi_thuong_da_tra_usd"
      # )
      cols <- c(
        "so_tien_net_vnd",
        "so_tien_net_usd",
        "du_phong_boi_thuong_vnd",
        "du_phong_boi_thuong_usd",
        "boi_thuong_da_tra_vnd", 
        "boi_thuong_da_tra_usd", 
        "boi_thuong_con_lai_vnd", 
        "boi_thuong_con_lai_usd", 
        "boi_thuong_con_lai_cua_bao_viet_vnd", 
        "boi_thuong_con_lai_cua_bao_viet_usd",
        "du_chi_phi_giam_dinh_net_vnd",
        "du_chi_phi_giam_dinh_net_usd",
        "phi_giam_dinh_da_tra_vnd" ,
        "phi_giam_dinh_da_tra_usd" ,
        "phi_giam_dinh_con_lai_vnd" ,
        "phi_giam_dinh_con_lai_usd",                    
        "du_chi_phi_giam_dinh_con_lai_cua_bao_viet_vnd" ,   "du_chi_phi_giam_dinh_con_lai_cua_bao_viet_usd"
      )
      df[cols] <- lapply(df[cols], function(x) as.numeric(gsub("[^0-9.-]", "", x)))
      
      df$nv <- stringr::str_to_upper(df$nv)
      df <- df %>%
        unite("so_ho_so", so_ho_so_1, so_ho_so_2, so_ho_so_3, so_ho_so_4, so_ho_so_5, sep = "_", na.rm = TRUE)
      
      
      
      
      df <- df %>%
        mutate(
          paid_osc = case_when(
            
            grepl("osc", source, ignore.case = TRUE) ~
              coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd), 0) +
              coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_usd), 0) * usd_rate,
            
            grepl("paid", source, ignore.case = TRUE) &
              !grepl("gđ|gd", thanh_toan, ignore.case = TRUE) ~
              coalesce(as.numeric(so_tien_net_vnd), 0) +
              coalesce(as.numeric(so_tien_net_usd), 0) * usd_rate,
            
            TRUE ~ NA_real_
          )
        )
     
      
      
      
      incProgress(0.4, detail = "Đang chuẩn hóa tên công ty...")
      df <- df %>%
        mutate(
          cong_ty = don_vi_cap_don  %>%
            str_to_lower() %>%
            str_remove_all("công ty bảo việt|bảo việt|bv") %>%
            str_squish() %>%
            str_to_title() %>%
            stri_trans_general("Latin-ASCII"),
          
          cong_ty = case_when(
            str_detect(cong_ty, "Tổng Công Ty|Trụ Sở Chính|Tsc") ~ "TSC",
            str_detect(cong_ty, "^Dac Lac") ~ "Dak Lak",
            str_detect(cong_ty, "^Dac Nong") ~ "Dak Nong",
            str_detect(cong_ty, "^Bac Can") ~ "Bac Kan",
            str_detect(cong_ty, "^Vung Tau|Ba Ria-Vung Tau") ~ "Ba Ria - Vung Tau",
            str_detect(cong_ty, "^Ho Chi Minh|Tp Ho Chi Minh") ~ "Thanh Pho Ho Chi Minh",
            TRUE ~ cong_ty
          )
        )
      
      
      incProgress(0.6, detail = "Đọc file doanh thu...")
      dt <- read_excel("doanh_thu_moi.xlsx") %>%
        mutate(
          `Công ty` = `Công ty` %>%
            str_to_lower() %>%
            str_squish() %>%
            str_to_title() %>%
            stri_trans_general("Latin-ASCII")
        )
      
      incProgress(1, detail = "Join dữ liệu...")
      hehe <- df %>%
        left_join(dt, by = c("cong_ty" = "Công ty")) 
      
      hehe$source_file <-  paste0(  input$year, "_",tolower(input$quarter)  )
      
      
      
      
      

      hehe_eng(hehe)  # ✅ Lưu vào reactiveVal để dùng cho download
      
      
      showNotification("✅ Đã xử lý xong dữ liệu TS-KT-TN!", type = "message")
      # 2. SHOW FREQ MODAL
      # ==============================
      tam <- hehe %>% filter(is.na(Co_id))
      freq <- table(tam$don_vi_cap_don)
      
      # Chuyển table thành HTML
      freq_df <- as.data.frame(freq)
      
      freq_html <- paste0(
        "<table class='table table-bordered table-striped' style='width:100%'>",
        "<thead><tr><th>Công ty</th><th>Số lượng</th></tr></thead><tbody>",
        paste(
          apply(freq_df, 1, function(row) {
            sprintf("<tr><td>%s</td><td>%s</td></tr>", row[1], row[2])
          }),
          collapse = ""
        ),
        "</tbody></table>"
      )
      
      waiter_hide()
      
      # Show modal freq
      showModal(
        modalDialog(
          title = "📊 Tần suất công ty chưa có Co_id",
          HTML(freq_html),
          easyClose = FALSE,
          footer = tagList(
            actionButton("freq_ok_eng", "Tôi đã hiểu", class = "btn-primary")
          )
        )
      )
    })
  }, error = function(e) {
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi xử lý",
      HTML(paste0("Đã xảy ra lỗi: <br>", e$message)),
      easyClose = TRUE
    ))
  })
})    

observeEvent(input$freq_ok_eng, {
  req(hehe_eng())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_eng()
    
      file_name_rds <- file.path(folder_eng, paste0(  input$year, "_",tolower(input$quarter) ,  ".rds"))
      print(file_name_rds)
      
      if (file.exists(file_name_rds)) {
        # Tắt spinner trước khi hiện modal
        waiter_hide()
        # Đọc file cũ và đếm kích thước
        old_data <- readRDS(file_name_rds)
        n_rows_old <- nrow(old_data)
        n_cols_old <- ncol(old_data)
        # Số hàng/cột của file mới
        n_rows_new <- nrow(hehe)
        n_cols_new <- ncol(hehe)
        
        showModal(modalDialog(
          title = "⚠️ File đã tồn tại",
          HTML(paste0(
            "File ", file_name_rds, " đã tồn tại.<br>",
            "File cũ: ", n_rows_old, " hàng, ", n_cols_old, " cột.<br>",
            "File mới: ", n_rows_new, " hàng, ", n_cols_new, " cột."
          )),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Đóng"),
            actionButton("overwrite", "Ghi đè file này", class = "btn-danger")
          )
        ))
        # dừng tiếp xử lý ở đây vì người dùng cần chọn overwrite hay không
        return(invisible())
      } else {
        #______________
        saveRDS(hehe, file_name_rds)
        showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
        # ✅ 2️⃣ Đường dẫn file RDS
        paid_file <- "www/paid_ts_kt_tn.rds"
        
        # ==== HÀM TIỆN ÍCH: chuẩn hóa cột trước khi ghép ====
        align_columns <- function(new_data, old_data) {
          # Nếu file cũ rỗng, trả lại new_data
          if (is.null(old_data) || nrow(old_data) == 0) return(new_data)
          
          # Danh sách cột
          common_cols <- intersect(names(new_data), names(old_data))
          missing_in_new <- setdiff(names(old_data), names(new_data))
          extra_in_new <- setdiff(names(new_data), names(old_data))
          
          # Thêm cột thiếu
          for (col in missing_in_new) {
            new_data[[col]] <- NA
          }
          
          # Giữ đúng thứ tự & cột của file cũ
          new_data <- new_data[, names(old_data)]
          
          # Thông báo
          showModal(modalDialog(
            title = "📊 Thông tin ghép dữ liệu",
            HTML(paste0(
              "Có <b>", length(common_cols), "</b> cột chung: ",
              paste(common_cols, collapse = ", "),
              if (length(missing_in_new) > 0) paste0("<br>Thiếu (đã thêm NA): ", paste(missing_in_new, collapse = ", ")) else "",
              if (length(extra_in_new) > 0) paste0("<br>Thừa (đã bỏ): ", paste(extra_in_new, collapse = ", ")) else ""
            )),
            easyClose = TRUE
          ))
          
          return(new_data)
        }
        
        # ✅ 3️⃣ Xử lý file paid_pa
        if (file.exists(paid_file)) {
          old_paid <- readRDS(paid_file)
          hehe <- align_columns(hehe, old_paid)
          hehe <- bind_rows(old_paid, hehe) # %>% distinct()
          
        } else {
          old_paid <- NULL
          showModal(modalDialog(
            title = "⚠️ File chưa tồn tại",
            HTML(paste0("File <b>", paid_file, "</b> chưa tồn tại.<br>",
                        "👉 Hệ thống sẽ tự động <b>tạo mới</b> file này.")),
            easyClose = TRUE
          ))
          
        }
        hehe <- hehe  %>% filter(!grepl("closed", source, ignore.case = TRUE))
        check_eng(hehe %>%
                    group_by(so_ho_so, so_don_bao_hiem) %>%
                    filter(
                      n_distinct(source_file) >= 2,
                      any(source_file == paste0( input$year, "_",tolower(input$quarter) ))
                    ) %>%
                    ungroup()  )
        # ✅ 5️⃣ Ghi lại file
        hehe <- hehe  %>% filter(grepl("paid", source, ignore.case = TRUE))
        saveRDS(hehe, paid_file)
        showNotification(paste("✅ Đã lưu file:",  paid_file), type = "message")
        
        }
     
      # ẩn spinner khi hoàn tất
      waiter_hide()
  }, error = function(e) {
    # Khi lỗi → ẩn spinner và báo lỗi
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi xử lý",
      HTML(paste0("Đã xảy ra lỗi: <br>", e$message)),
      easyClose = TRUE
    ))
  }, finally = {
    # đảm bảo ẩn spinner nếu chưa ẩn
    try({ waiter_hide() }, silent = TRUE)
  })
})
# Khi người dùng bấm "Ghi đè"
observeEvent(input$overwrite, {
  req(hehe_eng())
  # 🔹 Đảm bảo chỉ chạy 1 lần duy nhất
  removeModal()           # đóng modal trước khi xử lý
  isolate({                # không cho reactive khác tự chạy lại trong khi xử lý
    waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  })
  tryCatch({
    file_name_rds <- file.path(folder_eng, paste0(  input$year, "_",tolower(input$quarter) ,  ".rds"))
    saveRDS(hehe_eng(), file_name_rds)
    showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
    hehe = hehe_eng()
    # ✅ 2️⃣ Đường dẫn file RDS
    paid_file <- "www/paid_ts_kt_tn.rds"
    
    # ✅ 3️⃣ Kỳ cần xử lý (dấu hiệu nhận biết)
    current_source <-   paste0( input$year, "_",tolower(input$quarter) )
    
    # ==== HÀM TIỆN ÍCH: chuẩn hóa cột & ghi đè theo kỳ ====
    align_and_replace <- function(new_data, old_data, source_col = "source_file") {
      # Nếu file cũ rỗng → trả lại new_data
      if (is.null(old_data) || nrow(old_data) == 0) return(new_data)
      
      # 🧩 Danh sách cột
      common_cols <- intersect(names(new_data), names(old_data))
      missing_in_new <- setdiff(names(old_data), names(new_data))
      extra_in_new <- setdiff(names(new_data), names(old_data))
      
      # 🧱 Thêm cột thiếu (NA)
      for (col in missing_in_new) new_data[[col]] <- NA
      
      # 🔄 Giữ đúng thứ tự & cột của file cũ
      new_data <- new_data[, names(old_data)]
      
      # 🗑️ Xóa dữ liệu trùng kỳ trong file cũ (nếu có)
      if (source_col %in% names(old_data)) {
        old_data <- old_data %>%
          filter(.data[[source_col]] != current_source)
      }
      
      # 🧩 Ghép lại
      result <- bind_rows(old_data, new_data) 
      
      # 📢 Thông báo
      showModal(modalDialog(
        title = "📊 Thông tin ghép dữ liệu",
        HTML(paste0(
          "Có <b>", length(common_cols), "</b> cột chung: ",
          paste(common_cols, collapse = ", "),
          if (length(missing_in_new) > 0) paste0("<br>Thiếu (đã thêm NA): ", paste(missing_in_new, collapse = ", ")) else "",
          if (length(extra_in_new) > 0) paste0("<br>Thừa (đã bỏ): ", paste(extra_in_new, collapse = ", ")) else "",
          "<br><b>Đã xóa dữ liệu cũ của kỳ:</b> ", current_source
        )),
        easyClose = TRUE
      ))
      
      return(result)
    }
    
    # ✅ 4️⃣ Xử lý file paid_pa
    if (file.exists(paid_file)) {
      old_paid <- readRDS(paid_file)
      hehe <- align_and_replace(hehe, old_paid)
    } else {
      old_paid <- NULL
    }
    
    hehe <- hehe  %>% filter(!grepl("closed", source, ignore.case = TRUE))
    check_eng(hehe %>%
                group_by(so_ho_so, so_don_bao_hiem) %>%
                filter(
                  n_distinct(source_file) >= 2,
                  any(source_file == paste0( input$year, "_",tolower(input$quarter) ))
                ) %>%
                ungroup()  )
    
    
    # ✅ 6️⃣ Lưu lại file RDS
    hehe <- hehe  %>% filter(grepl("paid", source, ignore.case = TRUE))
    saveRDS(hehe, paid_file)
    showNotification(paste("✅ Đã lưu file:",  paid_file), type = "message")
    
    
    
    removeModal()
    waiter_hide()
    # ✅ Thông báo xác nhận
    showModal(modalDialog(
      title = "✅ Hoàn tất",
      paste("Đã ghi đè thành công file", basename(file_name_rds)),
      easyClose = TRUE
    ))
    # 🔹 Ẩn spinner khi hoàn tất
    # ✅ Reset lại input$overwrite (tránh trigger lại)
    # ✅ Reset overwrite input để không bị trigger lại
    session$sendCustomMessage("resetOverwrite", NULL)
  }, error = function(e) {
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi khi ghi đè",
      HTML(paste0("Đã có lỗi: ", e$message)),
      easyClose = TRUE
    ))
  }, finally = {
    try({ waiter_hide() }, silent = TRUE)
  })
})

# ✅ Nút download: truy cập dữ liệu qua hehe_pa()
output$download_eng <- downloadHandler(
  filename = function() {
    paste0("ts_kt_tn_",   input$year, "_",tolower(input$quarter) , ".xlsx")
  },
  content = function(file) {
    req(hehe_eng())  # đảm bảo có dữ liệu
    writexl::write_xlsx(hehe_eng(), file)
  }
)


# ✅ Nút download: truy cập dữ liệu qua hehe_eng()
output$download_eng_check <- downloadHandler(
  filename = function() {
    paste0("ts_kt_tn_check_", input$year, "_",tolower(input$quarter) , ".xlsx")
  },
  content = function(file) {
    req(check_eng())  # đảm bảo có dữ liệu
    writexl::write_xlsx(check_eng(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_eng(),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "400px",
      autoWidth = TRUE,
      columnDefs = list(list(width = '150px', targets = "_all"))
    ),
    class = "display compact stripe hover nowrap"
  )
})