  nv <- c("xcg", "pa","marine","cargo","health_care", "ts_kt_tn")
  
  get_file1 <- function(nv, year, quarter, type) {
    folder <- ifelse(type == "PAID", "paid", "osc")
    file_name <- paste0(year, "_", tolower(quarter), ".rds")
    if (nv == "ts_kt_tn" ){
      file.path("www", nv,  file_name)
    }else{
    file.path("www", nv, folder, file_name)
    }
  }
  
  # ===========================
  # 🔹 Reactive: danh sách file
  # ===========================
  file_list <- eventReactive(input$hqqu_run, {
   
    if ( input$hqqu_type== "PAID"){
      nv  <- c("marine","cargo","ts_kt_tn")
    }
    
    paths <- sapply(nv, function(x) {
      get_file1(x, input$hqqu_year, input$hqqu_quarter, input$hqqu_type)
    })
    
    
    data.frame(
      Nghiệp_vụ = nv,
      Đường_dẫn = paths,
      Tồn_tại = file.exists(paths),
      stringsAsFactors = FALSE
    )
  })
  
  
  
  # ===========================
  # 🔹 Hàm chuẩn hóa data
  # ===========================
  normalize_data <- function(df, nv) {
    
    if (nv == "ts_kt_tn"){ 
      key = input$hqqu_type
      print(key)
      df= df %>% filter(grepl(key, source, ignore.case = TRUE)) }
    
    result <- data.frame(
      ma_cong_ty = rep(NA, nrow(df)),
      ten_cong_ty = rep(NA, nrow(df)),
      so_don = rep(NA, nrow(df)),
      so_hsbt = rep(NA, nrow(df)),
      nghiep_vu = rep(nv, nrow(df)),
      nghiep_vu_chi_tiet = rep(NA, nrow(df)),
      nghiep_vu_hq = rep(NA, nrow(df)),
      ngay_hieu_luc = rep(NA, nrow(df)),
      ngay_ton_that = rep(NA, nrow(df)),
      stbh_usd = rep(NA, nrow(df)),
      dbh = rep(NA, nrow(df)),
      tich_tu = rep(NA, nrow(df)),
      paid_osc = rep(NA, nrow(df)),
      #hoi_bh = rep(NA, nrow(df)),
      stringsAsFactors = FALSE
    )
    if (nv == "xcg") {
      result$ma_cong_ty <- safe_col(df, "Co_id")
      result$ten_cong_ty <- safe_col(df, "cong_ty")
      result$so_don <- safe_col(df, "don_ij")
      result$so_hsbt <- safe_col(df, "so_kn")
      result$nghiep_vu <- "Xe cơ giới"
      result$nghiep_vu_chi_tiet <- safe_col(df, "pham_vi_bt")
      result$nghiep_vu_hq  = "Xe cơ giới"
      result$ngay_hieu_luc <-  safe_date_char(df$ngay_hieu_luc)
      result$ngay_ton_that <-  safe_date_char(df$ngay_ton_that)
      result$paid_osc<- safe_col(df, "paid_osc")
    }
    
    if (nv == "pa") {
      result$ma_cong_ty <- df$Co_id
      result$ten_cong_ty <- df$CTTV_Ten
      result$so_don <- df$CTTV_MaHopDong
      result$so_hsbt <- df$SoTrenIJ
      result$nghiep_vu = rep("Con người", nrow(df))
      result$nghiep_vu_hq  =rep("Con người", nrow(df))
      result$ngay_hieu_luc <-  safe_date(
                df$ThoiHanBaoHiem_Tu_Nam,
                df$ThoiHanBaoHiem_Tu_Thang,
                df$ThoiHanBaoHiem_Tu_Ngay)
      
      result$ngay_ton_that <- 
        safe_date(
                  df$NgayTonThat_Nam,
                  df$NgayTonThat_Thang,
                  df$NgayTonThat_Ngay) 

      result$paid_osc <- safe_col(df, "paid_osc")
    }
    
    if (nv == "marine") {
      result$ma_cong_ty <- df$Co_id
      result$ten_cong_ty <- df$don_vi
      result$so_don <- df$so_nb
      result$so_hsbt <- df$so_khieu_nai_ij
      result$nghiep_vu = rep("Tàu", nrow(df))
      result$nghiep_vu_chi_tiet <- df$source
      result$nghiep_vu_hq =   dplyr::case_when(
        grepl("^hull$", result$nghiep_vu_chi_tiet, ignore.case = TRUE) ~ "Thân tàu biển",
        grepl("^p&i$", result$nghiep_vu_chi_tiet, ignore.case = TRUE) ~ "P&I",
        TRUE ~ "Tàu khác"
      )
      result$ngay_hieu_luc <- df$thoi_han_bao_hiem_tu
      result$ngay_ton_that <- df$ngay_ton_that
      result$paid_osc <- safe_col(df, "paid_osc")
      
      stbh = as.numeric(gsub(",", "", df$STBH))
      
      currency_col <- grep("si_currency|loai_tien", colnames(df), value = TRUE)[1]
      
      currency <- df[[currency_col]]
      
      result$stbh_usd <- ifelse(
        grepl("VND", currency, ignore.case = TRUE),
        stbh / 26227,
        ifelse(
          grepl("EUR", currency, ignore.case = TRUE),
          stbh * 1.14,
          stbh
        )
      )
      
      # Tìm index cột chứa keyword
      idx <- grep("ty_le_dong|dbh", colnames(df), ignore.case = TRUE)[1]
  
      
      if (is.na(idx)) {
        message(paste0("⚠️ NV không có DBH: ", nv))
        result$dbh <- NA
      } else {
        # Lấy tên cột phía sau
        next_col_name <- colnames(df)[idx]
        result$dbh = df[[next_col_name]]
      }
      
      # Kiểm tra tồn tại
      #if (is.na(idx)) stop("❌ Không tìm thấy cột chứa 'dong_bao_hiem' hoặc 'dbh'")
      
      
      
      #result$hoi_bh =df$hoi_bh
      
      
      result$ngay_hieu_luc <- safe_date_char(result$ngay_hieu_luc)
      result$ngay_ton_that <- safe_date_char(result$ngay_ton_that)
    }
    
    if (nv == "cargo") {
      result$ma_cong_ty <- df$Co_id
      result$ten_cong_ty <- df$don_vi
      result$so_don <- df$so_nb
      result$so_hsbt <- df$so_khieu_nai_insure_j
      result$nghiep_vu = rep("Hàng hóa", nrow(df))
      result$nghiep_vu_hq =rep("Hàng hóa", nrow(df))
      #result$nghiep_vu_chi_tiet <- df$nv
      result$ngay_hieu_luc <- df$ngay_khoi_hanh
      result$ngay_ton_that <- df$ngay_tai_nan_ton_that
      
      
      stbh = as.numeric(gsub(",", "", df$stbh))
      # 2. Xác định cột bên cạnh cột 'stbh'
      idx_stbh <- which(names(df) == "stbh")
      col_currency <- df[[idx_stbh + 1]] # Lấy dữ liệu của cột bên cạnh
      
      # 3. Tính toán chuyển đổi sang USD
      result$stbh_usd <- ifelse(
        grepl("VND", col_currency, ignore.case = TRUE),
        stbh / 26227,
        ifelse(
          grepl("EUR", col_currency, ignore.case = TRUE),
          stbh * 1.14,
          ifelse(
            grepl("HKD", col_currency, ignore.case = TRUE),
            stbh / 7.8,
            ifelse(
              grepl("JPY", col_currency, ignore.case = TRUE),
              stbh / 162,
              ifelse(
                grepl("THB", col_currency, ignore.case = TRUE),
                stbh / 33.5,
                ifelse(
                  grepl("CHF", col_currency, ignore.case = TRUE),
                  stbh / 0.8,
                  ifelse(
                    grepl("AUD", col_currency, ignore.case = TRUE),
                    stbh * 0.68,
                    stbh # Mặc định (giả định là USD)
                  )
                )
              )
            )
          )
        )
      )
      # result$stbh_usd = ifelse(
      #   grepl("VND", df$loai_tien, ignore.case = TRUE),
      #   stbh / 26227,
      #   ifelse(
      #     grepl("EUR", df$loai_tien, ignore.case = TRUE),
      #     stbh * 1.14,
      #     ifelse(
      #       grepl("HKD", df$loai_tien, ignore.case = TRUE),
      #       stbh /7.8,
      #       ifelse(
      #         grepl("JPY", df$loai_tien, ignore.case = TRUE),
      #         stbh /162,
      #         ifelse(
      #           grepl("THB", df$loai_tien, ignore.case = TRUE),
      #           stbh /33.5,
      #           ifelse(
      #             grepl("CHF", df$loai_tien, ignore.case = TRUE),
      #             stbh /0.8,
      #             ifelse(
      #               grepl("AUD", df$loai_tien, ignore.case = TRUE),
      #               stbh *0.68,
      #               stbh
      #             )
      # ))))))
      
      # Tìm index cột chứa keyword
      idx <- grep("dong_bao_hiem", colnames(df), ignore.case = TRUE)[1]
      
      # Kiểm tra tồn tại
      if (is.na(idx)) stop("❌ Không tìm thấy cột chứa 'dong_bao_hiem' hoặc 'dbh'")
      
      # Kiểm tra có cột phía sau không
      if (idx == ncol(df)) stop("❌ Đây là cột cuối, không có cột phía sau")
      
      # Lấy tên cột phía sau
      next_col_name <- colnames(df)[idx + 1]
      
     
      result$dbh = df[[next_col_name]]
      result$paid_osc <- safe_col(df, "paid_osc")
      result$ngay_hieu_luc <- safe_date_char(result$ngay_hieu_luc)
      result$ngay_ton_that <- safe_date_char(result$ngay_ton_that)
      
    }
    
    if (nv == "health_care") {
      du_lich_codes <- c("DQT","YDL","FLE","FLA","FLB","FVJ","DTN","FED",
                         "NND","TLE","GAE","AIA","FCI","HPI","VRI","B2B")
      
      result$ma_cong_ty <- df$Co_id
      result$ten_cong_ty <- df$cong_ty
      result$so_don <- df$so_gcnbh
      result$so_hsbt <- df$so_to_trinh_boi_thuong
      result$nghiep_vu = rep("Y tế", nrow(df))
      result$nghiep_vu_chi_tiet <- df$san_pham
      result$nghiep_vu_hq =  ifelse(
        result$nghiep_vu_chi_tiet %in% du_lich_codes,
        "Du lịch",
        "Y tế"
      )
        
        
        
        
      result$ngay_hieu_luc <- df$thoi_han_bh_tu
      result$ngay_ton_that <- df$ngay_rui_ro
      result$paid_osc <- safe_col(df, "paid_osc")
      result$ngay_hieu_luc <- safe_date_char(result$ngay_hieu_luc)
      result$ngay_ton_that <- safe_date_char(result$ngay_ton_that)
    }
    
    if (nv == "ts_kt_tn") {
      result$ma_cong_ty <- df$Co_id
      result$ten_cong_ty <- df$cong_ty
      result$so_don <- df$so_don_bao_hiem
      result$so_hsbt <- df$so_ho_so
      result$nghiep_vu = df$nv
      result$nghiep_vu_chi_tiet <- df$source
      result$nghiep_vu_hq =   dplyr::case_when(
        grepl("^e", result$nghiep_vu_chi_tiet, ignore.case = TRUE) ~ "Kỹ thuật",
        grepl("^p", result$nghiep_vu_chi_tiet, ignore.case = TRUE) ~ "Tài sản",
        grepl("^m", result$nghiep_vu_chi_tiet, ignore.case = TRUE) ~ "Trách nhiệm",
        # grepl("^H", result$nghiep_vu_chi_tiet, ignore.case = TRUE) ~ "Hàng hóa",
        TRUE ~ "không rõ"
      )
      result$ngay_hieu_luc <-  safe_date(
                df$don_hieu_luc_tu_nam,
                df$don_hieu_luc_tu_th,
                df$don_hieu_luc_tu_ng
                
                )
      
      result$ngay_ton_that <-  safe_date(
                df$ngay_ton_that_nam,
                df$ngay_ton_that_th,
                df$ngay_ton_that_ng
                
                )
      
      #result$stbh = df$so_tien_bao_hiem_vnd/26227 +so_tien_bao_hiem_usd
      result$stbh_usd = 
        ifelse(is.na(as.numeric(df$so_tien_bao_hiem_vnd)), 0, as.numeric(df$so_tien_bao_hiem_vnd))/26227 + 
        ifelse(is.na(as.numeric(df$so_tien_bao_hiem_usd)), 0, as.numeric(df$so_tien_bao_hiem_usd))
      
      
      # Tìm index cột chứa keyword
      idx <- grep("ty_le_dong|dbh", colnames(df), ignore.case = TRUE)[1]
      result$tich_tu = df$tich_tu
      # Kiểm tra tồn tại
      if (is.na(idx)) stop("❌ Không tìm thấy cột chứa 'dong_bao_hiem' hoặc 'dbh'")
      
      # Lấy tên cột phía sau
      next_col_name <- colnames(df)[idx]
      
      
      result$dbh = df[[next_col_name]]
      
     
      
       result$paid_osc<- safe_col(df, "paid_osc")
    }
    
    return(result)
  }
  
  # ===========================
  # 🔹 Reactive: đọc & chuẩn hóa data
  # ===========================
  merged_data <- eventReactive(input$hqqu_run, {
    
    df <- file_list()
    paths_exist <- df$Đường_dẫn[df$Tồn_tại]
    nv_names <- df$Nghiệp_vụ[df$Tồn_tại]
    
    
    validate(
      need(length(paths_exist) > 0, "Không có file nào tồn tại!")
    )
    
    result <- do.call(rbind, lapply(seq_along(paths_exist), function(i) {
      
      data <- readRDS(paths_exist[i])
      normalize_data(data, nv_names[i])
    }))
    
    result %>% filter(paid_osc !=0 )
  })
  
  observe({
    req(merged_data(),input$hqqu_type , input$hqqu_year, input$hqqu_quarter)
    
    data <- isolate(merged_data())
    
    dir_path <- "www/data_sent_ITC"
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    file_name <- paste0("HQQU_",input$hqqu_type, "_",  input$hqqu_year, "_", input$hqqu_quarter, ".xlsx")
    file_path <- file.path(dir_path, file_name)
    
    if (file.exists(file_path)) {
      
      showModal(modalDialog(
        title = "File đã tồn tại",
        paste0("File ", file_name, " đã tồn tại. Bạn có muốn ghi đè không?"),
        footer = tagList(
          modalButton("Huỷ"),
          actionButton("confirm_overwrite", "Ghi đè")
        )
      ))
      
    } else {
      openxlsx::write.xlsx(data, file_path)
      showNotification(
        paste0("Đã lưu file: ", file_name),
        type = "message",
        duration = 5
      )
    }
  })
  
  observeEvent(input$confirm_overwrite, {
    
    removeModal()
    
    req(merged_data(), input$hqqu_year,input$hqqu_type, input$hqqu_quarter)
    
    file_name <- paste0("HQQU_",input$hqqu_type, "_", input$hqqu_year, "_", input$hqqu_quarter, ".xlsx")
    file_path <- file.path("www/data_sent_ITC", file_name)
    
    openxlsx::write.xlsx(merged_data(), file_path)
    showNotification(
      paste0("Đã ghi đè file: ", file_name),
      type = "warning",
      duration = 5
    )
  })
  
  
  
  
  
  output$data_preview <- renderTable({
    req(merged_data())
    #head(merged_data(), 10)
    head(merged_data(), 100)
  })
  
  output$download_it <- downloadHandler(
    
    filename = function() {
      paste0("HQQU_",input$hqqu_type, "_", input$hqqu_year, "_", input$hqqu_quarter, ".xlsx")
    },
    
    content = function(file) {
      
      data <- merged_data()
      
      withProgress(message = "Đang export...", value = 0, {
        
        
        # 👉 Ghi Excel chuẩn
        #xlsx_path <- file.path(temp_dir, "HQQU_standard.xlsx")
        openxlsx::write.xlsx(data, file)
        incProgress(0.7)
      })
    }
  )
  
  
  
  
  
  #________________________________________
  # ===========================
  # 🔹 Hiển thị bảng file
  # ===========================
  output$file_table <- renderTable({
    req(file_list())
    file_list()
  })
  
  # ===========================
  # 🔹 Download
  # ===========================
  output$download_hqqu <- downloadHandler(
    
    filename = function() {
      paste0("HQQU_", input$hqqu_type, "_", input$hqqu_year, "_", input$hqqu_quarter, ".zip")
    },
    
    content = function(file) {
      
      req(file_list())
      
      df <- file_list()
      paths_exist <- df$Đường_dẫn[df$Tồn_tại]
      nv_names <- df$Nghiệp_vụ[df$Tồn_tại]
      
      validate(
        need(length(paths_exist) > 0, "Không có file nào tồn tại để tải!")
      )
      
      withProgress(message = "Đang xử lý...", value = 0, {
        
        temp_dir <- tempdir()
        xlsx_files <- c()
        n <- length(paths_exist)
        
        for (i in seq_along(paths_exist)) {
          
          incProgress(0.8/n, detail = paste("Đang xử lý:", nv_names[i]))
          
          # 👉 Đọc RDS
          data <- readRDS(paths_exist[i])
          
          # 👉 Tạo tên file XLSX
          xlsx_name <- paste0(nv_names[i], "_",input$hqqu_type, "_", input$hqqu_year, "_", input$hqqu_quarter, ".xlsx")
          xlsx_path <- file.path(temp_dir, xlsx_name)
          
          # 👉 Ghi Excel
          openxlsx::write.xlsx(data, xlsx_path)
          
          xlsx_files <- c(xlsx_files, xlsx_path)
        }
        
        incProgress(0.2, detail = "Đang nén file zip...")
        
        # 👉 Fix lỗi zip Windows
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        
        zip::zip(
          zipfile = file,
          files = basename(xlsx_files),
          mode = "cherry-pick"
        )
      })
    }
  )
#_________________________________________________plot
  
  
  # update list nghiệp vụ
  observe({
    req(merged_data())
    
    updateSelectInput(
      session,
      "nv_filter",
      choices = unique(merged_data()$nghiep_vu_hq),
      selected = unique(merged_data()$nghiep_vu_hq)
    )
  })
  
  # data xử lý
  # stbh_data <- reactive({
  #   req(merged_data())
  #   
  #   df <- merged_data()
  #   
  #   # ⚠️ convert nếu cần
  #   if (!is.numeric(df$stbh_usd)) {
  #     df$stbh_usd <- suppressWarnings(as.numeric(df$stbh_usd))
  #   }
  #   
  #   df %>%
  #     dplyr::filter(nghiep_vu_hq %in% input$nv_filter)
  # })
  stbh_data <- reactive({
    req(merged_data(), input$nv_filter, input$value_col)
    
    df <- merged_data()
    
    # convert numeric an toàn
    df[[input$value_col]] <- suppressWarnings(as.numeric(df[[input$value_col]]))
    
    df %>%
      dplyr::filter(nghiep_vu_hq %in% input$nv_filter) %>%
      dplyr::filter(!is.na(.data[[input$value_col]]))
  })
  
  # 📊 BOXPLOT
  output$stbh_boxplot <- renderPlot({
    req(stbh_data())
    
    df <- stbh_data()
    
    p <- ggplot(df, aes(x = nghiep_vu_hq, y = .data[[input$value_col]])) +
      geom_boxplot() +
      theme_minimal()+
      labs(y = input$value_col)
    
    if (input$log_scale) {
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  # 📊 HISTOGRAM
  output$stbh_hist <- renderPlot({
    req(stbh_data())
    
    df <- stbh_data()
    
    p <- ggplot(df, aes(x = .data[[input$value_col]])) +
      geom_histogram(bins = 50) +
      facet_wrap(~ nghiep_vu_hq, scales = "free") +
      theme_minimal()
    
    if (input$log_scale) {
      p <- p + scale_x_log10()
    }
    
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  