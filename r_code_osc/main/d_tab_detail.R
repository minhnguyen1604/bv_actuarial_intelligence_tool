
prev_quarter <- function(year, quarter) {
  q <- as.numeric(gsub("Q", "", quarter))
  if (q == 1) {
    list(year = year - 1, quarter = "Q4")
  } else {
    list(year = year, quarter = paste0("Q", q - 1))
  }
}
de_detail <- reactiveVal()
de_data <- reactiveVal()
  observeEvent(input$de_run, {
    
    req(input$de_year, input$de_quarter, input$de_type, input$de_nv)
    
    y  <- input$de_year
    q  <- input$de_quarter
    nv <- tolower(input$de_nv)
    tp <- tolower(input$de_type)
    
    print(nv)
    prev <- prev_quarter(y, q)
    
    
    # =========================
    # CASE 1: TS-KT-TN (logic riêng)
    # =========================
    if (nv %in% c("eng", "fire", "misc")) {
      path_q4 <- file.path("www", "ts_kt_tn", paste0(y, "_", tolower(q), ".rds"))
      path_q3 <- file.path("www", "ts_kt_tn", paste0(prev$year, "_", tolower(prev$quarter), ".rds")) 
      #path_paid <- file.path("www", "ts_kt_tn",  paste0(y, "_", tolower(q), ".rds"))   
      
      
      
      if (!file.exists(path_q4) || !file.exists(path_q3)) {
        showNotification("❌ Không tìm thấy file OSC", type = "error")
        
        de_data (
          tibble::tibble(
            chi_tieu = character(),
            so_don = numeric(),
            gia_tri  = numeric()
          )
        )
        return(NULL)
      }
      
      
      ty_gia <- readRDS("ty_gia.rds")
      usd_rate <- ty_gia %>%
        filter(Thoi_gian == paste0(q, "/", y)) %>%
        pull(USD) %>% .[1]
      print(usd_rate)
      
      q4 <- readRDS(path_q4)
      q3 <- readRDS(path_q3)
     
      
      prefix = substr(nv, 1, 1)
      prefix = ifelse(prefix == "f", "p", prefix)
      
      paid_q4 <- q4 %>% filter( grepl(paste0("^", prefix), source, ignore.case = TRUE)) %>%
                        filter(grepl("paid", source, ignore.case = TRUE)) %>% 
                        filter(!grepl("gđ|gd", thanh_toan, ignore.case = TRUE)) 
      
      
      
      q4 <- q4 %>% filter( grepl(paste0("^", prefix), source, ignore.case = TRUE),
        grepl("osc", source, ignore.case = TRUE) )
      
      q3 <- q3 %>% filter( grepl(paste0("^", prefix), source, ignore.case = TRUE),
                           grepl("osc", source, ignore.case = TRUE) )
      
      
      
      
        q4 <- q4 %>%
          mutate(paid_osc =   coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd), 0) +
                   coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_usd), 0) * usd_rate
          )
        
        q3 <- q3 %>%
          mutate(paid_osc =   coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd), 0) +
                   coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_usd), 0) * usd_rate
          )
        
        if (!is.null(paid_q4)) {
          paid_q4 <- paid_q4 %>%
            mutate(paid_osc = coalesce(as.numeric(so_tien_net_vnd), 0) +
                     coalesce(as.numeric(so_tien_net_usd), 0) * usd_rate)
        }
      
      
    }else{
    
    # =========================
    # CASE 2: NV bình thường (code cũ)
    # =========================
    path_q4 <- file.path("www", nv, "osc", paste0(y, "_", tolower(q), ".rds"))
    path_q3 <- file.path("www", nv, "osc", paste0(prev$year, "_", tolower(prev$quarter), ".rds")) 
    path_paid <- file.path("www", nv, "paid", paste0(y, "_", tolower(q), ".rds")) 
    print(path_q3) 
    print(path_q4) 
  
    if (!file.exists(path_q4) || !file.exists(path_q3)) {
      showNotification("❌ Không tìm thấy file OSC", type = "error")
      
      de_data (
        tibble::tibble(
          chi_tieu = character(),
          so_don = numeric(),
          gia_tri  = numeric()
        )
      )
      return(NULL)
    }
    
    
    
    q4 <- readRDS(path_q4)
    q3 <- readRDS(path_q3)
    
    
    paid_q4 <- if (file.exists(path_paid)) readRDS(path_paid) else NULL
    
    if (nv == "pa") {
      q4 <- q4 %>%
        mutate(paid_osc = as.numeric(SoTienDaTraBT_VND))
      
      q3 <- q3 %>%
        mutate(paid_osc = as.numeric(SoTienDaTraBT_VND))
      
      if (!is.null(paid_q4)) {
        paid_q4 <- paid_q4 %>%
          mutate(paid_osc = as.numeric(SoTienDaTraBT_VND))
      }
      
    } else {
    q4 <- q4 %>%
      mutate(paid_osc = as.numeric(paid_osc))
    
    q3 <- q3 %>%
      mutate(paid_osc = as.numeric(paid_osc))
    
    if (!is.null(paid_q4)) {
      paid_q4 <- paid_q4 %>%
        mutate(paid_osc = as.numeric(paid_osc))
    }
    }
    
    
    }
    
    print(table(q4$source))
    print(table(q3$source))
    print(table(paid_q4$source))
    
    # ---- Gộp theo đơn ----
    
    key_col <-if ("so_nb" %in% names(q4)) {
      "so_nb"
    } else if ("so_don_bao_hiem" %in% names(q4)) {
      "so_don_bao_hiem"
    } else if ("so_khieu_nai_ij" %in% names(q4)) {
      "so_khieu_nai_ij"
    } else if ("so_khieu_nai_insure_j" %in% names(q4)) {
      "so_khieu_nai_insure_j"
    } else if ("don_ij" %in% names(q4)) {
      "don_ij"
    }else if ("so_ho_so" %in% names(q4)) {
      "so_ho_so"
    }
    else if ("CTTV_MaHopDong" %in% names(q4)) {
      "CTTV_MaHopDong"
    } else {
      stop("Không tìm thấy cột key")
    }
    
    q4[[key_col]] <- as.character(q4[[key_col]])
    q3[[key_col]] <- as.character(q3[[key_col]])
    
    if (!is.null(paid_q4)) {
      paid_q4[[key_col]] <- as.character(paid_q4[[key_col]])
    }
    
    print(nv)
    if (nv %in% c("health_care", "pa", "eng", "fire", "misc", "cargo")){
      q4_sum <- q4 %>%
        group_by(.data[[key_col]]   ) %>%
        summarise(OSC_Q4 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
      
      q3_sum <- q3 %>%
        group_by(.data[[key_col]]) %>%
        summarise(OSC_Q3 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
      
      paid_sum <- if (!is.null(paid_q4)) {
        paid_q4 %>%
          group_by(.data[[key_col]]) %>%
          summarise(Paid_Q4 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
      } else {
        tibble(!!key_col := character(), Paid_Q4 = numeric())
        #tibble(!!key_col := character(), Paid_Q4 = numeric())
      }
      
      # ---- FULL JOIN tất cả ----
      df_master <- full_join(q4_sum, q3_sum, by =key_col) %>%
        full_join(paid_sum, by = key_col)
      
    }else if (nv %in% c("xcg")){
      q4_sum <- q4 %>%
        group_by(.data[[key_col]] ,pham_vi_bt, ngay_ton_that  ) %>%
        summarise(OSC_Q4 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
      
      q3_sum <- q3 %>%
        group_by(.data[[key_col]], pham_vi_bt, ngay_ton_that ) %>%
        summarise(OSC_Q3 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
      
      paid_sum <- if (!is.null(paid_q4)) {
        paid_q4 %>%
          group_by(.data[[key_col]], pham_vi_bt, ngay_ton_that ) %>%
          summarise(Paid_Q4 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
      } else {
        tibble(!!key_col := character(), pham_vi_bt = character(), ngay_ton_that= date(),  Paid_Q4 = numeric())
        #tibble(!!key_col := character(), Paid_Q4 = numeric())
      }
      
      # ---- FULL JOIN tất cả ----
      df_master <- full_join(q4_sum, q3_sum, by = c(key_col, "pham_vi_bt", "ngay_ton_that")) %>%
        full_join(paid_sum, by = c(key_col, "pham_vi_bt", "ngay_ton_that"))
      
      
    }else {
    # ---- SUM theo từng nguồn ----
    q4_sum <- q4 %>%
      group_by(.data[[key_col]] ,source  ) %>%
      summarise(OSC_Q4 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
    
    q3_sum <- q3 %>%
      group_by(.data[[key_col]], source) %>%
      summarise(OSC_Q3 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
    
    paid_sum <- if (!is.null(paid_q4)) {
      paid_q4 %>%
        group_by(.data[[key_col]], source) %>%
        summarise(Paid_Q4 = sum(paid_osc, na.rm = TRUE), .groups = "drop")
    } else {
      tibble(!!key_col := character(), source = character(), Paid_Q4 = numeric())
      #tibble(!!key_col := character(), Paid_Q4 = numeric())
    }
    
    # ---- FULL JOIN tất cả ----
    df_master <- full_join(q4_sum, q3_sum, by = c(key_col, "source")) %>%
      full_join(paid_sum, by = c(key_col, "source"))
    }
    
    # df_master <- full_join(q4_sum, q3_sum, by = key_col) %>%
    #   full_join(paid_sum, by = key_col)
    
    # ---- Replace NA = 0 ----
    df_master <- df_master %>%
      mutate(
        OSC_Q4 = coalesce(OSC_Q4, 0),
        OSC_Q3 = coalesce(OSC_Q3, 0),
        Paid_Q4 = coalesce(Paid_Q4, 0)
      )
    
    # ---- LOGIC PHÂN LOẠI ----
    df_master <- df_master %>%
      mutate(
        result = case_when(
          OSC_Q4 == 0 & OSC_Q3 == 0 & Paid_Q4 == 0 ~ "OK",
          OSC_Q4 == 0 & OSC_Q3 == 0 & Paid_Q4 != 0 ~ "Paid trực tiếp, ko OSC",
          OSC_Q4 == 0 & OSC_Q3 != 0 & Paid_Q4 == 0 ~ "Bỏ OSC",
          OSC_Q4 == 0 & OSC_Q3 != 0 & Paid_Q4 != 0 ~ "Paid xong, bỏ OSC",
          OSC_Q4 != 0 & OSC_Q3 == 0 ~ "Đơn mới",
          OSC_Q4 != 0 & OSC_Q3 == OSC_Q4 ~ "OSC không đổi",
          OSC_Q4 != 0 & OSC_Q3 != OSC_Q4 & Paid_Q4 == 0 ~ "OSC thay đổi",
          TRUE ~ "Paid + thay đổi OSC"
        )
      )
    df_master <- df_master %>%
      mutate(
        flag = case_when(
          Paid_Q4 > OSC_Q3 & OSC_Q3 != 0 ~ "⚠ Paid > reserve",
          Paid_Q4 > OSC_Q4 & OSC_Q4 != 0 ~ "⚠ Paid > closing reserve",
          TRUE ~ ""
        )
      )
    report_df <- df_master %>%
      group_by(result) %>%
      summarise(
        so_don = n(),
        osc_this_quarter = sum(OSC_Q4, na.rm = TRUE),
        osc_pre_quarter = sum(OSC_Q3, na.rm = TRUE),
        paid_this_quarter = sum(Paid_Q4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # mutate(
      #   check = osc_pre_quarter + paid_this_quarter - osc_this_quarter
      # )%>%
      rename(chi_tieu = result)
    
    
    # ---- summary 3 data ----
    summary_3data <- tibble::tibble(
      chi_tieu = c("TOTAL", "COUNT"),
      
      so_don = c(
        NA,
        NA
      ),
      
      osc_this_quarter = c(
        sum(df_master$OSC_Q4, na.rm = TRUE),
        sum(df_master$OSC_Q4 != 0)
      ),
      
      osc_pre_quarter = c(
        sum(df_master$OSC_Q3, na.rm = TRUE),
        sum(df_master$OSC_Q3 != 0)
      ),
      
      paid_this_quarter = c(
        sum(df_master$Paid_Q4, na.rm = TRUE),
        sum(df_master$Paid_Q4 != 0)
      )
    )
    
    
    # ---- ghép vào report ----
    report_df <- bind_rows(
      summary_3data,
      report_df
      
    )
    
    
    de_detail(df_master)
    de_data(report_df)
    
    
  })
    
  output$de_table <- DT::renderDT({
    req(de_data())
    
    validate(need(nrow(de_data()) > 0, "Chưa có dữ liệu"))
    
    df_show <- de_data() %>%
      rename(
        `Chỉ tiêu` = chi_tieu,
        `Số đơn` = so_don,
        `OSC Qúy này` = osc_this_quarter,
        `OSC Qúy trước` = osc_pre_quarter,
        `Paid Qúy này` = paid_this_quarter
        
        #`Check` = check
      )
    
    DT::datatable(
      df_show,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        dom = "t"
      )
    ) %>%
      # format accounting
      DT::formatCurrency(
        columns = c("OSC Qúy này", "OSC Qúy trước", "Paid Qúy này"),
        currency = "",
        digits = 0,
        mark = ","
      ) %>%
      # căn phải số
      DT::formatStyle(
        columns = c("OSC Qúy này", "OSC Qúy trước", "Paid Qúy này"),
        `text-align` = "right"
      )
  })
  
output$de_download_detail <- downloadHandler(
      filename = function() {
        paste0(
          "Chi_tiet_bien_dong_",
          input$de_nv, "_",
          input$de_year, "_",
          input$de_quarter, ".xlsx"
        )
      },
      content = function(file) {
        write_xlsx(de_detail(), file)
      }
    )    
    
    
    
    
    
    
    
  
  
#__________________________ bản cũ dùng được 
  
  
  # prev_quarter <- function(year, quarter) {
  #   q <- as.numeric(gsub("Q", "", quarter))
  #   if (q == 1) {
  #     list(year = year - 1, quarter = "Q4")
  #   } else {
  #     list(year = year, quarter = paste0("Q", q - 1))
  #   }
  # }
  # 
  # de_data = reactiveVal()
  # observeEvent(input$de_run, {
  #   
  #   req(input$de_year, input$de_quarter, input$de_type, input$de_nv)
  #   
  #   y  <- input$de_year
  #   q  <- input$de_quarter
  #   nv <- tolower(input$de_nv)
  #   tp <- tolower(input$de_type)
  #   
  #   prev <- prev_quarter(y, q)
  #   
  #   path_q4 <- file.path("www", nv, "osc", paste0(y, "_", tolower(q), ".rds"))
  #   path_q3 <- file.path("www", nv, "osc", paste0(prev$year, "_", tolower(prev$quarter), ".rds")) 
  #   path_paid <- file.path("www", nv, "paid", paste0(y, "_", tolower(q), ".rds")) 
  #   print(path_q3) 
  #   print(path_q4) 
  #   
  #   if (!file.exists(path_q4) || !file.exists(path_q3)) {
  #     showNotification("❌ Không tìm thấy file OSC", type = "error")
  #     
  #     de_data (
  #       tibble::tibble(
  #         chi_tieu = character(),
  #         so_don = numeric(),
  #         gia_tri  = numeric()
  #       )
  #     )
  #     return(NULL)
  #   }
  #   
  #   q4 <- readRDS(path_q4)
  #   q3 <- readRDS(path_q3)
  #   
  #   
  #   paid_q4 <- if (file.exists(path_paid)) readRDS(path_paid) else NULL
  #   q4 <- q4 %>%
  #     mutate(paid_osc = as.numeric(paid_osc))
  #   
  #   q3 <- q3 %>%
  #     mutate(paid_osc = as.numeric(paid_osc))
  #   
  #   paid_q4 <- paid_q4 %>%
  #     mutate(paid_osc = as.numeric(paid_osc))
  #   # print(nrow(q4))
  #   # print(nrow(q3))
  #   # print(nrow(paid_q4))
  #   # ---- Gộp theo đơn ----
  #   
  #   key_col <- if ("so_khieu_nai_ij" %in% names(q4)) {
  #     "so_khieu_nai_ij"
  #   } else if ("so_khieu_nai_insure_j" %in% names(q4)) {
  #     "so_khieu_nai_insure_j"
  #   }  else if ("don_ij" %in% names(q4)) {
  #     "don_ij"
  #   } else {
  #     stop("Không tìm thấy cột key")
  #   }
  #   
  #   q4_sum <- q4 %>%
  #     group_by(.data[[key_col]]) %>%
  #     summarise(paid_osc = sum(paid_osc, na.rm = TRUE), .groups = "drop")
  #   
  #   q3_sum <- q3 %>%
  #     group_by(.data[[key_col]]) %>%
  #     summarise(paid_osc = sum(paid_osc, na.rm = TRUE), .groups = "drop")
  #   
  #   # ---- Phân loại đơn ----
  #   don_moi <- q4_sum %>% anti_join(q3_sum, by = key_col)
  #   don_huy <- q3_sum %>% anti_join(q4_sum, by = key_col)
  #   
  #   don_thay_doi <- q4_sum %>%
  #     inner_join(q3_sum, by = key_col, suffix = c("_q4", "_q3")) %>%
  #     filter(paid_osc_q4 != paid_osc_q3)
  #   
  #   don_khong_doi <- q4_sum %>%
  #     semi_join(q3_sum, by = key_col) %>%
  #     anti_join(don_thay_doi, by = key_col)
  #   
  #   don_huy_co_paid <- if (!is.null(paid_q4)) {
  #     don_huy %>% semi_join(paid_q4, by = key_col)
  #   } else {
  #     don_huy[0, ]
  #   }
  #   
  #   don_huy_khong_paid <- don_huy %>%
  #     anti_join(don_huy_co_paid, by = key_col)
  #   
  #   # ---- BẢNG BÁO CÁO ----
  #   report_df <- tibble::tibble(
  #     chi_tieu = c(
  #       "OSC đầu kỳ",
  #       "OSC cuối kỳ",
  #       "Đơn mới",
  #       "Đơn không đổi",
  #       "Đơn bị hủy đã paid",
  #       "Đơn bị hủy không paid",
  #       "Đơn thay đổi (giá trị Q3)",
  #       "Đơn thay đổi (giá trị Q4)"
  #     ),
  #     so_don = c(
  #       nrow(q3_sum),
  #       nrow(q4_sum),
  #       nrow(don_moi),
  #       nrow(don_khong_doi),
  #       nrow(don_huy_co_paid),
  #       nrow(don_huy_khong_paid),
  #       nrow(don_thay_doi),
  #       nrow(don_thay_doi)
  #     ),
  #     gia_tri = c(
  #       sum(q3_sum$paid_osc, na.rm = TRUE),
  #       sum(q4_sum$paid_osc, na.rm = TRUE),
  #       sum(don_moi$paid_osc, na.rm = TRUE),
  #       sum(don_khong_doi$paid_osc, na.rm = TRUE),
  #       sum(don_huy_co_paid$paid_osc, na.rm = TRUE),
  #       sum(don_huy_khong_paid$paid_osc, na.rm = TRUE),
  #       sum(don_thay_doi$paid_osc_q3, na.rm = TRUE),
  #       sum(don_thay_doi$paid_osc_q4, na.rm = TRUE)
  #     )
  #     
  #   )
  #   de_data(report_df)
  # })
  # 
  # # observeEvent(de_data(), {
  # #   req(de_data())
  # #   print(getwd())
  # #   saveRDS(de_data(), "hehe.rds")
  # # })
  # 
  # observeEvent(de_data(), {
  #   print(de_data())
  #   cat("nrow =", nrow(de_data()), "\n")
  #   
  # })
  # output$de_info <- renderText({
  #   paste(
  #     "Nghiệp vụ:", input$de_nv, "|",
  #     "Kỳ:", input$de_quarter, input$de_year, "|",
  #     "Loại:", input$de_type
  #   )
  # })
  # 
  # output$de_table <- DT::renderDT({
  #   req(de_data())
  #   validate(need(nrow(de_data()) > 0, "Không có dữ liệu"))
  #   df_show <- de_data() %>%
  #     dplyr::rename(
  #       `Chỉ tiêu` = chi_tieu,
  #       `Số đơn`   = so_don,
  #       `Giá trị (VND)` = gia_tri
  #     )
  #   DT::datatable(
  #     df_show,
  #     rownames = FALSE,
  #     options = list(dom = "t", ordering = FALSE)
  #   ) %>%
  #     DT::formatCurrency("Giá trị (VND)", currency = "", digits = 0, mark = ",") %>%
  #     DT::formatStyle("Chỉ tiêu", fontWeight = "bold")
  # })
  # 
  # 
  # 
  # 
  # 
  # output$de_download_table <- downloadHandler(
  #   filename = function() {
  #     paste0(
  #       "Bao_cao_bien_dong_",
  #       input$de_nv, "_",
  #       input$de_year, "_",
  #       input$de_quarter, ".xlsx"
  #     )
  #   },
  #   content = function(file) {
  #     write_xlsx(de_data(), file)
  #   }
  # )

  