# ================================
# SERVER — TAB OUTPUT
# Tổng theo nghiệp vụ, so sánh nhiều quý
# ================================
# FUNCTION
get_file <- function(nv, year, quarter, type) {
  folder <- ifelse(type == "PAID", "paid", "osc")
  file_name <- paste0(year, "_", tolower(quarter), ".rds")
  file.path("www", nv, folder, file_name)
}

# Tách year / quarter từ "2024Q3"
parse_quarter <- function(q) {
  list(
    year = substr(q, 1, 4),
    quarter = paste0("Q", substr(q, 6, 6))
  )
}

expand_period <- function(p) {
  # Nếu là năm (VD: "2025")
  if (grepl("^\\d{4}$", p)) {
    return(paste0(p, "Q", 1:4))
  }
  # Nếu là quý
  return(p)
}

# Hàm an toàn: đọc file + sum
safe_sum <- function(path, expr) {
  tryCatch({
    df <- readRDS(path)
    df %>%
      mutate(value = eval(expr)) %>%
      summarise(total = sum(value, na.rm = TRUE),
                count = sum(!is.na(value) & value != 0)  # 👈 số vụ
                ) 
    # %>%
    # pull(total)
  }, error = function(e) {
    tibble(total = NA_real_, count = NA_real_)
  })
}

# ================================
# CALC THEO NGHIỆP VỤ
# ================================

calc_health <- function(year, quarter, type) {
  path <- get_file("health_care", year, quarter, type)
  safe_sum(
    path,
    quote(as.numeric(
      so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh
    ))
  )
}

calc_pa <- function(year, quarter, type) {
  path <- get_file("pa", year, quarter, type)
  safe_sum(path, quote(as.numeric(SoTienDaTraBT_VND)))
}

calc_xcg <- function(year, quarter, type) {
  path <- get_file("xcg", year, quarter, type)
  safe_sum(path, quote(as.numeric(paid_osc)))
}

calc_cargo <- function(year, quarter, type) {
  path <- get_file("cargo", year, quarter, type)
  safe_sum(path, quote(as.numeric(paid_osc)))
}

calc_marine <- function(year, quarter, type) {
  path <- get_file("marine", year, quarter, type)
  safe_sum(path, quote(as.numeric(paid_osc)))
}




# ================================
# ENG / FIRE / MISC (có tỷ giá)
# ================================

calc_tskt <- function(year, quarter, prefix, type) {
  
  file_name <- paste0(year, "_", tolower(quarter), ".rds")
  path <- file.path("www", "ts_kt_tn", file_name)
  
  ty_gia <- readRDS("ty_gia.rds")
  usd_rate <- ty_gia %>%
    filter(Thoi_gian == paste0(quarter, "/", year)) %>%
    pull(USD) %>% .[1]
  print(usd_rate)
  
  
  
  
  tryCatch({
    df <- readRDS(path)

    df2 <- df %>%
      filter(
        grepl(paste0("^", prefix), source, ignore.case = TRUE),
        grepl(type, source, ignore.case = TRUE)
      )

    if (tolower(type) == "osc") {
      df2 <- df2 %>%
        mutate(
          value =
            coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd), 0) +
            coalesce(as.numeric(boi_thuong_con_lai_cua_bao_viet_usd), 0) * usd_rate
        )
      return( df2 %>%
        summarise(total = sum(value, na.rm = TRUE),
                  count = sum(!is.na(value) & value != 0)))
      
    } else if (tolower(type) == "paid") {
      df2 <- df2 %>%
        mutate(
          value =
            coalesce(as.numeric(so_tien_net_vnd), 0) +
            coalesce(as.numeric(so_tien_net_usd), 0) * usd_rate
        )
      return( df2 %>%
                #filter(grepl("k0tt", source, ignore.case = TRUE)) %>% 
                filter(grepl("paid", source, ignore.case = TRUE)) %>% 
                filter(!grepl("gđ|gd", thanh_toan, ignore.case = TRUE)) %>%
                summarise(total = sum(value, na.rm = TRUE),
                          count = sum(!is.na(value) & value != 0)) )
                # %>%
                # pull(total))
      
    } else {
      return(tibble(total = NA_real_, count = NA_real_))
    }
   

  }, # Sửa lại đoạn cuối của hàm calc_tskt
  error = function(e) {
  return(tibble(total = NA_real_, count = NA_real_))
})

}

calc_all_nv <- function(year, quarter, type) {
  bind_rows(
    calc_health(year, quarter, type),
    calc_pa(year, quarter, type),
    calc_xcg(year, quarter, type),
    calc_tskt(year, quarter, "e", type),
    calc_tskt(year, quarter, "p", type),
    calc_tskt(year, quarter, "m", type),
    calc_cargo(year, quarter, type),
    calc_marine(year, quarter, type)
  )
}
# ================================
# OBSERVE EVENT
# ================================
output_df <- reactiveVal(NULL)

observeEvent(input$v_run, {
  
  req(input$v_periods, input$v_type)
  
  result <- tibble(
    NghiepVu = c(
      "HEALTH_CARE", "PA", "XCG",
      "ENG", "FIRE", "MISC",
      "CARGO", "MARINE"
    )
  )
  
  for (p in input$v_periods) {
    
    # ============================
    # CASE 1: CHỌN NĂM
    # ============================
    if (grepl("^\\d{4}$", p)) {
      
      qs <- paste0(p, "Q", 1:4)
      # khởi tạo trước khi loop
      year_total_sum   <- rep(0, nrow(result))
      year_total_count <- rep(0, nrow(result))
      
      for (q in qs) {
        tq <- parse_quarter(q)
        values <- calc_all_nv(tq$year, tq$quarter, input$v_type)
        
        result[[paste0(q, "_sum")]] <- values$total
        result[[paste0(q, "_count")]] <- values$count
        
        # cộng dồn năm
        year_total_sum   <- year_total_sum   + replace_na(values$total, 0)
        year_total_count <- year_total_count + replace_na(values$count, 0)
        
        # result[[q]] <- values
        #year_total <- if (is.null(year_total)) values else year_total + replace_na(values, 0)
      }
      
      #result[[paste0(p, "_TOTAL")]] <- year_total
      # thêm TOTAL của năm
      result[[paste0(p, "_sum_TOTAL")]]   <- year_total_sum
      result[[paste0(p, "_count_TOTAL")]] <- year_total_count
      result[[paste0(q, "_avg")]] <- result[[paste0(p, "_sum_TOTAL")]] / result[[paste0(p, "_count_TOTAL")]]
      
    }
    
    # ============================
    # CASE 2: CHỌN QUÝ
    # ============================
    else {
      # tq <- parse_quarter(p)
      # values <- calc_all_nv(tq$year, tq$quarter, input$v_type)
      # 
      # result[[paste0(p, "_sum")]]   <- values$total
      # result[[paste0(p, "_count")]] <- values$count
     
      #_________________ lũy kế đến quý đã chọn
      
 
        tq <- parse_quarter(p)
        
        # chạy bình thường trước
        values <- calc_all_nv(tq$year, tq$quarter, input$v_type)
        
        # =========================
        # PAID + nghiệp vụ lũy kế
        # =========================
        if (input$v_type == "PAID") {
          
          nv_ytd <- c("ENG", "FIRE", "MISC", "CARGO", "MARINE")
          
          q_num <- as.numeric(substr(p, 6, 6))
          qs_ytd <- paste0(tq$year, "Q", 1:q_num)
          
          # 🔥 FIX CHÍNH: gán NghiepVu bên trong map_dfr
          values_ytd <- purrr::map_dfr(qs_ytd, function(qi) {
            tqi <- parse_quarter(qi)
            
            calc_all_nv(tqi$year, tqi$quarter, input$v_type) %>%
              dplyr::mutate(NghiepVu = result$NghiepVu)
          }) %>%
            dplyr::group_by(NghiepVu) %>%
            dplyr::summarise(
              total = sum(total, na.rm = TRUE),
              count = sum(count, na.rm = TRUE),
              .groups = "drop"
            )
          
          # join để replace đúng nghiệp vụ
          values <- values %>%
            dplyr::mutate(NghiepVu = result$NghiepVu) %>%
            dplyr::left_join(values_ytd, by = "NghiepVu", suffix = c("", "_ytd")) %>%
            dplyr::mutate(
              total = ifelse(NghiepVu %in% nv_ytd, total_ytd, total),
              count = ifelse(NghiepVu %in% nv_ytd, count_ytd, count)
            ) %>%
            dplyr::select(total, count)
        }
        
        result[[paste0(p, "_sum")]] <- values$total
        result[[paste0(p, "_count")]] <- values$count
    }
  }
  
  # ============================
  # TOTAL ROW
  # ============================
  total_row <- result %>%
    summarise(
      NghiepVu = "TOTAL",
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
    )
  
  output_df(bind_rows(result, total_row))
})


# ================================
# HIỂN THỊ TABLE
# ================================

output$v_table_ui <- renderUI({
  
  req(output_df())
  
  df_show <- output_df() %>%
    mutate(
      across(
        ends_with("_sum"),
        ~ ifelse(
          is.na(.x),
          "Chưa có thông tin",
          formatC(.x, format = "f", big.mark = ",", digits = 0)
        )
      ),
      across(
        ends_with("_count"),
        ~ ifelse(
          is.na(.x),
          "Chưa có thông tin",
          as.character(.x)   # 👈 count giữ nguyên
        )
      )
    )

  
  HTML(
    kable(
      df_show,
      format = "html",
      table.attr = "class='table table-bordered table-striped'"
    ) %>%
      kable_styling(full_width = TRUE)
  )
})


# ================================
# DOWNLOAD EXCEL
# ================================

output$v_download_table <- downloadHandler(
  filename = function() {
    paste0("OUTPUT_NGHIEP_VU_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    openxlsx::write.xlsx(output_df(), file, asTable = TRUE)
  }
)
