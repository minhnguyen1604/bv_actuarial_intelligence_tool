get_year_data <- function(nv, type, year_select) {
  
  cat(">>> Đang vào get_year_data\n")
  
  folder_path <- file.path("www",
                           tolower(nv),
                           tolower(type))
  
  cat(">>> Folder path:", folder_path, "\n")
  
  pattern_year <- paste0("^", year_select, "_q[1-4]\\.rds$")
  
  files <- list.files(folder_path,
                      pattern = pattern_year,
                      full.names = TRUE)
  
  cat(">>> Số file tìm được:", length(files), "\n")
  
  if (length(files) == 0) {
    stop(paste("Không tìm thấy file cho năm", year_select))
  }
  
  all_data <- purrr::map_dfr(files, readRDS)
  
  cat(">>> Đã đọc file xong\n")
  
  all_data$payment_year <- year_select
  
  return(all_data)
}
triangle_data <- eventReactive(input$f_run, {
  
  years <- c(2024, 2025)
  
  tryCatch({
    
    all_year_data <- purrr::map_dfr(years, function(y) {
      
      df <- get_year_data(input$fnghiep_vu,
                          input$fdata_type,
                          y)
      
      df$ngay_tai_nan_ton_that <- as.Date(
        df$ngay_tai_nan_ton_that,
        format = "%d/%m/%Y"
      )
      
      df$paid_osc <- as.numeric(df$paid_osc)
      df$AY <- format(df$ngay_tai_nan_ton_that, "%Y")
      
      return(df)
    })
    summary_df <- all_year_data %>%
      group_by(payment_year, AY) %>%
      summarise(value = sum(paid_osc, na.rm = TRUE),
                .groups = "drop")
    
    result <- summary_df %>%
      tidyr::pivot_wider(
        names_from = AY,
        values_from = value,
        values_fill = 0
      )
    
    # ---- Sort AY columns tăng dần ----
    ay_cols <- setdiff(names(result), "payment_year")
    ay_cols_sorted <- sort(ay_cols)
    
    result <- result %>%
      dplyr::select(payment_year, all_of(ay_cols_sorted))
    
    # ---- Thêm cột Tổng năm ----
    result <- result %>%
      dplyr::mutate(Tong_nam = rowSums(across(-payment_year), na.rm = TRUE)) %>%
      arrange(payment_year)
    
    for (col in ay_cols_sorted) {
      result[[col]] <- ifelse(
        as.numeric(col) > result$payment_year,
        NA,
        result[[col]]
      )
    }
    
    result
    
    # summary_df <- all_year_data %>%
    #   group_by(payment_year, AY) %>%
    #   summarise(value = sum(paid_osc, na.rm = TRUE),
    #             .groups = "drop")
    
    # summary_df %>%
    #   tidyr::pivot_wider(names_from = AY,
    #                      values_from = value) %>%
    #   arrange(payment_year)
    
  }, error = function(e) {
    showNotification(e$message, type = "error")
    return(NULL)
  })
})
output$f_table <- DT::renderDT({
  
  req(triangle_data())
  
  df <- triangle_data()
  
  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 10,
      dom = "t",
      ordering = FALSE
    )
  ) %>%
    DT::formatCurrency(
      columns = names(df)[-1],
      currency = "",
      mark = ",",
      digits = 0
    ) %>%
    DT::formatStyle(
      "Tong_nam",
      fontWeight = "bold",
      backgroundColor = "#f2f2f2"
    )
})

# output$f_table <- DT::renderDT({
#   
#   req(triangle_data())
#   
#   DT::datatable(
#     triangle_data(),
#     rownames = FALSE,
#     options = list(scrollX = TRUE)
#   ) %>%
#     DT::formatCurrency(
#       columns = names(triangle_data())[-1],
#       currency = "",
#       mark = ","
#     )
# })
