
excel_file <- reactiveVal(NULL)
table_file <- reactiveVal(NULL)
#________________________________________________________________

detect_anomaly_last_quarter <- function(df, threshold_yoy = 0.3, threshold_z = 3) {
  num_cols <- setdiff(names(df), "Line")
  
  mat <- as.matrix(df[, num_cols])
  mat <- apply(mat, 2, as.numeric)
  
  n_col <- ncol(mat)
  
  # ===== kiểm tra đủ dữ liệu YoY =====
  if (n_col < 5) {
    return(list(
      flag = rep(FALSE, nrow(mat)),
      yoy = rep(NA, nrow(mat)),
      last_col = num_cols[n_col]
    ))
  }
  
  # ===== YoY của quý cuối =====
  last_yoy <- (mat[, n_col] - mat[, n_col - 4]) / mat[, n_col - 4]
  
  # ===== z-score của quý cuối =====
  zscore_last <- apply(mat, 1, function(x) {
    if (sd(x, na.rm = TRUE) == 0) return(0)
    (tail(x, 1) - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  })
  
  # ===== flag =====
  flag <- (abs(last_yoy) > threshold_yoy) | (abs(zscore_last) > threshold_z)
  flag[is.na(flag)] <- FALSE
  
  list(
    flag = flag,
    yoy = last_yoy,
    last_col = num_cols[n_col]
  )
}

compare_analysis <- reactive({
  req(input$compare_quy_luyke, input$compare_type, input$compare_metric, input$compare_start_year)
  
  df <- data() %>%
    dplyr::mutate(Year = as.numeric(substr(Năm, 1, 4))) %>%
    dplyr::filter(
      `Quý/Lũy kế` == input$compare_quy_luyke,
      Type == input$compare_type,
      Year >= input$compare_start_year
    ) %>%
    dplyr::select(Line, Năm, all_of(input$compare_metric)) %>%
    tidyr::pivot_wider(names_from = Năm, values_from = all_of(input$compare_metric))
  
  anomaly <- detect_anomaly_last_quarter(df)
  
  list(data = df, anomaly = anomaly)
})


output$compare_comment <- renderUI({
  res <- compare_analysis()
  df <- res$data
  flag <- res$anomaly$flag
  yoy <- res$anomaly$yoy
  last_period <- res$anomaly$last_col
  
  idx <- which(flag)
  
  if (length(idx) == 0) {
    return(
      div(style = "color:#2c7a7b;",
          paste0("✅ Quý ", last_period, " không phát hiện biến động bất thường.")
      )
    )
  }
  
  comments <- lapply(idx, function(i) {
    line_name <- df$Line[i]
    change_txt <-scales::percent(yoy[i], accuracy = 0.1)
    
    tags$li(
      paste0(
        "⚠️ ", line_name,
        " biến động mạnh",
        " (YoY ", change_txt, ")"
      )
    )
  })
  
  tagList(
    div(style = "color:#c53030; font-weight:600; margin-bottom:6px;",
        paste0("🚨 Biến động bất thường tại ", last_period, ":")
    ),
    tags$ul(comments)
  )
})













#________________________________________________________
output$compare_table <- DT::renderDataTable({
  req(input$compare_quy_luyke, input$compare_type, input$compare_metric, input$compare_start_year)

  data_filtered <- data() %>%
    dplyr::mutate(Year = as.numeric(substr(Năm, 1, 4))) %>%
    dplyr::filter(
      `Quý/Lũy kế` == input$compare_quy_luyke,
      Type == input$compare_type,
      Year >= input$compare_start_year
    ) %>%
    dplyr::select(Line, Năm, all_of(input$compare_metric)) %>%
    tidyr::pivot_wider(names_from = Năm, values_from = all_of(input$compare_metric))


  # --- Format dữ liệu ---
  if (input$compare_metric %in% c("OsC", "UPR", "Written", "Paid", "@OsC", "@UPR", "@IBNR","@CAT", "Sub total(earned)" , "Sub total(incurred)" )) {
    # Đơn vị: tỷ (1e6)
    data_filtered <- data_filtered %>%
      dplyr::mutate(across(-Line, ~ .x ))  # * 1e6))
    table_file(data_filtered)
    DT::datatable(
      data_filtered,
      options = list(pageLength = 15, scrollX = TRUE , dom = "tpi"   ),
      caption = paste("So sánh", input$compare_metric, "từ", input$compare_start_year, "đến", max(data_filtered$Year, na.rm = TRUE))
    ) %>%
      DT::formatRound(columns = 2:ncol(data_filtered), digits = 0) %>%
      DT::formatCurrency(columns = 2:ncol(data_filtered), currency = "", mark = ",", digits = 0)

  } else if (input$compare_metric %in% c("Paid/Written", "Incurred/Earned")) {
    # Dạng tỷ lệ %
    # data_filtered <- data_filtered %>%
    #   dplyr::mutate(across(-Line, ~ .x * 100))
    data_filtered <- data_filtered %>%
      mutate(across(
        -Line,
        ~ round(as.numeric(.x) * 100, 1)
      ))
    table_file(data_filtered)
    DT::datatable(
      data_filtered,

      options = list(pageLength = 15, scrollX = TRUE, dom = "tpi" ),
      caption = paste("So sánh", input$compare_metric, "từ", input$compare_start_year, "đến", max(data_filtered$Year, na.rm = TRUE))
    ) %>%
      DT::formatRound(columns = 2:ncol(data_filtered), digits = 0) %>%
      DT::formatString(columns = 2:ncol(data_filtered), suffix = "%")
  }

})
output$compare_title <- renderUI({
  req(input$compare_metric)

  latest_period <- max(data()$Năm, na.rm = TRUE)

  HTML(paste0(
    "📊 So sánh <b>", input$compare_metric, " ", latest_period , " ", input$compare_quy_luyke
    #"</b> giữa các Type — ",
  ))
})

output$download_compare_excel1 <- downloadHandler(
  filename = function() {
    paste0("compare_cac_quy ", input$compare_metric,  ".xlsx")
  },
  content = function(file) {
    openxlsx::write.xlsx(table_file(), file)
  }
)

















#________________________________________________________________
check_structure_rules <- function(df) {
  
  # đảm bảo có đủ cột
  needed_cols <- c("DIRECT", "INWARD", "RECOVERY", "RETROCESSION")
  exist_cols <- intersect(needed_cols, names(df))
  
  res <- list(
    recovery_issue = integer(0),
    retro_issue = integer(0)
  )
  
  # ===== Rule 1: Recovery < Direct =====
  if (all(c("RECOVERY", "DIRECT") %in% names(df))) {
    res$recovery_issue <- which(
      !is.na(df$RECOVERY) &
        !is.na(df$DIRECT) &
        df$RECOVERY > df$DIRECT
    )
  }
  
  # ===== Rule 2: Retro < Inward =====
  if (all(c("RETROCESSION", "INWARD") %in% names(df))) {
    res$retro_issue <- which(
      !is.na(df$RETROCESSION) &
        !is.na(df$INWARD) &
        df$RETROCESSION > df$INWARD
    )
  }
  
  res
}

compare2_analysis <- reactive({
  
  req(input$compare_quy_luyke, input$compare_metric, input$compare_type)
  df <- data()
  
  latest_period <- max(df$Năm, na.rm = TRUE)
  
  data_filtered <- df %>%
    dplyr::filter(
      `Quý/Lũy kế` == input$compare_quy_luyke,
      Năm == latest_period
    ) %>%
    dplyr::select(Line, Type, all_of(input$compare_metric)) %>%
    tidyr::pivot_wider(
      names_from = Type,
      values_from = all_of(input$compare_metric)
    )
  
  rules <- check_structure_rules(data_filtered)
  
  list(
    data = data_filtered,
    rules = rules,
    period = latest_period
  )
})

output$compare2_comment <- renderUI({
  
  res <- compare2_analysis()
  df <- res$data
  rules <- res$rules
  period <- res$period
  
  msgs <- list()
  
  # ===== Recovery vs Direct =====
  if (length(rules$recovery_issue) > 0) {
    items <- lapply(rules$recovery_issue, function(i) {
      tags$li(
        paste0(
          "⚠️ ", df$Line[i],
          ": Recovery lớn hơn Direct"
        )
      )
    })
    
    msgs <- c(msgs, list(
      div(
        style = "color:#c53030; font-weight:600;",
        "🚨 Vi phạm rule Recovery < Direct:"
      ),
      tags$ul(items)
    ))
  }
  
  # ===== Retro vs Inward =====
  if (length(rules$retro_issue) > 0) {
    items <- lapply(rules$retro_issue, function(i) {
      tags$li(
        paste0(
          "⚠️ ", df$Line[i],
          ": Retrocession lớn hơn Inward"
        )
      )
    })
    
    msgs <- c(msgs, list(
      div(
        style = "color:#c53030; font-weight:600; margin-top:8px;",
        "🚨 Vi phạm rule Retrocession < Inward:"
      ),
      tags$ul(items)
    ))
  }
  
  # ===== nếu không có vấn đề =====
  if (length(msgs) == 0) {
    return(
      div(style = "color:#2c7a7b;",
          paste0("✅ ", period, ": Không phát hiện bất thường cấu trúc.")
      )
    )
  }
  
  tagList(msgs)
})


output$compare_table2 <- DT::renderDataTable({
  
  req(input$compare_quy_luyke, input$compare_metric, input$compare_type)
  df <- data()
  
  # ✅ lấy quý gần nhất của năm gần nhất
  latest_period <- max(df$Năm, na.rm = TRUE)
  
  data_filtered <- df %>%
    dplyr::filter(
      `Quý/Lũy kế` == input$compare_quy_luyke,
      Năm == latest_period
    ) %>%
    dplyr::select(Line, Type, all_of(input$compare_metric)) %>%
    tidyr::pivot_wider(
      names_from = Type,   # 🔥 column = compare_type
      values_from = all_of(input$compare_metric)
    )
  
  # --- Format dữ liệu ---
  if (input$compare_metric %in% c("OsC", "UPR", "Written", "Paid", "@OsC", "@UPR", "@IBNR","@CAT", "Sub total(earned)" , "Sub total(incurred)" )) {
    # Đơn vị: tỷ (1e6)
    data_filtered <- data_filtered %>%
      dplyr::mutate(across(-Line, ~ .x * 1e6))
    excel_file(data_filtered)
    DT::datatable(
      data_filtered,
      options = list(pageLength = 15, scrollX = TRUE, dom = "tpi" ),
      caption = paste("So sánh", input$compare_metric)
    ) %>%
      DT::formatRound(columns = 2:ncol(data_filtered), digits = 0) %>%
      DT::formatCurrency(columns = 2:ncol(data_filtered), currency = "", mark = ",", digits = 0)
    
  } else if (input$compare_metric %in% c("Paid/Written", "Incurred/Earned")) {
    
    data_filtered <- data_filtered %>%
      mutate(across(
        -Line,
        ~ round(as.numeric(.x) * 100, 1)
      ))
    excel_file(data_filtered)
    DT::datatable(
      data_filtered,
      
      options = list(pageLength = 15, scrollX = TRUE, dom = "tpi" ),
      caption = paste("So sánh", input$compare_metric, "từ", input$compare_start_year, "đến", max(data_filtered$Year, na.rm = TRUE))
    ) %>%
      DT::formatRound(columns = 2:ncol(data_filtered), digits = 0) %>%
      DT::formatString(columns = 2:ncol(data_filtered), suffix = "%")
  }
  
})




output$download_compare_excel <- downloadHandler(
  filename = function() {
    paste0("compare_", input$compare_metric, ".xlsx")
  },
  content = function(file) {
    openxlsx::write.xlsx(excel_file(), file)
  }
)

# --- Reactive lưu data đã xử lý để dùng lại ---
# Reactive dữ liệu đã xử lý
compare_data <- reactive({
  req(input$compare_quy_luyke, input$compare_type, input$compare_metric, input$compare_start_year)
  
  df <- data() %>%
    dplyr::mutate(Year = as.numeric(substr(Năm, 1, 4))) %>%
    dplyr::filter(
      `Quý/Lũy kế` == input$compare_quy_luyke,
      Type == input$compare_type,
      Year >= input$compare_start_year
    ) %>%
    dplyr::select(Line, Năm, all_of(input$compare_metric)) %>%
    tidyr::pivot_wider(names_from = Năm, values_from = all_of(input$compare_metric))
  
  if (input$compare_metric %in% c("OsC", "UPR", "Written", "Paid", "@OsC", "@UPR", "@IBNR","@CAT", "Sub total(earned)" , "Sub total(incurred)" )) {
    df <- df %>% dplyr::mutate(across(-Line, ~ .x * 1e6))
  } else if (input$compare_metric %in% c("Paid/Written", "Incurred/Earned")) {
    df <- df %>% dplyr::mutate(across(-Line, ~ round(as.numeric(.x) * 100, 1)))
  }
  
  df
})

# Tạo UI động cho các plot
output$compare_plot_ui <- renderUI({
  df <- compare_data()
  req(nrow(df) > 0)
  
  # Lặp từng line, chia 4 plot/row
  plot_list <- lapply(seq_along(df$Line), function(i){
    ln <- df$Line[i]
    plotname <- paste0("plot_", gsub("[^0-9a-zA-Z]", "_", ln))
    
    column(width = 3,
           plotlyOutput(plotname, height = "300px")
    )
  })
  
  # Chia hàng 4 plot
  n <- length(plot_list)
  rows <- split(plot_list, ceiling(seq_along(plot_list)/4))
  
  ui_rows <- lapply(rows, function(row) {
    fluidRow(row)
  })
  
  do.call(tagList, ui_rows)
})

# Tạo từng bar plot riêng
observe({
  df <- compare_data()
  req(nrow(df) > 0)
  
  for(line_name in unique(df$Line)){
    local({
      ln <- line_name
      #print(ln)
      plotname <- paste0("plot_", gsub("[^0-9a-zA-Z]", "_", ln))
      df_long <- df %>%
        filter(Line == ln) %>%
        pivot_longer(
          cols = -Line,
          names_to = "Year",
          values_to = "Value",
          values_drop_na = FALSE   # giữ lại các giá trị NA
        ) %>%
        mutate(
          Value = as.numeric(Value),
          Year = factor(Year, levels = colnames(df)[-1])
        )
      
      output[[plotname]] <- plotly::renderPlotly({
        plotly::plot_ly(df_long,
                        x = ~Year,
                        y = ~Value,
                        type = "bar",
                        text = ~Value,
                        textposition = "auto"
        ) %>%
          plotly::layout(
            title = ln,
            xaxis = list(title = "Năm"),
            yaxis = list(title = input$compare_metric)
          )
      })
    })
  }
})
