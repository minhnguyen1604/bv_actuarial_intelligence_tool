output$kpi_cards <- renderUI({
  req(input$quy_luy_ke,input$year, input$quarter, input$type)
  quy_luy_ke <- input$quy_luy_ke
  year_sel <- input$year
  quarter_sel <- input$quarter
  type_sel <- input$type
  
  # data for selected period
  df_curr <- data() %>%
    filter(`Quý/Lũy kế` == quy_luy_ke, `Năm LK` == year_sel, Qúy == quarter_sel, Type == type_sel)
  
  # data for same quarter previous year
  prev_year <- as.character(as.integer(year_sel) - 1)
  df_prev <- data() %>%
    filter(`Quý/Lũy kế` == quy_luy_ke, `Năm LK` == prev_year, Qúy == quarter_sel, Type == type_sel)
  
  # KPIs for current period by Line
  kpi_curr <- df_curr %>%
    group_by(Line) %>%
    summarise(
      WP = sum(Written, na.rm = TRUE),
      PaidRatio = mean(`Paid/Written`, na.rm = TRUE),
      LossRatio = mean(`Incurred/Earned`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # WP for prev period by Line
  kpi_prev <- df_prev %>%
    group_by(Line) %>%
    summarise(
      WP_prev = sum(Written, na.rm = TRUE),
      .groups = "drop"
    )
  
  # join current + prev
  kpi <- left_join(kpi_curr, kpi_prev, by = "Line")
  
  # compute YOY = (WP - WP_prev) / WP_prev * 100
  kpi <- kpi %>%
    mutate(
      YOY = ifelse(is.na(WP_prev) | WP_prev == 0, NA_real_, (WP - WP_prev) / WP_prev * 100)
    )
  # Ép Line theo thứ tự này
  kpi$Line <- factor(kpi$Line, levels = line_order)
  
  # Sắp xếp lại theo thứ tự đã định
  kpi <- kpi %>% arrange(Line)
  
  # If your dataset already contains a "Total" Line and you want it first:
  if ("Total" %in% kpi$Line) {
    kpi <- bind_rows(filter(kpi, Line == "Total"), filter(kpi, Line != "Total"))
  }
  
  # render cards (one big box containing a grid of sub-cards)
  make_card_div <- function(row) {
    # careful with NA YOY
    yoy_val <- row$YOY
    if (is.na(yoy_val)) {
      yoy_text <- "NA NA%"
      yoy_class <- ""
    } else {
      yoy_val_r <- round(yoy_val, 1)
      yoy_icon <- if (yoy_val_r >= 0) "▲" else "▼"
      yoy_text <- paste0(yoy_icon, " ", formatC(yoy_val_r, format = "f", digits = 1), "%")
      yoy_class <- if (yoy_val_r >= 0) "kpi-green" else "kpi-red"
    }
    
    icon_html <- line_icons[[row$Line]]
    # if icon missing, fallback to empty span
    if (is.null(icon_html)) icon_html <- span()
    
    div(class="kpi-card",
        div(class="kpi-title", icon_html, row$Line),
        div(class="kpi-row", span("WP"), span(formatC(row$WP, format = "f", digits = 1, big.mark = ","))),
        div(class="kpi-row", span("YOY %"), span(class = yoy_class, yoy_text)),
        div(class="kpi-row", span("Paid ratio"), span(scales::percent(row$PaidRatio, accuracy = 0.1))),
        div(class="kpi-row", span("Loss ratio"), span(scales::percent(row$LossRatio, accuracy = 0.1)))
    )
  }
  
  card_divs <- lapply(split(kpi, seq(nrow(kpi))), make_card_div)
  
  # one big box containing the grid
  box(
    title = "Lines",
    width = 12,
    div(class = "kpi-grid", card_divs)
  )
})

data_filtered <- reactive({
  df <- data() %>%
    filter(
      `Quý/Lũy kế` == input$quy_luy_ke,
      Qúy == input$quarter,
      Type == input$type
    ) %>%
    group_by(Line, `Năm LK`) %>%
    summarise(WP = sum(Written, na.rm = TRUE), .groups = "drop")
  #saveRDS(df, "D:\\R_Project\\ALCO\\test.rds")
  # Tính YOY
  df <- df %>%
    arrange(Line, `Năm LK`) %>%
    group_by(Line) %>%
    mutate(YOY = (WP - lag(WP)) / lag(WP) * 100) %>%
    ungroup()
  
  # Pivot wider
  df_wide <- tidyr::pivot_wider(
    df,
    id_cols = Line,
    names_from = `Năm LK`,
    values_from = c(WP, YOY),
    names_glue = "{`Năm LK`}_{.value}"
  )
  
  
  # Sắp xếp lại cột
  years <- sort(unique(df$`Năm LK`))
  new_order <- c("Line", as.vector(t(outer(years, c("WP","YOY"), paste, sep="_"))))
  # df_wide <- df_wide[, new_order]
  # safe select: giữ thứ tự new_order nhưng bỏ qua cột không tồn tại
  df_wide <- df_wide %>% select(any_of(new_order))
  df_wide$Line <- factor(df_wide$Line, levels = line_order2)
  
  df_wide <- df_wide[order(df_wide$Line), ]
  return(df_wide)
})

output$table <- DT::renderDataTable({
  df_wide <- data_filtered()
  
  # Lấy danh sách năm từ tên cột
  years <- unique(gsub("_(WP|YOY)$", "", colnames(df_wide)))
  years <- years[years != "Line"]
  
  # Format YOY thành HTML
  for (y in years) {
    col <- paste0(y, "_YOY")
    if (col %in% names(df_wide)) {
      df_wide[[col]] <- sapply(df_wide[[col]], function(val) {
        if (is.na(val)) return("<span style='color:gray'>NA</span>")
        if (val > 0) {
          sprintf("<span style='color:green'>▲ %.1f%%</span>", val)
        } else if (val < 0) {
          sprintf("<span style='color:red'>▼ %.1f%%</span>", val)
        } else {
          "<span style='color:gray'>0.0%%</span>"
        }
      })
    }
  }
  
  df_wide = df_wide %>% select(-`2020_YOY` )
  dt= DT::datatable(
    df_wide ,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      dom = 't',        # chỉ hiển thị table (không có search, không có show entries, không có paging)
      paging = FALSE,   # tắt phân trang
      ordering = FALSE, # <<--- tắt sort (mũi tên ↑↓)
      scrollX = TRUE,   # cho phép scroll ngang
      autoWidth = TRUE,
      fixedColumns = list(leftColumns = 1), # freeze cột đầu tiên
      scrollCollapse = TRUE
    ),
    extensions = c('FixedColumns')
  )
  n = 1: round((ncol(df_wide) )/2)
  tam =  2*n-1
  tam[1] = 2
  dt <- formatRound(dt, columns =  tam, digits = 2)
  #dt <- formatPercentage(dt, columns = c(3,5,7,9), digits = 2)
  dt 
})



#___________________________________________________________View
output$line_bar <- renderPlotly({
  req(input$quy_luy_ke,input$type)
  quy_luy_ke <- input$quy_luy_ke
  type_sel <- input$type

  df = data() %>%
    filter(`Quý/Lũy kế` == quy_luy_ke,  Type == type_sel)

  df_line <- df %>%
    filter(Line == input$line) %>%
    mutate(YearQuarter = paste0(`Năm LK`, "_", Qúy)) %>%
    arrange(`Năm LK`, Qúy)

  colorway <- c("blue", "yellow","darkorange","darkgreen")
  plot_ly(df_line, x = ~YearQuarter) %>%
    add_bars(y = ~Paid, name = "Paid") %>%
    add_bars(y = ~`Sub total(incurred)`, name = "Incurred Loss") %>%
    add_bars(y = ~Written, name = "Written") %>%
    add_bars(y = ~`Sub total(earned)`, name = "Earned Premium") %>%
    layout(
      barmode = "group",
      colorway = colorway,
      #title = paste("Premium & Loss -", input$line),
      
      title = list(
      text = paste("Premium & Loss -", input$line),
      y = 1.25,              # 👈 đẩy tiêu đề lên cao hơn (mặc định là ~0.9)
      x = 0.5,
      xanchor = "center"
      ),
  
      xaxis = list(
        #title = "Year & Quarter",
        title = "",  
        tickfont = list(size = 10),  # 👈 giảm kích thước chữ
        tickangle = -45              # 👈 nghiêng nhãn cho dễ đọc
      ),
      yaxis = list(
        title = "", 
        tickfont = list(size = 10)    # 👈 nhỏ chữ trục y nếu muốn
      ),
      
      legend = list(
        orientation = "h",       # 👈 horizontal
        x = 0.5,                 # 👈 căn giữa theo chiều ngang
        xanchor = "center",
        y = 1.05                 # 👈 đặt phía trên biểu đồ
      )
      
    )
  

})
output$line_ratio <- renderPlotly({
  req(input$quy_luy_ke, input$type)
  quy_luy_ke <- input$quy_luy_ke
  type_sel <- input$type
  df <- data() %>%
    filter(`Quý/Lũy kế` == quy_luy_ke, Type == type_sel)
  
  df_line <- df %>%
    filter(Line == input$line) %>%
    mutate(YearQuarter = paste0(`Năm LK`, "Q", Qúy)) %>%
    arrange(`Năm LK`, Qúy)
  
  plot_ly(df_line, x = ~YearQuarter) %>%
    add_lines(y = ~`Paid/Written`, name = "Paid/Written", line = list(color = "darkgreen")) %>%
    add_lines(y = ~`Incurred/Earned`, name = "Incurred/Earned", line = list(color = "skyblue")) %>%
    layout(
      title = list(
        text = paste("Ratios -", input$line),
        y = 1.25,              # 👈 đẩy tiêu đề lên cao hơn (mặc định là ~0.9)
        x = 0.5,
        xanchor = "center"
      ),
      
      xaxis = list(
        #title = "Year & Quarter",
        title = "",  
        tickfont = list(size = 10),  # 👈 giảm kích thước chữ
        tickangle = -45              # 👈 nghiêng nhãn cho dễ đọc
      ),
      yaxis = list(
        title = "", 
        #title = "Ratio (%)",
        tickformat = ".0%",           # 👈 hiển thị phần trăm, không có chữ số thập phân
        tickfont = list(size = 10)    # 👈 nhỏ chữ trục y nếu muốn
      ),
      legend = list(
        orientation = "h",       # 👈 horizontal
        x = 0.5,                 # 👈 căn giữa theo chiều ngang
        xanchor = "center",
        y = 1.05                 # 👈 đặt phía trên biểu đồ
      )
      
      
    )
})
