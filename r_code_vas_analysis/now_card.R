output$now_cards <- renderUI({
  req(input$quy_lk,input$namlk, input$quy, input$TYPE,final_data())
  
  df= final_data()
  quy_luy_ke <- input$quy_lk
  year_sel <- input$namlk
  quarter_sel <- input$quy
  type_sel <- input$TYPE
  
  # data for selected period
  df_curr <- df %>%
    filter(`Quý/Lũy kế` == quy_luy_ke, `Năm LK` == year_sel, Qúy == quarter_sel, Type == type_sel)
  
  # df for same quarter previous year
  prev_year <- as.character(as.integer(year_sel) - 1)
  df_prev <- data() %>%
    filter(`Quý/Lũy kế` == quy_luy_ke, `Năm LK` == prev_year, Qúy == quarter_sel, Type == type_sel)
  
  # nows for current period by Line
  now_curr <- df_curr %>%
    group_by(Line) %>%
    summarise(
      WP = sum(Written, na.rm = TRUE),
      PaidRatio = mean(`Paid/Written`, na.rm = TRUE),
      LossRatio = mean(`Incurred/Earned`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # WP for prev period by Line
  now_prev <- df_prev %>%
    group_by(Line) %>%
    summarise(
      WP_prev = sum(Written, na.rm = TRUE),
      .groups = "drop"
    )
  
  # join current + prev
  now <- left_join(now_curr, now_prev, by = "Line")
  
  # compute YOY = (WP - WP_prev) / WP_prev * 100
  now <- now %>%
    mutate(
      YOY = ifelse(is.na(WP_prev) | WP_prev == 0, NA_real_, (WP - WP_prev) / WP_prev * 100)
    )
  # Ép Line theo thứ tự này
  now$Line <- factor(now$Line, levels = line_order)
  
  # Sắp xếp lại theo thứ tự đã định
  now <- now %>% arrange(Line)
  
  # If your dataset already contains a "Total" Line and you want it first:
  if ("Total" %in% now$Line) {
    now <- bind_rows(filter(now, Line == "Total"), filter(now, Line != "Total"))
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
  
  card_divs <- lapply(split(now, seq(nrow(now))), make_card_div)
  
  # one big box containing the grid
  box(
    title = "Lines",
    width = 12,
    div(class = "kpi-grid", card_divs)
  )
})