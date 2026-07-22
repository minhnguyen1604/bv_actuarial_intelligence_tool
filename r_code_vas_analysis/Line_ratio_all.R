
  # UI động cho từng line
  output$all_line_ratios <- renderUI({
    req(input$quy_luy_ke, input$TYPE)
    lines <- unique(data()$Line)
    
    rows <- lapply(seq(1, length(lines), by = 3), function(i) {
      fluidRow(
        lapply(lines[i:min(i+2, length(lines))], function(l) {
          column(
            width = 4,
            box(
              width = 12,
              solidHeader = TRUE,
              title = l,
              status = "primary",
              plotlyOutput(outputId = paste0("plot_", l), height = "250px")
            )
          )
        })
      )
    })
    
    do.call(tagList, rows)
  })
  
  # Vẽ chart cho từng line
  observe({
    req(input$quy_luy_ke, input$TYPE)
    
    df_filtered <- data() %>%
      filter(`Quý/Lũy kế` == input$quy_luy_ke, Type == input$TYPE) %>%
      mutate(YearQuarter = paste0(`Năm LK`,  Qúy)) %>%
      arrange(`Năm LK`, Qúy)
    
    for (l in unique(df_filtered$Line)) {
      local({
        line_name <- l
        output[[paste0("plot_", line_name)]] <- renderPlotly({
          df_line <- df_filtered %>% filter(Line == line_name)
          
          # Rolling average
          df_line <- df_line %>%
            mutate(
              roll_avg = zoo::rollmean(`Incurred/Earned`, k = 4, fill = NA, align = "right")
            )
          
          # Mean ± 2SD
          mean_ratio <- mean(df_line$`Incurred/Earned`, na.rm = TRUE)
          sd_ratio   <- sd(df_line$`Incurred/Earned`, na.rm = TRUE)
          upper_band <- mean_ratio + 2 * sd_ratio
          lower_band <- mean_ratio - 2 * sd_ratio
          # Định dạng sang % với 1 chữ số thập phân
          mean_label <- paste0("Mean = ", scales::percent(mean_ratio, accuracy = 0.1))
          band_label <- paste0("Mean ± 2SD = [", 
                               scales::percent(lower_band, accuracy = 0.1), " , ",
                               scales::percent(upper_band, accuracy = 0.1), "]")
          # Xác định điểm bất thường
          df_line <- df_line %>%
            mutate(flag_outlier = (`Incurred/Earned` > upper_band) | (`Incurred/Earned` < lower_band))
          
          plot_ly(df_line, x = ~YearQuarter) %>%
            add_lines(y = ~`Paid/Written`, name = "Paid/Written", line = list(color = "darkgreen")) %>%
            add_lines(y = ~`Incurred/Earned`, name = "Incurred/Earned", line = list(color = "skyblue")) %>%
            add_lines(y = ~roll_avg, name = "Rolling Avg (4Q)", line = list(color = "red", dash = "dash")) %>%
            add_lines(y = rep(mean_ratio, nrow(df_line)), name = mean_label, line = list(color = "orange", dash = "dot")) %>%
            add_lines(y = rep(upper_band, nrow(df_line)), name = "Mean + 2SD", line = list(color = "gray", dash = "dot"),
                      showlegend = FALSE) %>%
            add_lines(y = rep(lower_band, nrow(df_line)), name = "Mean - 2SD", line = list(color = "gray", dash = "dot"),
                      showlegend = FALSE) %>%
            # Thêm trace dummy chỉ để legend gọn lại
            add_trace(
              x = df_line$YearQuarter[1],   # 1 điểm bất kỳ
              y = upper_band,               # giá trị bất kỳ
              type = "scatter",
              mode = "lines",
              line = list(color = "gray", dash = "dot", width = 1),
              name = "Mean ± 2SD", #band_label,
              showlegend = TRUE
            ) %>%
            add_markers(
              data = df_line %>% filter(flag_outlier),
              x = ~YearQuarter, y = ~`Incurred/Earned`,
              marker = list(color = "red", size = 5, symbol = "circle"),
              name = "Outlier"
            ) %>%
            layout(
              xaxis = list(title = "", tickangle = -45 , tickfont = list(size = 8)),
              yaxis = list(title = "", tickformat = ".0%", tickfont = list(size = 9)),
              legend = list(
                orientation = "h",
                x = 0,
                y = 1.1,
                xanchor = "left",
                yanchor = "bottom",
                font = list(size = 9),
                itemsizing = "constant",
                tracegroupgap = 5,        # khoảng cách nhỏ hơn giữa các item
                itemwidth = 10            # thu nhỏ ô màu (mặc định ~30)
              ),
              margin = list(t = 0, b = 0, l = 0, r = 0)
            )
        })
      })
    }
  })

