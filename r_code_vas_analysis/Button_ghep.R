final_data <- reactiveVal(NULL)

observeEvent(input$ghep1, {
  removeModal()   # đóng modal
  # Code xử lý tiếp ở đây
  #showNotification("Đang chuyển đổi dữ liệu...", type = "message")
  enable("update")
  req(combined_data(),input$namlk, input$quy)
  q_n =  paste0(input$namlk, input$quy)
  prev_q_n = prev_quarter(q_n)
  
  #saveRDS(combined_data(),"D:\\R_Project\\ALCO\\combined_data.rds" )
  
  df_2024Q1 <- combined_data() %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>%
    select(Line, Type, `@UPR`, `@OsC`, `@IBNR`, `@CAT`) #giữ các chỉ tiêu cần

  
  df_2023Q4 <- data() %>%
    filter(Năm == prev_q_n & `Quý/Lũy kế` == "Theo quý" ) %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>% 
    select(Line, Type, `@UPR`, `@OsC`)   # giữ các chỉ tiêu cần
  print(prev_q_n)
  
  # Join 2 quý theo Line + Type
  df_compare <- df_2024Q1 %>%
    left_join(df_2023Q4, by = c("Line", "Type"),
              suffix = c("", "_2023Q4")) %>%
    mutate(
      UPR = `@UPR` - `@UPR_2023Q4`,
      OsC = `@OsC` - `@OsC_2023Q4`
    )
  df_compare <- df_compare %>% select( -c(`@UPR_2023Q4`,`@OsC_2023Q4` ))
  
  if (input$quy =="Q1"){                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
    df_now = df_compare
    
  }else{
    
    df_compare_lk = df_compare
    
    df_2023Q4 <- data() %>%
      filter(Năm == prev_q_n & `Quý/Lũy kế` == "Lũy kế" ) %>%
      mutate(
        Line = str_trim(Line),
        Type = str_trim(Type)
      ) %>% 
      select(Line, Type, UPR, OsC, `@IBNR`, `@CAT`)  # giữ các chỉ tiêu cần 
    
    df_now <- df_compare_lk %>%
      left_join(df_2023Q4, by = c("Line", "Type"),
                suffix = c("_2024Q1", "_2023Q4")) %>%
      mutate(
        UPR = `UPR_2024Q1` + `UPR_2023Q4`,
        OsC = `OsC_2024Q1` + `OsC_2023Q4`,
        `@IBNR` = `@IBNR_2024Q1`,
        `@CAT` =`@CAT_2024Q1`
      )
    df_now <- df_now %>% select( -c(`UPR_2023Q4`,`OsC_2023Q4`,`OsC_2024Q1`, `UPR_2024Q1` , `@IBNR_2023Q4`,`@IBNR_2024Q1`,`@CAT_2023Q4`,`@CAT_2024Q1`))
  }
  
#_____________________________________________________  written paid
  
  df_2024Q1 <- combined_data() %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>%
    select(Line, Type,  Written, Paid)   # giữ các chỉ tiêu cần
  
  df_now <- df_now %>%
    left_join(df_2024Q1, by = c("Line", "Type"))
  
  if (input$quy =="Q1"){  
    # Join 2 quý theo Line + Type
    df_compare <- df_compare %>%
      left_join(df_2024Q1, by = c("Line", "Type"))
  }else{
    
  df_2023Q4 <- data() %>%
    filter(Năm == prev_q_n & `Quý/Lũy kế` == "Lũy kế" ) %>%          # "Theo quý" ) %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>% 
    select(Line, Type,  Written, Paid)   # giữ các chỉ tiêu cần
  
  # Join 2 quý theo Line + Type
  df_tam <- df_2024Q1 %>%
    left_join(df_2023Q4, by = c("Line", "Type"),
              suffix = c("_2024Q1", "_2023Q4")) %>%
    mutate(
      Written = `Written_2024Q1`- `Written_2023Q4`,
      Paid = `Paid_2024Q1` - `Paid_2023Q4`
    )
  df_tam <- df_tam %>% select(Line, Type,  Written, Paid) 
  df_compare <- df_compare %>%
    left_join(df_tam, by = c("Line", "Type"))
  
  }
  
  df_compare$`Quý/Lũy kế` = "Theo quý"
  df_compare$`Năm LK` = input$namlk
  df_compare$`Qúy` = input$quy
  df_now$`Quý/Lũy kế` = "Lũy kế"
  df_now$`Năm LK` = input$namlk
  df_now$`Qúy` = input$quy
  print(setdiff(names(df_compare), names(df_now)))
  print(setdiff(names(df_now), names(df_compare)))
  
  he= rbind(df_compare, df_now)    # df_compare: theo quý, df_now: lũy kế
  #______________________________________________________ 4Q
  
  df_current <- df_compare %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>%
    select(Line, Type, `@UPR`, `@OsC`)
  
  df_ibnr <- df_compare %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>%
    select(Line, Type, `@IBNR`, `@CAT`)
  
  last4 <- get_last_4_quarters(q_n)
  print(last4)


  df_4Q <- data() %>%
    filter(Năm %in% last4, `Quý/Lũy kế` == "Theo quý") %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>%
    group_by(Line, Type) %>%
    summarise(
      Written = sum(Written, na.rm = TRUE),
      Paid = sum(Paid, na.rm = TRUE),
      .groups = "drop"
    )

  prev_q_4 <- prev_quarter(prev_quarter(prev_quarter(prev_quarter(q_n))))

  df_lag4 <- data() %>%
    filter(Năm == prev_q_4, `Quý/Lũy kế` == "Theo quý") %>%
    mutate(
      Line = str_trim(Line),
      Type = str_trim(Type)
    ) %>%
    select(Line, Type,
           `@UPR_lag4` = `@UPR`,
           `@OsC_lag4` = `@OsC`)

  df_compare_4Q <- df_4Q %>%
    left_join(df_current, by = c("Line", "Type")) %>%   # 👈 THÊM DÒNG NÀY
    left_join(df_lag4, by = c("Line", "Type")) %>%
    mutate(
      UPR = `@UPR` - `@UPR_lag4`,
      OsC = `@OsC` - `@OsC_lag4`
    ) %>%
    select(-c(`@UPR_lag4`, `@OsC_lag4`))
  
  df_compare_4Q <- df_compare_4Q %>%
    left_join(df_ibnr, by = c("Line", "Type"))

  df_compare_4Q$`Quý/Lũy kế` = "4Q"
  df_compare_4Q$`Năm LK` = input$namlk
  df_compare_4Q$`Qúy` = input$quy
  
  # print(colnames(he))
  # print(colnames(df_compare_4Q))

  he= rbind(he, df_compare_4Q)
  
  
  
  
  #___________________________________
  he$`Năm` = q_n
  he$`Sub total(incurred)` = he$Paid + he$OsC
  he$`Sub total(earned)` = he$Written - he$UPR
  he$`Paid/Written` <- ifelse(he$Written == 0, 0, he$Paid / he$Written)
  he$`Incurred/Earned` <- ifelse(he$`Sub total(earned)` == 0, 0,
                                 he$`Sub total(incurred)` / he$`Sub total(earned)`)
  # Ghép thêm cột Nghiệp vụ và Nhóm NV vào df gốc
  he <- he %>%
    left_join(line_map, by = "Line")
  
  
  
  # Lưu RDS (đúng đuôi file)
  saveRDS(
    he,
    file = paste0("D:/R_Project/ALCO/cur_data/", input$namlk, input$quy, ".rds")
  )
  
  final_data(he) 
  
})

output$combined <- renderDT({
  req(final_data())
  df <- final_data()
  df <- df[, colnames(data())[colnames(data()) %in% colnames(df)], drop = FALSE]
  df$Line <- factor(df$Line, levels = line_order2)
  df <- df[order(df$Line), ]
  df <- df %>% select(-c(`Năm LK`,`Qúy` ))
  
  dt = datatable(
    df,
    rownames = FALSE,
    options = list(
      #scrollX = TRUE,
      scrollY = "500px",
      paging = FALSE,
      ordering = TRUE,
      autoWidth = TRUE,     # hoặc FALSE nếu bạn muốn chỉ dùng width ở columnDefs
      fixedHeader = TRUE,
      dom = 't',
      columnDefs = list(
        list(width = '40px',  targets = 0),
        list(width = '100px', targets = 1),
        list(width = '80px',  targets = 2),
        list(width = '100px', targets = 3),
        list(width = '40px',  targets = 4)
      ),
      initComplete = JS(
        "function(settings) {",
        "  var api = this.api();",
        "  $(api.table().header()).find('th').css({",
        "    'white-space': 'normal',",
        "    'overflow-wrap': 'anywhere',",
        "    'word-break': 'break-word',",
        "    'line-height': '1.2'",
        "  });",
        "  var header = $(api.table().header());",
        "  var filterRow = header.clone().appendTo(header.parent());",
        "  $(filterRow).addClass('dt-filter-row');",
        "  $(filterRow).find('th').each(function(i) {",
        "    $(this).html('');",
        "    var column = api.column(i);",
        "    var select = $('<select><option value=\"\"></option></select>')",
        "      .appendTo($(this))",
        "      .on('change', function() {",
        "        var val = $.fn.dataTable.util.escapeRegex($(this).val());",
        "        column.search(val ? '^'+val+'$' : '', true, false).draw();",
        "      });",
        "    column.data().unique().sort().each(function(d, j) {",
        "      if(d != null && d != '') select.append('<option value=\"'+d+'\">'+d+'</option>');",
        "    });",
        "  });",
        "  setTimeout(function(){ api.columns.adjust(); }, 0);",
        "}"
      )
    ),
    escape = FALSE,
    selection = "none",
    class = "display nowrap compact"  # vẫn giữ nowrap cho body; header đã override bằng CSS
  )
  
  dt <- formatRound(dt, columns = c(6:11, 14:15), digits = 2)
  dt <- formatPercentage(dt, columns = c(12:13), digits = 2)
  dt
})






