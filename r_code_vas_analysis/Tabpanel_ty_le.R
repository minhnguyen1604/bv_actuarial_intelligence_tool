output$tyle_year_ui <- renderUI({
  req(data())
  
  years <- sort(unique(data()$Năm))
  
  selectInput(
    "tyle_year", 
    "Chọn quý:",
    choices = years,
    selected = tail(years, 1)   # 👈 auto chọn cái cuối
  )
})

output$ty_le_quy_luyke_ui <- renderUI({
  req(data())
  df <- data()
  
  selectInput(
    "ty_le_quy_luyke", "Chọn Quý/Lũy kế:",
    choices = unique(df$`Quý/Lũy kế`),
    selected = unique(df$`Quý/Lũy kế`)[1]
  )
})

data_tam <- reactiveVal()
data_loss  <- reactiveVal()

output$compare_nv <- DT::renderDT({
  
  req(input$tyle_line, input$tyle_year, input$ty_le_quy_luyke)
  
  df <- data()  %>%
    mutate(
      Type = factor(Type,
                    levels = c("DIRECT", "RECOVERY", "INWARD", 
                               "RETROCESSION", "NET"))) %>%
    filter(
      Line == input$tyle_line,
      `Năm` == input$tyle_year,
      `Quý/Lũy kế` == input$ty_le_quy_luyke
    )
  
  validate(
    need(nrow(df) > 0, "Không có dữ liệu")
  )
  
  summary_df <- df %>%
    group_by(Type) %>%
    summarise(
      OsC_trich_them = sum(OsC*10^6, na.rm = TRUE),
      UPR_trich_them = sum(UPR*10^6, na.rm = TRUE),
      `@OsC` = sum(`@OsC`*10^6, na.rm = TRUE),
      `@UPR` = sum(`@UPR`*10^6, na.rm = TRUE),
      Written = sum(Written *10^6 , na.rm = TRUE),
      Paid = sum(Paid *10^6, na.rm = TRUE),
      `Sub total(earned)` = sum(`Sub total(earned)` *10^6, na.rm = TRUE),
      `Sub total(incurred)` = sum(`Sub total(incurred)` *10^6, na.rm = TRUE),
      `Incurred/Earned` = sum(`Incurred/Earned` , na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = -Type,
      names_to = "Chỉ tiêu",
      values_to = "Giá trị"
    ) %>%
    arrange(Type) %>%   # 👈 sắp xếp theo thứ tự factor
    tidyr::pivot_wider(
      names_from = Type,
      values_from = `Giá trị`
    )
  data_tam(summary_df)
  
  dt <- DT::datatable(
    summary_df,
    rownames = FALSE,
    options = list(
      dom = 't',
      scrollX = TRUE,
      rowCallback = JS(
        "
      function(row, data, index) {

        // Lấy tổng số cột
        var nCols = $('td', row).length;

        // Earned + Incurred
        if(index != 8){
          for(var i = 1; i < nCols; i++){
            var cell = $('td:eq(' + i + ')', row);
            var val = parseFloat(cell.text());
            if(!isNaN(val)){
              cell.html(val.toLocaleString('en-US', {maximumFractionDigits: 0}));
            }
          }
        }

        // Loss_ratio
        if(index == 8){
          for(var i = 1; i < nCols; i++){
            var cell = $('td:eq(' + i + ')', row);
            var val = parseFloat(cell.text());
            if(!isNaN(val)){
              cell.html((val*100).toFixed(1) + '%');
            }
          }
        }

      }
      "
    )
  ))
  dt
})

output$loss_nv_net <- DT::renderDT({
  
  req(input$tyle_line,  input$ty_le_quy_luyke)
  
  df <- data()  %>%
    filter(
      Line == input$tyle_line,
      Type == "NET",
      `Quý/Lũy kế` == input$ty_le_quy_luyke
    )%>%
    filter(`Năm` %in% tail(unique(`Năm`), 9)) 
   # filter(as.numeric(substr(`Năm`, 1, 4)) >= 2024)  # 👈 lấy từ 2024
  
  validate(
    need(nrow(df) > 0, "Không có dữ liệu")
  )
  
  summary_df <- df %>%
    group_by(Năm) %>%
    summarise(
      Earned = sum(`Sub total(earned)` *10^6, na.rm = TRUE),
      Incurred = sum(`Sub total(incurred)` *10^6, na.rm = TRUE),
      Loss_ratio = ifelse(Earned == 0, NA, Incurred / Earned),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = - `Năm` ,
      names_to = "Chỉ tiêu",
      values_to = "Giá trị"
    ) %>%
    arrange(`Năm`) %>%   # 👈 sắp xếp theo thứ tự factor
    tidyr::pivot_wider(
      names_from = `Năm`,
      values_from = `Giá trị`
    )
  
  
  summary_df$`Chỉ tiêu` <- factor(
    summary_df$`Chỉ tiêu`,
    levels = c("Earned", "Incurred", "Loss_ratio")
  )
  
  summary_df <- summary_df %>%
    arrange(`Chỉ tiêu`)
  
  data_loss(summary_df)
  
  value_cols <- setdiff(colnames(summary_df), "Chỉ tiêu")
  datatable(
    summary_df,
    rownames = FALSE,
    options = list(
      dom = "t",
      scrollX = TRUE,
      rowCallback = JS(
        "
      function(row, data, index) {

        // Lấy tổng số cột
        var nCols = $('td', row).length;

        // Earned + Incurred
        if(index == 0 || index == 1){
          for(var i = 1; i < nCols; i++){
            var cell = $('td:eq(' + i + ')', row);
            var val = parseFloat(cell.text());
            if(!isNaN(val)){
              cell.html(val.toLocaleString('en-US', {maximumFractionDigits: 0}));
            }
          }
        }

        // Loss_ratio
        if(index == 2){
          for(var i = 1; i < nCols; i++){
            var cell = $('td:eq(' + i + ')', row);
            var val = parseFloat(cell.text());
            if(!isNaN(val)){
              cell.html((val*100).toFixed(1) + '%');
            }
          }
        }

      }
      "
      )
    )
  )
  

})


output$loss_nv_goc <- DT::renderDT({
  
  req(input$tyle_line,  input$ty_le_quy_luyke)
  
  df <- data()  %>%
    filter(
      Line == input$tyle_line,
      Type == "DIRECT",
      `Quý/Lũy kế` == input$ty_le_quy_luyke
    )%>%
    filter(`Năm` %in% tail(unique(`Năm`), 9)) 
  # filter(as.numeric(substr(`Năm`, 1, 4)) >= 2024)  # 👈 lấy từ 2024
  
  validate(
    need(nrow(df) > 0, "Không có dữ liệu")
  )
  
  summary_df <- df %>%
    group_by(Năm) %>%
    summarise(
      Earned = sum(`Sub total(earned)` *10^6, na.rm = TRUE),
      Incurred = sum(`Sub total(incurred)` *10^6, na.rm = TRUE),
      Loss_ratio = ifelse(Earned == 0, NA, Incurred / Earned),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = - `Năm` ,
      names_to = "Chỉ tiêu",
      values_to = "Giá trị"
    ) %>%
    arrange(`Năm`) %>%   # 👈 sắp xếp theo thứ tự factor
    tidyr::pivot_wider(
      names_from = `Năm`,
      values_from = `Giá trị`
    )
  
  
  summary_df$`Chỉ tiêu` <- factor(
    summary_df$`Chỉ tiêu`,
    levels = c("Earned", "Incurred", "Loss_ratio")
  )
  
  summary_df <- summary_df %>%
    arrange(`Chỉ tiêu`)
  
  data_loss(summary_df)
  
  value_cols <- setdiff(colnames(summary_df), "Chỉ tiêu")
  datatable(
    summary_df,
    rownames = FALSE,
    options = list(
      dom = "t",
      scrollX = TRUE,
      rowCallback = JS(
        "
      function(row, data, index) {

        // Lấy tổng số cột
        var nCols = $('td', row).length;

        // Earned + Incurred
        if(index == 0 || index == 1){
          for(var i = 1; i < nCols; i++){
            var cell = $('td:eq(' + i + ')', row);
            var val = parseFloat(cell.text());
            if(!isNaN(val)){
              cell.html(val.toLocaleString('en-US', {maximumFractionDigits: 0}));
            }
          }
        }

        // Loss_ratio
        if(index == 2){
          for(var i = 1; i < nCols; i++){
            var cell = $('td:eq(' + i + ')', row);
            var val = parseFloat(cell.text());
            if(!isNaN(val)){
              cell.html((val*100).toFixed(1) + '%');
            }
          }
        }

      }
      "
      )
    )
  )
  
  
})

output$comment_nv <- renderUI({
  
  req(data_tam())
  
  df <- data_tam()
  # Loại dòng không cần so (nếu có)
  df_check <- df %>% 
    filter(!is.na(DIRECT), !is.na(RECOVERY))
  
  # Điều kiện 1: |DIRECT| > |RECOVERY|
  cond_abs <- abs(df_check$DIRECT) > abs(df_check$RECOVERY)
  
  # Điều kiện 2: cùng dấu
  cond_sign <- sign(df_check$DIRECT) == sign(df_check$RECOVERY)
  
  all_abs  <- all(cond_abs)
  all_sign <- all(cond_sign)
  
  # Tìm dòng vi phạm
  violated_abs  <- df_check$`Chỉ tiêu`[!cond_abs]
  violated_sign <- df_check$`Chỉ tiêu`[!cond_sign]
  
  # Lấy giá trị Direct và Recovery
  written_direct   <- df %>% filter(`Chỉ tiêu` == "Written") %>% pull(DIRECT)
  written_recovery <- df %>% filter(`Chỉ tiêu` == "Written") %>% pull(RECOVERY)
  
  paid_direct   <- df %>% filter(`Chỉ tiêu` == "Paid") %>% pull(DIRECT)
  paid_recovery <- df %>% filter(`Chỉ tiêu` == "Paid") %>% pull(RECOVERY)
  
  # Tính tỷ lệ
  ratio_written <- written_recovery / written_direct
  ratio_paid    <- paid_recovery / paid_direct
  
  diff_ratio <- abs(ratio_written - ratio_paid)
  
  tagList(
    h4("Kiểm tra tính hợp lý Direct vs Recovery"),
    
    if (all_abs) {
      p("✔ Ở tất cả các chỉ tiêu: |Direct| luôn lớn hơn |Recovery|.")
    } else {
      p(
        paste0(
          "⚠ Các chỉ tiêu có |Recovery| ≥ |Direct|: ",
          paste(violated_abs, collapse = ", ")
        )
      )
    },
    
    if (all_sign) {
      p("✔ Direct và Recovery luôn cùng dấu.")
    } else {
      p(
        paste0(
          "⚠ Các chỉ tiêu có lệch dấu: ",
          paste(violated_sign, collapse = ", ")
        )
      )
    },
    
    p(
      paste0(
        "Tỷ lệ Recovery/Direct (Written): ",
        scales::percent(ratio_written, accuracy = 0.1)
      )
    ),
    
    p(
      paste0(
        "Tỷ lệ Recovery/Direct (Paid): ",
        scales::percent(ratio_paid, accuracy = 0.1)
      )
    )
  )
})
