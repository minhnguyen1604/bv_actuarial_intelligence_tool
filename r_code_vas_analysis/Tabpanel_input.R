combined_data <- reactiveVal(NULL)
#________________________________
# Reactive lấy tên sheet
sheet_names <- reactive({
  req(input$file1)
  
  getSheetNames(input$file1$datapath)
})

# UI chọn sheet
output$sheet_selector1 <- renderUI({
  req(sheet_names())
  selectInput("sheet1", "Chọn sheet:", choices = sheet_names())
})
# UI chọn sheet
output$sheet_selector2 <- renderUI({
  req(sheet_names())
  selectInput("sheet2", "Chọn sheet:", choices = sheet_names())
})


# Reactive đọc Excel
excel_data <- reactive({
  req(input$file1, input$sheet1)
  read_excel(
    input$file1$datapath,
    sheet = input$sheet1,
    col_types = "text"  # tất cả thành character
  )
})


# Reactive đọc Excel
dtbt_data <- reactive({
  req(input$file1, input$sheet1)
  read_excel(
    input$file1$datapath,
    sheet = input$sheet2,
    range = "A1:AD500",   # cần cả hàng và cột
    col_types = "text"  # tất cả thành character
  )
})
# Disable check1 ban đầu
disable("check1")
disable("update")
# Quan sát cả 2 sheet, chỉ enable khi đều đã chọn
observe({
  if (!is.null(input$sheet1) && input$sheet1 != "" &&
      !is.null(input$sheet2) && input$sheet2 != "") {
    enable("check1")
  } else {
    disable("check1")
  }
})

observeEvent(input$check1, {
  req(input$file1, input$sheet1, input$sheet2, excel_data(), input$namlk, input$quy)
  
  df <- excel_data()
  dtbt <- dtbt_data()
  
  # Lấy cột thứ 8
  col8 <- dtbt[[8]]
  
  # Kiểm tra xem có ô nào chứa "code" (không phân biệt hoa/thường)
  has_code <- any(grepl("code", col8, ignore.case = TRUE))
  
  if (!has_code | ncol(dtbt) <16) {
    showModal(modalDialog(
      title = "Sai định dạng dữ liệu",
      "File Excel bạn chọn không đúng form yêu cầu. Vui lòng kiểm tra lại sheet, hoặc chọn lại file.",
      easyClose = TRUE,
      footer = modalButton("Đóng")
    ))
    return()
  }
  
  
  if (!check_form(df)) {
    showModal(modalDialog(
      title = "Sai định dạng dữ liệu",
      "File Excel bạn chọn không đúng form yêu cầu. Vui lòng kiểm tra lại sheet, hoặc chọn lại file.",
      easyClose = TRUE,
      footer = modalButton("Đóng")
    ))
    return()
  }
  enable("ghep1")
  withProgress(message = "Đang kiểm tra dữ liệu...", value = 0, {
    # Nếu không đúng form thì hiện modal yêu cầu chọn lại
    
    incProgress(0.1, detail = "Đang đọc dữ liệu Excel...")
    rows <- which(grepl("Line", pull(df, 1), ignore.case = TRUE))
    i <- rows[1]
    
    vt <- which(grepl("Direct|Inward|Recovery|Retro|Net", 
                      as.character(unlist(df[i, ])), ignore.case = TRUE))[1:5]
    
    df <- df[, c(1, vt)]
    
    print(vt)
    
    # Xác định vị trí bắt đầu
    starts <- c(
      upr  = which(grepl("UPR", df[[1]], ignore.case = TRUE)),
      osc  = which(grepl("OSC", df[[1]], ignore.case = TRUE)),
      ibnr = which(grepl("IBNR", df[[1]], ignore.case = TRUE)),
      cat  = which(grepl("CAT", df[[1]], ignore.case = TRUE))
    )
    starts <- sort(starts)
    
    # Cắt dữ liệu thành list
    df_list <- purrr::map2(
      starts, dplyr::lead(starts, default = nrow(df) + 1) - 1,
      ~ df[.x:.y, ]
    )
    
    names(df_list) <- names(starts)
    df_list$cat <- df_list$cat[, 1:2] 
    
    upr_long <- process_block(df_list$upr, "UPR")
    osc_long <- process_block(df_list$osc, "OSC")
    print("0")
    ibnr_long <- process_block(df_list$ibnr, "IBNR")
    cat_long  <- process_block_cat(df_list$cat, "CAT")
    upr_long <- process_block(df_list$upr, "UPR") %>%
      group_by(Line, Type) %>%
      summarise(UPR = sum(UPR, na.rm = TRUE), .groups = "drop")
    
    osc_long <- process_block(df_list$osc, "OSC") %>%
      group_by(Line, Type) %>%
      summarise(OSC = sum(OSC, na.rm = TRUE), .groups = "drop")
    
    ibnr_long <- process_block(df_list$ibnr, "IBNR") %>%
      group_by(Line, Type) %>%
      summarise(IBNR = sum(IBNR, na.rm = TRUE), .groups = "drop")
    
    cat_long <- process_block_cat(df_list$cat, "CAT") %>%
      group_by(Line, Type) %>%
      summarise(CAT = sum(CAT, na.rm = TRUE), .groups = "drop")
    
    
    
    # Kết hợp
    combined_long <- upr_long %>%
      full_join(osc_long,  by = c("Line", "Type")) %>%
      full_join(ibnr_long, by = c("Line", "Type")) %>%
      full_join(cat_long,  by = c("Line", "Type"))

    
    # Check sự khớp Line
    missing_lines <- setdiff(unique(data()$Line), unique(combined_long$Line))
    print(missing_lines)
     
    # Nếu thiếu Travel thì thêm Travel = 0
    if ("Travel" %in% missing_lines) {
      travel_add <- combined_long %>%
        distinct(Type) %>%              # lấy các Type đang có
        mutate(Line = "Travel",
               UPR = 0,
               OSC = 0)
      
      combined_long <- bind_rows(combined_long, travel_add)
    }
    
    # Cộng gộp Healthcare + Travel
    row_add <- combined_long %>%
      filter(Line %in% c("Healthcare", "Travel")) %>%
      group_by(Type) %>%
      summarise(
        Line = "Healthcare + Travel",
        UPR = sum(UPR, na.rm = TRUE),
        OSC = sum(OSC, na.rm = TRUE),
        IBNR = sum(IBNR, na.rm = TRUE),
        CAT = sum(CAT, na.rm = TRUE),
        .groups = "drop"
      )
    
    combined_long <- bind_rows(combined_long, row_add) %>%
      group_by(Line, Type) %>%
      summarise(
        `@UPR` = sum(UPR / 1e6, na.rm = TRUE),
        `@OsC` = sum(OSC / 1e6, na.rm = TRUE),
        `@IBNR` = sum(IBNR / 1e6, na.rm = TRUE),
        `@CAT` = sum(CAT / 1e6, na.rm = TRUE),
        .groups = "drop"
      )

    #____________________________________________________________sheet DT-BT
    # vector các từ khóa (match chính xác)
    keys <- c("CODE", "PHIGOC", "NHAN", "NHUONG CUA GOC","NHUONG CUA NHAN",  "HOANPHI","GIAMPHI", "PHI_GLAI")
    
    # tìm dòng nào chứa header thật sự
    row_idx <- which(apply(dtbt, 1, function(x) any(x %in% keys)))
    
    # lấy dòng header
    header_row <- as.character(unlist(dtbt[row_idx[1], ]))
    
    # gán làm colnames
    colnames(dtbt) <- header_row
    
    # # bỏ những dòng header và phía trên
    # dtbt <- dtbt[-c(1:row_idx[1]), ]
    
    # chỉ giữ đúng các cột có tên trong keys
    dtbt <- dtbt[, names(dtbt) %in% keys]
    
    
    rows <- which(grepl("BT", pull(dtbt, 1), ignore.case = TRUE))
    dt  <- dtbt[1:(rows-1), ]
    bt  <- dtbt[(rows+1): nrow(dtbt), ]
    # Hàm clean chung
    
    clean_dtbt <- function(dtbt) {
      colnames(dtbt) <- c("Line", "DIRECT", "INWARD","RECOVERY","RETROCESSION", "HOANPHI","GIAMPHI", "NET")
      
      dtbt <- dtbt[-1, ] %>%
        filter(!is.na(Line), Line != "")
      # Giả sử data của bạn tên là df
      vt = which(dtbt[[1]] == "Z")[1]
      if (vt == 12){
        hehe <- c("Cargo in transit", "Hull & PI", "Aviation & Oil", "Aviation & Oil",
                  "Engineering", "Fire and Misc.", "General Liability", "Motor Vehicles",
                  "Personal Accident",  "Healthcare", "Agriculture")
        dtbt <- dtbt[1:11,]
        dtbt$Line <- hehe
        # Tạo dòng mới "Travel" với các cột khác rỗng
        new_row <- dtbt[1, ]              # copy cấu trúc
        new_row[,] <- ""                  # tất cả = ""
        new_row$Line <- "Travel"          # riêng Line = "Travel"
        
        # Gắn thêm vào cuối
        dtbt <- rbind(dtbt, new_row)
      }else{
        hehe <- c("Cargo in transit", "Hull & PI", "Aviation & Oil", "Aviation & Oil",
                  "Engineering", "Fire and Misc.", "General Liability", "Motor Vehicles",
                  "Personal Accident", "Travel", "Healthcare", "Agriculture")
        dtbt <- dtbt[1:12,]
        dtbt$Line <- hehe
        
      }
      dtbt <- dtbt %>% mutate(across(c(DIRECT, INWARD,RECOVERY,RETROCESSION, HOANPHI, GIAMPHI, NET), as.numeric))
      
      dtbt$DIRECT = dtbt$DIRECT - dtbt$HOANPHI -dtbt$GIAMPHI 
      dtbt = dtbt  %>% select(c("Line", "DIRECT", "INWARD",  "RECOVERY","RETROCESSION", "NET"))
      
      rownames(dtbt) <- NULL
      return(dtbt)
    }
    
    dt <- clean_dtbt(dt)
    bt <- clean_dtbt(bt)
    
      # Hàm thêm dòng gộp + Total
      process_dtbt <- function(df, ten) {
        df2 <- df %>%
          mutate(across(-Line, ~ suppressWarnings(as.numeric(.))))
        
        groups <- list(
          "Healthcare + Travel"   = c("Healthcare", "Travel"),
          "HC & PA & Travel"      = c("Travel", "Healthcare", "Personal Accident"),
          "Total"                 = unique(df2$Line)  # tất cả
        )
        
        added <- lapply(names(groups), function(new_name) {
          codes <- groups[[new_name]]
          df2 %>%
            filter(Line %in% codes) %>%
            summarise(across(-Line, sum, na.rm = TRUE)) %>%
            mutate(Line = new_name) %>%
            select(names(df2))
        })
        
        df2 = bind_rows(df2, dplyr::bind_rows(added)) %>%
          group_by(Line) %>%
          summarise(across(c(DIRECT, INWARD, RECOVERY,RETROCESSION, NET), ~ sum(.x/(10^6), na.rm = TRUE)), .groups = "drop")
        df2 %>%
          pivot_longer(
            cols = c(DIRECT, INWARD,RECOVERY,RETROCESSION,  NET),   # các cột cần pivot
            names_to = "Type",               # tên cột mới chứa tên biến
            values_to = ten              # tên cột mới chứa giá trị
          )
      }
    
    # Xử lý dt và bt
    dt_final <- process_dtbt(dt, "Written")
    bt_final <- process_dtbt(bt,"Paid" )
    df_joined <- dt_final %>%
      full_join(bt_final, by = c("Line", "Type"))
    df_joined <- df_joined %>%
      full_join(combined_long,by = c("Line", "Type"))
    
    
    combined_data(df_joined) 
    
    showModal(modalDialog(
      title = paste("File", input$quy,input$namlk ,": Đã nhận file đúng định dạng"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Đóng"),
        actionButton("ghep1", "Tiếp tục chuyển đổi dữ liệu")   # nút mới
      )
    ))
  })
})
