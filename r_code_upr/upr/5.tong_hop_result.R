observeEvent(input$summarize_result, {
  folder_path <- "www/output_excel"
  files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE)
  
  # Lưu lại danh sách file để dùng sau này
  updateSelectFiles <<- reactiveVal(files)
  

  folder_input <- paste0("www/cur_data_",date_now())
  
  # Lấy file .rds đầu vào
  rds_files <- list.files(folder_input, pattern = "\\.rds$", full.names = FALSE)
  
  # Đổi tên sang đuôi .xlsx để đối chiếu
  expected_output_files <- sub("\\.rds$", ".xlsx", rds_files)
  
  # Lấy danh sách file đã xử lý (thực tế đã có trong output)
  actual_output_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE)
  
  # So sánh: file rds nào chưa có file xlsx tương ứng?
  files_unprocessed <- setdiff(expected_output_files, actual_output_files)
  
  # Ghép thành message
  message_text <- if (length(files_unprocessed) > 0) {
    paste("🔍 Các file chưa được xử lý:", paste(files_unprocessed, collapse = ", "))
  } else {
    "✅ Tất cả các file đã được xử lý."
  }
  
  showModal(modalDialog(
    title = "📂 Chọn file để tổng hợp",
    # Thông báo file chưa xử lý
    p(message_text, style = "color: #2c3e50; font-style: italic; margin-top: 10px;"),
    
    # Vùng chọn file
    tagList(
      
      div(
        style = "text-align: right;",
        actionButton("select_all_files", "Chọn tất cả"),
        actionButton("deselect_all_files", "Bỏ chọn tất cả")
      ),
      checkboxGroupInput("selected_files", "Chọn các file:",
                         choices = files,
                         selected = NULL)
    ),
    
    # Footer
    footer = tagList(
      modalButton("Hủy"),
      actionButton("start_summary", "Bắt đầu tổng hợp", class = "btn-primary")
    ),
    
    easyClose = TRUE
  ))
})

# Chọn tất cả
observeEvent(input$select_all_files, {
  files <- updateSelectFiles()
  updateCheckboxGroupInput(session, "selected_files", selected = files)
})

# Bỏ chọn tất cả
observeEvent(input$deselect_all_files, {
  updateCheckboxGroupInput(session, "selected_files", selected = character(0))
})

#____________ active file

observeEvent(input$start_summary, {
  showModal(modalDialog(
    title = "⏳ Đang xử lý...",
    "Quá trình tổng hợp có thể mất tới 5 phút. Vui lòng đợi.",
    easyClose = FALSE,
    footer = NULL
  ))
  
  future::future({
    # ----- Hàm activate file Excel để hiện giá trị tính toán -----
    activate_excel_file <- function(file_path) {
      try({
        excel_app <- RDCOMClient::COMCreate("Excel.Application")
        excel_app[['Visible']] <- FALSE
        workbook <- excel_app$Workbooks()$Open(normalizePath(file_path))
        excel_app$CalculateFull()
        workbook$Save()
        workbook$Close(FALSE)
        excel_app$Quit()
      }, silent = TRUE)
    }

    
# 🔄 Duyệt các file đã được chọn
folder_path <- "www/output_excel"
selected_files <- input$selected_files  # <-- Lấy từ input
full_paths <- file.path(folder_path, selected_files)


lapply(full_paths, activate_excel_file)
#__________________ 

# Tạo list chứa các nhóm dữ liệu
group_data <- list(
  LT = list(),
  Travel = list(),
  TTTBVV = list(),
  ShortTerm = list()
)

for (file in full_paths) {
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Đọc sheet "Result"
  df_raw <- read.xlsx(file, sheet = "Result", colNames = FALSE)
  
  if (nrow(df_raw) < 2) next  # Bỏ qua nếu sheet quá ngắn
  
  # Bỏ dòng đầu tiên, dòng thứ 2 làm colnames
  colnames(df_raw) <- as.character(unlist(df_raw[2, ]))
  df <- df_raw[-c(1, 2), , drop = FALSE]
  
  
  # Ép kiểu số cho các cột cần tính
  df <- df %>% mutate(across(-Quy, ~ suppressWarnings(as.numeric(.))))
  
  # Summarise tất cả các cột trừ cột Quy
  df <- df %>%
    group_by(Quy) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
    #summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  
  
  
  # Xác định nhóm
  if (str_detect(tolower(file_name), "lt")) {
    group_data$LT[[file_name]] <- df
  } else if (str_detect(tolower(file_name), "travel|vietjet")) {
    group_data$Travel[[file_name]] <- df
  } else if (str_detect(tolower(file_name), "tttbvv")) {
    group_data$TTTBVV[[file_name]] <- df
  } else {
    group_data$ShortTerm[[file_name]] <- df
  }
}

# Tạo workbook mới
wb_new <- createWorkbook()
style_acc <- createStyle(numFmt = "#,##0")  # Không có ký hiệu $
#style_acc <- createStyle(numFmt = "_(* #,##0_);_(* (#,##0);_(* \"-\"??_);_(@_)")

# Ghép dữ liệu cho nhóm LT, Travel, ShortTerm
for (grp in c("LT", "Travel", "ShortTerm")) {
  if (length(group_data[[grp]]) > 0) {
    merged_df <- bind_rows(group_data[[grp]], .id = "SourceFile")
    if ("Ky_phi" %in% names(merged_df)) {
      merged_df <- merged_df %>% select(-Ky_phi)
    }
    addWorksheet(wb_new, grp)
    # Áp dụng định dạng accounting
    numeric_cols <- which(sapply(merged_df, is.numeric))
    if (length(numeric_cols) > 0) {
      addStyle(wb_new, sheet = grp, style = style_acc,
               rows = 2:(nrow(merged_df)+1),
               cols = which(sapply(merged_df, is.numeric)),
               gridExpand = TRUE)
      setColWidths(
        wb_new,
        sheet = grp,
        cols = 1:ncol(merged_df),
        widths = "auto"
      )
    }
    
    writeData(wb_new, grp, merged_df,withFilter = TRUE)
  }
}

# Thêm TTTBVV từng file riêng
if (length(group_data$TTTBVV) > 0) {
  for (nm in names(group_data$TTTBVV)) {
    addWorksheet(wb_new, nm)
    merged_df <- group_data$TTTBVV[[nm]]
    if ("Ky_phi" %in% names(merged_df)) {
      merged_df <- merged_df %>% select(-Ky_phi)
    }
    writeData(wb_new, nm, merged_df,withFilter = TRUE)
    # Áp dụng định dạng accounting
    numeric_cols <- which(sapply(merged_df, is.numeric))
    if (length(numeric_cols) > 0) {
      addStyle(wb_new, sheet = nm, style = style_acc,
               rows = 2:(nrow(merged_df)+1),
               cols = which(sapply(merged_df, is.numeric)),
               gridExpand = TRUE)
      setColWidths(
        wb_new,
        sheet = nm,
        cols = 1:ncol(merged_df),
        widths = "auto"
      )
    }
  }
}

# # Lưu file
# saveWorkbook(wb_new, file = "www/Tong_Hop_Result.xlsx", overwrite = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_name <- paste0("Tong_Hop_Result_", timestamp, ".xlsx")
file_path <- file.path("www", file_name)

saveWorkbook(wb_new, file = file_path, overwrite = TRUE)

list(file_name = file_name, time = Sys.time())
  
 
# })
# return(TRUE)
  }) %...>% {
    # Xử lý sau khi hoàn tất future
    res <- .
    latest_file(res$file_name)
    latest_time(res$time)
    # Lưu ra ổ cứng
    saveRDS(list(file_name = res$file_name, time = res$time), "latest_file_info.rds")
    
    removeModal()
    showModal(modalDialog(
      title = "✅ Hoàn tất",
      "Đã tổng hợp xong dữ liệu. Bạn có thể tải file:",
      downloadButton("download_summary", paste0("📥 Tải file ", res$file_name)),
      # downloadButton("download_summary", "📥 Tải file Tong_Hop_Result.xlsx"),
      easyClose = TRUE
    ))
  } %...!% {
    # Bắt lỗi nếu có
    removeModal()
    showModal(modalDialog(
      title = "❌ Lỗi",
      "Đã xảy ra lỗi khi tổng hợp. Vui lòng kiểm tra lại.",
      easyClose = TRUE
    ))
  }
})

  
  # UI động cho nút tải file (hiện thời gian mới nhất)
  output$download_btn_ui <- renderUI({
    if (is.null(latest_file())) {
      downloadButton("download_summary", "📥 Chưa có file")
    } else {
      label_text <- paste0("📥 Tải file (", format(latest_time(), "%d/%m/%Y %H:%M:%S"), ")")
      downloadButton("download_summary", label_text)
    }
  })


output$download_summary <- downloadHandler(
  filename = function() {
    latest_file() %||% "Chua_co_file.xlsx"
  },
  content = function(file) {
    req(latest_file())
    file.copy(file.path("www", latest_file()), file)
  }
)


