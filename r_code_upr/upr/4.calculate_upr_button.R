vals <- reactiveValues()
# Đường dẫn tới file gốc chứa macro

vals$processing <- NULL
vals$queue <- NULL
vals$completed <- character()
#_______________________________________

# Ham trả về tag (HTML) cho choiceNames:
mark_calculated_tags <- function(files, calculated_files) {
  lapply(files, function(f) {
    if (f %in% calculated_files) {
      # đã tính => hiển thị bình thường
      tags$span(f)
    } else {
      # chưa tính => chữ màu xanh
      tags$span(style = "color:green; font-weight:bold;", f)
    }
  })
}

observeEvent(input$cal, {
  # Lấy danh sách file .rds hiện có
  existing_files <- tools::file_path_sans_ext(
    list.files(paste0("www/cur_data_",date_now()), pattern = "\\.rds$", full.names = FALSE)
  )
  
  # Lấy danh sách file đã tính trong www/output_excel
  calculated_files <- tools::file_path_sans_ext(
    list.files("www/output_excel", pattern = "\\.xlsx$", full.names = FALSE)
  )
  
  # Lọc file expected tồn tại
  available_expected_files <- expected_files[expected_files %in% existing_files]
  
  if (length(available_expected_files) == 0) {
    showNotification("❗Không có file .rds nào phù hợp trong www/cur_data", type = "warning")
    return()
  }
  
  
  
  # Chia Long/Short và gắn nhãn
  long_files <- available_expected_files[grepl("LT", available_expected_files)]
  short_files <- available_expected_files[!grepl("LT", available_expected_files)]
  
  vals$long_files <- long_files
  vals$short_files <- short_files
  
  # Đọc tỷ giá
  ty_gia_data <- readRDS("ty_gia.rds")
  ty_gia_latest <- tail(ty_gia_data, 1)
  
  quy_gan_nhat <- ty_gia_latest$Thoi_gian
  usd_latest   <- ty_gia_latest$USD
  eur_latest   <- ty_gia_latest$EUR
  
  # Modal chọn file
  showModal(modalDialog(
    title = "📂 Chọn file để tính toán",
    tags$p(style = "color:blue; font-weight:bold;",
           paste0("🕒 Thời điểm tính dự phòng: ",
                  input$dpnv_ngay, "/", input$dpnv_thang, "/", input$dpnv_nam)),
    tags$p(style = "color:blue; font-weight:bold;",
           paste0("💱 Tỷ giá quý ", quy_gan_nhat, 
                  " — USD: ", format(usd_latest, big.mark = ","),
                  " | EUR: ", format(eur_latest, big.mark = ","))),
    

    tags$div(
      style = "display: flex; justify-content: flex-end; gap: 5px; margin-bottom: 5px;",
      actionButton("select_all", "Select All"),
      actionButton("deselect_all", "Remove All")
    ),
    tags$div(
      style = "display: flex; align-items: center; margin-top: 5px;",
      tags$div(style = "width: 15px; height: 15px; background-color: green; margin-right: 5px;"),
      tags$span("Chưa tính"),
      tags$div(style = "width: 15px; height: 15px; background-color: black; margin-left: 15px; margin-right: 5px;"),
      tags$span("Đã tính")
    ),
    hr(),
    fluidRow(
      column(6,
             # Render HTML labels cho checkboxGroupInput
             HTML(as.character(
               checkboxGroupInput(
                 "selected_rds_long",
                 "📦 Long Term",
                 choiceNames = mark_calculated_tags(long_files, calculated_files),
                 choiceValues = long_files
                 #choices = setNames(long_files, mark_calculated(long_files))
               )
             ))
      ),
      column(6,
             HTML(as.character(
               checkboxGroupInput(
                 "selected_rds_short",
                 "📦 Short Term",
                 choiceNames = mark_calculated_tags(short_files, calculated_files),
                 choiceValues = short_files
                 #choices = setNames(short_files, mark_calculated(short_files))
               )
             ))
      )
      ),

    footer = tagList(
      modalButton("❌ Hủy"),
      actionButton("confirm_calc", "✅ Thực hiện tính toán")
    ),
    easyClose = TRUE
  ))
})

observeEvent(input$select_all, {
  updateCheckboxGroupInput(session, "selected_rds_long", selected = vals$long_files)
  updateCheckboxGroupInput(session, "selected_rds_short", selected = vals$short_files)
})

observeEvent(input$deselect_all, {
  updateCheckboxGroupInput(session, "selected_rds_long", selected = character(0))
  updateCheckboxGroupInput(session, "selected_rds_short", selected = character(0))
})


output$progress_ui <- renderUI({
  tagList(
    if (!is.null(vals$processing)) {
      h4(paste("⏳ Đang xử lý:", vals$processing))
    },
    if (length(vals$queue) > 0) {
      tagList(
        h5("📋 File chưa xử lý:"),
        tags$ul(lapply(vals$queue, function(f) tags$li(f)))
      )
    },
    if (length(vals$done) > 0) {
      tagList(
        h5("✅ Đã xong:"),
        tags$ul(lapply(vals$done, function(f) tags$li(f)))
      )
    }
  )
})


source("upr/4.1.ky_mau.R",local = TRUE)



observeEvent(input$confirm_calc, {
  
  # Nếu cả 2 đều NULL => dùng merged_group_code
  if (is.null(input$selected_rds_long) && is.null(input$selected_rds_short)) {
    # Lấy file duy nhất
    file_to_process <- merged_group_code()
    
    # Chia long / short dựa trên tên file
    if (grepl("LT", file_to_process)) {
      vals$long_files <- file_to_process
      vals$short_files <- character(0)
    } else {
      vals$long_files <- character(0)
      vals$short_files <- file_to_process
    }
    
    vals$queue <- file_to_process
    
  } else {
    vals$long_files <- input$selected_rds_long %||% character(0)
    vals$short_files <- input$selected_rds_short %||% character(0) 
    vals$queue <- c(input$selected_rds_long, input$selected_rds_short)
  }
  
  vals$done <- character(0)
  vals$processing <- NULL
  # Hiện modal

  showModal(modalDialog(
    title = "⚙️ Xử lý File",
    uiOutput("progress_ui"),
    footer = tagList(
      #downloadButton("download_all", "⬇️ Tải toàn bộ kết quả"),
      modalButton("Đóng")
    ),
    size = "l",
    easyClose = TRUE
  ))

  result_list <- list()
  process_next_file()

})




  









