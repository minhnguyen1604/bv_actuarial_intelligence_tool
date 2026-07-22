
source("upr/0.start.R", local = TRUE)

ui <- fluidPage(
  useShinyjs(),
  # Thêm CSS căn chỉnh toàn bộ nội dung
  
  tags$head(
    includeCSS("www/style.css")
  ),
  # ==== BƯỚC 0: 3 box chọn module ====
  div(
    id = "select_module",
    tags$div(
      style = "display: flex; align-items: center; margin-bottom: 20px;",
      tags$img(src = "Logo-BaoViet-insurance.webp", height = "50px", style = "margin-right:15px;"),
    ),
    div(
    style = "display: flex; justify-content: center; gap: 70px; margin-top: 220px;",
    actionButton("btn_upr", "UPR", class = "btn-module upr")
    #,
    # actionButton("btn_osc", "OSC", class = "btn-module osc"),
    # actionButton("btn_khac", "KHÁC", class = "btn-module khac")
    ),
    # Footer
    tags$footer(class = "footer",
                  "© 2025 BVGI Actuary"
    )
    ),
  
  # ==== BƯỚC 1: Nhập thời điểm tính DPNV ====
  hidden(
    uiOutput("step1_ui")
  ),
  hidden(
    uiOutput("step2_ui")
  ),
  
  hidden(
    div(
      id = "main_ui",
  div(class = "main-container",
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
      # Logo + Tiêu đề
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        tags$img(src = "Logo-BaoViet-insurance.webp", height = "50px", style = "margin-right:15px;"),
      ),
      # Nút quay lại bên phải
      actionButton("back_to_menu2", "⬅ Quay lại", class = "btn-back")
),
  fluidRow(
               
               # ====== CỘT 1: Upload Excel + Long/Short Files ======
               column(
                 width = 5,
                 wellPanel(
                   #h4("📂 Upload Excel Files", style = "font-weight:bold; color:#0066CC"),
                   fluidRow(
                     column(
                       width =12,
                       wellPanel(
                         h4("📂 Upload Excel Files", style = "font-weight:bold; color:#0066CC"),
                         #h5("📥 TS-KT-TN, Marine, PA, XCG_CWVN, PA_TTTBVV, Travel"),
                         fileInput("file1", NULL, accept = c(".xlsx", ".xlsm")),
                         uiOutput("sheet_selector1"),
                         actionButton("check0", "ℹ Kiểm tra")
                       ))),
                   # Long/Short Table Section
                   h4("📊 Current Files", style = "font-weight:bold; color:#0066CC"),
                   div(
                     style = "text-align: right;",
                     actionButton("refresh_data", label = tagList(icon("sync-alt"), "Refresh"),  class = "btn btn-danger")
                   ),
                   
                   fluidRow(
                     column(
                       width = 6,
                       h5("📦 Long Term"),
                       div(class = "table-box", DTOutput("table_lt"))
                     ),
                     column(
                       width = 6,
                       h5("📦 Short Term"),
                       div(class = "table-box", DTOutput("table_st"))
                     )
                   )
                 )
               ),
               
               # ====== CỘT 2 ======
               column(
                 width = 3,
                 
                 wellPanel(
                   actionButton(
                     "cal",label = tagList(icon("calculator"), "Calculate UPR")
                   )
                 ),
                 wellPanel(
                   h4("📂 Danh sách các file đã tính xong", style = "font-weight:bold; color:#0066CC"),
                   # Hàng chứa cả nút Download All và Delete All
                   div(
                     style = "display:flex; justify-content:flex-end; gap:10px; margin-bottom:5px;",
                     
                     downloadButton(
                       outputId = "download_all",
                       label = "Download All",
                       class = "btn btn-success",
                       style = "padding:3px 8px; font-size:14px;"
                     ),
                     div(
                       id = "loading_msg",
                       style = "display: none; color: black; font-weight: light; margin-bottom: 10px;",
                       icon("spinner", class = "fa-spin"),
                       " Đang nén file và chuẩn bị tải xuống..."
                     ),
                     
                     actionButton(
                       "delete_all",
                       label = tagList(icon("trash"), "Remove All"),
                       #class = "btn btn-danger"
                       style = "background-color:red; color:white; padding:3px 8px; font-size:14px;"
                     )
                   ),
                   
                   DTOutput("file_table"),
                   br(),
                   actionButton("delete_selected", "🗑 Remove selected files", #class = "btn btn-danger")
                                style = "background-color:red; color:white;")
                 ),
                 wellPanel(
                   h4("⚙️ Tổng hợp", style = "font-weight:bold; color:#0066CC"),
                   br(),
                   actionButton("summarize_result", label = tagList(icon("table"), "Tổng hợp kết quả")),
                   br(),
                   uiOutput("download_btn_ui"), # UI động cho nút tải file
                   br()
                   
                 )
                 
                 
               ),

               column(
                 width = 1,
                 div(
                   style = "display: flex; justify-content: center; align-items: center; height: 100%;",
                   div(class = "vertical-divider")
                 )
               ),
               # ====== CỘT 3: Kiểm tra ======
               column(
                 width = 3,
                 uiOutput("compare_ui")
                 
               )
             ),

  # 👉 Footer nằm ngoài div chính
  tags$footer(class = "footer",
              "© 2025 BVGI Actuary"
  )
))
))

server <- function(input, output, session) {
  
  date_now <- reactiveVal(NULL)
  step_done <- reactiveVal(FALSE)
  # Bấm UPR -> hiện step1_ui, ẩn chọn module
  observeEvent(input$btn_upr, {
    hide("select_module")
    show("step1_ui")
  })
  # UI nhập thời điểm tính + tỷ giá
  output$step1_ui <- renderUI({
    if (!step_done()) {
      div(
        class = "center-box",
        fluidRow(
          column(
            4,
            wellPanel(
              h4("📅 Nhập thời điểm tính DPNV", style = "font-weight:bold; color:#0066CC"),
              fluidRow(
                column(4, numericInput("dpnv_ngay", "Ngày", value = last_values$ngay, min = 1, max = 31)),
                column(4, numericInput("dpnv_thang", "Tháng", value = last_values$thang, min = 1, max = 12)),
                column(4, numericInput("dpnv_nam", "Năm", value = last_values$nam, min = 1900, max = 2100))
              ),
              actionButton("confirm_date", "Xác nhận", class = "btn btn-primary"),
              actionButton("back_to_menu1", "⬅ Quay lại", class = "btn btn-secondary")
            )
          ),
          column(
            8,
            wellPanel(
              h4("💱 Tỷ giá", style = "font-weight:bold; color:#0066CC"),
              h5("Có thể thêm, xóa dòng/cột ở data dưới đây"),
              h5("Ấn 'Save' để lưu tỷ giá mới"),
              div(
                style = "text-align: right;",
                actionButton("save_btn", label = tagList(icon("save"), "SAVE"), class = "btn btn-success")
              ),
              br(),
              rHandsontableOutput("ty_gia_table"),
              br()
            )
          )
          
        )
      )
    }
  })
  
  # observeEvent(input$btn_osc, {
  #   hide("select_module")
  #   show("step2_ui")
  # })
  
  
  
  
  # Tải dữ liệu từ file .rds hoặc khởi tạo
  file_path <- "ty_gia.rds"
  if (file.exists(file_path)) {
    data_init <- readRDS(file_path)
  } else {
    data_init <- data.frame(
      Thoi_gian = character(),
      USD = numeric(),
      EUR = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  ty_gia_data <- reactiveVal(data_init)
  
  output$ty_gia_table <- renderRHandsontable({
    rhandsontable(ty_gia_data(), rowHeaders = NULL) %>%
      hot_table(stretchH = "all") 
  })
  ngay_input <- reactiveVal(NULL)
  
  observeEvent(input$confirm_date, {
    
    # Ghép chuỗi theo định dạng chuẩn, có zero-padding
    date_str <- sprintf(
      "%04d-%02d-%02d",
      as.integer(input$dpnv_nam),
      as.integer(input$dpnv_thang),
      as.integer(input$dpnv_ngay)
    )

    parsed <- as.Date(date_str, format = "%Y-%m-%d")
    
    valid_date <- !is.na(parsed) && identical(format(parsed, "%Y-%m-%d"), date_str)
    
    
    if (!valid_date) {
      showModal(modalDialog(
        title = "❌ Ngày không hợp lệ",
        "Vui lòng nhập ngày tháng năm hợp lệ ",
        easyClose = TRUE
      ))
      return() # Dừng ở đây nếu ngày không hợp lệ
    }
    
    ngay_input( make_date(
      year  = input$dpnv_nam,
      month = input$dpnv_thang,
      day   = input$dpnv_ngay
    ))
    
    quarter <- ceiling(as.numeric(input$dpnv_thang) / 3)
    
    # Lấy năm
    year <- input$dpnv_nam
    
    # Kết hợp lại
    Quy_nam = paste0("Q", quarter, "/", year)
    # cập nhật vào biến reactiveVal thay vì gán trực tiếp
    date_now(paste0("Q", quarter, "_", year))
    
    
    ty_gia= ty_gia_data()
    
    if (nrow(ty_gia) > 0) {
      last_date <- tail(ty_gia$Thoi_gian, 1)  # Lấy giá trị cuối cùng cột Thoi_gian
      
      if (last_date!= Quy_nam) {
        showModal(modalDialog(
          title = "Cập nhật tỷ giá",
          "Vui lòng kiểm tra lại tỷ giá tương ứng với quý tính dự phòng",
          easyClose = TRUE
        ))
        return() # Dừng ở đây nếu ngày không hợp lệ
    }
    }
    
    
    # Nếu hợp lệ thì lưu
    hide("step1_ui")
    dpnv_values <- list(
      ngay = input$dpnv_ngay,
      thang = input$dpnv_thang,
      nam = input$dpnv_nam
    )
    saveRDS(dpnv_values, "dpnv_last_values.rds")
    
    last_values <- readRDS("dpnv_last_values.rds")
    
    if (!dir.exists(paste0("www/cur_data_", date_now()))) {
      showNotification("Tính dự phòng cho quý mới", type = "error")
      # Tạo thư mục đích: www/excel_Qx_yyyy
      folder_excel <- paste0("www/excel_result_", pre_quarter(date_now()))
      if (!dir.exists(folder_excel)) dir.create(folder_excel, recursive = TRUE)
      
      # Đường dẫn thư mục nguồn
      src_dir <- "www/output_excel"
      
      # Lấy danh sách file trong output_excel
      files <- list.files(src_dir, full.names = TRUE)
      
      # Nếu có file, sao chép sang thư mục mới
      if (length(files) > 0) {
        file.copy(files, folder_excel, overwrite = TRUE)
        
        # Sau khi sao chép thành công, xóa file gốc
        file.remove(files)
        
        showNotification(paste0("Đã lưu ", length(files), " file vào ", folder_excel, " và xóa khỏi output_excel"), type = "message")
      } else {
        showNotification("Không có file nào trong thư mục output_excel", type = "warning")
      }
    }
    show("main_ui")
    step_done(TRUE)
  })
  
  
  # Nút quay lại từ step1_ui về menu
  observeEvent(input$back_to_menu1, {
    hide("step1_ui")
    show("select_module")
    step_done(FALSE)
  })
  
  # Nút quay lại từ main_ui về menu
  observeEvent(input$back_to_menu2, {
    hide("main_ui")
    show("step1_ui")
    step_done(FALSE)
  })
  
  
  # ReactiveVal để lưu tên file và thời gian
  latest_file <- reactiveVal(NULL)
  latest_time <- reactiveVal(NULL)
  
  # Nếu đã có file info trước đó, load lên
  if (file.exists("upr/latest_file_info.rds")) {
    last_info <- readRDS("upr/latest_file_info.rds")
    latest_file(last_info$file_name)
    latest_time(last_info$time)
  }
  
  
  folder_path <- "www/output_excel"
  
  # reactiveVal lưu danh sách file
  files_rv <- reactiveVal(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
  
  # Cập nhật dữ liệu khi người dùng chỉnh sửa bảng
  observe({
    if (!is.null(input$ty_gia_table)) {
      ty_gia_data(hot_to_r(input$ty_gia_table))
    }
  })
  # Lưu lại dữ liệu
  observeEvent(input$save_btn, {
    saveRDS(ty_gia_data(), "ty_gia.rds")
    showNotification("✅ Đã lưu dữ liệu vào 'ty_gia.rds'", type = "message")
  })
  
  observe({
    req(input$dpnv_ngay, input$dpnv_thang,input$dpnv_nam )
    dpnv_values <- list(
      ngay = input$dpnv_ngay,
      thang = input$dpnv_thang,
      nam = input$dpnv_nam
    )
    saveRDS(dpnv_values, "dpnv_last_values.rds")
  })

  source("upr/6.show_excel_output.R", local = TRUE)
  source("upr/1.check_input.R", local = TRUE)
  source("upr/2.ghep_file.R", local = TRUE)
  source("upr/3.View_file_da_ghep.R", local = TRUE)
  source("upr/4.calculate_upr_button.R", local = TRUE)
  source("upr/5.tong_hop_result.R", local = TRUE)
  #source("upr/7.check.R", local = TRUE)
  # source("osc/ts-kt-tn.R", local = TRUE)
  # source("osc/pa.R", local = TRUE)
}

shinyApp(ui, server)
