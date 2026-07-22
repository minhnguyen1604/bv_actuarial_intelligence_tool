source("main/0.start.R", local = TRUE)

ui <- fluidPage(
  useShinyjs(),  # 👈 Thêm dòng này
  div(
    class = "app-header",
    div(
      class = "header-left",
      tags$img(src = "Logo-BaoViet-insurance.webp", height = "50px", style = "margin-right:15px;")
    ),
    div(
      class = "header-right",
      h1("Tool Tổng hợp Dự phòng nghiệp vụ"),
      p("2 - Xử  lý  và tổng hợp thông tin từ các files Paid/OSC")
    )
  ),
  # ✅ Thêm CSS vào đây
  tags$head(
    includeCSS("www/style.css")
  ),
  tabsetPanel(
    id = "tabs",
    
    # ===========================
    # 🔵 TAB 1 – MAIN PROCESSING
    # ===========================
    tabPanel("INPUT GỐC",
  # ===============================
  # 1️⃣ CHỌN NGHIỆP VỤ & LOẠI DỮ LIỆU
  # ===============================
  div(class = "input-box",
      h4("1️⃣ Lựa chọn nghiệp vụ và loại dữ liệu"),
      fluidRow(
        column(3,
               selectInput("nghiep_vu", "Chọn nghiệp vụ:",
                           choices = c("CARGO", "MARINE","XCG", "PA", "TS-KT-TN", "HEALTH CARE"))
        ),
        column(3,
               selectInput("data_type", "Chọn loại dữ liệu:",
                           choices = c("OSC", "PAID"))
        ),
        column(1, numericInput("year", "Năm:",
                               value = year(Sys.Date()), min = 2020, max = 2100)),
        column(1, selectInput("quarter", "Quý:",
                              choices = paste0("Q", 1:4), selected = "Q4")),
        column(2, actionButton("confirm", "✅ Chốt lựa chọn"))
      )
  ),
  # ===============================
  # 🧩 BOX 2 — INPUT TÙY THEO NGHIỆP VỤ
  # ===============================
  div(class = "input-box",
      h4("2️⃣ Nhập dữ liệu và xử lý"),
      uiOutput("dynamic_inputs")
  ),
  # ===============================
  # 🧩 BOX 3 — KẾT QUẢ
  # ===============================
  div(class = "input-box",
      h4("3️⃣ Kết quả xử lý"),
      fluidRow(column(12, DTOutput("result")))
  )
  ),
  # ===========================
  # 🔵 TAB 1 – MAIN PROCESSING
  # ===========================
  tabPanel("INPUT TBH",
           # ===============================
           # 1️⃣ CHỌN NGHIỆP VỤ & LOẠI DỮ LIỆU
           # ===============================
           div(class = "input-box",
               h4("1️⃣ UPLOAD file OSC SUMMARY"),
               fluidRow( column(6,
                 fileInput(
                 inputId = "file_excel",
                 label   = "Upload Excel file",
                 accept  = c(".xls", ".xlsx")
               ),
               uiOutput("sheet_ui")),
               
               column(2, 
                      actionButton("tbh_run", "Run"),
                      downloadButton("tbh_download", "Download"))
               )
           ),
           # ===============================
           # 🧩 BOX 3 — KẾT QUẢ
           # ===============================
           div(class = "input-box",
               h4("2️⃣ Kết quả xử lý"),
               fluidRow(column(12, 
                               DTOutput("result_table")))
           )
  ),
  tabPanel(
    "OUTPUT",
    br(),
    
    div(
      class = "input-box",
      h4("SUMMARY"),
      
      fluidRow(
        column(
          2,
          selectInput(
            "v_type",
            "OSC | PAID:",
            choices = c("OSC", "PAID"),
            selected = "OSC"
          )
        ),
        
        column(
          5,
          selectizeInput(
            "v_periods",
            "Chọn năm hoặc quý để so sánh:",
            choices = c(
              as.character(2023:2026),
              paste0(rep(2023:2026, each = 4), "Q", 1:4)
            ),
            multiple = TRUE,
            options = list(placeholder = "VD: 2025 hoặc 2024Q3, 2024Q4")
          )
        ),
        
        column(
          2,
          actionButton("v_run", "🔎 Hiển thị")
        ),
        
        column(
          2,
          downloadButton("v_download_table", "Download")
        )
      )
    ),
    
    div(
      class = "input-box",
      uiOutput("v_table_ui")
    )
  ),

  # ===========================
  # 🔵 TAB 2 – VISUALIZE DATA
  # ===========================
  tabPanel("SUMMARY",
           br(),
           
           div(class = "input-box",
               h4("📊 Tổng hợp dự phòng theo công ty & nghiệp vụ"),
           fluidRow(
             column(2, numericInput("viz_year", "Năm:", 
                                    value = year(Sys.Date()), min = 2020, max = 2100)),
             column(2, selectInput("viz_quarter", "Quý:",
                                   choices = paste0("Q", 1:4), selected = "Q3")),
             column(2, selectInput("viz_type", "OSC | PAID:",
                                   choices = c("OSC", "PAID"), selected = "OSC")),
             column(2, actionButton("viz_run", "🔎 Hiển thị"))
           ),
           
           br(),
           downloadButton("download_table", "Download")),
           div(class = "input-box",
           uiOutput("viz_table_ui"))
  ),
  # ===========================
  # 🔵 TAB 3 – DETAIL
  # ===========================
  tabPanel("DETAIL",
           br(),
           div(class = "input-box",
               h4("📊 Tổng hợp dự phòng theo công ty & nghiệp vụ"),
               fluidRow(
                 column(2, numericInput("de_year", "Năm:", 
                                        value = year(Sys.Date()), min = 2020, max = 2100)),
                 column(2, selectInput("de_quarter", "Quý:",
                                       choices = paste0("Q", 1:4), selected = "Q3")),
                 column(2, selectInput("de_type", "OSC | PAID:",
                                       choices = c("OSC", "PAID"), selected = "OSC")),
                 column(2, selectInput("de_nv", "Nghiệp vụ:",
                                       choices = c("CARGO", "MARINE","XCG", "PA", "ENG", "FIRE", "MISC", "HEALTH_CARE"))),
                 column(2, actionButton("de_run", "🔎 Hiển thị"))
               ),
               
               br(),
               downloadButton("de_download_detail", "Download")
               ),
           div(class = "input-box",
               h3("📄 BÁO CÁO BIẾN ĐỘNG DỰ PHÒNG"),
               p(textOutput("de_info")),
               
               DT::DTOutput("de_table"))
           
  ),
  # ===========================
  # 🔵 TAB 4 – TRIANGLE
  # ===========================
  tabPanel("TRIANGLE",
           br(),
           div(class = "input-box",
               h4("📊 Tổng hợp dự phòng nghiệp vụ"),
               fluidRow(
               column(3,
                      selectInput("fnghiep_vu", "Chọn nghiệp vụ:",
                                  choices = c("CARGO", "MARINE","XCG", "PA", "TS_KT_TN", "HEALTH_CARE"))
               ),
               column(3,
                      selectInput("fdata_type", "Chọn loại dữ liệu:",
                                  choices = c("OSC", "PAID"))
               ),
               column(2, actionButton("f_run", "🔎 Hiển thị")))
           ),
           div(class = "input-box",
               h3("TRIANGLE"),
               DT::DTOutput("f_table")
               )
           
  ),
  # ===========================
  # 🔵 TAB 5 – HQQU
  # ===========================
  tabPanel("HQQU",
           br(),
           div(class = "input-box",
               h4("📊 Tổng hợp dự phòng nghiệp vụ"),
               fluidRow(
                 column(2, numericInput("hqqu_year", "Năm:", 
                                        value = year(Sys.Date()), min = 2020, max = 2100)),
                 column(2, selectInput("hqqu_quarter", "Quý:",
                                       choices = paste0("Q", 1:4), selected = "Q1")),
                 column(3,
                        selectInput("hqqu_type", "Chọn loại dữ liệu:",
                                    choices = c("OSC", "PAID"))
                 ),
                 
                 column(2, actionButton("hqqu_run", "🔎 Hiển thị"))
                 )
           ),
           div(class = "input-box",
               fluidRow(
                 column(3,
               h3("Danh sách file") ,
               div(
                 class = "table-box",
                 style = "max-height: 400px; overflow: auto;",
                 tableOutput("file_table")
               )),
               #tableOutput("file_table")),
               column(9,
                      h3("📊 Data Preview"),
                      div(class = "table-box",
                        style = "max-height: 300px; overflow-y: auto; overflow-x: auto;",
                        tableOutput("data_preview")
                      ))),
               fluidRow(
                 column(12,
               h3("HQQU"),
               downloadButton("download_hqqu", "Download zip all"),
               downloadButton("download_it", "Download sent to ITC"))
               # ,
               # actionButton("tich_tu", "Tích tụ"))
                 )
               )
           ,
           br(),
           
           div(class = "input-box",
               
               fluidRow(
                 column(4,
                        h4("Tuỳ chọn"),
                        checkboxInput("log_scale", "Log scale", TRUE),
                        selectInput(
                          "value_col",
                          "Chọn chỉ tiêu",
                          choices = c("STBH" = "stbh_usd",
                                      "Paid" = "paid_osc"),
                          selected = "stbh_usd"
                        ),
                        selectInput("nv_filter", "Chọn nghiệp vụ", choices = NULL, multiple = TRUE)
                        
                 ),
                 
                 column(8,
                        h4("Boxplot"),
                        plotOutput("stbh_boxplot", height = 400)
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        h4("Histogram"),
                        plotOutput("stbh_hist", height = 400)
                 )
               )
           )
  )
  )
)


server <- function(input, output, session) {

  # ===============================
  # 🔄 RESET khi đổi NĂM hoặc QUÝ
  # ===============================
  observeEvent({
    list(input$year, input$quarter)
  }, {
    # 🧹 Reset các fileInput
    reset("folder")
    reset("folder1")
    reset("file_xcg")
    reset("file_xcg1")
    reset("file_cargo")
    reset("file_cargo_tn")
    reset("file_cargo1")
    reset("file_cargo_tn1")
    reset("file_pa")
    reset("file_pa1")
    reset("file_eng1")
    reset("file_eng2")
    reset("file_marine11")
    reset("file_marine12")
    reset("file_marine13")
    reset("file_marine21")
    reset("file_marine22")
    reset("file_marine23")
    
    
    # 🧹 Reset kết quả hiển thị
    output$result <- renderDT(NULL)
    
    # 🧹 Reset lại các nút (vì Shiny không cho disable button trực tiếp)
    updateActionButton(session, "run", label = "Chạy xử lý PAID HEALTH CARE")
    updateActionButton(session, "run1", label = "Chạy xử lý OSC HEALTH CARE")
    updateActionButton(session, "run_xcg", label = "Chạy xử lý PAID XCG")
    updateActionButton(session, "run_xcg1", label = "Chạy xử lý OSC XCG")
    updateActionButton(session, "run_cargo", label = "Chạy xử lý PAID CARGO")
    updateActionButton(session, "run_cargo1", label = "Chạy xử lý OSC CARGO")
    updateActionButton(session, "run_pa", label = "Chạy xử lý PAID PA")
    updateActionButton(session, "run_pa1", label = "Chạy xử lý OSC PA")
    updateActionButton(session, "run_eng", label = "Chạy xử lý TS-KT-TN")
    updateActionButton(session, "run_marine", label = "Chạy xử lý PAID Marine")
    updateActionButton(session, "run_marine1", label = "Chạy xử lý OSC Marine")
    
    showNotification("🔄 Đã reset toàn bộ input và kết quả do thay đổi năm hoặc quý!", type = "message")
    })
  observe({
    # Danh sách các nhóm file và nút tương ứng
    mapping <- list(
      folder   = c("run",   "downloadData",       "downloadDatacheck"),
      folder1  = c("run1",  "downloadData1",      "downloadDatacheck1"),
      file_xcg = c("run_xcg", "download_xcg",     "download_xcg_check"),
      file_xcg1= c("run_xcg1", "download_xcg1",   "download_xcg_check1"),
      file_cargo = c("run_cargo", "download_cargo",     "download_cargo_check"),
      file_cargo1= c("run_cargo1", "download_cargo1",   "download_cargo_check1"),
      file_cargo_tn = c("run_cargo", "download_cargo",     "download_cargo_check"),
      file_cargo_tn1= c("run_cargo1", "download_cargo1",   "download_cargo_check1"),
      file_marine11 = c("run_marine", "download_marine",     "download_marine_check"),
      file_marine12 = c("run_marine", "download_marine",     "download_marine_check"),
      file_marine13 = c("run_marine", "download_marine",     "download_marine_check"),
      file_marine21 = c("run_marine1", "download_marine1",     "download_marine_check1"),
      file_marine22 = c("run_marine1", "download_marine1",     "download_marine_check1"),
      file_marine23 = c("run_marine1", "download_marine1",     "download_marine_check1"),
      file_pa  = c("run_pa",  "download_pa",      "download_pa_check"),
      file_pa1 = c("run_pa1", "download_pa1",     "download_pa_check1"),
      file_eng1 = c("run_eng",  "download_eng",      "download_eng_check"),
      file_eng2 = c("run_eng",  "download_eng",      "download_eng_check")
    )
    
    # Lặp qua từng nhóm
    for (key in names(mapping)) {
      # Lấy giá trị file input
      file_input <- input[[key]]
      # Lấy danh sách các nút cần disable/enable
      btns <- mapping[[key]]
      
      # Kiểm tra nếu chưa upload file
      if (is.null(file_input) || (is.data.frame(file_input) && nrow(file_input) == 0)) {
        lapply(btns, shinyjs::disable)
      } else {
        lapply(btns, shinyjs::enable)
      }
    }
  })
  
  
  # ⚙️ Hiển thị phần input khác nhau theo nghiệp vụ
  output$dynamic_inputs <- renderUI({
    req(input$nghiep_vu, input$data_type)
    input$year
    input$quarter

    if (input$nghiep_vu == "HEALTH CARE" && input$data_type == "PAID" ) {
      fluidRow(
        column(3, fileInput("folder", "Upload tất cả file Excel trong folder:",
                            multiple = TRUE, accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        # conditionalPanel(
        #   condition = "input.file_pa != null",
        column(2, 
               actionButton("run", "Chạy xử lý PAID HEALTH CARE"),
               downloadButton("downloadData", "Tải dữ liệu quý này"),
               downloadButton("downloadDatacheck", "Tải file check trùng paid"))
      )#)
    } else if (input$nghiep_vu == "HEALTH CARE" && input$data_type == "OSC" ) {
      fluidRow(
        column(3, fileInput("folder1", "Upload tất cả file Excel trong folder:",
                            multiple = TRUE, accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, 
               actionButton("run1", "Chạy xử lý OSC HEALTH CARE"),
               downloadButton("downloadData1", "Tải dữ liệu quý này"),
               downloadButton("downloadDatacheck1", "Tải file check trùng"))
      )
    } else if (input$nghiep_vu == "XCG" && input$data_type == "PAID" ) {
      fluidRow(
        column(3, fileInput("file_xcg", "Chọn file dữ liệu nghiệp vụ XCG:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_xcg", "Chạy xử lý PAID XCG"),
        downloadButton("download_xcg", "Tải dữ liệu quý này"),
        downloadButton("download_xcg_check", "Tải file check trùng"))
      )
    } else if (input$nghiep_vu == "XCG" && input$data_type == "OSC" ) {
      fluidRow(
        column(3, fileInput("file_xcg1", "Chọn file dữ liệu nghiệp vụ XCG:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_xcg1", "Chạy xử lý OSC XCG"),
        downloadButton("download_xcg1", "Tải dữ liệu quý này"),
        downloadButton("download_xcg_check1", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "CARGO" && input$data_type == "PAID" ) {
      fluidRow(
        column(3, 
               fileInput("file_cargo", "Chọn file BT HÀNG:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_cargo_tn", "Chọn file ĐƠN TNDS:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_cargo", "Chạy xử lý PAID CARGO"),
               downloadButton("download_cargo", "Tải dữ liệu quý này"),
               downloadButton("download_cargo_check", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "CARGO" && input$data_type == "OSC" ) {
      fluidRow(
        column(3, 
               fileInput("file_cargo1", "Chọn file OSC HÀNG:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_cargo_tn1", "Chọn file ĐƠN TNDS:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_cargo1", "Chạy xử lý OSC CARGO"),
               downloadButton("download_cargo1", "Tải dữ liệu quý này"),
               downloadButton("download_cargo_check1", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "MARINE" && input$data_type == "PAID" ) {
      fluidRow(
        column(3, 
               fileInput("file_marine11", "Chọn file Paid HULL:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_marine12", "Chọn file Paid P&I:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_marine13", "Chọn file thống kê những vụ đã bồi thường:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_marine", "Chạy xử lý PAID MARINE"),
               downloadButton("download_marine", "Tải dữ liệu quý này"),
               downloadButton("download_marine_check", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "MARINE" && input$data_type == "OSC" ) {
      fluidRow(
        column(3, 
               fileInput("file_marine21", "Chọn file OSC HULL:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_marine22", "Chọn file OSC P&I:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_marine23", "Chọn file thống kê những vụ chưa bồi thường:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_marine1", "Chạy xử lý osc MARINE"),
               downloadButton("download_marine1", "Tải dữ liệu quý này"),
               downloadButton("download_marine_check1", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "PA" && input$data_type == "PAID" ) {
      fluidRow(
        column(3, fileInput("file_pa", "Chọn file dữ liệu nghiệp vụ PA:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2,
               actionButton("run_pa", "Chạy xử lý PAID PA"),
               downloadButton("download_pa", "Tải dữ liệu quý này"),
               downloadButton("download_pa_check", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "PA" && input$data_type == "OSC" ) {
      fluidRow(
        column(3, fileInput("file_pa1", "Chọn file dữ liệu nghiệp vụ PA:",accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2, actionButton("run_pa1", "Chạy xử lý OSC PA"),
               downloadButton("download_pa1", "Tải dữ liệu quý này"),
               downloadButton("download_pa_check1", "Tải file check trùng"))
      )
    }else if (input$nghiep_vu == "TS-KT-TN"  ) {
      fluidRow(
        column(3, 
               fileInput("file_eng1", "Chọn file TRONG PC nghiệp vụ TS-KT-TN:", accept = c(".xlsx", ".xls", ".xlsm")),
               fileInput("file_eng2", "Chọn file TREN PC nghiệp vụ TS-KT-TN:", accept = c(".xlsx", ".xls", ".xlsm"))),
        use_waiter(),
        column(2,
               actionButton("run_eng", "Chạy xử lý TS-KT-TN"),
               downloadButton("download_eng", "Tải dữ liệu quý này"),
               downloadButton("download_eng_check", "Tải file check trùng"))
      )
    }
    
    # Có thể thêm B, C tương tự
  })
  
  observeEvent(input$confirm, {
    req(input$nghiep_vu, input$data_type)

    print(paste("Đã chốt:", input$nghiep_vu, "-", input$data_type))
    
    script <- switch(
      paste(input$nghiep_vu, input$data_type),
      "HEALTH CARE PAID" = "main/1.health_care_paid.R",
      "HEALTH CARE OSC"  = "main/1.health_care_osc.R",
      "XCG PAID"         = "main/2.xcg_paid.R",
      "XCG OSC"         = "main/2.xcg_osc.R",
      "PA PAID"         = "main/3.pa_paid.R",
      "PA OSC"         = "main/3.pa_osc.R",
      "TS-KT-TN OSC"         = "main/4.ts_kt_tn.R",
      "TS-KT-TN PAID"         = "main/4.ts_kt_tn.R",
      "CARGO PAID"         = "main/5. hang_paid.R",
      "CARGO OSC"         = "main/5. hang_osc.R",
      "MARINE PAID"         = "main/6. tau_paid.R",
      "MARINE OSC"         = "main/6. tau_osc.R",
      NULL
    )
    
    if (!is.null(script)) {
      message("📂 Nạp script: ", script)
      source(script, local = TRUE)
      showNotification(paste("✅ Đã nạp:", script), type = "message")
    } else {
      showNotification("⚠️ Chưa có script cho lựa chọn này!", type = "error")
    }
  })
  
  
  #____________________________________________________
  #______         VISUALIZE                 ___________
  #____________________________________________________
  source("main/a_tab_input_tai.R", local = TRUE)
  source("main/b_tab_output.R", local = TRUE)
  source("main/c_tab_summary.R", local = TRUE)
  source("main/d_tab_detail.R", local = TRUE)
  source("main/f_HQQU.R", local = TRUE)
  source("main/e_tab_triangle.R", local = TRUE)
  
  
}

shinyApp(ui, server)
