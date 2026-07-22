source("start.R", local = TRUE)
source("function.R", local = TRUE)
library(zoo)
ui <- dashboardPage(
  dashboardHeader(title = "ALCO"),

  dashboardSidebar(
    #id = "sidebar",  # 👈 đặt id để điều khiển bằng shinyjs
    useShinyjs(),    # 👈 kích hoạt shinyjs
    uiOutput("quy_luy_ke_ui"),
    uiOutput("year_ui"),
    uiOutput("quarter_ui"),
    uiOutput("type_ui")
  ),
  dashboardBody(
    useShinyjs(),  # 👈 cần gọi lại trong body để dùng shinyjs
    
    includeCSS("www/style.css"),
    # JavaScript để theo dõi tab và ẩn sidebar
    tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleSidebar', function(tab) {
    if (tab === 'Input' || tab === 'Compare' || tab === 'Overview' || tab === 'ty_le') {
      $('body').addClass('sidebar-collapse');
    } else {
      $('body').removeClass('sidebar-collapse');
    }
    });
    
    
     // Bắt phím Enter trong ô password
  $(document).on('keypress', '#password', function(e) {
    if (e.which == 13) {  // 13 = Enter
      $('#login_btn').click();
    }
  });
    ")),

    div(
      id = "sticky-tabbox",
      style = "position: sticky; top: 0; z-index: 1000; background: white;",

    tabBox(
      id = "tabs",  # 👈 dùng để theo dõi tab đang chọn
      width = 12,
      tabPanel("INPUT", value = "Input",
               
               uiOutput("auth_ui")  # 👈 sẽ render động
      ),
      tabPanel("VIEW", value = "view_tab" , 
               fluidRow(
                 uiOutput("kpi_cards")
               ),
               fluidRow(
                 box(width = 12, DT::dataTableOutput("table")),
               ),
               fluidRow(
                 box(
                   width = 6,
                   uiOutput("line_ui"),
                   #selectInput("line", "Chọn Nghiệp vụ:", choices = unique(data()$Line)),
                   plotlyOutput("line_bar", height = "400px"),
                   
                 ),
                 box(
                   width = 6,
                 plotlyOutput("line_ratio", height = "400px")
                 )
               )
      ),
      tabPanel("COMPARE", value = "Compare" , 
               fluidRow(
                 box(width = 12, title = "🔍 Bộ lọc so sánh giữa các quý",
                     column(
                       width = 2,
                       uiOutput("compare_quy_luyke_ui")
                       # selectInput("compare_quy_luyke", "Chọn Quý/Lũy kế:", 
                       #             choices = unique(data()$`Quý/Lũy kế`), selected = "Theo quý")
                     ),
                     column(
                       width = 1,
                       uiOutput("compare_type_ui")
                       # selectInput("compare_type", "Chọn Type:", 
                       #             choices = unique(data()$Type), selected = "NET")
                     ),
                     column(
                       width = 2,
                       selectInput("compare_metric", "Chọn chỉ tiêu:", 
                                   choices = c("OsC", "UPR", "Written", "Paid", 
                                               "Paid/Written", "Incurred/Earned", "@OsC", "@UPR", "@IBNR","@CAT","Sub total(earned)" , "Sub total(incurred)" ),
                                   selected = "OsC")
                     ),
                     column(
                       width = 2,
                       selectInput("compare_start_year", "Chọn năm bắt đầu:", 
                                   choices = 2020:2025, selected = 2023)
                     ),
                     column(
                       width = 3,
                       downloadButton("down", "Tải toàn bộ DATA"),
                       downloadButton("download_compare_excel1", "Tải bảng 1"),
                       downloadButton("download_compare_excel", "Tải bảng 2")
                     )
                 )
               ),
              
               fluidRow(
                 box(
                   width = 9,
                   title = "📊 So sánh chỉ tiêu giữa các quý",
                   DT::dataTableOutput("compare_table")
                 ),
                 box(
                   width = 3,
                   title = "🧠 Nhận xét tự động",
                   uiOutput("compare_comment")
                 )
               ),
        
               fluidRow(
                 box(
                   id = "box_type",
                   width = 9, title = uiOutput("compare_title"),
                     DT::dataTableOutput("compare_table2")),
                 box(
                   width = 3,
                   title = "🧠 Nhận xét tự động",
                 uiOutput("compare2_comment")
                 )
               ),
               fluidRow(
                 box(
                   id = "box_plot",
                   width = 12, title = "📈 Biểu đồ so sánh các năm",
                     uiOutput("compare_plot_ui"))
                 
               )
      ),
      tabPanel("ALL", value = "Overview" , 
               
               fluidRow(
                 box(width = 12, title = "Xem dữ liệu kiểu Excel",
                     DTOutput("table")
               )
      ))
    ,
    tabPanel("CHI TIẾT", value = "ty_le" ,        ################################# file: "Tabpanel_ty_le.R
             fluidRow(
               box(width = 12, title = "So sánh Gốc, Nhận, Nhượng",
                   
                   column(
                     width = 2,
                     uiOutput("tyle_line_ui"),
                   ),
                   column(
                     width = 3,
                     uiOutput("tyle_year_ui")
                   ),
                   column(
                     width = 3,
                     uiOutput("ty_le_quy_luyke_ui")
                   ),
                   
                   column(
                     width = 2,
                     downloadButton("download_tyle", "Tải về Excel")
                   )
               )
             ),
             fluidRow(
               
                 # ===== HÀNG 1 =====
                 box(
                   width = 7, 
                   title = "Chi tiết nghiệp vụ",
                   DT::dataTableOutput("compare_nv")
                 ),
                 box(
                   width = 5,
                   title = "Nhận xét",
                   uiOutput("comment_nv")
                 ),
                 box(
                   width = 12,
                   title = "Loss Ratio - Net",
                   DT::dataTableOutput("loss_nv_net")
                 ),
                 
                 box(
                   width = 12,
                   title = "Loss Ratio - Direct",
                   DT::dataTableOutput("loss_nv_goc")
                 )
                 # box(
                 #   width = 12, 
                 #   title = "Loss Ratio",
                 #   DT::dataTableOutput("loss_nv_net"),
                 #   DT::dataTableOutput("loss_nv_goc")
                 # ),
              
             )
    ))),
    tags$div(style = "height: 100px;"),  # 👈 thêm khoảng trống cuối trang

    tags$footer(
      class = "footer",
      style = "
    background-color: #f9f9f9;
    border-top: 1px solid #ddd;
    padding: 10px;
    text-align: center;
    font-size: 14px;
    color: blue;
    position: fixed;
    bottom: 0;
    z-index: 1000;

  ",

    tags$a(
        href = "https://www.baoviet.com.vn/insurance/",  # 👈 link đến trang Bảo Việt
        style = "text-decoration: none; color: gray;",
        "© 2025 BVGI Actuary"
      )
  )
  )
)


server <- function(input, output, session) {
  # password đúng (bạn có thể đổi)
  correct_password <- "228800"
  
  # trạng thái login
  auth <- reactiveVal(FALSE)
  
  observeEvent(input$login_btn, {
    if (input$password == correct_password) {
      auth(TRUE)
    } else {
      showNotification("Sai mật khẩu!", type = "error")
    }
  })
  
  # UI động
  output$auth_ui <- renderUI({
    if (!auth()) {
      # 👇 giao diện nhập password
      fluidRow(
        column(
          width = 4, offset = 4,
          box(
            title = "🔒 Nhập mật khẩu",
            width = 12,
            passwordInput("password", "Password:"),
            actionButton("login_btn", "Đăng nhập")
          )
        )
      )
    } else {
      # 👇 TOÀN BỘ NỘI DUNG TAB INPUT CỦA BẠN (copy vào đây)
      tagList(
        
                 fluidRow(
                   box(width = 12, #title = "Thông tin & Upload",
                   column(
                     width =2,
                     box(
                       width = 12,
                     # wellPanel(
                       textInput("namlk", "NĂM:", value = "2025"),
                       selectInput("quy", "QUÝ:", choices = c("Q1", "Q2", "Q3", "Q4")),
                     #
                     # ),
                     # wellPanel(
                       h4("📂 Upload VAS full", style = "font-weight:bold; color:#0066CC"),
                       fileInput("file1", NULL, accept = c(".xlsx", ".xlsm")),
                       h5("Sheet Result", style = "font-weight:bold; color:#0066CC"),
                       uiOutput("sheet_selector1"),
                       h5("Sheet Doanh thu bồi thường", style = "font-weight:bold; color:#0066CC"),
                       uiOutput("sheet_selector2"),
                       actionButton("check1", "ℹ Kiểm tra"),
                       actionButton("update", "Update DATA hiện có")
                     #, downloadButton("down", "Tải toàn bộ DATA")
                     )
                     ),
                   column(
                     width =10,
                     uiOutput("box_combined")
                   ))),
                 # Box 1

                 fluidRow(
                   box(width = 12,
                   column(
                     width = 2,
                     box(width = 12,  title = "CHỌN THAM SỐ HIỂN THỊ",
                     selectInput("quy_lk", "Qúy/Lũy kế:", choices = NULL),
                     selectInput("TYPE", "Chọn type:", choices = NULL)

                   )),
                   column(
                     width = 10,
                   uiOutput("now_cards")
                   )
                 )
                 ),
                 fluidRow(
                   box(width = 12,
                   uiOutput("all_line_ratios")

                   )
                 )
        
      )
    }
  })
  #___________________________________
  
  
  
  
  observe({
    session$sendCustomMessage("toggleSidebar", input$tabs)
  })
  output$box_combined <- renderUI({
    box(
      title = paste("KẾT QUẢ TỔNG HỢP",  paste0(input$quy,"/", input$namlk)),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      div(
        style = "overflow-x: auto; min-height:500px;",
        DTOutput("combined")   # dùng DT thay vì tableOutput
      )
    )
  })
  data <- reactiveVal(readRDS("data.rds"))
  
  output$compare_quy_luyke_ui <- renderUI({
    req(data())  # đảm bảo data không NULL
    df <- data()
    
    selectInput(
      "compare_quy_luyke", "Chọn Quý/Lũy kế:",
      choices = unique(df$`Quý/Lũy kế`),
      selected = unique(df$`Quý/Lũy kế`)[1]
    )
  })
  
  output$compare_type_ui <- renderUI({
    req(data())
    df <- data()
    
    selectInput(
      "compare_type", "Chọn Type:",
      choices = unique(df$Type),
      selected = unique(df$Type)[1]
    )
  })
  
  output$quy_luy_ke_ui <- renderUI({
    selectInput("quy_luy_ke", "Quý/Lũy kế:", choices = unique(data()$`Quý/Lũy kế`))
  })
  output$year_ui <- renderUI({
    selectInput("year", "Chọn năm:", choices = unique(data()$`Năm LK`))
  })
  output$quarter_ui <- renderUI({
    selectInput("quarter", "Chọn quý:", choices = unique(data()$`Qúy`))
  })
  output$type_ui <- renderUI({
    selectInput("type", "Chọn type:", choices = unique(data()$Type))
  })
  observe({
    df <- data()
    updateSelectInput(session, "quy_lk", choices = unique(df$`Quý/Lũy kế`))
    updateSelectInput(session, "TYPE", choices = unique(df$Type))
  })
  # Thêm line selectInput
  output$line_ui <- renderUI({
    req(data())
    selectInput("line", "Chọn Nghiệp vụ:", choices = unique(data()$Line))
  })
  output$tyle_line_ui <- renderUI({
  selectInput("tyle_line", "Chọn nghiệp vụ:", 
                           choices = unique(data()$Line))
  })
  source("Tabpanel_input.R", local = TRUE)
  source("Button_ghep.R", local = TRUE)
  source("now_card.R", local = TRUE)
  source("Button_update.R", local = TRUE)
  source("Line_ratio_all.R", local = TRUE)
  observeEvent(input$tabs, {
    if (input$tabs == "view_tab") {
      req(data())
      
      # vector quý
      quarters <- unique(data()$Năm)
      quarters <- sort(quarters)   # sắp xếp theo thứ tự
      
      # tạo dãy quý đầy đủ từ min đến max
      start <- quarters[1]
      end   <- quarters[length(quarters)]
      
      # chuyển về dạng date để dễ tạo chuỗi quý
      to_date <- function(x) {
        year <- as.integer(substr(x, 1, 4))
        q    <- as.integer(substr(x, 6, 6))
        as.Date(paste0(year, "-", (q - 1) * 3 + 1, "-01"))
      }
      from <- to_date(start)
      to   <- to_date(end)
      
      full_seq <- seq(from, to, by = "quarter")
      full_quarters <- paste0(format(full_seq, "%Y"), "Q", (as.integer(format(full_seq, "%m")) - 1) %/% 3 + 1)
      
      # tìm quý bị thiếu
      missing <- setdiff(full_quarters, quarters)
      
      msg <- paste0("Dữ liệu có từ ", start, " đến ", end, ".")
      if (length(missing) == 0) {
        msg <- paste0(msg, " Không thiếu quý nào.")
      } else {
        msg <- paste0(msg, " Thiếu các quý: ", paste(missing, collapse = ", "))
      }
      
      showModal(modalDialog(
        title = "Thông tin dữ liệu",
        msg,
        easyClose = TRUE,
        footer = modalButton("Đóng")
      ))
    }
  })
  
  
  source("Tabpanel_view.R", local = TRUE)
  source("Tabpanel_compare.R", local = TRUE)
  source("Tabpanel_excel.R", local = TRUE)
  source("Tabpanel_ty_le.R", local = TRUE)
}

shinyApp(ui, server)

