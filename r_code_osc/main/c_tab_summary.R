# ================================================
# 📊 TAB VISUALIZE — TẠO BẢNG NGHIỆP VỤ × CÔNG TY
# ================================================

# FUNCTION
get_file <- function(nv, year, quarter, type) {
  folder <- ifelse(type == "PAID", "paid", "osc")
  file_name <- paste0(year, "_", tolower(quarter), ".rds")
  file.path("www", nv, folder, file_name)
}

make_zero_df <- function(master_df) {
  tibble(
    Co_id = master_df$Co_id,
    DuPhong = 0
  )
}
# ================================================

dt = read_excel("doanh_thu_moi.xlsx")
# Danh sách các công ty
co_ids <- unique(dt$Co_id) 
cong_ty <- unique(dt$`Công ty`) 
# Tạo bảng master
master_df <- tibble(
  Co_id = co_ids,
  `Công ty` = cong_ty,
  HEALTH_CARE = 0,
  XCG = 0,
  PA = 0,
  ENG = 0,
  FIRE = 0,
  MISC = 0,
  CARGO =0,
  MARINE =0
)


# Lưu master table bằng reactiveVal
master_df_r <- reactiveVal(master_df)


# Hàm cập nhật master
update_master <- function(master_df, df_sum, nghiep_vu_col) {
  master_df <- master_df %>%
    left_join(df_sum, by = c("Co_id" = "Co_id")) %>%
    mutate(
      !!nghiep_vu_col := ifelse(!is.na(DuPhong), DuPhong, !!sym(nghiep_vu_col))
    ) %>%
    select(-DuPhong)
  
  return(master_df)
}


observeEvent(input$viz_run, {
  req(input$viz_year, input$viz_quarter, input$viz_type)
  # Tạo biến thời gian theo input
  target_quarter <- paste0( input$viz_quarter, "/", input$viz_year)
  print(target_quarter)
  # Lọc dòng tương ứng
  
  ty_gia <- readRDS("ty_gia.rds")
  ty_gia_selected <- ty_gia %>%
    filter(Thoi_gian == target_quarter)
  
  # Lấy tỷ giá USD
  usd_rate <- ty_gia_selected$USD[1]  # [1] để chắc chắn lấy 1 giá trị
  print(usd_rate)
  # Tạo đường dẫn file
  path <- get_file("health_care",input$viz_year, input$viz_quarter, input$viz_type)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )
  
  df_sum <- tryCatch({
    df <- readRDS(path)
    df_sum <- df %>%
          mutate(DuPhong = as.numeric(so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh)) %>%
          group_by(Co_id) %>%
          summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
  }, error = function(e){
    message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
    make_zero_df(master_df_r())
  })
 
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "HEALTH_CARE")
  
  master_df_r(master_df_new)
  
  
  #______________________________________________________________________________________________________PA
  
  # Tạo đường dẫn file
  path <- get_file("pa",input$viz_year, input$viz_quarter, input$viz_type)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )
  
  df_sum <- tryCatch({
    df <- readRDS(path)
    df_sum <- df %>%
      mutate(DuPhong = as.numeric(SoTienDaTraBT_VND)) %>%
      group_by(Co_id) %>%
      summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
  }, error = function(e){
    message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
    make_zero_df(master_df_r())
  })
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "PA")
  
  master_df_r(master_df_new)
  
  
  #______________________________________________________________________________________________________XCG
  
  # Tạo đường dẫn file
  path <- get_file("xcg",input$viz_year, input$viz_quarter, input$viz_type)

  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )

  df_sum <- tryCatch({
    df <- readRDS(path)
    df_sum <- df %>%
      mutate(DuPhong = as.numeric(paid_osc)) %>%
      group_by(Co_id) %>%
      summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
  }, error = function(e){
    message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
    make_zero_df(master_df_r())
  })
  

  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "XCG")

  master_df_r(master_df_new)
  
  #______________________________________________________________________________________________________eng
  
  nv = "ts_kt_tn"
  folder <- ifelse(input$viz_type == "PAID", "paid", "osc")
  
  file_name <- paste0(input$viz_year, "_", tolower(input$viz_quarter), ".rds")
  path <- file.path("www", nv,  file_name)
  print(path)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )

  if (folder == "osc") {
  df_sum <- tryCatch({
    df <- readRDS(path)
    df_sum <- df %>% filter(grepl("^e",source, ignore.case = TRUE) ) %>% filter( grepl("osc",source, ignore.case = TRUE)) %>%
      mutate(DuPhong = as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd) + as.numeric(boi_thuong_con_lai_cua_bao_viet_usd) * usd_rate) %>%
      group_by(Co_id) %>%
      summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
  }, error = function(e){
    message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
    make_zero_df(master_df_r())
  })
  
  } else {
    df_sum <- tryCatch({
      df <- readRDS(path)
      df_sum <- df %>% filter(grepl("^e",source, ignore.case = TRUE) ) %>%
        filter(grepl("paid", source, ignore.case = TRUE)) %>% 
        filter(!grepl("gđ|gd", thanh_toan, ignore.case = TRUE)) %>%
        mutate(DuPhong = coalesce(as.numeric(so_tien_net_vnd), 0) +
                 coalesce(as.numeric(so_tien_net_usd), 0) * usd_rate
        )%>% group_by(Co_id) %>%
        summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
    }, error = function(e){
      message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
      make_zero_df(master_df_r())
    })
    
    
  }
  
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "ENG")
  
  master_df_r(master_df_new)
  
  #______________________________________________________________________________________________________fire
  
  nv = "ts_kt_tn"
  #folder <- ifelse(input$viz_type == "PAID", "paid", "osc")
  file_name <- paste0(input$viz_year, "_", tolower(input$viz_quarter), ".rds")
  path <- file.path("www", nv,  file_name)
  print(path)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )
  
  if (folder == "osc") {
    df_sum <- tryCatch({
      df <- readRDS(path)
      df_sum <- df %>% filter(grepl("^p",source, ignore.case = TRUE) ) %>% filter( grepl("osc",source, ignore.case = TRUE)) %>%
        mutate(DuPhong = as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd) + as.numeric(boi_thuong_con_lai_cua_bao_viet_usd) * usd_rate) %>%
        group_by(Co_id) %>%
        summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
    }, error = function(e){
      message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
      make_zero_df(master_df_r())
    })
    
  } else {
    df_sum <- tryCatch({
      df <- readRDS(path)
      df_sum <- df %>% filter(grepl("^p",source, ignore.case = TRUE) ) %>%
        filter(grepl("paid", source, ignore.case = TRUE)) %>% 
        filter(!grepl("gđ|gd", thanh_toan, ignore.case = TRUE)) %>%
        mutate(DuPhong = coalesce(as.numeric(so_tien_net_vnd), 0) +
                 coalesce(as.numeric(so_tien_net_usd), 0) * usd_rate
        )%>% group_by(Co_id) %>%
        summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
    }, error = function(e){
      message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
      make_zero_df(master_df_r())
    })
    
    
  }
  
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "FIRE")
  
  master_df_r(master_df_new)
  #______________________________________________________________________________________________________ misc
  
  nv = "ts_kt_tn"
  #folder <- ifelse(input$viz_type == "PAID", "paid", "osc")
  file_name <- paste0(input$viz_year, "_", tolower(input$viz_quarter), ".rds")
  path <- file.path("www", nv,  file_name)
  print(path)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )
  
  if (folder == "osc") {
    df_sum <- tryCatch({
      df <- readRDS(path)
      df_sum <- df %>% filter(grepl("^m",source, ignore.case = TRUE) ) %>% filter( grepl("osc",source, ignore.case = TRUE)) %>%
        mutate(DuPhong = as.numeric(boi_thuong_con_lai_cua_bao_viet_vnd) + as.numeric(boi_thuong_con_lai_cua_bao_viet_usd) * usd_rate) %>%
        group_by(Co_id) %>%
        summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
    }, error = function(e){
      message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
      make_zero_df(master_df_r())
    })
    
  } else {
    df_sum <- tryCatch({
      df <- readRDS(path)
      df_sum <- df %>% filter(grepl("^m",source, ignore.case = TRUE) ) %>%
        filter(grepl("paid", source, ignore.case = TRUE)) %>% 
        filter(!grepl("gđ|gd", thanh_toan, ignore.case = TRUE)) %>%
        mutate(DuPhong = coalesce(as.numeric(so_tien_net_vnd), 0) +
                 coalesce(as.numeric(so_tien_net_usd), 0) * usd_rate
        ) %>% group_by(Co_id) %>%
        summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
    }, error = function(e){
      message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
      make_zero_df(master_df_r())
    })
    
    
  }
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "MISC")
  
  master_df_r(master_df_new)
  #______________________________________________________________________________________________________ hàng
  
  # Tạo đường dẫn file
  path <- get_file("cargo",input$viz_year, input$viz_quarter, input$viz_type)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )

  df_sum <- tryCatch({
    df <- readRDS(path)
    df_sum <- df %>%
      mutate(DuPhong = as.numeric(paid_osc)) %>%
      group_by(Co_id) %>%
      summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
  }, error = function(e){
    message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
    make_zero_df(master_df_r())
  })
  print(nrow(df_sum))
  
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "CARGO")
  
  master_df_r(master_df_new)
  #______________________________________________________________________________________________________ tàu
  
  # Tạo đường dẫn file
  path <- get_file("marine",input$viz_year, input$viz_quarter, input$viz_type)
  
  showNotification(
    paste("📁 PATH:", normalizePath(path, mustWork = FALSE)),
    type = "message",
    duration = 5
  )
  
  df_sum <- tryCatch({
    df <- readRDS(path)
    df_sum <- df %>%
      mutate(DuPhong = as.numeric(paid_osc)) %>%
      group_by(Co_id) %>%
      summarise(DuPhong = sum(DuPhong, na.rm = TRUE), .groups = "drop")
  }, error = function(e){
    message("⚠️ File không tồn tại hoặc lỗi đọc file: ", path)
    make_zero_df(master_df_r())
  })
  print(nrow(df_sum))
  
  # Cập nhật master_df
  master_df_new <- update_master(master_df_r(), df_sum, "MARINE")
  
  #master_df_r(master_df_new)
  
  # Sau khi cập nhật một nghiệp vụ OSC
  master_df_new <- master_df_r() %>%
    mutate(TONG_OSC = XCG + PA + ENG + FIRE + MISC + CARGO + MARINE + HEALTH_CARE )
  
  # Lưu lại
  master_df_r(master_df_new)
  
  output$viz_table_ui <- renderUI({
    req(master_df_r() )
    
    # Chuyển các cột số thành dạng accounting
    master_df_display <- master_df_r() %>%
      mutate(across(where(is.numeric), ~ formatC(.x, format="f", big.mark=",", digits=0)))
    HTML(paste0(
      "<style>
       table.table td { text-align: right !important; }
     </style>",
      kable(master_df_display, format = "html",
            table.attr = "class='table table-bordered table-striped'") %>%
        kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed"))
    ))
  })
  
  # Download handler
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("master_df_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Ghi file Excel
      openxlsx::write.xlsx(master_df_r(), file, asTable = TRUE)
    }
  )
 
})