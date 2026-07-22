observeEvent(input$confirm_date, {
  # =========================
  # 1. XÁC ĐỊNH FOLDER QUÝ
  # =========================
  cur_dir <- reactive({
    req(date_now())
    file.path("www", paste0("cur_data_", date_now()))
  })
  
  pre_dir <- reactive({
    req(date_now())
    file.path("www", paste0("cur_data_", pre_quarter(date_now())))
  })
  
  # =========================
  # 2. ĐỌC DATA RDS
  # =========================
  read_rds_folder <- function(folder) {
    files <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) == 0) return(NULL)
    
    data_list <- lapply(files, function(f) {
      df <- readRDS(f)
      
      # ép kiểu các cột tiền
      money_cols <- grep("So_tien|Phi|Tien", names(df), value = TRUE)
      
      df <- df |>
        dplyr::mutate(
          dplyr::across(
            all_of(money_cols),
            ~ suppressWarnings(as.numeric(.))
          )
        )
      
      df
    })
    
    dplyr::bind_rows(data_list)
  }
  
  
  cur_data <- reactive({
    req(cur_dir())
    read_rds_folder(cur_dir())
  })
  
  pre_data <- reactive({
    req(pre_dir())
    read_rds_folder(pre_dir())
  })
  print("huhu")
  
  # =========================
  # 3. SO SÁNH 2 QUÝ
  # =========================
  diff_all <- reactive({
    req(cur_data(), pre_data())
    
    keys <- c("So_InsureJ", "So_don_Ma_hop_dong_Ma_SDBS")
    
    cur <- cur_data()
    pre <- pre_data()
    
    full <- dplyr::full_join(
      cur, pre,
      by = keys,
      suffix = c("_cur", "_pre")
    )
    
    # tránh lỗi data rỗng
    if (nrow(full) == 0) return(NULL)
    
    full$Trang_thai <- dplyr::case_when(
      is.na(full[[paste0(keys[1], "_pre")]]) ~ "Mới",
      is.na(full[[paste0(keys[1], "_cur")]]) ~ "Mất",
      TRUE ~ "Tồn tại"
    )
    
    full
  })
  
  # =========================
  # 4. CHIA THEO NGHIỆP VỤ
  # =========================
  diff_by_line <- reactive({
    req(diff_all())
    
    split(diff_all(), diff_all()$Nghiep_vu)
  })
  
  # =========================
  # 5. HIỂN THỊ RA MÀN HÌNH
  # =========================
  output$ui_result <- renderUI({
    req(diff_by_line())
    
    tagList(
      lapply(names(diff_by_line()), function(nv) {
        column(
          width = 3,
          box(
            title = nv,
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DT::DTOutput(paste0("table_", nv)),
            br(),
            downloadButton(
              outputId = paste0("download_", nv),
              label = "Tải Excel"
            )
          )
        )
      })
    )
  })
  
  # =========================
  # 6. RENDER TABLE + DOWNLOAD
  # =========================
  observe({
    req(diff_by_line())
    
    lapply(names(diff_by_line()), function(nv) {
      
      local({
        nv_local <- nv
        data_nv <- diff_by_line()[[nv_local]]
        
        output[[paste0("table_", nv_local)]] <- DT::renderDT({
          DT::datatable(
            data_nv,
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        
        output[[paste0("download_", nv_local)]] <- downloadHandler(
          filename = function() {
            paste0("So_sanh_", nv_local, "_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            openxlsx::write.xlsx(data_nv, file)
          }
        )
      })
      
    })
  })
}) 
  