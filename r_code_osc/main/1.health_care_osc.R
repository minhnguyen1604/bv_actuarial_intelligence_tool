hehe_data1 <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
#check_data <- reactiveVal(NULL) 
folder_path1 <- "www/health_care/osc"
need_continue <- reactiveVal(FALSE)


observeEvent(input$run1, {
  req(input$folder1)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
    withProgress(message = "Đang xử lý dữ liệu...", {
      incProgress(0.2, detail = "Đang đọc các file Excel...")
      files <- input$folder1$datapath
      names(files) <- input$folder1$name
      
      read_main_sheet <- function(path) {
        sheets <- excel_sheets(path)
        
        sheets %>%
          map(~ read_excel(path, sheet = .x, col_types = "text")) %>%
          keep(~ ncol(.x) > 30) %>%   # <-- logic bạn yêu cầu
          pluck(1)
      }
      
      df <- if (length(files) == 1) {
        
        print(excel_sheets(files[1]))
        
        read_main_sheet(files[1]) %>%
          clean_names() %>%
          mutate(file = names(files)[1], .before = 1)
        
      } else {
        
        files %>%
          map(read_main_sheet) %>%
          bind_rows(.id = "file") %>%
          mutate(file = names(files)[as.integer(file)]) %>%
          clean_names()
      } 
      saveRDS(df, "test.rds")
      # 
      # df <- files %>%
      #   map(~ read_excel(.x, col_types = "text")) %>%
      #   bind_rows(.id = "source_file") %>%
      #   clean_names()
      
      incProgress(0.4, detail = "Đang chuẩn hóa tên công ty...")
      df <- df %>%
        mutate(
          cong_ty_clean = cong_ty %>%
            str_to_lower() %>%
            str_replace_all("tổng công ty bảo hiểm.*", "tsc") %>%
            str_replace_all("trụ sở chính.*", "tsc") %>%
            str_replace_all("tsc.*", "tsc") %>%
            str_remove_all("công ty bảo việt|bảo việt|bv") %>%
            str_squish() %>%
            str_to_title() %>%
            stri_trans_general("Latin-ASCII")
        ) %>%
        mutate(
          cong_ty_clean = cong_ty_clean %>%
            str_replace_all("^Dac Lac.*", "Dak Lak") %>%
            str_replace_all("^Bac Can.*", "Bac Kan") %>%
            str_replace_all("^Vung Tau.*", "Ba Ria - Vung Tau") %>%
            str_replace_all("^Ho Chi Minh.*", "Thanh Pho Ho Chi Minh")
        )
      
      # --- Đọc file doanh thu ---
      
      incProgress(0.6, detail = "Đọc file doanh thu...")
      dt <- read_excel("doanh_thu_moi.xlsx") %>%
        mutate(
          `Công ty` = `Công ty` %>%
            str_to_lower() %>%
            str_squish() %>%
            str_to_title() %>%
            stri_trans_general("Latin-ASCII")
        )
      
      incProgress(1, detail = "Join dữ liệu...")
      hehe <- df %>%
        left_join(dt, by = c("cong_ty_clean" = "Công ty")) %>%
        mutate(across(matches("ngay|thoi_han"), convert_excel_or_string_date)) 
      # Lưu dữ liệu vào reactiveVal, chờ bấm freq_ok
      hehe$source_file <- paste0(input$year, "_", tolower(input$quarter), "_osc")
      hehe$paid_osc = hehe$so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh
      hehe_data1(hehe)
      # ==============================
      # 2. SHOW FREQ MODAL
      # ==============================
      # --- Tính freq ---
      tam <- hehe %>% filter(is.na(Co_id))
      freq <- table(tam$cong_ty)
      freq_df <- as.data.frame(freq)
      freq_html <- paste0(
        "<table class='table table-bordered table-striped' style='width:100%'>",
        "<thead><tr><th>Công ty</th><th>Số lượng</th></tr></thead><tbody>",
        paste(
          apply(freq_df, 1, function(row) {
            sprintf("<tr><td>%s</td><td>%s</td></tr>", row[1], row[2])
          }),
          collapse = ""
        ),
        "</tbody></table>"
      )
      
     
      
      waiter_hide()
      
      # Show modal freq
      showModal(
        modalDialog(
          title = "📊 Tần suất công ty chưa có Co_id",
          HTML(freq_html),
          easyClose = FALSE,
          footer = tagList(
            actionButton("freq_ok1", "Tôi đã hiểu", class = "btn-primary")
          )
        )
      )
    })
  }, error = function(e) {
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi xử lý",
      HTML(paste0("Đã xảy ra lỗi: <br>", e$message)),
      easyClose = TRUE
    ))
  })
})
observeEvent(input$freq_ok1, {
  req(hehe_data1())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_data1()
    file_name_rds <- file.path(folder_path1, paste0(input$year, "_", tolower(input$quarter), ".rds"))
      print(file_name_rds)
      if (file.exists(file_name_rds)) {
        # Tắt spinner trước khi hiện modal
        waiter_hide()
        # Đọc file cũ và đếm kích thước
        old_data <- readRDS(file_name_rds)
        n_rows_old <- nrow(old_data)
        n_cols_old <- ncol(old_data)
        # Số hàng/cột của file mới
        n_rows_new <- nrow(hehe)
        n_cols_new <- ncol(hehe)
        
        showModal(modalDialog(
          title = "⚠️ File đã tồn tại",
          HTML(paste0(
            "File ", file_name_rds, " đã tồn tại.<br>",
            "File cũ: ", n_rows_old, " hàng, ", n_cols_old, " cột.<br>",
            "File mới: ", n_rows_new, " hàng, ", n_cols_new, " cột."
          )),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Đóng"),
            actionButton("overwrite", "Ghi đè file này", class = "btn-danger")
          )
        ))
        # dừng tiếp xử lý ở đây vì người dùng cần chọn overwrite hay không
        return(invisible())
      } else {
        # Ghi file mới
        saveRDS(hehe, file_name_rds)
        showModal(modalDialog(
          title = "✅ Hoàn tất",
          paste("Đã lưu file", basename(file_name_rds)),
          easyClose = TRUE
        ))
      }
      
      waiter_hide()
  }, error = function(e) {
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi lưu file",
      HTML(paste0("Đã có lỗi: ", e$message)),
      easyClose = TRUE
    ))
  })
})

# Khi người dùng bấm "Ghi đè"
observeEvent(input$overwrite, {
  req(hehe_data1())
  # Hiện spinner nhỏ khi ghi đè
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  tryCatch({
    file_name_rds <- file.path(folder_path1, paste0( input$year, "_",tolower(input$quarter) , ".rds"))
    saveRDS(hehe_data1(), file_name_rds)
    removeModal()
    waiter_hide()
    # ✅ Thông báo xác nhận
    showModal(modalDialog(
      title = "✅ Hoàn tất",
      paste("Đã ghi đè thành công file", basename(file_name_rds)),
      easyClose = TRUE
    ))
    # 🔹 Ẩn spinner khi hoàn tất
    # ✅ Reset lại input$overwrite (tránh trigger lại)
    session$sendInputMessage("overwrite", NULL)
    
  }, error = function(e) {
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi khi ghi đè",
      HTML(paste0("Đã có lỗi: ", e$message)),
      easyClose = TRUE
    ))
  }, finally = {
    try({ waiter_hide() }, silent = TRUE)
  })
})

# ✅ Nút download: truy cập dữ liệu qua hehe_data1()
output$downloadData1 <- downloadHandler(
  
  filename = function() {
    paste0("hc_osc_", tolower(input$quarter), "_", input$year, ".xlsx")
  },
  content = function(file) {
    req(hehe_data1())  # đảm bảo có dữ liệu
    
    writexl::write_xlsx(hehe_data1(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_data1(),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "400px",
      autoWidth = TRUE,
      columnDefs = list(list(width = '150px', targets = "_all"))
    ),
    class = "display compact stripe hover nowrap"
  )
})

# ✅ Nút download: truy cập dữ liệu qua hehe_data1()
output$downloadDatacheck1 <- downloadHandler(
  filename = function() {
    paste0("hc_check_osc_",tolower(input$quarter), "_", input$year, ".xlsx")
  },
  content = function(file) {
    req(hehe_data1())  # đảm bảo có dữ liệu
    waiter_show(html = tagList(
      spin_ellipsis(),
      h4("⏳ Đang tạo file kiểm tra...")
    ))
    tryCatch({
    paid_file <- "www/paid_y_te.rds"
    # ==== HÀM TIỆN ÍCH: chuẩn hóa cột trước khi ghép ====
    align_columns <- function(new_data, old_data) {
      # Nếu file cũ rỗng, trả lại new_data
      if (is.null(old_data) || nrow(old_data) == 0) return(new_data)
      
      # Danh sách cột
      common_cols <- intersect(names(new_data), names(old_data))
      missing_in_new <- setdiff(names(old_data), names(new_data))
      extra_in_new <- setdiff(names(new_data), names(old_data))
      
      # Thêm cột thiếu
      for (col in missing_in_new) {
        new_data[[col]] <- NA
      }
      
      # Giữ đúng thứ tự & cột của file cũ
      new_data <- new_data[, names(old_data)]
      
      return(new_data)
    }
    
    
    # ✅ 3️⃣ Xử lý file paid_y_te
    if (file.exists(paid_file)) {
      old_paid <- readRDS(paid_file)
      hehe = hehe_data1()
      hehe <- align_columns(hehe, old_paid)
      df <- bind_rows(old_paid, hehe) # %>% distinct()
      
      check_data=df %>%  filter(!is.na(so_to_trinh_boi_thuong)) %>%
        group_by(so_to_trinh_boi_thuong) %>%
        filter(
          n() > 1,
          any(source_file == paste0( input$year, "_",tolower(input$quarter) ,"_osc"))
        ) %>%
        ungroup()  
      #saveRDS(df,"D:\\R_Project\\Hieu_qua_quy_uoc\\www\\df.rds" )
      
      
    } else {
      waiter_hide()
      showModal(modalDialog(
        title = "⚠️ Không tìm thấy file dữ liệu",
        "Không có file www/paid_y_te.rds để so sánh!",
        easyClose = TRUE
      ))
      return()
    }
    writexl::write_xlsx(check_data, file)
    # ✅ Ẩn spinner sau khi hoàn tất
    waiter_hide()
    }, error = function(e) {
      waiter_hide()
      showModal(modalDialog(
        title = "❌ Lỗi khi tạo file",
        HTML(paste0("Đã có lỗi: ", e$message)),
        easyClose = TRUE
      ))
    })
  }
)