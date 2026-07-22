hehe_data <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
check_data <- reactiveVal(NULL) 
folder_path <- "www/health_care/paid"

observeEvent(input$run, {
  req(input$folder)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
  withProgress(message = "Đang xử lý dữ liệu...", {
    incProgress(0.5, detail = "Đang đọc các file Excel...")
    # print(input$folder)
    # print(input$folder$datapath)
    # print(input$folder$name)
    
    files <- input$folder$datapath
    names(files) <- input$folder$name
    
    read_main_sheet <- function(path) {
      sheets <- excel_sheets(path)
      
      sheets %>%
        map(~ read_excel(path, sheet = .x, col_types = "text")) %>%
        keep(~ ncol(.x) > 20) %>%   # <-- logic bạn yêu cầu
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
    
    # files <- input$folder$datapath
    # names(files) <- input$folder$name
    # 
    # df <- files %>%
    #   map(~ read_excel(.x, col_types = "text")) %>%
    #   bind_rows(.id = "file") %>%
    #   clean_names()
    # saveRDS(df, "test.rds" )
    
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
    print("1")
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
      mutate(across(matches("ngay|thoi_han"), convert_excel_or_string_date)) %>%
      mutate(
        ngay_phe_duyet = as.Date(ngay_phe_duyet),
        year = year(ngay_phe_duyet),
        month = month(ngay_phe_duyet)
      )
    
    hehe$check1 <- paste0(
      hehe$year, "_q",
      ifelse(hehe$month <= 3, 1,
             ifelse(hehe$month <= 6, 2,
                    ifelse(hehe$month <= 9, 3, 4)))
    )
    
    hehe$source_file <-  paste0( input$year, "_",tolower(input$quarter) , "_paid")
    hehe$paid_osc = hehe$so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh
    
    hehe_data(hehe)  # ✅ Lưu vào reactiveVal để dùng cho download
    # ==============================
    # 2. SHOW FREQ MODAL
    # ==============================
    tam <- hehe %>% filter(is.na(Co_id))
    freq <- table(tam$cong_ty)
    
    # Chuyển table thành HTML
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
          actionButton("freq_ok", "Tôi đã hiểu", class = "btn-primary")
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

observeEvent(input$freq_ok, {
  req(hehe_data())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_data()
    file_name_rds <- file.path(folder_path, paste0( input$year, "_",tolower(input$quarter) , ".rds"))
    
    if (file.exists(file_name_rds)) {
      # Tắt spinner trước khi hiện modal
      # waiter_hide()
      # Đọc file cũ và đếm kích thước
      old_data <- readRDS(file_name_rds)
      n_rows_old <- nrow(old_data)
      n_cols_old <- ncol(old_data)
      # Số hàng/cột của file mới
      n_rows_new <- nrow(hehe)
      n_cols_new <- ncol(hehe)
      waiter_hide()
      
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
      saveRDS(hehe, file_name_rds)
      tuchoi <- hehe %>%
        filter(so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh == 0)
      
      hehe <- hehe %>%
        filter(so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh != 0)
      
      # ✅ 2️⃣ Đường dẫn file RDS
      paid_file <- "www/paid_y_te.rds"
      reject_file <- "www/tu_choi_y_te.rds"

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
        
        # Thông báo
        showModal(modalDialog(
          title = "📊 Thông tin ghép dữ liệu",
          HTML(paste0(
            "Có <b>", length(common_cols), "</b> cột chung: ",
            paste(common_cols, collapse = ", "),
            if (length(missing_in_new) > 0) paste0("<br>Thiếu (đã thêm NA): ", paste(missing_in_new, collapse = ", ")) else "",
            if (length(extra_in_new) > 0) paste0("<br>Thừa (đã bỏ): ", paste(extra_in_new, collapse = ", ")) else ""
          )),
          easyClose = TRUE
        ))
        
        return(new_data)
      }
      
      # ✅ 3️⃣ Xử lý file paid_y_te
      if (file.exists(paid_file)) {
        old_paid <- readRDS(paid_file)
        hehe <- align_columns(hehe, old_paid)
        hehe <- bind_rows(old_paid, hehe) # %>% distinct()
        check_data(hehe %>%
                     group_by(so_to_trinh_boi_thuong) %>%
                     filter(
                       n() > 1,
                      
                       any(source_file == paste0( input$year, "_",tolower(input$quarter), "_paid"))
                     ) %>%
                     ungroup()  )
      } else {
        old_paid <- NULL
        showModal(modalDialog(
          title = "⚠️ File chưa tồn tại",
          HTML(paste0("File <b>", paid_file, "</b> chưa tồn tại.<br>",
                      "👉 Hệ thống sẽ tự động <b>tạo mới</b> file này.")),
          easyClose = TRUE
        ))
      }
      
      # ✅ 4️⃣ Xử lý file tu_choi_y_te
      if (file.exists(reject_file)) {
        old_reject <- readRDS(reject_file)
        tuchoi <- align_columns(tuchoi, old_reject)
        tuchoi <- bind_rows(old_reject, tuchoi) # %>% distinct()
      } else {
        showModal(modalDialog(
          title = "⚠️ File chưa tồn tại",
          HTML(paste0("File <b>", paid_file, "</b> chưa tồn tại.<br>",
                      "👉 Hệ thống sẽ tự động <b>tạo mới</b> file này.")),
          easyClose = TRUE
        ))
      }
      
      
      # ✅ 5️⃣ Ghi lại file
      saveRDS(hehe, paid_file)
      saveRDS(tuchoi, reject_file)
      
    }
    # ẩn spinner khi hoàn tất
    waiter_hide()
  }, error = function(e) {
    # Khi lỗi → ẩn spinner và báo lỗi
    waiter_hide()
    showModal(modalDialog(
      title = "❌ Lỗi xử lý",
      HTML(paste0("Đã xảy ra lỗi: <br>", e$message)),
      easyClose = TRUE
    ))
  })
})
# Khi người dùng bấm "Ghi đè"
observeEvent(input$overwrite, {
  req(hehe_data())
  # Hiện spinner nhỏ khi ghi đè
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  tryCatch({
  file_name_rds <- file.path(folder_path, paste0( input$year, "_",tolower(input$quarter) , ".rds"))
  saveRDS(hehe_data(), file_name_rds)
  # ✅ 1️⃣ Tách dữ liệu
  tuchoi <- hehe_data()%>%
    filter(so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh == 0)
  
  hehe <- hehe_data() %>%
    filter(so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh != 0)
  
  # ✅ 2️⃣ Đường dẫn file RDS
  paid_file <- "www/paid_y_te.rds"
  reject_file <- "www/tu_choi_y_te.rds"
  
  # ✅ 3️⃣ Kỳ cần xử lý (dấu hiệu nhận biết)
  current_source <- paste0(input$year, "_", tolower(input$quarter), "_paid")
  
  # ==== HÀM TIỆN ÍCH: chuẩn hóa cột & ghi đè theo kỳ ====
  align_and_replace <- function(new_data, old_data, source_col = "source_file") {
    # Nếu file cũ rỗng → trả lại new_data
    if (is.null(old_data) || nrow(old_data) == 0) return(new_data)
    
    # 🧩 Danh sách cột
    common_cols <- intersect(names(new_data), names(old_data))
    missing_in_new <- setdiff(names(old_data), names(new_data))
    extra_in_new <- setdiff(names(new_data), names(old_data))
    
    # 🧱 Thêm cột thiếu (NA)
    for (col in missing_in_new) new_data[[col]] <- NA
    
    # 🔄 Giữ đúng thứ tự & cột của file cũ
    new_data <- new_data[, names(old_data)]
    
    # 🗑️ Xóa dữ liệu trùng kỳ trong file cũ (nếu có)
    if (source_col %in% names(old_data)) {
      old_data <- old_data %>%
        filter(.data[[source_col]] != current_source)
    }
    
    # 🧩 Ghép lại
    result <- bind_rows(old_data, new_data) 
    
    # 📢 Thông báo
    showModal(modalDialog(
      title = "📊 Thông tin ghép dữ liệu",
      HTML(paste0(
        "Có <b>", length(common_cols), "</b> cột chung: ",
        paste(common_cols, collapse = ", "),
        if (length(missing_in_new) > 0) paste0("<br>Thiếu (đã thêm NA): ", paste(missing_in_new, collapse = ", ")) else "",
        if (length(extra_in_new) > 0) paste0("<br>Thừa (đã bỏ): ", paste(extra_in_new, collapse = ", ")) else "",
        "<br><b>Đã xóa dữ liệu cũ của kỳ:</b> ", current_source
      )),
      easyClose = TRUE
    ))
    
    return(result)
  }
  
  # ✅ 4️⃣ Xử lý file paid_y_te
  if (file.exists(paid_file)) {
    old_paid <- readRDS(paid_file)
    hehe <- align_and_replace(hehe, old_paid)
  } 
  
  # ✅ 5️⃣ Xử lý file tu_choi_y_te
  if (file.exists(reject_file)) {
    old_reject <- readRDS(reject_file)
    tuchoi <- align_and_replace(tuchoi, old_reject)
  } 
  check_data(hehe %>%
               group_by(so_to_trinh_boi_thuong) %>%
               filter(
                 n() > 1,
                 any(source_file == paste0( input$year, "_",tolower(input$quarter), "_paid"))
               ) %>%
               ungroup()  )
  
  # ✅ 6️⃣ Lưu lại file RDS
  saveRDS(hehe, paid_file)
  saveRDS(tuchoi, reject_file)
  # 🔹 Ẩn spinner khi hoàn tất
  waiter_hide()
  
  # ✅ 8️⃣ Thông báo tổng kết
  showModal(modalDialog(
    title = "✅ Hoàn tất xử lý dữ liệu",
    HTML(paste0(
      "Đã cập nhật:<br>",
      "- <b>paid_y_te.rds</b>: ", nrow(hehe), " hàng, ", ncol(hehe), " cột.<br>",
      "- <b>tu_choi_y_te.rds</b>: ", nrow(tuchoi), " hàng, ", ncol(tuchoi), " cột.<br>",
      "<b>Kỳ hiện tại:</b> ", current_source
    )),
    easyClose = TRUE
  ))
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
# ✅ Nút download: truy cập dữ liệu qua hehe_data()
output$downloadData <- downloadHandler(
  
  filename = function() {
    paste0("hc_paid_",tolower(input$quarter), "_", input$year, ".xlsx")
  },
  content = function(file) {
    req(hehe_data())  # đảm bảo có dữ liệu
    df = hehe_data()
    # Chuyển kiểu dữ liệu
    df$so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh <- as.numeric(df$so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh)
    
    # Tạo cột logic mtn
    df$mtn <- ifelse(
      !is.na(df$xcg_bhyt_pa) &
        (df$so_tien_uoc_boi_thuong_so_tien_boi_thuong_sau_dbh - df$xcg_bhyt_pa > 0),
      TRUE,
      FALSE
    )
    
    
    writexl::write_xlsx(df, file)
  }
)

# ✅ Nút download: truy cập dữ liệu qua hehe_data()
output$downloadDatacheck <- downloadHandler(
  
  filename = function() {
    paste0("hc_check_paid_",tolower(input$quarter), "_", input$year,  ".xlsx")
  },
  content = function(file) {
    req(check_data())  # đảm bảo có dữ liệu
    writexl::write_xlsx(check_data(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_data(),
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