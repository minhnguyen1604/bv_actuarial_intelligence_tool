hehe_xcg <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
check_xcg <- reactiveVal(NULL) 
folder_xcg <- "www/xcg/paid"
observeEvent(input$run_xcg, {
  req(input$file_xcg)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
    withProgress(message = "Đang xử lý dữ liệu...", {
      incProgress(0.2, detail = "Đang đọc các file Excel...")
      files <- input$file_xcg$datapath
      names(files) <- input$file_xcg$name
      
      df <- files %>%
        map(~ read_excel(.x, col_types = "text")) %>%
        bind_rows(.id = "source_file") %>%
        clean_names()
     
      new_names <- colnames(df)
      new_names[grepl("parent|cong", new_names, ignore.case = TRUE)] <- "cong_ty"
      new_names[grepl("BUSINESS|KD|phong", new_names, ignore.case = TRUE)] <- "phong_kd"
      new_names[grepl("COV|PHAM|phạm", new_names, ignore.case = TRUE)] <- "pham_vi_bt"
      new_names[grepl("CLAIM_URN|so kn|khieu.*nai", new_names, ignore.case = TRUE)] <- "so_kn"
      new_names[grepl("HOLDER|KHaCH|^Chu", new_names, ignore.case = TRUE)] <- "chu_xe"
      new_names[grepl("BKS|bien.*k", new_names, ignore.case = TRUE)] <- "bks"
      new_names[grepl("POLICY_URN|ĐƠN|don", new_names, ignore.case = TRUE)] <- "don_ij"
      new_names[grepl("TO$|THuC|thúc", new_names, ignore.case = TRUE)] <- "ngay_ket_thuc"
      new_names[grepl("FROM$|bat dau|bắt|n.*luc", new_names, ignore.case = TRUE)] <- "ngay_hieu_luc"
      new_names[grepl("LOSS|THaT|tổn", new_names, ignore.case = TRUE)] <- "ngay_ton_that"
      new_names[grepl("REPORTED|Bao|báo", new_names, ignore.case = TRUE)] <- "ngay_thong_bao"
      new_names[grepl("tien.*boi|Paid|boi.*tra", new_names, ignore.case = TRUE)] <- "paid_osc"
      
      new_names[grepl("ngay.*duyet", new_names, ignore.case = TRUE)] <- "ngay_duyet_boi_thuong"
      
      
      new_names[grepl("n.*bt", new_names, ignore.case = TRUE)] <- "ngay_boi_thuong"
      
      new_names[grepl("thu ly", new_names, ignore.case = TRUE)] <- "thu_ly"
      new_names[grepl("TT_KN", new_names, ignore.case = TRUE)] <- "tt_kn"
      colnames(df) <- new_names
      
      
      print(colnames(df))
      
      incProgress(0.4, detail = "Đang chuẩn hóa tên công ty...")
      
      if (!"cong_ty" %in% names(df)) {
        waiter_hide()
        showModal(modalDialog(
          title = "❌ Thiếu cột bắt buộc",
          HTML(paste0(
            "Không tìm thấy cột <b>cong_ty</b> trong dữ liệu.<br>",
            "Vui lòng kiểm tra lại file Excel — có thể tên cột bị sai, ví dụ: <br>",
            "<i>", paste(names(df), collapse = ", "), "</i>"
          )),
          easyClose = TRUE
        ))
        return(invisible())  # 🔹 Dừng toàn bộ xử lý ở đây
      }
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
            str_replace_all("^Dac Nong.*", "Dak Nong") %>%
            str_replace_all("^Bac Can.*", "Bac Kan") %>%
            str_replace_all("^Vung Tau.*", "Ba Ria - Vung Tau") %>%
            str_replace_all("^Ho Chi Minh.*|Tp Ho Chi Minh", "Thanh Pho Ho Chi Minh")
        )
      
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
      
      hehe$source_file <-  paste0( input$year, "_",tolower(input$quarter) , "_paid" )
      
      
      hehe_xcg(hehe)  # ✅ Lưu vào reactiveVal để dùng cho download
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
            actionButton("freq_xcg", "Tôi đã hiểu", class = "btn-primary")
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

observeEvent(input$freq_xcg, {
  req(hehe_xcg())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_xcg()
      
      file_name_rds <- file.path(folder_xcg, paste0( input$year, "_",tolower(input$quarter) ,  ".rds"))
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
        saveRDS(hehe, file_name_rds)
        showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
        # ✅ 2️⃣ Đường dẫn file RDS
        paid_file <- "www/paid_xcg.rds"
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
        
        # ✅ 3️⃣ Xử lý file paid_xcg
        if (file.exists(paid_file)) {
          old_paid <- readRDS(paid_file)
          hehe <- align_columns(hehe, old_paid)
          hehe <- bind_rows(old_paid, hehe) # %>% distinct()
          
        } else {
          old_paid <- NULL
          showModal(modalDialog(
            title = "⚠️ File chưa tồn tại",
            HTML(paste0("File <b>", paid_file, "</b> chưa tồn tại.<br>",
                        "👉 Hệ thống sẽ tự động <b>tạo mới</b> file này.")),
            easyClose = TRUE
          ))
        }
        check_xcg(hehe %>%
                    group_by(bks, ngay_ton_that, pham_vi_bt)  %>%
                    filter(
                      n() > 1,
                      n_distinct(source_file) > 1,
                      any(source_file ==   paste0( input$year, "_",tolower(input$quarter) , "_",tolower(input$quarter) , "_paid" ))
                    ) %>%
                    ungroup()  )
        # ✅ 5️⃣ Ghi lại file
        saveRDS(hehe, paid_file)
        showNotification(paste("✅ Đã lưu file:",  paid_file), type = "message")
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
  }, finally = {
    # đảm bảo ẩn spinner nếu chưa ẩn
    try({ waiter_hide() }, silent = TRUE)
  })
})
# Khi người dùng bấm "Ghi đè"
observeEvent(input$overwrite, {
  req(hehe_xcg())
  # 🔹 Đảm bảo chỉ chạy 1 lần duy nhất
  removeModal()           # đóng modal trước khi xử lý
  isolate({                # không cho reactive khác tự chạy lại trong khi xử lý
    waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  })
  tryCatch({
    file_name_rds <- file.path(folder_xcg, paste0( input$year, "_",tolower(input$quarter) ,  ".rds"))
    saveRDS(hehe_xcg(), file_name_rds)
    showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
    hehe = hehe_xcg()
    # ✅ 2️⃣ Đường dẫn file RDS
    paid_file <- "www/paid_xcg.rds"
    
    # ✅ 3️⃣ Kỳ cần xử lý (dấu hiệu nhận biết)
    current_source <- paste0(input$year, "_",tolower(input$quarter), "_paid" )
    
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
    
    # ✅ 4️⃣ Xử lý file paid_xcg
    if (file.exists(paid_file)) {
      old_paid <- readRDS(paid_file)
      hehe <- align_and_replace(hehe, old_paid)
    } else {
      old_paid <- NULL
    }

    check_xcg(hehe %>%
                 group_by(bks, ngay_ton_that, pham_vi_bt)  %>%
                 filter(
                   n() > 1,
                   n_distinct(source_file) > 1,
                   any(source_file ==   paste0( input$year, "_",tolower(input$quarter) , "_paid" ))
                 ) %>%
                 ungroup()  )
    
    # ✅ 6️⃣ Lưu lại file RDS
    saveRDS(hehe, paid_file)
    showNotification(paste("✅ Đã lưu file:",  paid_file), type = "message")
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
    # ✅ Reset overwrite input để không bị trigger lại
    session$sendCustomMessage("resetOverwrite", NULL)
    
    
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

# ✅ Nút download: truy cập dữ liệu qua hehe_xcg()
output$download_xcg <- downloadHandler(
  
  filename = function() {
    paste0("xcg_paid_",  input$year, "_",tolower(input$quarter) , ".xlsx")
  },
  content = function(file) {
    req(hehe_xcg())  # đảm bảo có dữ liệu
    writexl::write_xlsx(hehe_xcg(), file)
  }
)


# ✅ Nút download: truy cập dữ liệu qua hehe_xcg()
output$download_xcg_check <- downloadHandler(
  filename = function() {
    paste0("xcg_check_paid_",input$year, "_",tolower(input$quarter) , ".xlsx")
  },
  content = function(file) {
    req(check_xcg())  # đảm bảo có dữ liệu
    writexl::write_xlsx(check_xcg(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_xcg(),
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