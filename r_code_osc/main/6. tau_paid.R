hehe_marine <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
check_marine <- reactiveVal(NULL) 
folder_marine <- "www/marine/paid"
observeEvent(input$run_marine, {
  req(input$file_marine11,input$file_marine12, input$file_marine13)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
    withProgress(message = "Đang xử lý dữ liệu...", {
      incProgress(0.2, detail = "Đang đọc các file Excel...")
      target_quarter <- paste0( input$quarter, "/", input$year)
      ty_gia <- readRDS("ty_gia.rds")
      ty_gia_selected <- ty_gia %>%
        filter(Thoi_gian == target_quarter)
      
      # Lấy tỷ giá USD
      usd_rate <- ty_gia_selected$USD[1]  # [1] để chắc chắn lấy 1 giá trị
      print(usd_rate)
      
      #_____________________________________________   hull
      files <- input$file_marine11$datapath
      names(files) <- input$file_marine11$name
      
      all_data <- process_file(files)
      df_taubien= all_data[[1]]
      pos <- grep("quy_.*vnd", colnames(df_taubien), ignore.case = TRUE)
      df_taubien$paid_osc <- coalesce(as.numeric(gsub("[^0-9.-]", "", df_taubien[[pos[1]]])), 0) 
      df_taubien$source = "hull"
      
      #_____________________________________________   p&i
      files <- input$file_marine12$datapath
      names(files) <- input$file_marine12$name
      
      all_data <- process_file(files)
      df_pi= all_data[[1]]
      map_tau <- read_excel("map_tau_CTTV.xlsx")
      
      map_tau <- map_tau %>%
        mutate(
          ID_clean = ID %>%
            str_to_upper() %>%
            str_trim() %>%
            str_replace("^BV\\s*", "")   # bỏ "BV " ở đầu
        )
      
      donvi_cols <- c(
        "don_vi_hach_toan",
        "don_vi_cap_don",
        "don_vi_ht",
        "don_vi_cap"
      )
      
      donvi_col <- intersect(donvi_cols, names(df_pi))[1]
      
      if (is.na(donvi_col)) {
        stop("Không tìm thấy cột đơn vị trong file")
      }
      
      
      df_pi <- df_pi %>%
        mutate(
          hic = .data[[donvi_col]] %>%
            str_to_upper() %>%
            str_trim() %>%
            str_replace("^DVU\\d+.*", "DVU") %>%
            str_replace("^HCM\\d+.*", "TPHCM") %>%
            str_replace("^BV\\s*", "")   # bỏ "BV " nếu có
        ) %>%
        left_join(map_tau, by = c("hic" =  "ID_clean")) 
        # %>%
        # select(-hic)
      
      df_pi$don_vi_cap_duoi = df_pi[[donvi_col]]
      
      
      
      
      
      
      
      pos <- grep("^so_tien", colnames(df_pi), ignore.case = TRUE)
      
      so_tien_num <- coalesce(
        as.numeric(gsub("[^0-9.-]", "", df_pi[[pos[1]+1]])),
        0
      )

      
      
      
      col_usd_1 <- df_pi[[pos[1] + 1]]
      col_usd_2 <- df_pi[[pos[1] + 2]]
      
      df_pi$paid_osc <- ifelse(
        grepl("usd", col_usd_1, ignore.case = TRUE) |
          grepl("usd", col_usd_2, ignore.case = TRUE),
        so_tien_num * usd_rate,
        so_tien_num
      )
      
      
      cols <- tolower(names(df_pi))

      has_hoi_bh <- any(grepl("hoi_bh", cols))
      has_dbh    <- any(grepl("dbh", cols))

      if (has_hoi_bh) {
        hoi_bh_name <- names(df_pi)[grep("hoi_bh", names(df_pi), ignore.case = TRUE)[1]]
        df_pi$STBH <- df_pi[[hoi_bh_name]]
      } else {
        message("❌ Không tìm thấy cột hoi_bh")
      }

      if (has_dbh) {
        dbh_name <- names(df_pi)[grep("dbh", names(df_pi), ignore.case = TRUE)[1]]
        df_pi$ty_le_dong_bv <- df_pi[[dbh_name]]
      } else {
        message("❌ Không tìm thấy cột dbh")
      }
      
      
      
      
      df_pi <- df_pi %>%
        rename(so_khieu_nai_ij = any_of("ij"))
      
      df_pi$source = "p&i"
      
      #_____________________________________________   thống kê
      files <- input$file_marine13$datapath
      names(files) <- input$file_marine13$name
      
      all_data <- process_file(files)
      
      # Lưu RDS nếu muốn
      df_thongke <- bind_rows(all_data, .id = "source")
      
      df_thongke <- df_thongke %>%
        mutate(source = recode(source,
                               "p&i" = "p&i",
                               "TNDS TNĐ" = "TNDS TNĐ",
                               "hull" = "hull",
                               "TTC" = "TC",
                               "TNĐ" = "TNĐ",
                               "TNDS Tau ca" = "TNDS Tau ca",
                               "P&I TNĐ" = "TNDS TNĐ",
                               "TC" = "TC"
        ))
      
      
      
      
      pos <- grep("quy_.*vnd", colnames(df_thongke), ignore.case = TRUE)
      df_thongke$paid_osc <- coalesce(as.numeric(gsub("[^0-9.-]", "", df_thongke[[pos[1]]])), 0) 
      
      df = bind_rows(df_taubien, df_thongke)
      df <- df %>% select(where(~ !all(is.na(.))))
      # df2 <- df_pi %>% 
      #   select(any_of(colnames(df)))   # chọn đúng cột có trong df, giữ thứ tự cột
      
      df2 <- df_pi %>% 
        select(any_of(c(colnames(df), "hoi_bh")))
      
      
      
      df <- bind_rows(df, df2)
      df = df %>% mutate(across(matches("ngay|thoi_han"), convert_excel_or_string_date)) 
      
      # df <- df %>%
      #   filter(!(is.na(.[[2]]) & is.na(.[[3]]))) %>% filter(
      #     !(is.na(.[[1]]) & is.na(.[[3]]))
      #   )
      
      incProgress(0.4, detail = "Đang chuẩn hóa tên công ty...")
      
      if (!"don_vi" %in% names(df)) {
        waiter_hide()
        showModal(modalDialog(
          title = "❌ Thiếu cột bắt buộc",
          HTML(paste0(
            "Không tìm thấy cột <b>don_vi</b> trong dữ liệu.<br>",
            "Vui lòng kiểm tra lại file Excel — có thể tên cột bị sai, ví dụ: <br>",
            "<i>", paste(names(df), collapse = ", "), "</i>"
          )),
          easyClose = TRUE
        ))
        return(invisible())  # 🔹 Dừng toàn bộ xử lý ở đây
      }
      df <- df %>%
        mutate(
          cong_ty_clean = don_vi %>%
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
        left_join(dt, by = c("cong_ty_clean" = "Công ty")) 
      hehe$source_file <-  paste0( input$year, "_",tolower(input$quarter) , "_paid" )
      
      # hehe$vuot_mtn <- ifelse(
      #   !is.na(hehe$tau_hang),
      #   hehe$paid_osc - hehe$tau_hang,
      #   0
      # )
      
      
      
      
      
      hehe <- hehe %>%
        mutate(
          don_vi = if_else(
            grepl("p\\s*&\\s*i", source, ignore.case = TRUE),
            don_vi_cap_duoi,
            don_vi   # giữ nguyên các dòng khác
          )
        )
      
      
      
      
      hehe_marine(hehe)  # ✅ Lưu vào reactiveVal để dùng cho download
      # ==============================
      # 2. SHOW FREQ MODAL
      # ==============================
      tam <- hehe %>% filter(is.na(Co_id))
      freq <- table(tam$don_vi)
      
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
            actionButton("freq_marine", "Tôi đã hiểu", class = "btn-primary")
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

observeEvent(input$freq_marine, {
  req(hehe_marine())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_marine()
    
    file_name_rds <- file.path(folder_marine, paste0( input$year, "_",tolower(input$quarter) ,  ".rds"))
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
      paid_file <- "www/paid_marine.rds"
      
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
      
      # ✅ 3️⃣ Xử lý file paid_marine
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
      check_marine(hehe %>% filter(!is.na(so_khieu_nai_ij)) %>%
                    group_by(so_khieu_nai_ij, paid_osc)  %>%
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
  req(hehe_marine())
  # 🔹 Đảm bảo chỉ chạy 1 lần duy nhất
  removeModal()           # đóng modal trước khi xử lý
  isolate({                # không cho reactive khác tự chạy lại trong khi xử lý
    waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  })
  tryCatch({
    file_name_rds <- file.path(folder_marine, paste0( input$year, "_",tolower(input$quarter) ,  ".rds"))
    saveRDS(hehe_marine(), file_name_rds)
    showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
    hehe = hehe_marine()
    # ✅ 2️⃣ Đường dẫn file RDS
    paid_file <- "www/paid_marine.rds"
    
    # ✅ 3️⃣ Kỳ cần xử lý (dấu hiệu nhận biết)
    current_source <- paste0(input$year, "_",tolower(input$quarter) , "_paid")
    
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
    
    # ✅ 4️⃣ Xử lý file paid_marine
    if (file.exists(paid_file)) {
      old_paid <- readRDS(paid_file)
      hehe <- align_and_replace(hehe, old_paid)
    } else {
      old_paid <- NULL
    }
    
    check_marine(hehe %>% filter(!is.na(so_khieu_nai_ij)) %>%
                 group_by(so_khieu_nai_ij, paid_osc)  %>%
                   filter(
                     n() > 1,
                     n_distinct(source_file) > 1,
                     any(source_file ==   paste0( input$year, "_",tolower(input$quarter) , "_",tolower(input$quarter) , "_paid" ))
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

# ✅ Nút download: truy cập dữ liệu qua hehe_marine()
output$download_marine <- downloadHandler(
  
  filename = function() {
    paste0("marine_paid_",  input$year, "_",tolower(input$quarter) , ".xlsx")
  },
  content = function(file) {
    req(hehe_marine())  # đảm bảo có dữ liệu
    writexl::write_xlsx(hehe_marine(), file)
  }
)


# ✅ Nút download: truy cập dữ liệu qua hehe_marine()
output$download_marine_check <- downloadHandler(
  filename = function() {
    paste0("marine_check_paid_",input$year, "_",tolower(input$quarter) , ".xlsx")
  },
  content = function(file) {
    req(check_marine())  # đảm bảo có dữ liệu
    writexl::write_xlsx(check_marine(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_marine(),
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