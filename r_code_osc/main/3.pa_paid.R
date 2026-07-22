hehe_pa <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
check_pa <- reactiveVal(NULL) 
folder_pa <- "www/pa/paid"
observeEvent(input$run_pa, {
  req(input$file_pa)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
    withProgress(message = "Đang xử lý dữ liệu...", {
      incProgress(0.2, detail = "Đang đọc các file Excel...")
      files <- input$file_pa$datapath
      names(files) <- input$file_pa$name
      
      df_list <- map(files, function(file_path) {
        valid_sheet <- get_valid_sheet(file_path)
        if (is.null(valid_sheet)) return(NULL)
        read_excel(file_path, sheet = valid_sheet, col_types = "text")
      })
      
      # Gộp các file hợp lệ
      df <- df_list[[1]]
      
      if ("Mã Cty" %in% df[[4]]) {
        df <- df[, -c(4, 5, 7)]
      }
      
      he <- which(df[[2]] == 2)
      if (length(he) == 0) {
        stop("❌ Không tìm thấy dòng có giá trị 2 ở cột thứ 2.")
      }
      df <- df[(he + 1):nrow(df), 1:35]
      
      col_names <- c(
        "STT",
        "CTTV_Ten", "CTTV_Ma",  "CTTV_MaNvụ","CTTV_MaHopDong",
        "SoSDBS_MaCty","SoSDBS_MaNvụ", "SoSDBS_MaHopDong", "SoSDBS_MaSDBS", 
        "SoTrenIJ",
        "NguoiDuocBaoHiem",
        "GioiHanMucTrachNhiem",
        "ThoiHanBaoHiem_Tu_Ngay", "ThoiHanBaoHiem_Tu_Thang", "ThoiHanBaoHiem_Tu_Nam",
        "ThoiHanBaoHiem_Den_Ngay", "ThoiHanBaoHiem_Den_Thang", "ThoiHanBaoHiem_Den_Nam",
        "SoTienBaoHiem_VND", "SoTienBaoHiem_USD",
        "TongPhiBaoHiemKhongThue_VND", "TongPhiBaoHiemKhongThue_USD",
        "NgayTonThat_Ngay", "NgayTonThat_Thang", "NgayTonThat_Nam",
        "NgayThongBao_Ngay", "NgayThongBao_Thang", "NgayThongBao_Nam",
        "NgayChiTraBT_Ngay", "NgayChiTraBT_Thang", "NgayChiTraBT_Nam",
        "SoTienDaTraBT_VND", "SoTienDaTraBT_USD",
        "KenhKhaiThac",
        "SoHoSoBT"
      )
      colnames(df) = col_names
      
      
      incProgress(0.4, detail = "Đang chuẩn hóa tên công ty...")
      df <- df %>%
        mutate(
          cong_ty = `CTTV_Ten` %>%
            str_to_lower() %>%
            str_remove_all("công ty bảo việt|bảo việt|bv") %>%
            str_squish() %>%
            str_to_title() %>%
            stri_trans_general("Latin-ASCII"),
          
          cong_ty = case_when(
            str_detect(cong_ty, "Tổng Công Ty|Trụ Sở Chính|Tsc") ~ "TSC",
            str_detect(cong_ty, "^Dac Lac") ~ "Dak Lak",
            str_detect(cong_ty, "^Dac Nong") ~ "Dak Nong",
            str_detect(cong_ty, "^Bac Can") ~ "Bac Kan",
            str_detect(cong_ty, "^Vung Tau") ~ "Ba Ria - Vung Tau",
            str_detect(cong_ty, "^Ho Chi Minh|Tp Ho Chi Minh") ~ "Thanh Pho Ho Chi Minh",
            str_detect(cong_ty, "^Hue") ~ "Thua Thien Hue",
            TRUE ~ cong_ty
          )
        )
      df$SoTienDaTraBT_VND = as.numeric(df$SoTienDaTraBT_VND )
      df$SoTienDaTraBT_USD = as.numeric(df$SoTienDaTraBT_USD )
      df$paid_osc = df$SoTienDaTraBT_VND
      df <- df %>%
        mutate(
          NguoiDuocBaoHiem = NguoiDuocBaoHiem %>%
            # đưa về chữ thường
            str_to_lower() %>%
            str_squish() %>%
            # viết hoa chữ cái đầu
            str_to_title()
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
        left_join(dt, by = c("cong_ty" = "Công ty")) 
      
      # hehe$source_file <-  paste0( input$year , "_paid")
      hehe$source_file <-  paste0( input$year, "_",tolower(input$quarter) , "_paid")
      hehe <- combine_date_cols(hehe)
      hehe <- hehe %>%
        mutate(check_missing_dates = if_any(
          c(NgayTonThat, ThoiHanBaoHiem_Tu, ThoiHanBaoHiem_Den, NgayThongBao, NgayChiTraBT),
          is.na
        ))
      hehe_pa(hehe)  # ✅ Lưu vào reactiveVal để dùng cho download
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
            actionButton("freq_ok_pa", "Tôi đã hiểu", class = "btn-primary")
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

observeEvent(input$freq_ok_pa, {
  req(hehe_pa())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_pa()
    
      file_name_rds <- file.path(folder_pa, paste0( input$year, "_",tolower(input$quarter) , ".rds"))
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
        paid_file <- "www/paid_pa.rds"
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
        
        # ✅ 3️⃣ Xử lý file paid_pa
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
        check_pa(hehe %>%
                   group_by(CTTV_MaHopDong , NgayTonThat_Ngay, NgayTonThat_Thang, NgayTonThat_Nam, NguoiDuocBaoHiem, SoTienDaTraBT_VND)  %>%
                   filter(
                     n() > 1,
                     any(source_file == paste0( input$year, "_",tolower(input$quarter), "_paid"))
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
  })
})
# Khi người dùng bấm "Ghi đè"
observeEvent(input$overwrite, {
  req(hehe_pa())
  # 🔹 Đảm bảo chỉ chạy 1 lần duy nhất
  removeModal()           # đóng modal trước khi xử lý
  isolate({                # không cho reactive khác tự chạy lại trong khi xử lý
    waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  })
  tryCatch({
    file_name_rds <- file.path(folder_pa,paste0( input$year, "_",tolower(input$quarter) ,   ".rds"))
    saveRDS(hehe_pa(), file_name_rds)
    showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
    hehe = hehe_pa()
    # ✅ 2️⃣ Đường dẫn file RDS
    paid_file <- "www/paid_pa.rds"
    
    # ✅ 3️⃣ Kỳ cần xử lý (dấu hiệu nhận biết)
    current_source <-   paste0( input$year, "_",tolower(input$quarter) , "_paid")
    
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
    
    # ✅ 4️⃣ Xử lý file paid_pa
    if (file.exists(paid_file)) {
      old_paid <- readRDS(paid_file)
      hehe <- align_and_replace(hehe, old_paid)
    } else {
      old_paid <- NULL
    }
    
    check_pa(hehe %>%
               group_by(CTTV_MaHopDong , NgayTonThat_Ngay, NgayTonThat_Thang, NgayTonThat_Nam, NguoiDuocBaoHiem, SoTienDaTraBT_VND) %>%
               filter(
                 n() > 1,
                 any(source_file == paste0( input$year, "_",tolower(input$quarter), "_paid"))
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

# ✅ Nút download: truy cập dữ liệu qua hehe_pa()
output$download_pa <- downloadHandler(
  
  filename = function() {
    paste0("pa_paid_",  tolower(input$quarter), "_", input$year, ".xlsx")
  },
  content = function(file) {
    req(hehe_pa())  # đảm bảo có dữ liệu
    writexl::write_xlsx(hehe_pa(), file)
  }
)


# ✅ Nút download: truy cập dữ liệu qua hehe_pa()
output$download_pa_check <- downloadHandler(
  filename = function() {
    paste0("pa_check_paid_",tolower(input$quarter), "_", input$year, ".xlsx")
  },
  content = function(file) {
    req(check_pa())  # đảm bảo có dữ liệu
    writexl::write_xlsx(check_pa(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_pa(),
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