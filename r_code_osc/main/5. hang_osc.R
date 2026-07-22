hehe_cargo1 <- reactiveVal(NULL)  # ✅ nơi lưu dữ liệu kết quả
check_cargo1 <- reactiveVal(NULL) 
folder_cargo1 <- "www/cargo/osc"

observeEvent(input$run_cargo1, {
  req(input$file_cargo1)
  waiter_show(html = tagList(
    spin_ellipsis(),
    h4("⏳ Đang xử lý dữ liệu, vui lòng đợi...")
  ))
  # Wrap tryCatch để luôn đảm bảo waiter_hide được gọi
  tryCatch({
    withProgress(message = "Đang xử lý dữ liệu...", {
      incProgress(0.2, detail = "Đang đọc các file Excel...")
      files <- input$file_cargo1$datapath
      names(files) <- input$file_cargo1$name
      sheets <- excel_sheets(files)
      
      data_list <- setNames(lapply(sheets, function(sh) {
        process_sheet(files, sheet_name = sh)
      }), sheets)
      
      # Lọc bỏ các phần tử NULL (tức là sheet có < 20 cột hoặc xử lý lỗi)
      data_list <- data_list[!vapply(data_list, is.null, logical(1))]
      clean_list_names <- function(lst) {
        names(lst) <- janitor::make_clean_names(names(lst))  # ví dụ "VNĐ" -> "vnd"
        lst
      }
      
      data_list <- clean_list_names(data_list)
      
      
      target_quarter <- paste0( input$quarter, "/", input$year)
      ty_gia <- readRDS("ty_gia.rds")
      ty_gia_selected <- ty_gia %>%
        filter(Thoi_gian == target_quarter)
      
      # Lấy tỷ giá USD
      usd_rate <- ty_gia_selected$USD[1]  # [1] để chắc chắn lấy 1 giá trị
      print(usd_rate)
      
      #_____________________________________________TNDS
      
      file_tn1 <- input$file_cargo_tn1$datapath
      file_path <- file_tn1[1]
      sheets1 <- excel_sheets(file_path)
      sheets_paid <- sheets1[str_detect(sheets1, regex("osc", ignore_case = TRUE))]
      
      if(length(sheets_paid) == 1) {
        df_paid <- process_sheet(file_path, sheet_name = sheets_paid)
      } else if(length(sheets_paid) > 1) {
        # Nếu nhiều sheet, ghép lại
        df_paid <- bind_rows(lapply(sheets_paid, function(sh) {
          process_sheet(file_path, sheet_name = sh)
        }))
      } else {
        message("Không tìm thấy sheet 'paid' nào trong file!")
      }
      
      
      
      idx = grep("stbh", names(df_paid), ignore.case = TRUE)[1]
      #names(df_paid)[grepl("stbh", names(df_paid), ignore.case = TRUE)]
      if(is.na(idx)) stop("❌ Không tìm thấy cột stbh")
      
      if(idx == ncol(df_paid)) stop("❌ stbh là cột cuối, không có cột USD phía sau")
      
  
      stbh_name = names(df_paid)[idx]
      usd_name = names(df_paid)[idx + 1]
      
    
      df_paid = df_paid %>%
        mutate(
          vnd = .data[[stbh_name]],
          usd = .data[[usd_name]],
          
          loai_tien = if_else(!is.na(usd) & usd != "" & usd != "0", "USD", "VND"),
          stbh = if_else(!is.na(usd) & usd != "" & usd != "0", usd, vnd)
        ) %>%
        select(-vnd, -usd)
      
  
      
      
      
      #_________________________ check lại
      pos_paid <- grep("uoc_.*thuong", colnames(df_paid ), ignore.case = TRUE)
      df_paid$paid_osc <-coalesce(as.numeric(gsub("[^0-9.-]", "", df_paid[[pos_paid[1]]])), 0) 
      
      
      #___________________________ VNĐ
      vnd <- data_list$vnd
      pos_vnd <- grep("du_phong", colnames(vnd), ignore.case = TRUE)
      vnd$paid_osc <- coalesce(as.numeric(gsub("[^0-9.-]", "", vnd[[pos_vnd[1]]])), 0) 
      
      #___________________________ USD
      usd <- data_list$usd
      pos_usd <- grep("quy_.*vnd", colnames(usd), ignore.case = TRUE)
      usd = usd[, -pos_usd[1]]
      pos_usd <- grep("du_phong", colnames(usd), ignore.case = TRUE)
      usd$paid_osc <- coalesce(as.numeric(gsub("[^0-9.-]", "", usd[[pos_usd[1]]])), 0)  * usd_rate
      
      #___________________________ ij
      other_name <- setdiff(names(data_list), c("vnd", "usd"))
      ngoai_ij <- NULL
      
      if (length(other_name) == 1) {
        ngoai_ij <- data_list[[other_name]]
      }
      
      if (length(other_name) > 1) {
        ij_name <- grep("ij", other_name, ignore.case = TRUE, value = TRUE)
        ngoai_ij <- data_list[[ij_name]]
      }
      
      pos_ij <- grep("du_phong", colnames(ngoai_ij), ignore.case = TRUE)
      
      
      ngoai_ij$paid_osc <- coalesce(as.numeric(gsub("[^0-9.-]", "", ngoai_ij[[pos_ij[1]+2]])), 0) 
      
      
      
      idx = grep("stbh", names(ngoai_ij), ignore.case = TRUE)[1]
                 
      if(is.na(idx)) stop("❌ Không tìm thấy cột stbh")
      
      if(idx == ncol(ngoai_ij)) stop("❌ stbh là cột cuối, không có cột USD phía sau")
     
      stbh_name = names(ngoai_ij)[idx]
      usd_name = names(ngoai_ij)[idx + 1]
      
      # print(stbh_name)
      # print(usd_name)
      ngoai_ij = ngoai_ij %>%
        mutate(
          vnd = .data[[stbh_name]],
          usd = .data[[usd_name]],
          
          loai_tien = if_else(!is.na(usd) & usd != "" & usd != "0", "USD", "VND"),
          stbh = if_else(!is.na(usd) & usd != "" & usd != "0", usd, vnd)
        ) %>%
        select(-vnd, -usd)
      #print(colnames(ngoai_ij))
      
      
      
      #____________________________
      colnames(vnd) <- str_remove_all(colnames(vnd), "_\\d+_*\\d*")
      names(vnd) <- make.unique(names(vnd))
      min1 = min(ncol(vnd), ncol(usd))
      vnd = vnd[,1:min1]
      usd = usd[,1:min1]
      colnames(usd) <- colnames(vnd)  # hoặc tạo tên chung: paste0("V", 1:ncol(df1))
      df <- rbind(vnd, usd)
      
      if (nrow(ngoai_ij)>0){
        df2 <- ngoai_ij %>% 
          select(any_of(colnames(df)))   # chọn đúng cột có trong df, giữ thứ tự cột
        
        df<- bind_rows(df, df2)
      }
      
      if (nrow(df_paid)>0){
        df2 <- df_paid %>% 
          select(any_of(colnames(df)))   # chọn đúng cột có trong df, giữ thứ tự cột
        
        df<- bind_rows(df, df2)
      }
      
      
      
      
      
      
      
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
      
      # Lưu dữ liệu vào reactiveVal, chờ bấm freq_ok
      hehe$source_file <- paste0(input$year, "_", tolower(input$quarter), "_osc")
      
      
      hehe$vuot_mtn <- ifelse(
        !is.na(hehe$tau_hang),
        hehe$paid_osc - hehe$tau_hang,
        0
      )
      
      
      
      
      
      
      
      
      
      
      hehe_cargo1(hehe)  # ✅ Lưu vào reactiveVal để dùng cho download
      # ==============================
      # 2. SHOW FREQ MODAL
      # ==============================
      # --- Tính freq ---
      tam <- hehe %>% filter(is.na(Co_id))
      freq <- table(tam$don_vi)
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
            actionButton("freq_cargo1", "Tôi đã hiểu", class = "btn-primary")
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
observeEvent(input$freq_cargo1, {
  req(hehe_cargo1())
  removeModal()
  waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang lưu dữ liệu...")))
  
  tryCatch({
    hehe <- hehe_cargo1()
    
    file_name_rds <- file.path(folder_cargo1, paste0( input$year, "_", tolower(input$quarter),  ".rds"))
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
      paid_file <- "www/paid_cargo.rds"
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
      
      # ✅ 3️⃣ Xử lý file paid_cargo1
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
      check_cargo1(hehe %>%
                     group_by(so_hsbt, paid_osc)  %>%
                   filter(
                     n() > 1,
                     
                     any(source_file == paste0( input$year, "_", tolower(input$quarter),"_osc" ) )
                   ) %>%
                   ungroup()  )
      
      #check_cargo1( hehe %>% group_by(bks, ngay_ton_that, pham_vi_bt) %>% filter(n()>1)  )
      
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
  req(hehe_cargo1())
  # 🔹 Đảm bảo chỉ chạy 1 lần duy nhất
  removeModal()           # đóng modal trước khi xử lý
  isolate({                # không cho reactive khác tự chạy lại trong khi xử lý
    waiter_show(html = tagList(spin_ellipsis(), h4("⏳ Đang ghi đè dữ liệu...")))
  })
  tryCatch({
    file_name_rds <- file.path(folder_cargo1, paste0( input$year, "_", tolower(input$quarter),  ".rds"))
    saveRDS(hehe_cargo1(), file_name_rds)
    showNotification(paste("✅ Đã lưu file:", basename(file_name_rds)), type = "message")
    hehe = hehe_cargo1()
    # ✅ 2️⃣ Đường dẫn file RDS
    paid_file <- "www/paid_cargo.rds"
    # ==== HÀM TIỆN ÍCH: chuẩn hóa cột & ghi đè theo kỳ ====
    align_and_replace <- function(new_data, old_data) {
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
      
      # 🧩 Ghép lại
      result <- bind_rows(old_data, new_data) 
      
      # 📢 Thông báo
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
      
      return(result)
    }
    
    # ✅ 4️⃣ Xử lý file paid_cargo1
    if (file.exists(paid_file)) {
      old_paid <- readRDS(paid_file)
      hehe <- align_and_replace(hehe, old_paid)
    } else {
      old_paid <- NULL
    }
    
    check_cargo1(hehe %>%
                   group_by(so_hsbt, paid_osc)  %>%
                 filter(
                   n() > 1,
                   
                   any(source_file ==  paste0( input$year, "_", tolower(input$quarter),"_osc" )  )
                 ) %>%
                 ungroup()  )
    
    removeModal()
    waiter_hide()
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

# ✅ Nút download: truy cập dữ liệu qua hehe_cargo1()
output$download_cargo1 <- downloadHandler(
  
  filename = function() {
    paste0("cargo_osc_",  input$year, "_", tolower(input$quarter), ".xlsx")
  },
  content = function(file) {
    req(hehe_cargo1())  # đảm bảo có dữ liệu
    writexl::write_xlsx(hehe_cargo1(), file)
  }
)


# ✅ Nút download: truy cập dữ liệu qua hehe_cargo1()
output$download_cargo_check1 <- downloadHandler(
  filename = function() {
    paste0("cargo_check_osc_",input$year, "_", tolower(input$quarter), ".xlsx")
  },
  content = function(file) {
    req(check_cargo1())  # đảm bảo có dữ liệu
    writexl::write_xlsx(check_cargo1(), file)
  }
)
output$result <- renderDT({
  datatable(
    hehe_cargo1(),
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