observeEvent(input$update, {
  req(final_data())
  df <- final_data()
  old_data <- data()
  
  print(nrow(df))
  # kiểm tra định dạng
  # if (nrow(df) != 84 || ncol(old_data) != ncol(df)) {
  #   showNotification("File sau khi tổng hợp chưa đúng định dạng để ghép. Vui lòng kiểm tra lại", type = "error")
  #   return()
  # }
  
  # xác định quý/năm trong file mới
  new_quarters <- unique(df$Năm)   # cột quý-năm trong df
  duplicated_quarters <- intersect(new_quarters, unique(old_data$Năm))
  
  if (length(duplicated_quarters) > 0) {
    # nếu đã có quý đó rồi => hỏi người dùng
    showModal(modalDialog(
      title = "Dữ liệu đã tồn tại",
      paste("Dữ liệu cho", paste(duplicated_quarters, collapse = ", "),
            "đã có trong hệ thống. Bạn có muốn thay thế không?"),
      footer = tagList(
        modalButton("Hủy"),
        actionButton("confirm_replace", "Thay thế")
      )
    ))
    
    # xử lý khi người dùng chọn thay thế
    observeEvent(input$confirm_replace, {
      removeModal()
      # loại bỏ quý cũ trong old_data
      updated <- old_data %>% dplyr::filter(!Năm %in% duplicated_quarters)
      # thêm df mới
      new_data <- dplyr::bind_rows(updated, df)
      
      saveRDS(new_data, "data.rds")
      data(new_data)
      
      showNotification("Đã thay thế dữ liệu thành công", type = "message")
    }, ignoreInit = TRUE, once = TRUE)
    
  } else {
    # nếu chưa có thì ghép bình thường
    new_data <- dplyr::bind_rows(old_data, df)
    saveRDS(new_data, "data.rds")
    data(new_data)
    
    showNotification("Đã ghép dữ liệu thành công", type = "message")
  }
})


output$down <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".xlsx", sep = "")
  },
  content = function(file) {
    write_xlsx(data(), path = file)
  }
)


