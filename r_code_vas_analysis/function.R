# Hàm cắt dữ liệu từ "Line" đến "Total"
slice_line_to_total <- function(d, 
                                start_pat = "\\bLine\\b", 
                                end_pat   = "\\bTotal\\b", 
                                col_index = 1) {
  # Lấy cột dùng để dò (mặc định là cột đầu tiên)
  col1 <- as.character(d[[col_index]])
  col1[is.na(col1)] <- ""
  
  # Vị trí "Line" đầu tiên
  start_pos <- which(grepl(start_pat, col1, ignore.case = TRUE))
  if (length(start_pos) == 0) {
    # Không có "Line" -> trả về data.frame rỗng
    return(d[0, , drop = FALSE]) 
  }
  start_pos <- start_pos[1]
  
  # Vị trí "Total" đầu tiên xuất hiện sau (hoặc tại) start_pos
  total_pos_all <- which(grepl(end_pat, col1, ignore.case = TRUE) & seq_along(col1) >= start_pos)
  if (length(total_pos_all) == 0) {
    # Không thấy "Total" sau "Line" -> cắt đến cuối bảng
    end_pos <- nrow(d)
  } else {
    end_pos <- total_pos_all[1]
  }
  
  he <- as.character(unlist(d[start_pos, , drop = TRUE]))
  he <- trimws(he)
  
  d= d[(start_pos+1):(end_pos), , drop = FALSE]
  #print(he)
  colnames(d) = he
  d %>% filter(!is.na(.[[1]]))
  
}


normalize_line <- function(line) {
  line <- str_to_lower(line) # chuẩn hóa
  
  case_when(
    str_detect(line, "^pa$|personal") ~ "Personal Accident",       # đúng "PA" riêng
    str_detect(line, "pa.*health") ~ "HC & PA & Travel", # nhóm PA + Health
    str_detect(line, "health") ~ "Healthcare",
    str_detect(line, "motor") ~ "Motor Vehicles",
    str_detect(line, "aviation|oil and gas") ~ "Aviation & Oil",
    str_detect(line, "agriculture") ~ "Agriculture",
    str_detect(line, "cargo") ~ "Cargo in transit",
    str_detect(line, "engineering") ~ "Engineering",
    str_detect(line, "fire") ~ "Fire and Misc.",
    str_detect(line, "liability|miscell") ~ "General Liability",
    str_detect(line, "hull") ~ "Hull & PI",
    str_detect(line, "travel") ~ "Travel",
    str_detect(line, "total") ~ "Total",
    TRUE ~ str_to_title(line) # giữ nguyên nếu không match
  )
}

# Hàm xử lý pivot_longer + chuẩn hóa Type
process_block <- function(d, var) {
  d %>%
    slice_line_to_total() %>%
    mutate(Line = normalize_line(Line)) %>%   # ⭐ normalize SỚM 
    tidyr::pivot_longer(
      cols = c(Direct, Inward, Recovery, Retrocession, tidyselect::matches("Net", ignore.case = TRUE)),
      names_to = "Type", values_to = var
    ) %>%
    mutate(
      Type = toupper(Type),
      Type = ifelse(grepl("NET", Type), "NET", Type),
      !!var := as.numeric(gsub("[^0-9.-]", "", .data[[var]]))
    ) 
  # %>%
  #   filter(Type %in% c("DIRECT", "INWARD", "NET"))
}

process_block_cat <- function(d, var) {
  
  d %>%
    slice_line_to_total() %>%
    rename(!!var := 2) %>%   # cột thứ 2 là giá trị CAT
    mutate(
      Line = normalize_line(Line),
      Type = "NET",          # hoặc "CAT" nếu bạn muốn
      !!var := as.numeric(gsub("[^0-9.-]", "", .data[[var]]))
    ) %>%
    select(Line, Type, !!var)
}


prev_quarter <- function(x) {
  # tách năm và quý
  year <- as.integer(substr(x, 1, 4))
  quarter <- as.integer(substr(x, 6, 6))
  
  # tính quý trước
  if (quarter == 1) {
    year <- year - 1
    quarter <- 4
  } else {
    quarter <- quarter - 1
  }
  
  paste0(year, "Q", quarter)
}

get_last_4_quarters <- function(q) {
  res <- c(q)
  for (i in 1:3) {
    q <- prev_quarter(q)
    res <- c(res, q)
  }
  return(res)
}


check_form <- function(df) {
  # Check có cột Line không
  if (!any(grepl("Line", names(df), ignore.case = TRUE)) &&
      !any(grepl("Line", df[[1]], ignore.case = TRUE))) {
    return(FALSE)
  }
  
  # Check có các type Direct, Inward, Recovery, Retrocession, Net không
  if (!any(grepl("Direct", df, ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("Inward", df, ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("Recovery", df, ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("Retrocession", df, ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("Net", df, ignore.case = TRUE))) return(FALSE)
  
  # Check có các chỉ tiêu chính không
  if (!any(grepl("UPR", df[[1]], ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("OSC", df[[1]], ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("IBNR", df[[1]], ignore.case = TRUE))) return(FALSE)
  if (!any(grepl("CAT", df[[1]], ignore.case = TRUE))) return(FALSE)
  
  return(TRUE)
}

