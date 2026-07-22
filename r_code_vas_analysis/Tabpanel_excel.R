output$table <- renderDT({
  df = data() %>%
         select(-c("Năm LK","Nhóm NV","Nghiệp vụ","Qúy","@IBNR","@CAT" ))

  cols_accounting <- intersect( c(
    "OsC"  , "Paid" ,"Sub total(incurred)", "Written" ,
     "UPR" ,"Sub total(earned)","@OsC","@UPR"
  ), colnames(df)
  )

  cols_percent <- intersect( c("Paid/Written"    ,    "Incurred/Earned") , colnames(df))
  # index for JS (0-based)
  acc_idx <- which(colnames(df) %in% cols_accounting)

  # --- DATATABLE ---
  datatable(
    df,
    options = list(
      paging = FALSE,
      scrollX = TRUE,
      scrollY = "600px",
      fixedHeader = TRUE,
      orderCellsTop = TRUE,

      columnDefs = list(
        list(
          targets = acc_idx,
          className = "dt-right",
          render = JS(
            "function(data, type, row) {",
            "  if (type === 'display' || type === 'filter') {",
            "    var num = parseFloat(data);",
            "    if (isNaN(num)) return data;",
            "    var abs = Math.abs(num).toLocaleString(undefined, {",
            "      minimumFractionDigits: 1,",
            "      maximumFractionDigits: 1",
            "    });",
            "    return num < 0 ? '(' + abs + ')' : abs;",
            "  }",
            "  return data;",
            "}"
          )
        )
      ),

      # --- MULTI SELECT FILTER (EXCEL-LIKE) ---
      initComplete = JS(
        "function () {
  var api = this.api();
  var table = $(api.table().node());

  api.columns().every(function (i) {
    var column = this;
    var th = $(column.header());

    // chỉ filter cột text
    if (column.data().unique().length < 2) return;

    // icon ▼
    var icon = $('<span class=\"excel-filter\">▼</span>');
    th.append(icon);

    icon.on('click', function (e) {
      e.stopPropagation();
      $('.excel-popup').remove(); // đóng popup khác

      createPopup(column, th);
    });
  });

  // đóng popup khi click ngoài
  $(document).on('click', function () {
    $('.excel-popup').remove();
  });

  function createPopup(column, th) {

    var popup = $('<div class=\"excel-popup\"></div>').appendTo('body');

    // position
    var offset = th.offset();
    popup.css({
      top: offset.top + th.outerHeight(),
      left: offset.left
    });

    // buttons
    var btnAll = $('<button>Select all</button>');
    var btnClear = $('<button>Clear</button>');

    popup.append(btnAll).append(btnClear);

    // select
    var select = $('<select multiple></select>').appendTo(popup);

    column.data().unique().sort().each(function (d) {
      select.append('<option value=\"' + d + '\">' + d + '</option>');
    });

    var s = select.selectize({
      plugins: ['remove_button'],
      placeholder: 'Search...',
      closeAfterSelect: false
    })[0].selectize;

    // Select all
    btnAll.on('click', function (e) {
      e.stopPropagation();
      s.setValue(Object.keys(s.options));
    });

    // Clear
    btnClear.on('click', function (e) {
      e.stopPropagation();
      s.clear();
      column.search('').draw();
    });

    // apply filter
    s.on('change', function () {
      var vals = s.getValue();
      if (vals.length) {
        var safe = vals.map(function (v) {
          return v.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&');
        });
        column.search('^(' + safe.join('|') + ')$', true, false).draw();
      } else {
        column.search('').draw();
      }
    });

    popup.on('click', function (e) {
      e.stopPropagation();
    });
  }
}"
      )
      
      
      
      
    )
  ) %>%

    # --- PERCENT FORMAT ---
    formatPercentage(
      columns = cols_percent,
      digits = 1
    )

})











# output$table <- renderDT({
#   df = data() %>%
#          select(-c("Năm LK","Nhóm NV","Nghiệp vụ","Qúy","@IBNR","@CAT" ))
# 
#   cols_accounting <- intersect( c(
#     "OsC"  , "Paid" ,"Sub total(incurred)", "Written" ,
#      "UPR" ,"Sub total(earned)","@OsC","@UPR"
#   ), colnames(df)
#   )
# 
#   cols_percent <- intersect( c("Paid/Written"    ,    "Incurred/Earned") , colnames(df))
#   # index for JS (0-based)
#   acc_idx <- which(colnames(df) %in% cols_accounting)
# 
#   # --- DATATABLE ---
#   datatable(
#     df,
#     options = list(
#       paging = FALSE,
#       scrollX = TRUE,
#       scrollY = "600px",
#       fixedHeader = TRUE,
#       orderCellsTop = TRUE,
# 
#       columnDefs = list(
#         list(
#           targets = acc_idx,
#           className = "dt-right",
#           render = JS(
#             "function(data, type, row) {",
#             "  if (type === 'display' || type === 'filter') {",
#             "    var num = parseFloat(data);",
#             "    if (isNaN(num)) return data;",
#             "    var abs = Math.abs(num).toLocaleString(undefined, {",
#             "      minimumFractionDigits: 1,",
#             "      maximumFractionDigits: 1",
#             "    });",
#             "    return num < 0 ? '(' + abs + ')' : abs;",
#             "  }",
#             "  return data;",
#             "}"
#           )
#         )
#       ),
# 
#       # --- MULTI SELECT FILTER (EXCEL-LIKE) ---
#       initComplete = JS(
#         "function() {",
#         "  var api = this.api();",
#         "  var header = $(api.table().header());",
#         "  var filterRow = $('<tr></tr>').appendTo(header);",
# 
#         "  api.columns().every(function() {",
#         "    var column = this;",
#         "    var th = $('<th></th>').appendTo(filterRow);",
# 
#         "    var select = $('<select multiple size=\"4\"></select>')",
#         "      .appendTo(th)",
#         "      .on('change', function() {",
#         "        var vals = $(this).val();",
#         "        if (vals && vals.length > 0) {",
#         "          var safe = vals.map(function(v){",
#         "            return v.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&');",
#         "          });",
#         "          column.search('^(' + safe.join('|') + ')$', true, false).draw();",
#         "        } else {",
#         "          column.search('', true, false).draw();",
#         "        }",
#         "      });",
# 
#         "    column.data().unique().sort().each(function(d) {",
#         "      select.append('<option value=\"' + d + '\">' + d + '</option>');",
#         "    });",
#         "  });",
#         "}"
#       )
#     )
#   ) %>%
# 
#     # --- PERCENT FORMAT ---
#     formatPercentage(
#       columns = cols_percent,
#       digits = 1
#     )
# 
# })
