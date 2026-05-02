library(shiny)
library(bslib)
library(jsonlite)
library(DT)
library(tools)

# ── Paths & config ─────────────────────────────────────────────────────────────
ROOT      <- normalizePath(file.path(getwd(), ".."))
DATA_FILE <- file.path(ROOT, "data", "photos.json")
IMG_DIR   <- file.path(ROOT, "images")

CATEGORIES <- c(
  "Landscapes"   = "landscapes",
  "Portraits"    = "portraits",
  "Street"       = "street",
  "Architecture" = "architecture"
)

CMS_USER <- Sys.getenv("CMS_USER", unset = "admin")
CMS_PASS <- Sys.getenv("CMS_PASS", unset = "auggie2026")

dir.create(IMG_DIR, showWarnings = FALSE, recursive = TRUE)
addResourcePath("photos", IMG_DIR)

# ── Helpers ────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

empty_photos <- function() {
  data.frame(
    id = character(), filename = character(), category = character(),
    title = character(), description = character(),
    featured = logical(), upload_date = character(),
    stringsAsFactors = FALSE
  )
}

read_photos <- function() {
  if (!file.exists(DATA_FILE)) return(empty_photos())
  tryCatch({
    raw <- fromJSON(DATA_FILE, simplifyDataFrame = TRUE)
    if (is.null(raw) || length(raw) == 0) return(empty_photos())
    df  <- as.data.frame(raw, stringsAsFactors = FALSE)
    for (col in c("id","filename","category","title","description","featured","upload_date")) {
      if (!col %in% names(df))
        df[[col]] <- if (col == "featured") logical(nrow(df)) else character(nrow(df))
    }
    df
  }, error = function(e) empty_photos())
}

write_photos <- function(photos) {
  dir.create(dirname(DATA_FILE), showWarnings = FALSE, recursive = TRUE)
  write_json(photos, DATA_FILE, pretty = TRUE, auto_unbox = TRUE)
}

save_upload <- function(file_info, category) {
  ext  <- tolower(file_ext(file_info$name))
  id   <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000L:9999L, 1L))
  name <- paste0(id, ".", ext)
  dest <- file.path(IMG_DIR, category)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  file.copy(file_info$datapath, file.path(dest, name), overwrite = TRUE)
  list(id = id, filename = name)
}

photo_src <- function(category, filename) paste0("photos/", category, "/", filename)

stat_card <- function(label, n) {
  card(
    class = "stat-card text-center",
    card_body(
      padding = "0.8rem",
      div(class = "stat-num", n),
      div(class = "stat-lbl text-muted", label)
    )
  )
}

# ── Login page ─────────────────────────────────────────────────────────────────
login_page <- function() {
  tagList(
    div(
      class = "login-page",
      div(
        class = "login-card",
        div(class = "brand-badge", "AS"),
        h4("Auggie Smith Photography", class = "mt-3 mb-1"),
        p(class = "text-muted small mb-3", "Content Management System"),
        textInput("login_user", NULL, placeholder = "Username", width = "100%"),
        passwordInput("login_pass", NULL, placeholder = "Password", width = "100%"),
        actionButton("login_btn", "Sign In", class = "btn-primary w-100 mt-2"),
        uiOutput("login_msg")
      )
    )
  )
}

# ── CMS page ───────────────────────────────────────────────────────────────────
cms_page <- function() {
  tagList(
    # Top navbar
    div(
      class = "cms-navbar",
      div(
        class = "cms-brand",
        span(class = "brand-badge-sm", "AS"),
        span("Auggie Smith Photography — CMS")
      ),
      div(
        class = "cms-nav-tabs",
        tags$button(class = "cms-tab active", `data-tab` = "dashboard",
                    tags$i(class = "fa-solid fa-gauge-high"), " Dashboard"),
        tags$button(class = "cms-tab", `data-tab` = "upload",
                    tags$i(class = "fa-solid fa-cloud-arrow-up"), " Upload"),
        tags$button(class = "cms-tab", `data-tab` = "manage",
                    tags$i(class = "fa-solid fa-images"), " Manage")
      ),
      actionButton("logout_btn", tagList(tags$i(class = "fa-solid fa-arrow-right-from-bracket"), " Log Out"),
                   class = "btn-sm btn-outline-light")
    ),

    # Tab: Dashboard
    div(
      id = "tab-dashboard", class = "cms-panel active",
      div(
        class = "cms-content",
        h4("Dashboard"),
        uiOutput("dash_stats"),
        hr(class = "my-4"),
        h5("Recent Uploads"),
        uiOutput("dash_recent")
      )
    ),

    # Tab: Upload
    div(
      id = "tab-upload", class = "cms-panel",
      div(
        class = "cms-content",
        h4("Upload New Photo"),
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Photo Details"),
            card_body(
              fileInput("upload_file", "Select image",
                        accept = c("image/jpeg","image/png","image/webp","image/gif"),
                        placeholder = "JPG, PNG, WebP or GIF"),
              textInput("upload_title", "Title", placeholder = "e.g. Mountain Sunrise at Dawn"),
              selectInput("upload_category", "Category", choices = CATEGORIES),
              textAreaInput("upload_desc", "Description (optional)",
                            placeholder = "Caption or notes", rows = 3),
              checkboxInput("upload_featured",
                            HTML('<i class="fa-solid fa-star" style="color:#c8a96e"></i> Feature on homepage'),
                            value = FALSE),
              actionButton("upload_btn",
                           HTML('<i class="fa-solid fa-upload"></i> Upload Photo'),
                           class = "btn-primary mt-1"),
              uiOutput("upload_msg")
            )
          ),
          card(
            card_header("Preview"),
            card_body(
              style = "min-height:320px; display:flex; align-items:center; justify-content:center;",
              uiOutput("preview_wrap")
            )
          )
        )
      )
    ),

    # Tab: Manage
    div(
      id = "tab-manage", class = "cms-panel",
      div(
        class = "cms-content",
        h4("Manage Photos"),
        div(
          class = "d-flex align-items-center gap-3 mb-3",
          div(
            style = "width:220px;",
            selectInput("filter_cat", NULL,
                        choices = c("All Categories" = "all", CATEGORIES),
                        selected = "all")
          ),
          uiOutput("manage_count")
        ),
        DTOutput("photos_dt")
      )
    ),

    # Tab-switching JS
    tags$script(HTML("
      document.querySelectorAll('.cms-tab').forEach(function(btn) {
        btn.addEventListener('click', function() {
          document.querySelectorAll('.cms-tab').forEach(b => b.classList.remove('active'));
          document.querySelectorAll('.cms-panel').forEach(p => p.classList.remove('active'));
          this.classList.add('active');
          document.getElementById('tab-' + this.dataset.tab).classList.add('active');
        });
      });
    "))
  )
}

# ── Main UI ────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly", primary = "#c8a96e"),
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
    ),
    includeCSS("www/cms.css")
  ),
  uiOutput("root_ui")
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  logged_in  <- reactiveVal(FALSE)
  photos_rv  <- reactiveVal(read_photos())
  edit_id_rv <- reactiveVal(NULL)
  del_id_rv  <- reactiveVal(NULL)

  # Root UI switch
  output$root_ui <- renderUI({
    if (logged_in()) cms_page() else login_page()
  })

  # ── Auth ───────────────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    if (trimws(input$login_user %||% "") == CMS_USER &&
        (input$login_pass %||% "") == CMS_PASS) {
      logged_in(TRUE)
    } else {
      output$login_msg <- renderUI(
        div(class = "alert alert-danger mt-2 py-2 small",
            tags$i(class = "fa-solid fa-circle-xmark"), " Invalid credentials.")
      )
    }
  })

  observeEvent(input$logout_btn, logged_in(FALSE))

  # ── Dashboard ──────────────────────────────────────────────────────────────
  output$dash_stats <- renderUI({
    p  <- photos_rv()
    n  <- nrow(p)
    cn <- function(cat) if (n > 0) sum(p$category == cat, na.rm = TRUE) else 0L
    layout_columns(
      col_widths = c(2, 2, 2, 2, 2, 2),
      stat_card("Total Photos", n),
      stat_card("Featured", if (n > 0) sum(p$featured, na.rm = TRUE) else 0L),
      stat_card("Landscapes",   cn("landscapes")),
      stat_card("Portraits",    cn("portraits")),
      stat_card("Street",       cn("street")),
      stat_card("Architecture", cn("architecture"))
    )
  })

  output$dash_recent <- renderUI({
    p <- photos_rv()
    if (nrow(p) == 0)
      return(p(class = "text-muted", "No photos uploaded yet."))
    recent <- p[seq(nrow(p), max(1L, nrow(p) - 5L)), ]
    div(
      class = "recent-grid",
      lapply(seq_len(nrow(recent)), function(i) {
        r <- recent[i, ]
        div(
          class = "recent-card",
          tags$img(src = photo_src(r$category, r$filename),
                   alt = r$title, class = "recent-img"),
          div(class = "recent-meta",
            div(class = "recent-title fw-semibold small", r$title),
            span(class = "badge bg-secondary", r$category)
          )
        )
      })
    )
  })

  # ── Upload ─────────────────────────────────────────────────────────────────
  output$preview_wrap <- renderUI({
    if (is.null(input$upload_file)) {
      div(class = "text-muted",
          tags$i(class = "fa-solid fa-image fa-2x mb-2"),
          br(), "No file selected")
    } else {
      imageOutput("preview_img", width = "100%", height = "auto", inline = TRUE)
    }
  })

  output$preview_img <- renderImage({
    req(input$upload_file)
    list(src         = input$upload_file$datapath,
         contentType = input$upload_file$type,
         style       = "max-width:100%; max-height:320px; object-fit:contain;")
  }, deleteFile = FALSE)

  observeEvent(input$upload_btn, {
    req(input$upload_file)
    title <- trimws(input$upload_title %||% "")

    if (nchar(title) == 0) {
      output$upload_msg <- renderUI(
        div(class = "alert alert-warning mt-2 py-2 small",
            tags$i(class = "fa-solid fa-triangle-exclamation"), " Title is required.")
      )
      return()
    }

    ext <- tolower(file_ext(input$upload_file$name))
    if (!ext %in% c("jpg","jpeg","png","webp","gif")) {
      output$upload_msg <- renderUI(
        div(class = "alert alert-danger mt-2 py-2 small",
            tags$i(class = "fa-solid fa-circle-xmark"),
            " Unsupported file type. Use JPG, PNG, WebP, or GIF.")
      )
      return()
    }

    cat   <- input$upload_category
    saved <- save_upload(input$upload_file, cat)

    new_row <- data.frame(
      id          = saved$id,
      filename    = saved$filename,
      category    = cat,
      title       = title,
      description = trimws(input$upload_desc %||% ""),
      featured    = isTRUE(input$upload_featured),
      upload_date = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )

    updated <- rbind(photos_rv(), new_row)
    write_photos(updated)
    photos_rv(updated)

    updateTextInput(session,    "upload_title", value = "")
    updateTextAreaInput(session, "upload_desc",  value = "")
    updateCheckboxInput(session, "upload_featured", value = FALSE)

    output$upload_msg <- renderUI(
      div(class = "alert alert-success mt-2 py-2 small",
          tags$i(class = "fa-solid fa-circle-check"),
          sprintf(' “%s” uploaded successfully.', title))
    )
  })

  # ── Manage ─────────────────────────────────────────────────────────────────
  filtered <- reactive({
    p   <- photos_rv()
    cat <- input$filter_cat %||% "all"
    if (nrow(p) == 0 || cat == "all") p else p[p$category == cat, ]
  })

  output$manage_count <- renderUI({
    n <- nrow(filtered())
    span(class = "text-muted small", sprintf("%d photo%s", n, if (n == 1L) "" else "s"))
  })

  output$photos_dt <- renderDT({
    p <- filtered()
    if (nrow(p) == 0) {
      return(datatable(data.frame(Status = "No photos found."),
                       options = list(dom = "t"), rownames = FALSE))
    }

    esc <- htmltools::htmlEscape

    tbl <- data.frame(
      ` `      = sprintf('<img src="%s" class="dt-thumb" alt="">',
                          photo_src(p$category, p$filename)),
      Title    = esc(p$title),
      Category = p$category,
      Featured = ifelse(
        p$featured,
        '<span class="badge bg-warning text-dark">Yes</span>',
        '<span class="badge bg-secondary">No</span>'
      ),
      Date     = p$upload_date,
      Actions  = sprintf(
        paste0(
          '<button class="btn btn-xs btn-outline-primary me-1" title="Edit" ',
          'onclick="Shiny.setInputValue(\'edit_id\',\'%1$s\',{priority:\'event\'})">',
          '<i class="fa fa-pen fa-xs"></i></button>',
          '<button class="btn btn-xs btn-outline-danger" title="Delete" ',
          'onclick="Shiny.setInputValue(\'del_id\',\'%1$s\',{priority:\'event\'})">',
          '<i class="fa fa-trash fa-xs"></i></button>'
        ),
        p$id
      ),
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )

    datatable(
      tbl,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        pageLength = 10,
        dom        = "frtip",
        columnDefs = list(
          list(orderable = FALSE, targets = c(0L, 5L)),
          list(width = "72px",  targets = 0L),
          list(width = "88px",  targets = 5L)
        )
      )
    )
  })

  # ── Edit modal ─────────────────────────────────────────────────────────────
  observeEvent(input$edit_id, {
    id  <- input$edit_id
    p   <- photos_rv()
    row <- p[p$id == id, ]
    if (nrow(row) == 0) return()

    edit_id_rv(id)
    showModal(modalDialog(
      title     = "Edit Photo",
      size      = "m",
      easyClose = TRUE,
      textInput("modal_title",    "Title",
                value = row$title, width = "100%"),
      selectInput("modal_category", "Category",
                  choices = CATEGORIES, selected = row$category, width = "100%"),
      textAreaInput("modal_desc", "Description",
                    value = row$description %||% "", rows = 3, width = "100%"),
      checkboxInput("modal_featured", "Feature on homepage",
                    value = isTRUE(row$featured)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit", "Save Changes", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$save_edit, {
    id  <- edit_id_rv()
    if (is.null(id)) return()

    p   <- photos_rv()
    idx <- which(p$id == id)
    if (length(idx) == 0) return()

    old_cat <- p$category[idx]
    new_cat <- input$modal_category

    if (old_cat != new_cat) {
      old_path <- file.path(IMG_DIR, old_cat, p$filename[idx])
      new_dir  <- file.path(IMG_DIR, new_cat)
      dir.create(new_dir, showWarnings = FALSE, recursive = TRUE)
      if (file.exists(old_path))
        file.rename(old_path, file.path(new_dir, p$filename[idx]))
      p$category[idx] <- new_cat
    }

    p$title[idx]       <- trimws(input$modal_title)
    p$description[idx] <- trimws(input$modal_desc %||% "")
    p$featured[idx]    <- isTRUE(input$modal_featured)

    write_photos(p)
    photos_rv(p)
    edit_id_rv(NULL)
    removeModal()
  })

  # ── Delete modal ───────────────────────────────────────────────────────────
  observeEvent(input$del_id, {
    id  <- input$del_id
    p   <- photos_rv()
    row <- p[p$id == id, ]
    if (nrow(row) == 0) return()

    del_id_rv(id)
    showModal(modalDialog(
      title     = "Delete Photo",
      size      = "s",
      easyClose = TRUE,
      p(sprintf('Delete “%s”? This cannot be undone.', row$title)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_del", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_del, {
    id <- del_id_rv()
    if (is.null(id)) return()

    p   <- photos_rv()
    row <- p[p$id == id, ]
    if (nrow(row) > 0) {
      img_path <- file.path(IMG_DIR, row$category, row$filename)
      if (file.exists(img_path)) file.remove(img_path)
      p <- p[p$id != id, ]
      write_photos(p)
      photos_rv(p)
    }

    del_id_rv(NULL)
    removeModal()
  })
}

shinyApp(ui, server)
