library(htmltools)

# ── Image URL helpers ──────────────────────────────────────────────────────────

#' Build a picsum.photos URL (seed-based for consistent results)
#' @param seed  Unique string seed
#' @param w     Width in pixels
#' @param h     Height in pixels
picsum_url <- function(seed, w, h) {
  sprintf("https://picsum.photos/seed/%s/%d/%d", seed, w, h)
}

# ── Gallery components ─────────────────────────────────────────────────────────

#' Create one lightbox-enabled gallery thumbnail
#'
#' @param seed         picsum seed string (used for both thumb and full image)
#' @param title        Caption shown in the lightbox
#' @param gallery_name Groups images for prev/next lightbox navigation
#' @param thumb_w,thumb_h  Thumbnail dimensions (px)
#' @param full_w,full_h    Full-resolution lightbox dimensions (px)
gallery_item <- function(seed, title, gallery_name,
                         thumb_w = 800L, thumb_h = 600L,
                         full_w  = 1600L, full_h = 1200L) {
  tags$div(
    class = "gallery-item",
    tags$a(
      href           = picsum_url(seed, full_w, full_h),
      class          = "lightbox",
      `data-gallery` = gallery_name,
      title          = title,
      tags$img(
        src = picsum_url(seed, thumb_w, thumb_h),
        alt = title
      )
    )
  )
}

#' Render a complete named gallery section (heading + subtitle + grid)
#'
#' @param id      HTML anchor id; also used as the lightbox group name
#' @param heading Display heading text
#' @param subtitle Small italic subtitle below the heading
#' @param photos  data.frame with (at minimum) columns \code{seed} and
#'                \code{title}; one row per photograph
gallery_section <- function(id, heading, subtitle, photos) {
  items <- mapply(
    FUN       = function(seed, title) gallery_item(seed, title, id),
    seed      = photos$seed,
    title     = photos$title,
    SIMPLIFY  = FALSE,
    USE.NAMES = FALSE
  )

  grid <- do.call(tags$div, c(list(class = "gallery-grid"), items))

  tagList(
    tags$h2(class = "section-title", id = id, heading),
    tags$hr(class = "section-divider"),
    tags$p(class = "section-subtitle", subtitle),
    grid
  )
}

# ── Homepage components ────────────────────────────────────────────────────────

#' Create the full-viewport hero banner
#'
#' @param seed    picsum seed for the background image
#' @param name    Photographer name (large heading)
#' @param tagline Short tagline displayed below the name
#' @param cta_label  Button label
#' @param cta_href   Button link destination
hero_section <- function(seed       = "auggie-hero",
                          name       = "Auggie Smith",
                          tagline    = paste("Capturing Light",
                                             "Telling Stories",
                                             "Finding Beauty",
                                             sep = "\u00a0\u00b7\u00a0"),
                          cta_label  = "View Portfolio",
                          cta_href   = "portfolio.html") {
  tags$div(
    class = "hero",
    tags$img(
      src   = picsum_url(seed, 1920L, 1080L),
      class = "hero-image",
      alt   = paste("Featured photograph by", name)
    ),
    tags$div(
      class = "hero-content",
      tags$h1(name),
      tags$p(class = "tagline", tagline),
      tags$a(href = cta_href, class = "btn-photo", cta_label)
    )
  )
}

#' Create one item in the homepage featured-work grid
#'
#' @param seed    picsum seed
#' @param alt     Alt text / caption
#' @param href    Click destination (default portfolio page)
#' @param wide    Logical; if TRUE, span two grid columns and use 16:9 ratio
featured_item <- function(seed, alt,
                           href = "portfolio.html",
                           wide = FALSE) {
  cls    <- if (wide) "photo-item wide" else "photo-item"
  w      <- if (wide) 1200L else 600L
  h      <- if (wide) 675L  else 450L

  tags$div(
    class = cls,
    tags$a(
      href = href,
      tags$img(src = picsum_url(seed, w, h), alt = alt)
    )
  )
}

#' Render the featured-work grid from a data frame
#'
#' @param photos data.frame with columns: seed, alt, wide (logical)
featured_grid <- function(photos) {
  items <- mapply(
    FUN       = function(seed, alt, wide) featured_item(seed, alt, wide = wide),
    seed      = photos$seed,
    alt       = photos$alt,
    wide      = photos$wide,
    SIMPLIFY  = FALSE,
    USE.NAMES = FALSE
  )
  do.call(tags$div, c(list(class = "photo-grid"), items))
}

# ── Generic layout helpers ─────────────────────────────────────────────────────

#' Render a call-to-action block (centred button)
cta_block <- function(label, href, muted = FALSE) {
  btn_class <- if (muted) "btn-photo btn-photo-muted" else "btn-photo"
  tags$div(
    class = "cta-block",
    tags$a(href = href, class = btn_class, label)
  )
}

#' Render a section heading + optional subtitle
section_heading <- function(heading, subtitle = NULL, id = NULL) {
  h2_args <- list(class = "section-title", heading)
  if (!is.null(id)) h2_args$id <- id

  tl <- tagList(
    do.call(tags$h2, h2_args),
    tags$hr(class = "section-divider")
  )
  if (!is.null(subtitle)) {
    tl <- tagList(tl, tags$p(class = "section-subtitle", subtitle))
  }
  tl
}
