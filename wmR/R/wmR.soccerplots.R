#' Create soccer analytics radar charts
#'
#' Generates soccer scouting radar charts with optional league-average overlays
#' and multi-player grid layouts.
#'
#' @param data data.frame or data.table containing player metrics
#' @param measurecols character vector of metric columns
#' @param IDcol column containing player IDs (default "player")
#' @param Players optional vector of IDs to plot
#' @param ShowAverage ID that represents league average (default NA)
#' @param colorPalette palette specification ("wesanderson" or color vector)
#' @param Normexplain logical, add normalization caption
#' @param grid logical, arrange multiple plots in grid
#' @param weights optional numeric vector controlling label fontsize
#'
#' @return ggplot object or list/grid of ggplots
#' @export
soccer_radar_plot <- function(
    data,
    measurecols,
    IDcol = "player",
    Players = NULL,
    ShowAverage = NA,
    colorPalette = "wesanderson",
    Normexplain = TRUE,
    grid = TRUE,
    weights = NULL
) {

  requireNamespace("data.table")
  requireNamespace("ggradar")
  requireNamespace("ggplot2")

  dt <- data.table::as.data.table(data)

  if (!IDcol %in% names(dt)) {
    stop("IDcol not found in data.")
  }

  if (!all(measurecols %in% names(dt))) {
    stop("Some measurecols are missing from data.")
  }

  if (is.null(weights)) {
    weights <- rep(1, length(measurecols))
  }

  if (length(weights) != length(measurecols)) {
    stop("weights must have same length as measurecols")
  }

  names(weights) <- measurecols

  if (!is.null(Players)) {
    dt <- dt[get(IDcol) %in% c(Players, ShowAverage)]
  }

  if (nrow(dt) == 0) {
    stop("No players remaining after filtering.")
  }

  dt_original <- data.table::copy(dt)

  dt_scaled <- data.table::copy(dt)

  dt_scaled[, (measurecols) := lapply(.SD, function(x) x / 100),
            .SDcols = measurecols]

  avg_row <- NULL

  if (!is.na(ShowAverage)) {

    avg_row <- dt_scaled[get(IDcol) == ShowAverage]

    if (nrow(avg_row) == 0) {
      stop("ShowAverage value not found in ID column.")
    }
  }

  player_rows <- dt_scaled

  if (!is.na(ShowAverage)) {
    player_rows <- dt_scaled[get(IDcol) != ShowAverage]
  }

  caption_text <- NULL
  if (Normexplain) {
    caption_text <- "Metrics normalized to percentile scale (0–100)."
  }

  make_colors <- function(n) {

    if (identical(colorPalette, "wesanderson")) {

      if (!requireNamespace("wesanderson", quietly = TRUE)) {
        stop("Package 'wesanderson' required for this palette.")
      }

      wesanderson::wes_palette("GrandBudapest1", max(n,1), type = "continuous")

    } else if (is.character(colorPalette)) {

      rep(colorPalette, length.out = n)

    } else {

      stop("Invalid colorPalette argument.")
    }
  }

  build_radar <- function(player_df) {

    plot_data <- player_df

    if (!is.null(avg_row)) {
      plot_data <- data.table::rbindlist(list(player_df, avg_row))
    }

    plot_data <- data.table::copy(plot_data)
    data.table::setnames(plot_data, IDcol, "group")

    n_groups <- nrow(plot_data)
    colors <- make_colors(n_groups)

    p <- ggradar::ggradar(
      plot_data[, c("group", measurecols), with = FALSE],
      fill = TRUE,
      fill.alpha = 0.35,
      group.line.width = 1.5,
      group.point.size = 3,
      gridline.mid.colour = "#E0E0E0",
      gridline.min.colour = "#EEEEEE",
      gridline.max.colour = "#BDBDBD",
      background.circle.colour = "#FAFAFA",
      group.colours = colors
    ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        panel.background = ggplot2::element_rect(fill = "transparent"),
        text = ggplot2::element_text(family = "sans", color = "#424242",size = 11),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(1, 1, 1, 1)
      )

    player_id <- player_df[[IDcol]]

    vals <- round(as.numeric(dt_original[get(IDcol) == player_id, ..measurecols]),0)

    angles <- seq(0, 2*pi, length.out = length(measurecols) + 1)[-(length(measurecols)+1)]

    label_radius <- 1.08

    label_df <- data.frame(
      x = label_radius * sin(angles),
      y = label_radius * cos(angles),
      label = vals,
      size = weights
    )

    p <- p +
      ggplot2::geom_text(
        data = label_df,
        ggplot2::aes(x = x, y = y, label = label, size = size),
        inherit.aes = FALSE
      ) +
      ggplot2::scale_size_identity()

    if (!is.null(caption_text)) {
      p <- p + ggplot2::labs(caption = caption_text)
    }

    p
  }

  if (nrow(player_rows) == 1) {
    return(build_radar(player_rows))
  }

  plots <- lapply(seq_len(nrow(player_rows)), function(i) {

    player_df <- player_rows[i]

    build_radar(player_df) +
      ggplot2::ggtitle(player_df[[IDcol]])
  })

  if (grid && requireNamespace("ggpubr", quietly = TRUE)) {

    n <- length(plots)
    nc <- ceiling(sqrt(n))
    nr <- ceiling(n / nc)

    return(ggpubr::ggarrange(plotlist = plots, ncol = nc, nrow = nr))

  } else {

    return(plots)
  }
}
