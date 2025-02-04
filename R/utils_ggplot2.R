# This file contains helper functions copied from ggplot2.

GeomDensity2dFilled <- ggproto("GeomDensity2dFilled", GeomPolygon)
GeomContourFilled <- ggproto("GeomContourFilled", GeomPolygon)
geom_spoke <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomSpoke,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
GeomCurve <- ggproto("GeomCurve", GeomSegment,
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                     draw_panel = function(data, panel_params, coord, curvature = 0.5, angle = 90,
                                           ncp = 5, arrow = NULL, arrow.fill = NULL, lineend = "butt", na.rm = FALSE) {

                       if (!coord$is_linear()) {
                         warn("geom_curve is not implemented for non-linear coordinates")
                       }

                       trans <- coord$transform(data, panel_params)

                       arrow.fill <- arrow.fill %||% trans$colour

                       curveGrob(
                         trans$x, trans$y, trans$xend, trans$yend,
                         default.units = "native",
                         curvature = curvature, angle = angle, ncp = ncp,
                         square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
                         gp = gpar(
                           col = alpha(trans$colour, trans$alpha),
                           fill = alpha(arrow.fill, trans$alpha),
                           lwd = trans$size * .pt,
                           lty = trans$linetype,
                           lineend = lineend),
                         arrow = arrow
                       )
                     }
)
GeomHex <- ggproto("GeomHex", Geom,
                   draw_group = function(data, panel_params, coord, lineend = "butt",
                                         linejoin = "mitre", linemitre = 10) {
                     if (empty(data)) {
                       return(zeroGrob())
                     }

                     # Get hex sizes
                     if (!is.null(data$width)) {
                       dx <- data$width[1] / 2
                     } else {
                       dx <- resolution(data$x, FALSE)
                     }
                     # Adjust for difference in width and height of regular hexagon. 1.15 adjusts
                     # for the effect of the overlapping range in y-direction on the resolution
                     # calculation
                     if (!is.null(data$height)) {
                       dy <- data$height[1] /  sqrt(3) / 2
                     } else {
                       dy <- resolution(data$y, FALSE) / sqrt(3) / 2 * 1.15
                     }

                     hexC <- hexbin::hexcoords(dx, dy, n = 1)

                     n <- nrow(data)

                     data <- data[rep(seq_len(n), each = 6), ]
                     data$x <- rep.int(hexC$x, n) + data$x
                     data$y <- rep.int(hexC$y, n) + data$y

                     coords <- coord$transform(data, panel_params)

                     ggname("geom_hex", polygonGrob(
                       coords$x, coords$y,
                       gp = gpar(
                         col = coords$colour,
                         fill = alpha(coords$fill, coords$alpha),
                         lwd = coords$size * .pt,
                         lty = coords$linetype,
                         lineend = lineend,
                         linejoin = linejoin,
                         linemitre = linemitre
                       ),
                       default.units = "native",
                       id.lengths = rep.int(6, n)
                     ))
                   },

                   required_aes = c("x", "y"),

                   default_aes = aes(
                     colour = NA,
                     fill = "grey50",
                     size = 0.5,
                     linetype = 1,
                     alpha = NA
                   ),

                   draw_key = draw_key_polygon
)

# from ggplot2 utilities.R
split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

# from ggplot2 performance.R
df_rows <- function(x, i) {
  new_data_frame(lapply(x, `[`, i = i))
}

# from ggplot2 compat-plyr.R
rbind_dfs <- function(dfs) {
  out <- list()
  columns <- unique(unlist(lapply(dfs, names)))
  nrows <- vapply(dfs, .row_names_info, integer(1), type = 2L)
  total <- sum(nrows)
  if (length(columns) == 0) return(new_data_frame(list(), total))
  allocated <- rep(FALSE, length(columns))
  names(allocated) <- columns
  col_levels <- list()
  ord_levels <- list()
  for (df in dfs) {
    new_columns <- intersect(names(df), columns[!allocated])
    for (col in new_columns) {
      if (is.factor(df[[col]])) {
        all_ordered <- all(vapply(dfs, function(df) {
          val <- .subset2(df, col)
          is.null(val) || is.ordered(val)
        }, logical(1)))
        all_factors <- all(vapply(dfs, function(df) {
          val <- .subset2(df, col)
          is.null(val) || is.factor(val)
        }, logical(1)))
        if (all_ordered) {
          ord_levels[[col]] <- unique(unlist(lapply(dfs, function(df) levels(.subset2(df, col)))))
        } else if (all_factors) {
          col_levels[[col]] <- unique(unlist(lapply(dfs, function(df) levels(.subset2(df, col)))))
        }
        out[[col]] <- rep(NA_character_, total)
      } else {
        out[[col]] <- rep(.subset2(df, col)[1][NA], total)
      }
    }
    allocated[new_columns] <- TRUE
    if (all(allocated)) break
  }
  is_date <- lapply(out, inherits, 'Date')
  is_time <- lapply(out, inherits, 'POSIXct')
  pos <- c(cumsum(nrows) - nrows + 1)
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    rng <- seq(pos[i], length.out = nrows[i])
    for (col in names(df)) {
      date_col <- inherits(df[[col]], 'Date')
      time_col <- inherits(df[[col]], 'POSIXct')
      if (is_date[[col]] && !date_col) {
        out[[col]][rng] <- as.Date(
          unclass(df[[col]]),
          origin = ggplot_global$date_origin
        )
      } else if (is_time[[col]] && !time_col) {
        out[[col]][rng] <- as.POSIXct(
          unclass(df[[col]]),
          origin = ggplot_global$time_origin
        )
      } else if (date_col || time_col || inherits(df[[col]], 'factor')) {
        out[[col]][rng] <- as.character(df[[col]])
      } else {
        out[[col]][rng] <- df[[col]]
      }
    }
  }
  for (col in names(ord_levels)) {
    out[[col]] <- ordered(out[[col]], levels = ord_levels[[col]])
  }
  for (col in names(col_levels)) {
    out[[col]] <- factor(out[[col]], levels = col_levels[[col]])
  }
  attributes(out) <- list(
    class = "data.frame",
    names = names(out),
    row.names = .set_row_names(total)
  )
  out
}

# from gglpot2 utilities-grid.r
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# from gglpot2 ggproto.r
ggproto_formals <- function(x) formals(environment(x)$f)

# from gglpot2 layer.r
obj_desc <- function(x) {
  if (isS4(x)) {
    paste0("an S4 object with class ", class(x)[[1]])
  } else if (is.object(x)) {
    if (is.data.frame(x)) {
      "a data frame"
    } else if (is.factor(x)) {
      "a factor"
    } else {
      paste0("an S3 object with class ", paste(class(x), collapse = "/"))
    }
  } else {
    switch(typeof(x),
      "NULL" = "a NULL",
      character = "a character vector",
      integer = "an integer vector",
      logical = "a logical vector",
      double = "a numeric vector",
      list = "a list",
      closure = "a function",
      paste0("a base object of type", typeof(x))
    )
  }
}

# from gglpot2 scale-type.r
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggiraph")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

# from gglpot2 utilities.r
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

# from gglpot2 utilities.r
firstUpper <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

# from ggplot2 utilities.r
is.waive <- function(x) inherits(x, "waiver")

# from ggplot2 utilities.r
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

# from ggplot2 utilities.r
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# from ggplot2 utilities.r
message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

# from ggplot2 grob-null.r
is.zero <- function(x)
  is.null(x) || inherits(x, "zeroGrob")

# from gglpot2 utilities.r
compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

# from ggplot2 geom.r
check_aesthetics <- function(x, n) {
  ns <- vapply(x, length, numeric(1))
  good <- ns == 1L | ns == n

  if (all(good)) {
    return()
  }

  abort(paste0(
    "Aesthetics must be either length 1 or the same as the data (", n, "):\n",
    paste(names(which(!good)), collapse = ", ")
  ))
}
