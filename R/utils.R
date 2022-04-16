#' @importFrom rlang abort
# Include generic helpers from ggplot2
#' @include utils_ggplot2.R
#' @include utils_ggplot2_performance.R

# FIXME
# dapply is in ggplot2/compat-plyr.R, but if we include that file,
# it causes more issues as it depends ggplot2 globals.
# dapply is only used in GeomInteractivePath.
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(new_data_frame())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(new_data_frame(unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    new_data_frame(res[intersect(c(fallback_order, names(res)), names(res))])
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  rbind_dfs(lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  }))
}

# Include parameters
#' @include ipar.R


#' Encodes the attribute value designated for tooltip
#' @importFrom htmltools htmlEscape
#' @noRd
encode_cr <- function(x) {
  newlines_pattern <- "(\r\n|\r|\n)"
  # quick check to see if we need to replace newline chars
  # exclude text marked as html with htmltools::HTML
  replace_newlines <- !inherits(x, "html") && any(grepl(newlines_pattern, x))
  if (replace_newlines) {
    x <- sapply(x, function(t) {
      # text might be markup anyway, check for opening/closing tags at start/end
      if (grepl(newlines_pattern, t) &&
          !(grepl("^\\s*<\\w+.*?/?>", t) && grepl("</?\\w+.*?/?>\\s*$", t))) {
        gsub(newlines_pattern, replacement = "<br/>", x = t)
      } else {
        t
      }
    })
  }
  htmltools::htmlEscape(x, attribute = TRUE)
}


#' @section Geoms:
#'
#' All `geom_*_interactive` functions (like `geom_point_interactive`) return a layer that
#' contains a `GeomInteractive*` object (like `GeomInteractivePoint`). The `Geom*`
#' object is responsible for rendering the data in the plot.
#'
#' See \code{\link{Geom}} for more information.
#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @title ggproto classes for ggiraph
#' @name GeomInteractive
#' @keywords internal
NULL


#' Generates a default aesthetics mapping for an interactive class
#' by copying the default aes and appending the interactive attrs.
#' @noRd
add_default_interactive_aes <- function(geom = Geom,
                                        defaults = IPAR_DEFAULTS) {
  append_aes(geom$default_aes, defaults)
}

#' Override of default parameters function in order to:
#' - Get the parameters/arguments from super class
#' - Add the extra .ipar argument
#' @noRd
interactive_geom_parameters <- function(self, extra = FALSE) {
  parent_params <- self$super()$parameters(extra = extra)
  panel_args <- names(ggproto_formals(self$draw_panel))
  group_args <- names(ggproto_formals(self$draw_group))
  if ((".ipar" %in% panel_args || ".ipar" %in% group_args) && !(".ipar" %in% parent_params)) {
    c(parent_params, ".ipar")
  } else {
    parent_params
  }
}

#' Override of default draw_key function, in order to add
#' the interactive attrs
#' @noRd
interactive_geom_draw_key <- function(self, data, params, size) {
  gr <- self$super()$draw_key(data, params, size)
  add_interactive_attrs(gr, data, data_attr = "key-id", ipar = get_ipar(params))
}

#' Appends a list of attributes to an aesthetic mapping.
#' @noRd
append_aes <- function(mapping, lst) {
  aes_new <- structure(lst, class = "uneval")
  mapping[names(aes_new)] <- aes_new
  mapping
}

#' Returns the arguments for a grob function
#' @noRd
grob_argnames <- function(x, grob) {
  intersect(names(formals(grob)), names(x))
}

#' Returns the contents of a file as text
#' @noRd
read_file <- function(path, ..., encoding = "UTF-8", warn = FALSE) {
  paste0(readLines(con = path, encoding = encoding, warn = warn, ...), collapse = "\n")
}

#' Returns the system os (lowercase: windows, osx, linux)
#' Taken from https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
#' @noRd
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
