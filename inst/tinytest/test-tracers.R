library(tinytest)
library(ggiraph)
library(xml2)
source("setup.R")

# tracer is working ----
{
  doc <- dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6), c(.4, .3))
    ids <- ggiraph:::dsvg_tracer_off()
  })

  expect_equal(length(ids), 2)
  expect_equal(ids, 1:2)

  circle_id <- sapply(xml_find_all(doc, ".//circle"), xml_attr, "id")
  expect_equal(circle_id, paste0("svgid_el_", as.character(1:2)))
}

# tracer does not work with no device ----
{
  if (length(dev.list()) == 0) {
    expect_error(ggiraph:::dsvg_tracer_on())
    expect_error(ggiraph:::dsvg_tracer_off())
    expect_error(ggiraph:::set_attr(1, "foo", "bar"))
  }
}

# tracer does not work with non-dsvg device ----
{
  file <- tempfile()
  devlength <- length(dev.list())
  tryCatch(
    {
      postscript(file)
      ids <- ggiraph:::dsvg_tracer_off()
    },
    finally = {
      if (length(dev.list()) > devlength) {
        dev.off()
      }
      unlink(file)
    }
  )

  expect_equal(length(ids), 0)
}

# tracer does not work with if not turned on ----
{
  doc <- dsvg_doc({
    plot.new()
    points(0.5, .6)
    ids <- ggiraph:::dsvg_tracer_off()
  })

  expect_equal(length(ids), 0)
}

# attributes are written ----
{
  doc <- dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6), c(.4, .3))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(
      ids = ids,
      str = c("alert(1)", "alert(2)"),
      attribute = "onclick"
    )
  })

  circle_nodes <- xml_find_all(doc, ".//circle")
  expect_equal(length(circle_nodes), 2)
  circle <- circle_nodes[[1]]
  expect_equal(xml_attr(circle, "onclick"), "alert(1)")
  circle <- circle_nodes[[2]]
  expect_equal(xml_attr(circle, "onclick"), "alert(2)")
}

# attributes cannot contain single quotes ----
{
  expect_error(dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6), c(.4, .3))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(
      ids = ids,
      str = c("alert('1')", "alert('2')"),
      attribute = "onclick"
    )
  }))
}

# set_attr throws error with no dsv device ----
{
  file <- tempfile()
  devlength <- length(dev.list())
  expect_error(tryCatch(
    {
      postscript(file)
      set_attr(1, "foo", "bar")
    },
    finally = {
      if (length(dev.list()) > devlength) {
        dev.off()
      }
      unlink(file)
    }
  ))
}

# set_attr throws error with invalid argument types ----
{
  expect_error(dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = "foo", str = 1)
  }))
  expect_error(dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = 1, str = "bar")
  }))
  expect_error(dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = "foo", attribute = "foo", str = "bar")
  }))
}

# set_attr converts factors to character ----
{
  doc <- dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(0.5, .6)
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = factor("foo"), str = factor("bar"))
  })

  circle_nodes <- xml_find_all(doc, ".//circle")
  expect_equal(length(circle_nodes), 1)
  circle <- circle_nodes[[1]]
  expect_equal(xml_attr(circle, "foo"), "bar")
}

# empty attributes are not set ----
{
  doc <- dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(0.5, .6)
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = "foo", str = "")
  })

  circle_node <- xml_find_first(doc, ".//circle")
  expect_false(xml_has_attr(circle_node, "foo"))
}

# set_attr can only set one attribute at a time ----
{
  expect_error(dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6, .7), c(.4, .3, .5))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = c("foo", "foo"), str = "bar")
  }))
}

# set_attr works with multiple ids and one value ----
{
  doc <- dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6), c(.4, .3))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = "foo", str = "bar")
  })
  circle_nodes <- xml_find_all(doc, ".//circle")
  expect_equal(length(circle_nodes), 2)
  expect_equal(xml_attr(circle_nodes[[1]], "foo"), "bar")
  expect_equal(xml_attr(circle_nodes[[2]], "foo"), "bar")
}

# set_attr works with multiple ids and less but even values ----
{
  doc <- dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6, .7, .8), c(.4, .3, .5, .6))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = "foo", str = c("bar1", "bar2"))
  })
  circle_nodes <- xml_find_all(doc, ".//circle")
  expect_equal(length(circle_nodes), 4)
  expect_equal(xml_attr(circle_nodes[[1]], "foo"), "bar1")
  expect_equal(xml_attr(circle_nodes[[2]], "foo"), "bar1")
  expect_equal(xml_attr(circle_nodes[[3]], "foo"), "bar2")
  expect_equal(xml_attr(circle_nodes[[4]], "foo"), "bar2")
}

# set_attr gives a warning with mismatched ids and values ----
{
  expect_warning(dsvg_doc({
    plot.new()
    ggiraph:::dsvg_tracer_on()
    points(c(0.5, .6, .7), c(.4, .3, .5))
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::set_attr(ids = ids, attribute = "foo", str = c("bar1", "bar2"))
  }))
}

# attributes with style css work ----
{
  attrs <- c("hover", "selected")
  types <- c("data", "key", "theme")

  for (type in types) {
    for (attr in attrs) {
      typename <- paste0(type, "-id")
      attrname <- paste0(attr, "_css")
      suffix <- "_"
      if (type != "data") {
        suffix <- paste0(suffix, type, "_")
      }
      doc <- dsvg_doc({
        plot.new()
        ggiraph:::dsvg_tracer_on()
        points(0.5, .6)
        ids <- ggiraph:::dsvg_tracer_off()
        ggiraph:::set_attr(ids = ids, attribute = typename, str = "id")
        ggiraph:::set_attr(
          ids = ids, attribute = attrname,
          str = ggiraph:::check_css_attr("cursor: pointer;")
        )
      })
      style_node <- xml_find_first(doc, ".//style")
      style <- xml2::xml_text(style_node)
      expect_equal(style, paste0(
        ".", attr,
        suffix,
        "svgid[",
        typename,
        " = \"id\"] { cursor: pointer; }"
      ))
    }
  }
}
