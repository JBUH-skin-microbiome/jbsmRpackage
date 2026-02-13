#' Create an empty PowerPoint file
#'
#' Create a new PowerPoint document and write it to disk.
#'
#' @param path File path to write the .pptx file.
#'
#' @return Invisibly returns \code{path}.
#' @export
#'
#' @examples
#' \dontrun{
#' make_pptx("figure.pptx")
#' }
make_pptx <- function(path) {
  officer::read_pptx() |>
    print(target = path)
  invisible(path)
}

#' Save a ggplot object into a PowerPoint slide
#'
#' Adds a new slide to an existing (or new) PowerPoint file and places
#' a ggplot object as an editable vector graphic (DrawingML).
#'
#' @param ggobj A ggplot object.
#' @param path Path to the target .pptx file. If it does not exist, a new file is created.
#' @param layout Slide layout name. Default is \code{"Title and Content"}.
#' @param master Slide master name. Default is \code{"Office Theme"}.
#' @param left,top,width,height Numeric positions/sizes (in inches) for placing the graphic.
#'
#' @return Invisibly returns \code{path}.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' save_ggplot_pptx(p, "figure.pptx")
#' }
save_gg_pptx <- function(
    ggobj,
    path,
    layout = "Title and Content",
    master = "Office Theme",
    left = 0, top = 0, width = 8, height = 4
) {
  # ggplot object -> editable DrawingML
  editable_graph <- rvg::dml(ggobj = ggobj)

  # create or open pptx
  if (file.exists(path)) {
    doc <- officer::read_pptx(path)
  } else {
    doc <- officer::read_pptx()
  }

  doc |>
    officer::add_slide(layout = layout, master = master) |>
    officer::ph_with(
      value = editable_graph,
      location = officer::ph_location(left = left, top = top, width = width, height = height)
    ) |>
    print(target = path)

  message(sprintf("Saved ggplot object to %s", path))
  invisible(path)
}
