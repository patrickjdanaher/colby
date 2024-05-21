#' Get colors from a numeric vector
#' 
#' Defines colors for 3 cases: non-negative vectors where position vis-a-vis 0 has meaning, vectors rounghly centered around 0, and vectors where location is irrelevant. 
#' Tries to infer which case pertains to your vector, but this logic can be overridden with the `type` argument. 
#' @param x vector to color by
#' @param type One of "nonnegative", "centered", "arbitrary", or NULL. 
#' \itemize{
#'  \item If "nonnegative", the color scale will extend from 0 upwards
#'  \item If "centered", the color scale will extend equidistant from 0 in both directions
#'  \item If "arbitrary", the color scale will extend along the range of x
#'  \item If NULL, then the function will do its best to choose from the above.
#' }
#' @param quant Data will be truncated at this quantile. 
#' @param log If TRUE, data will be log-transformed
#' @param cols Vector of colors to form a color palette from
#' @param na_col Color assigned to NA elements of x
#' @return A list: \enumerate{
#' \item col, a vector of colors corresponding to the elements of x
#' \item palette, the color palette used (for use in legendd)
#' \item legendtext, text to be used in legends
#' \item legendcols, colors to show in legends
#' }
#' @importFrom stats quantile
#' @importFrom grDevices colorRampPalette
#' @export
colby <- function(x, type = NULL, cols = NULL, quant = 0.99, log = FALSE, na_col = NA) {
  
  
  # log-transform if asked for
  if (log) {
    if (any(x < 0)) {
      stop("negative values found; log-transform can't be performed")
    } else {
      x <- log(x)
      x[x == -Inf] <- min(x[x != -Inf])
      type <- "arbitrary"
    }
  }
  
  # choose type:
  if (is.null(type)) {
    type <- chooseType(x)
  } 
  type <- match.arg(tolower(type),  c("nonnegative", "centered", "arbitrary"))
  
  # define cols if not provided:
  if (is.null(cols)) {
    cols = c("darkblue", "blue", "grey80", "red", "darkred")
    if (type == "nonnegative") {
      cols = c("grey80", "darkblue")
    }
  }
  
  # truncate
  if (type == "nonnegative") {
    upperquant <- quantile(x, probs = quant, na.rm = T)
    x <- pmin(x, upperquant)
    legendtext <- paste0(c(rep("", 4), ">="),
                         seq(0, signif(upperquant, 2), length.out = 5))
  }
  if (type == "centered") {
    fartherquant <- max(quantile(x, probs = quant, na.rm = TRUE), quantile(-x, probs = quant, na.rm = TRUE))
    x <- pmax(pmin(x, fartherquant), -fartherquant)
    legendtext <- paste0(c("<=", rep("", 3), ">="),
                         signif(seq(-fartherquant, fartherquant, length.out = 5), 2))
  }
  if (type == "arbitrary") {
    upperlimit <- quantile(x, probs = quant, na.rm = TRUE)
    lowerlimit <- quantile(x, probs = 1 - quant, na.rm = TRUE)
    x <- pmax(pmin(x, upperlimit), lowerlimit)
    legendtext <- paste0(c("<=", rep("", 3), ">="),
                         seq(signif(lowerlimit, 2), signif(upperlimit, 2), length.out = 5))
  }
  
  # define the color scale:
  palette <- grDevices::colorRampPalette(cols)(101)
  
  # map x to the color scale:
  if (type == "nonnegative") {
    col <- palette[round(1 + 100 * x / max(x))]
  }
  if (type == "centered") {
    col <- palette[round(51 + 50 * x / max(abs(x)))]
  }
  if (type == "arbitrary") {
    col <- palette[round(1 + 100 * (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))]
  }
  # internal check: NA's shouldn't have been created:
  if (any(is.na(x)[is.na(col)])) {
    stop("new NAs in col")
  }
  if (length(x) != length(col)) {
    stop("col isn't long enough")
  }
  
  legendcols <- palette[c(1, 26, 51, 76, 101)]
  
  return(list(col = col, palette = palette, legendtext = legendtext, legendcols = legendcols))
}



#' Infer what kind of color scale to draw
#' 
#' Use the range of x to determine what type of color scale would work best.
#' @param x Numeric vector
#' @return one of "nonnegative", "centered", or "arbitrary"
chooseType <- function(x) {
  type <- "arbitrary"
  if (log2(max(pmax(x, 0), na.rm = TRUE) / max(pmax(-x, 0), na.rm = TRUE)) <= 1) {
    type <- "centered"
  }
  if (all(x >= 0, na.rm = TRUE) & ((max(x, na.rm = TRUE) / min(x, na.rm = TRUE)) > 3)) {
    type <- "nonnegative"
  } 
  return(type)
}




#' Draw a legend based on the output
#' 
#' Draws a legend based on colby() outputs in a new graphics window.
#' @param res The list output by `colby`
#' @return Draws a legend
#' @importFrom graphics image
#' @importFrom graphics axis
#' @export
drawColorLegend <- function(res) {
  graphics::image(z = matrix(seq_len(101), nrow = 1), col = res$palette, axes = FALSE, las = 2)
  graphics::axis(2, at = seq(0, 1, length.out= length(res$legendtext)), labels = res$legendtext, las = 2)
}

