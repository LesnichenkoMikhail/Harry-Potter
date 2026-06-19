# градиент от золотого к бордовому для раскраски графиков
red_gold_gradient <- function(x, low = "#d3a625", high = "#7f0909") {
  if (length(x) == 0) {
    return(character())
  }

  palette <- grDevices::colorRampPalette(c(low, "#b85c1e", high))(100)
  value_range <- range(x, na.rm = TRUE)

  if (!is.finite(value_range[1]) || value_range[1] == value_range[2]) {
    return(rep(high, length(x)))
  }

  scaled <- scales::rescale(x, to = c(1, 100), from = value_range)
  palette[pmax(1, pmin(100, round(scaled)))]
}
