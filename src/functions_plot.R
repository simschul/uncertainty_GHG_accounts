# plot function 


weighted.var <- function (x, w = NULL, na.rm = FALSE){
  # from:   https://rdrr.io/github/hadley/bigvis/man/weighted.var.html
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
  sum(w * (x - weighted.mean(x, w))^2)/(sum(w) - 1)
}

weighted.varcoef <- function (x, w = NULL, na.rm = FALSE) {
  sqrt(weighted.var(x = x, w = w, na.rm = na.rm)) / mean(x, na.rm = na.rm)
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out) 
}

percentile <- function(x) {
  out <- quantile(x, probs = c(0.025,0.975))
  names(out) <- c("ymin","ymax")
  return(out)
}


get_gray_shades <- function(n, from = "grey50", to = "grey80") {
  if (n <= 1) {
    return(from)  # Return the lower boundary if n is 1 or less
  }
  
  if (n > 20) {
    stop("Maximum number of shades exceeded. Please choose a smaller value for n.")
  }
  
  # Parse the shade values from the 'from' and 'to' arguments
  from_value <- as.numeric(gsub("\\D", "", from))
  to_value <- as.numeric(gsub("\\D", "", to))
  
  # Calculate the increment between shades
  increment <- (to_value - from_value) / (n - 1)
  
  # Create a vector to store the shades
  shades <- vector("character", n)
  
  # Generate the shades of gray
  for (i in 1:n) {
    shade_value <- from_value + (i - 1) * increment
    shades[i] <- paste0("grey", shade_value)
  }
  
  return(shades)
}



quantile2 <- function(x, probs, mean) {
  x <- x[order(mean, decreasing = TRUE)]
  cumsum <- cumsum(x)
  out <- quantile(cumsum, probs = probs)
  names(out) <- c("y")
  return(out)
}

#' Title
#'
#' @param probs 
#' @param from 
#' @param to 
#'
#' @return
#' @export
#'
#' @examples
geom_cdf_bar <- function(probs = c(0.5, 0.66, 0.95, 0.99), 
                         from = "grey50", to = "grey80", 
                         mean, ...) {
  cols <- get_gray_shades(length(probs))
  geom_list <- lapply(length(probs):1, function(i) {
    stat_summary(fun.data =  function(x) quantile2(x, probs[i], mean), geom = 'col', 
                 width = 1 - probs[i], fill = cols[i], ...)
  })
  return(geom_list)
}


# give.n <- function(x){
#   # from: https://stackoverflow.com/questions/28846348/add-number-of-observations-per-group-in-ggplot2-boxplot
#   return(c(y = 0, label = length(x))) #median(x)*1.05 
#   # experiment with the multiplier to find the perfect position
# }

give.n <- function(x, y = 0){
  return(data.frame(y = y, label = paste0("N=",length(x))))
}










#' Modifies the ggplot geom_boxplot function to extend the whiskers to a specific
#' percentile instead of 1.5 QR. 
#' Copied from: https://gist.github.com/rabutler/bd97a6f49db87860f987156842fd4ee5
#'
#' @param mapping 
#' @param data 
#' @param geom 
#' @param position 
#' @param ... 
#' @param qs 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#'
#' @return
#' @export
#'
#' @examples

stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      qs = qs,
      ...
    )
  )
}

StatBoxplotCustom <- ggplot2::ggproto("StatBoxplotCustom", ggplot2::Stat,
                                      required_aes = c("x", "y"),
                                      non_missing_aes = "weight",
                                      
                                      setup_params = function(data, params) {
                                        params$width <- ggplot2:::"%||%"(params$width, (resolution(data$x) * 0.75))
                                        
                                        if (is.double(data$x) && !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                          warning(
                                            "Continuous x aesthetic -- did you forget aes(group=...)?",
                                            call. = FALSE)
                                        }
                                        
                                        params
                                      },
                                      
                                      compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(.05, .25, 0.5, 0.75, 0.95)) {
                                        
                                        if (!is.null(data$weight)) {
                                          mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                          stats <- as.numeric(stats::coef(mod))
                                        } else {
                                          stats <- as.numeric(stats::quantile(data$y, qs))
                                        }
                                        names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                                        iqr <- diff(stats[c(2, 4)])
                                        
                                        outliers <- (data$y < stats[1]) | (data$y > stats[5])
                                        #if (any(outliers)) {
                                        #  stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                                        #}
                                        
                                        if (length(unique(data$x)) > 1)
                                          width <- diff(range(data$x)) * 0.9
                                        
                                        df <- as.data.frame(as.list(stats))
                                        df$outliers <- list(data$y[outliers])
                                        
                                        if (is.null(data$weight)) {
                                          n <- sum(!is.na(data$y))
                                        } else {
                                          # Sum up weights for non-NA positions of y and weight
                                          n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                                        }
                                        
                                        df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                                        df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                                        
                                        df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                                        df$width <- width
                                        df$relvarwidth <- sqrt(n)
                                        df
                                      }
)



# Fill
scale_fill_colorblind7 = function(.ColorList = 2L:8L, ...){
  scale_fill_discrete(..., type = colorblind_pal()(8)[.ColorList])
}

# Color
scale_color_colorblind7 = function(.ColorList = 2L:8L, ...){
  scale_color_discrete(..., type = colorblind_pal()(8)[.ColorList])
}


ggsave2 <- function(filename = default_name(plot), height= 8, width=  7, 
                    dpi= 300, 
                    device = 'pdf', path = path2plot, ...) {
  ggsave(filename=filename, height=height, width=width, dpi=dpi, device = device,
         path = path, ...)
}

ggsave_data <- function(x, filename, path = path2plot, ...) {
  fwrite(x, file =file.path(path2plot,filename), ...)
}



