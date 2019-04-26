# Set the crayonmap class. Still not very useful but, since the crayons slot is a vector, it is important to memorize the number of rows and columns for future applications
setClass("crayonmap", representation(crayons = "character", row.num = "numeric", col.num = "numeric"))

#' Color key
#' @param values vector of numeric values to which colors will be mapped
#' @param pal color palette 
#' @param scale logical, should values be scaled? Defaults to TRUE
#' @return a vector of colors, mapped to the values and in the same order as the values vector
#' @author Giuseppe D'Agostino

colorKey <- function(values, 
                     pal = colorRampPalette(c("red", "gray", "blue"))(24), 
                     scale = TRUE) {
  require(pheatmap)

  values <- if (scale) scale(values) else values
  bks <- pheatmap:::generate_breaks(values, length(pal), center = FALSE)
  cols <- pheatmap:::scale_colours(values, col = pal, breaks = bks, na_col = "gray")
  cols <- as.character(cols)
  return(cols)
} 

setMethod("show", "crayonmap", function(object) cat(object@crayons, sep = ""))

rpad <- function(str, width = max(nchar(str))) {
  if (max(nchar(str)) > width) {
    str <- substr(str, 0, width)
  }
  mn <- max(nchar(str))
  paste0(
    str, 
    vapply(mn - nchar(str), 
      function(i) paste(rep(" ", i), collapse = ""),
      character(1)
    )
  )
}

#' Text-based heatmap,
#' @param dat matrix with numeric values, rownames and colnames
#' @param cols color palette 
#' @param cluster_cols logical, should columns be clustered by hierarchical clustering? default is TRUE
#' @param cluster_rows logical, should rows be clustered by hierarchical clustering? default is TRUE
#' @param dist_method character, distance method used by dist
#' @param clustering_method character, clustering method used by hclust 
#' @param main character, title of the heatmap
#' @param key character, title of the color legend
#' @param show_col_legend logical, should the column legend be shown at the end of the plot? default is TRUE
#' @return a text-based heatmap directly in the terminal output using ANSI background styles. Useful when your X11 forwarding is broken or for quick exploratory analysis of small datasets. Setup is largely inspired by pheatmap.
textHeatmap <-function(dat, 
    pal = colorRampPalette(c("red", "gray", "blue"))(24), 
    cluster_cols = TRUE, 
    cluster_rows = TRUE,
    dist_method = "euclidean",
    clustering_method = "complete",
    main = "Heatmap",
    key = "Key", 
    show_col_legend = TRUE,
    lpad = "") {

  require(crayon)

  ## Clustering
  ## Hierarchical clustering of the rows
  if (cluster_rows) { 
    hr <- hclust(                
      dist(dat, method = dist_method), 
      method = clustering_method
    )
  } else {
    hr <- list("order" = 1:nrow(dat))
  }

  ## Hierarchical clustering of columns (same as rows but transposing the matrix)
  if (cluster_cols) {
    hc <- hclust(
      dist(t(dat), method = dist_method),  
      method = clustering_method
    )
  } else {
    hc <- list("order" = 1:ncol(dat))
  }

  dat <- dat[hr$order, hc$order]

  ## Colour rendering and crayonmap object
  crayonstrings <- matrix(
    unlist(
      sapply(colorKey(as.vector(dat), pal = pal), 
        function(x) {
          crayon::make_style(x, bg = TRUE)("   ")
        }
      )
    ), 
    nrow = nrow(dat)
  )
  crayonstrings[, ncol(crayonstrings)] <- paste0(
    crayonstrings[, ncol(crayonstrings)], 
    crayon::reset("   "), 
    rpad(rownames(dat))
  )

  crayonstrings[, 1] <- paste(lpad, crayonstrings[, 1], sep = "")

  if (cluster_rows) {
    rd <- textDendrogram(hr, horiz = TRUE, yend = 20)
    crayonstrings <- cbind(crayonstrings, rd)
    crayonstrings[, ncol(crayonstrings)] <- paste0(
      crayonstrings[, ncol(crayonstrings)]
    )    
  } else {
    crayonstrings[, ncol(crayonstrings)] <- paste0(
      crayonstrings[, ncol(crayonstrings)],
      "\n"
    )
  }

  crayons <- as.vector(t(crayonstrings))

  cmap <- new("crayonmap", 
    crayons = crayons, 
    row.num = nrow(dat), 
    col.num = ncol(dat))

  ## Column dendrogram
  if (cluster_cols) {
    cat(as.vector(t(textDendrogram(hc, yend = 10))), sep = "")
  }
  ## Column names
  ## padding
  cat(lpad)

  for (k in seq_len(ncol(dat))) {
    if (nchar(k) == 1) {
      cat(paste(" ", k, " ", sep = ""))
    } else if (nchar(k) == 2) {
      cat(paste(k, " ", sep = ""))
    }
  }
  cat("\n")

  ## Plot the heatmap
  print(cmap)

  ## Color Key
  ## padding
  # cat("\n \n") 
  cat(lpad)
  ckey <- unique(
    unlist(
      sapply(pal, 
        function(x) {
          crayon::make_style(x, bg = TRUE)("   ")
        }
      )
    )
  )
  mid.value <- round((max(dat) - abs(min(dat))) / 2)

  cat("Key", "\n", 
    lpad, 
    round(min(dat)), 
    rep("   ", length(ckey) / 2), 
    mid.value,  
    rep("   ", (length(ckey) / 2) - 1), " ", 
    round(max(dat)), "\n", "   ", 
    ckey, "\n\n", 
    sep = ""
  )

  ## Column legend
  if (show_col_legend) {
    cat(lpad)
    cat("Column legend:\n")
    for (i in seq_len(ncol(dat))) {
      cat(paste(lpad, i, ":", colnames(dat)[i], "\n", sep = ""))
    }
  }
}


textDendrogram <- function(hc, horiz = FALSE, yend = 20) {
  d <- as.dendrogram(hc, hang = -1)
  ggd <- dendextend::as.ggdend(d)
  segs <- ggd$segments
  xmax <- max(segs$x)
  yscale <- c(1, yend)
  segs[, c("y", "yend")] <- lapply(segs[, c("y", "yend")], 
    scales::rescale, to = if (horiz) yscale else rev(yscale)
  )
  if (!horiz) {
    segs[, c("x", "xend")] <- round(segs[, c("x", "xend")] * 3)
  }
  
  if (horiz) {
    segs[, c("x", "xend")] <- lapply(segs[, c("x", "xend")], 
      function(x) ifelse(x < xmax, x, xmax)
    )
  }
  segs[, c("x", "y", "xend", "yend")] <- round(segs[, c("x", "y", "xend", "yend")])
  mat <- matrix(NA, ncol = max(segs$x), nrow = max(segs$y))
  if (horiz) {
    mat <- t(mat)
    segs[, c("x", "xend")] <- segs[, c("x", "xend")] + segs[, c("y", "yend")]
    segs[, c("y", "yend")] <- segs[, c("x", "xend")] - segs[, c("y", "yend")]
    segs[, c("x", "xend")] <- segs[, c("x", "xend")] - segs[, c("y", "yend")]    
  }
  for (i in seq_len(nrow(segs))) {
    mat <- add_text_line(mat, segs[i, "x"], segs[i, "xend"], segs[i, "y"], segs[i, "yend"])
  }
  mat[is.na(mat)] <- " "
  mat[, ncol(mat)] <- paste(mat[, ncol(mat)], crayon::reset(), "\n")
  if (!horiz) {
    mat[, 1] <- paste("   ", mat[, 1], sep = "")
  }
  mat
}

add_text_line <- function(matrix, x, xend, y, yend) {
  ## Vertical
  if (x == xend) {
    for (j in y:yend) {
      matrix[j, x] <- paste0(
        # crayon::make_style("black", bg = TRUE)(" "), 
        crayon::make_style("white", bg = TRUE)(" ")
        # crayon::make_style("black", bg = TRUE)(" ")
      )
    }
  ## Horizontal
  } else if (y == yend) {
    for (i in x:xend) {
      matrix[y, i] <- paste0(
        crayon::make_style("white", bg = TRUE)(" ")
      )
    }
    
  }
  matrix
}
