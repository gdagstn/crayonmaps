# Set the crayonmap class. Still not very useful but, since the crayons slot is a vector, it is important to memorize the number of rows and columns for future applications
setClass("crayonmap", representation(crayons = "character", row.num = "numeric", col.num = "numeric"))

# Set method to print crayonmaps to the terminal
setMethod("show", "crayonmap", function(object) cat(object@crayons, sep = ""))

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

#' Text-based heatmap
#' @param dat matrix with numeric values, rownames and colnames
#' @param cols color palette 
#' @param cluster_cols logical, should columns be clustered by hierarchical clustering? default is TRUE
#' @param cluster_rows logical, should rows be clustered by hierarchical clustering? default is TRUE
#' @param dist_method character, distance method used by dist
#' @param clustering_method character, clustering method used by hclust 
#' @param main character, title of the heatmap
#' @param key character, title of the color legend
#' @param show_col_legend logical, should the column legend be shown at the end of the plot? default is TRUE
#' @param show_dendro logical, should dendrograms be shown? default is FALSE
#' @param lpad character, left padding. Normally white spaces before each line of the heatmap. Default is no white spaces ("")
#' @return a text-based heatmap directly in the terminal output using ANSI background styles. Useful when your X11 forwarding is broken or for quick exploratory analysis of small datasets. Setup is largely inspired by pheatmap.
#' @author Giuseppe D'Agostino and Alan O'Callaghan
#' @export

textHeatmap <-function(dat, 
    pal = colorRampPalette(c("red", "gray", "blue"))(24), 
    cluster_cols = TRUE, 
    cluster_rows = TRUE,
    dist_method = "euclidean",
    clustering_method = "complete",
    main = "Heatmap",
    key = "Key", 
    show_col_legend = TRUE,
    show_dendro = FALSE,
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

  ## Row dendrogram. Now calling textDendro instead of textDendrogram

  if (cluster_rows == TRUE & show_dendro == TRUE) {
    rd <- textDendro(
      clust = hr, 
      horiz = TRUE, 
      xfac =1,
       ylim = 25
    )
    crayonstrings <- cbind(crayonstrings, t(rd))
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

  ## Column dendrogram. Now calling textDendro instead of textDendrogram
  if (cluster_cols == TRUE & show_dendro == TRUE) {
    cat("", textDendro( 
      clust = hc, 
      xfac = 3,
      ylim = ncol(hmap)), 
    sep = ""
    ) 
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


textDendrogram <- function(hc, horiz = FALSE, yend = 20, lpad = "") {
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
    mat[, 1] <- paste(lpad, mat[, 1], sep = "")
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


#' Rotate a matrix counter-clockwise
#' @param x matrix to be rotated
#' @return a matrix rotated counter-clockwise
#' @author Matthew Lundburg in https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
#' @export

rotateccw <- function(x) apply(t(x), 2, rev)


#' Text-based heatmap
#' @param hc hierarchical clustering object return by hclust()
#' @param xfac numeric, sets the spacing used to rescale the x coordinates. Default is 2
#' @param ylim numeric, sets the maximum amount of y (dendrogram height) used to rescale. Default is 20
#' @param cluster_rows logical, should rows be clustered by hierarchical clustering? default is TRUE
#' @param horiz logical, is the dendrogram oriented as left to right? Default is FALSE
#' @param col character, R color name used to draw the dendrogram
#' @return a text-based dendrogram
#' @author Alan O'Callaghan and Giuseppe D'Agostino
#' @export

textDendro <- function(hc, 
             xfac = 2, 
             ylim = 20, 
             horiz = FALSE, 
             col = "white"){

#Get dendrogram and segments
d <- as.dendrogram(object = hc, 
          hang = -1
          )

ggd <- dendextend::as.ggdend(dend = d)

segs <- ggd$segments

#Simplify horizontal segments bypassing nodes

  #Sort them by starting y position
  segs_sorted <- segs[order(segs$y),]

  #Select horizontal segments
  segs_sorted_hor <- segs_sorted[segs_sorted$y == segs_sorted$yend,]

  #Join them 2 by 2 along the rows
  cjoin <- do.call(what = cbind, args = split(x = 1:nrow(segs_sorted_hor), 1:2))

  #Get start and end by skipping one line
  segs_hor_red <- cbind(apply(x = cjoin, 
              MARGIN = 1, 
              function(x) segs_sorted_hor[x,3][1]), 
              unique(segs_sorted_hor$y), 
              apply(x = cjoin, 
                MARGIN = 1, 
                function(x) segs_sorted_hor[x,3][2]), 
              unique(segs_sorted_hor$y))
#Vertical segments
segs_vert <- segs[segs$x == segs$xend,]

#All x coordinates for rescaling
all_x <- c(segs_hor_red[,1], 
      segs_hor_red[,3], 
      segs_vert[,1], 
      segs_vert[,3]
       )

#Name (makes subsetting easier)
names(all_x) <- c(paste0("hor_xst", 1:nrow(segs_hor_red)), 
  paste0("hor_xend", 1:nrow(segs_hor_red)),  
  paste0("vert_xst", 1:nrow(segs_vert)), 
  paste0("vert_xend", 1:nrow(segs_vert))
  )

#Rescale adding a multiplicative x factor
rescaled_x <- scales::rescale(all_x, to = c(1, max(all_x)*xfac))

#Same for y
all_y <- c(segs_hor_red[,2], 
      segs_hor_red[,4], 
      segs_vert[,2], 
      segs_vert[,4]
       )

names(all_y) <- c(paste("hor_yst", 1:nrow(segs_hor_red)), 
  paste0("hor_yend", 1:nrow(segs_hor_red)),  
  paste0("vert_yst", 1:nrow(segs_vert)), 
  paste0("vert_yend", 1:nrow(segs_vert))
  )

#y values are rescaled according to the ylim value (number of rows or columns in the terminaò)
rescaled_y <- scales::rescale(all_y, to = c(1, ylim))

#Final set of rounded, rescaled segments
segs_all_round <- round(
          as.data.frame(
            cbind(
            rescaled_x[grep(pattern = "_xst", x = names(rescaled_x))], 
            rescaled_y[grep(pattern = "_yst", x = names(rescaled_y))], 
            rescaled_x[grep(pattern = "_xend", x = names(rescaled_x))], 
            rescaled_y[grep(pattern = "_yend", x = names(rescaled_y))]
              )
            )
          )

#Initialize empty matrix 
mat <- matrix(
    NA,
    nrow = max(segs_all_round[,c(1,3)]), 
    ncol = ylim
    )

#Change orientation of characters according to orientation of dendrogram

if(horiz == FALSE) {

  char_across = "-"
  char_down = "|"

} else{

  char_across = "|"
  char_down = "-"
}

#Populate the matrix with properly oriented ASCII
for(i in seq_len(nrow(segs_all_round)))
{
  if(segs_all_round[i,2] == segs_all_round[i,4]){

    mat[segs_all_round[i,1]:segs_all_round[i,3],segs_all_round[i,2]] <- crayon::make_style(colors = col, bg = FALSE)(char_across)

  } else if(segs_all_round[i,1] == segs_all_round[i,3]){

    mat[segs_all_round[i,1], segs_all_round[i,2]:segs_all_round[i,4]] <- crayon::make_style(colors = col, bg = FALSE)(char_down)
  }
}

#Empty spaces
mat[is.na(mat)] <- " "

#Rotate matrix based on the orientation and add carriage return
if(horiz == FALSE) {
  mat <- rotateccw(x = mat) 
  mat[,ncol(mat)] <- paste0(mat[,ncol(mat)], "\n")
  mat <- t(mat)
}else{
  mat[,ncol(mat)] <- paste0(mat[,ncol(mat)], "\n")
  mat <- t(mat)
}

return(mat)
}
