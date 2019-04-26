

#Set the crayonmap class. Still not very useful but, since the crayons slot is a vector, it is important to memorize the number of rows and columns for future applications
setClass("crayonmap", representation(crayons = "character", row.num = "numeric", col.num = "numeric"))


#' Color key
#' @param values vector of numeric values to which colors will be mapped
#' @param pal color palette 
#' @param scale logical, should values be scaled? Defaults to TRUE
#' @return a vector of colors, mapped to the values and in the same order as the values vector
#' @author Giuseppe D'Agostino

colorKey <- function(values,
		     pal = colorRampPalette(c("red", "gray", "blue"))(24), 
		     scale = TRUE)
{
    require(pheatmap)

    if(scale == TRUE) values_sc <- scale(values) else values_sc <- values
    bks <- pheatmap:::generate_breaks(values_sc, length(pal), center = F)
    cols <- pheatmap:::scale_colours(values_sc, col=pal, breaks=bks, na_col = "gray")
    cols <- as.character(cols)
    return(cols)
} 

#' Text-based bitmap. Not useful until I add functionalities
#' @param cmap object of class "crayonmap"
#' @return a text-based bitmap directly in the terminal output. 
#' @author Giuseppe D'Agostino

paintCrayon <- function(cmap)
{	
	cat(cmap@crayons, sep = "")
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
#' @author Giuseppe D'Agostino

textHeatmap <-function(dat, 
	pal = colorRampPalette(c("red", "gray", "blue"))(24), 
	cluster_cols = TRUE, 
	cluster_rows = TRUE,
	dist_method = "euclidean",
	clustering_method = "complete",
	main = "Heatmap",
	 key = "Key", 
	 show_col_legend = TRUE)
{

require(crayon)

#Clustering
if(cluster_rows == TRUE){ 
	hr = hclust(                #Hierarchical clustering of the rows
        dist(dat, method = dist_method), 
        method = clustering_method
                )
} else {
	hr = list("order" = 1:nrow(dat))
}

if(cluster_cols == TRUE){
 hc = hclust(
 	dist(t(dat), method = dist_method),  #Hierarchical clustering of columns (same as rows but transposing the matrix)
        method = clustering_method)
} else {
	hc = list("order" = 1:ncol(dat))
}

dat = dat[hr$order, hc$order]

#Colour rendering and crayonmap object
crayonstrings <- matrix(unlist(sapply(colorKey(as.vector(dat), pal = pal), function(x) crayon::make_style(x, bg = T)("   "))), nrow = nrow(dat))

crayonstrings[,ncol(crayonstrings)] <- paste(crayonstrings[,ncol(crayonstrings)], crayon::reset("   "), rownames(dat), "\n", sep = "")

	crayonstrings[,1] <- paste("   ", crayonstrings[,1], sep = "")

	crayons <- as.vector(t(crayonstrings))

	cmap <- new("crayonmap", crayons = crayons, row.num = nrow(dat), col.num = ncol(dat))

# Column names

	#padding
cat("\n \n")
cat("   ")

for(k in 1:ncol(dat)){
	if(nchar(k) == 1) cat(paste(" ",k," ", sep = ""))
		else if(nchar(k) == 2) cat(paste(k, " ",sep = ""))
}
cat("\n")

#Plot the heatmap
paintCrayon(cmap)

#Color Key

	#padding
cat("\n \n") 
cat("   ")
ckey <- unique(unlist(sapply(pal, function(x) crayon::make_style(x, bg = TRUE)("   "))))
mid.value = round(abs((max(dat))-min(abs(dat)))/2)
cat("Key", "\n", "   ", round(abs(min(dat))), rep("   ", length(ckey)/2), mid.value,  rep("   ", (length(ckey)/2)-1), " ", round(abs(max(dat))), "\n", "   ", ckey, "\n\n", sep = "")

#Column legend
if(show_col_legend == TRUE){
		cat("   ")
		cat("Column legend:\n")
	for(i in 1:ncol(dat)) cat(paste("   ", i, ":", colnames(dat)[i], "\n", sep = ""))
	}
}


#' Create crayonmap object
#' @param file BMP, TIFF, JPG or PNG bitmap file
#' @param parallel logical, enables multithreading for the ANSI conversion using parallel::detectCores() - 2 cores. Requires parallel. Default is TRUE
#' @return a "crayonmap" class object to be used by paintCrayon()
#' @author Giuseppe D'Agostino
 
readCrayonMap <- function(file, parallel = TRUE)
{
	require(crayon)
	require(readbitmap)

	pic <- read.bitmap(file)

	rgbmat <- rgb(matrix(pic[1:nrow(pic), 1:ncol(pic),], nrow = nrow(pic) * ncol(pic)))

	if (parallel == TRUE ){

		require(parallel)

		threads = parallel::makeCluster(parallel::detectCores()-2)
		crayonstrings = matrix(unlist(parallel::parLapply(rgbmat,  cl = threads , fun=function(x) crayon::make_style(x, bg = T)("  "))), nrow = nrow(pic))
		parallel::stopCluster(threads)

	} else {

		crayonstrings = matrix(sapply(rgbmat, function(x) crayon::make_style(x, bg = T)("  ")), nrow = nrow(pic))
	}

	crayonstrings[,ncol(crayonstrings)] <- paste(crayonstrings[,ncol(crayonstrings)], "\n", sep = "")

	crayonstrings[,1] <- paste("   ", crayonstrings[,1], sep = "")

	crayons <- as.vector(t(crayonstrings))

	cmap <- new("crayonmap", crayons = crayons, row.num = nrow(pic), col.num = ncol(pic))

	return(cmap)
}



