




#' Create crayonmap object
#' @param file BMP, TIFF, JPG or PNG bitmap file
#' @param char the character to be displayed. Defaults to a double white space 
#' @param bg logical: should the colour be applied to the background or foreground? defaults to TRUE for background
#' @return a "crayonmap" class object to be used by paintCrayon()
#' @author Giuseppe D'Agostino
 
readCrayonMap <- function(file, char = "  ", bg = TRUE, parallel = TRUE)
{
	require(crayon)
	require(readbitmap)

	pic <- read.bitmap(file)

	rgbmat <- rgb(matrix(pic[1:nrow(pic), 1:ncol(pic),], nrow = nrow(pic) * ncol(pic)))

	crayonstrings = matrix(sapply(rgbmat, function(x) crayon::make_style(x, bg = bg)(char)), nrow = nrow(pic))

	crayonstrings[,ncol(crayonstrings)] <- paste(crayonstrings[,ncol(crayonstrings)], "\n", sep = "")

	crayonstrings[,1] <- paste("   ", crayonstrings[,1], sep = "")

	crayons <- as.vector(t(crayonstrings))

	cmap <- new("crayonmap", crayons = crayons, row.num = nrow(pic), col.num = ncol(pic))

	return(cmap)
}

#' Clear the terminal screen

spacer = function()
{
	cat(rep("\n", times = 60))
}

#' Animate crayonmap objects
#' @param cmaplist a list of crayonmap objects
#' @param delay numeric, delay between frames (in ms)
#' @param show_frame logical: should the number of the frame be shown in the upper right corner? For debbugging purposes only. Default is FALSE
#' @return animated sequence of crayonmap objects in the terminal. The animation code is borrowed by Brodie Gaslam at https://raw.githubusercontent.com/brodieG/fansi/development/extra/scrolling-banner.R 
#' @author Giuseppe D'Agostino
 

animateCrayon <- function(cmaplist, delay = 0.2, show_frame = FALSE)
{	
	frames <- cmaplist
	spacer()
	i = 1
	repeat{
		cat(sprintf("\033[%dA",cmaplist[[i]]@row.num))
		if(show_frame) frames[[i]]@crayons[1] <- crayon::bgBlack(i)
		cat(frames[[i]]@crayons, sep = "")
		i = i+1
		if(i > length(cmaplist)) i = 1
		Sys.sleep(delay)
	}
}



