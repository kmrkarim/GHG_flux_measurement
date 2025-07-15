
# INSTALL AND LOAD NECESSARY LIBRARIES *****************************************
install.packages("BiocManager")
BiocManager::install("EBImage")


library(EBImage)
library(gridExtra)
library(grid)
library(ggplot2)
# ******************************************************************************



# DEFINE THE LEAF AREA CALCULATION FUNCTION ************************************

# Explanation of the input variables============================================

# `imagePath`: String that specifies the location (path) of the image file that
# you want to process. The image should be in a format that can be read by the
# `readImage` function from the `EBImage` package, such as a JPEG file.


# `threshold`: It is a numeric value that sets the cutoff point for distinguishing
# the leaf pixels from the background pixels based on their intensity values.
# This value can range from 0 (black) to 1 (white). Pixels with intensity values
# less than the threshold are considered to be part of the leaf, while those
# with values greater than or equal to the threshold are considered to be part
# of the background. The default value is 0.5, but you may need to adjust this
# depending on the specific characteristics of your images.
# VERY IMPORTANT: The thresholding is done on the inverse of the blue channel.
# The function imageData(channel(img, mode = "blue")) extracts the blue channel
# from the original image. Since the leaves are green and the background is
# usually brighter (often white), the blue channel helps to highlight the leaves
# against the background.
# However, in the blue channel of an RGB image, the intensity values of the blue
# areas are higher and those of the non-blue areas (like green leaves) are lower.
# To make the leaves stand out more in the image (i.e., have higher pixel
# values), we use `img.data <- 1 - img.data` to invert the image. After
# inversion, the areas that were originally blue (background) will now have
# lower pixel values, and the areas that were green (the leaves) will now have
# higher pixel values.
# Then, the thresholding is applied on this inverted blue channel. By choosing
# an appropriate threshold, we can distinguish between the pixel values of the
# leaves and the background, enabling the extraction of the leaf area.


# `imageDimension_xy_mm`: This is a numeric vector of length two that specifies
# the actual size of the image in millimeters. The first element of the vector
# is the width (x dimension) of the image, and the second element is the height
# (y dimension). This is used to convert the pixel area of the leaf into actual
# area in square centimeters. The default value is c(50, 50), meaning the image
# is assumed to be 50mm by 50mm in size if no other size is specified.

# End of explanation of input variables ========================================


calculateLeafArea <-
  function(imagePath,
           threshold = 0.5,
           imageDimension_xy_mm = c(50, 50)
           ) {

    # Read image from the specified path
    img <- readImage(imagePath)

    # Extract the blue channel from the image ==================================
    # Blue channel helps because:
    # i) This channel tends to have lower values for green pixels than the
    # green channel. So, if you're trying to threshold to separate green (leaf)
    # from not-green (background), the lower values in the blue channel can help
    # make that distinction clearer.
    # ii) This channel often carries more noise than the green channel,
    # which makes the leaf edges more pronounced and can help in some processing
    # steps.
    # ==========================================================================
    img.data <- imageData(channel(img, mode = "blue"))

    # Reverse the image color (white to black and vice versa)
    img.data <- 1 - img.data

    # Prepare a dataframe for histogram plotting
    df <- data.frame(Value = c(img.data))

    # Create a histogram plot of pixel values
    p1 <- ggplot(df, aes(x = Value)) +
      geom_histogram(binwidth = 0.01,
                     fill = "blue",
                     color = "black") +
      geom_vline(aes(xintercept = threshold),
                 color = "red",
                 linetype = "dashed") +
      ggtitle("Histogram of Pixel Values") +
      xlab("Pixel Value") +
      ylab("Frequency")

    # Thresholding operation on the image
    # Pixels having value less than the threshold are set to 0 (black)
    # Pixels having value greater or equal to the threshold are set to 1 (white)
    img.data[img.data < threshold] <- 0
    img.data[img.data >= threshold] <- 1

    # Label the connected regions in the image
    img.lab <- bwlabel(img.data)

    # Fill the holes in the labelled image
    img.lab <- fillHull(img.lab)

    # Create a disc-shaped brush for the erosion operation
    kern <- makeBrush(size = 3,
                      shape = "disc",
                      step = F)

    # Erode the image to remove noise
    img.lab <- erode(img.lab, kern)

    # Compute the shape features of the image
    # s.area gives the area of the object in the image (in pixels)
    img.shape <- computeFeatures.shape(img.lab)
    leaf.area <- img.shape[, "s.area"]

    # Convert the pixel area to cm^2
    # First compute the area of a pixel by dividing the total image area by the
    # number of pixels
    pixelSize <- prod(imageDimension_xy_mm) / length(img.data)
    # Multiply the pixel area of the leaf by the area of a pixel to get the
    # total leaf area in mm^2
    leaf.area <- leaf.area * pixelSize
    # Convert the total leaf area from mm^2 to cm^2
    leaf.area <- round(leaf.area / 100, 3)

    # Paint the detected leaf object in red color on the original image
    img.out <-
      paintObjects(img.lab,
                   img,
                   opac = c(NA, 0.45),
                   col = c(NA, "red"))

    # Convert the image data to a raster grob for plotting
    pic_out_data <- imageData(img.out)
    p2 <- rasterGrob(pic_out_data)

    # Combine the histogram and image in a grid layout
    # The first plot (histogram) has a height of 1 and the second plot (leaf
    # image) has a height of 2
    grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))

    # Return the total leaf area in cm^2
    return(paste0("Leaf area: ", leaf.area, " cm^2"))
  }


calculateLeafArea("/Users/ronysmac/Downloads/WhatsApp Image 2023-07-24 at 1.18.41 AM.jpeg")
