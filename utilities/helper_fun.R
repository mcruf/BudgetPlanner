
#################################
#                               #
#        Helper functions       #
#                               #
#################################

# Developed by: Marie-Christine Rufener
# For queries, feedback, and bugs, contact: < macrufener@gmail.com >



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# App default color palette
#>-----------------------------
# We need to create a named list where the names are the names 
# of our colour palettes. Each entry in the list is a vector of 
# the colours in that palette.

# Helpful website:
# https://www.colorhexa.com/bc4749




# Define default colors
#~~~~~~~~~~~~~~~~~~~~~~~
## main & dark version

my_colors <- list(main = c("#203a44",
                           #"#264653",
                           "#2a9d8f",
                           "#8ab17d",
                           "#e9c46a",
                           "#EFD595",
                           "#efb366",
                           "#f4a261",
                           "#e76f51",
                           "#e97c61",
                           "#bc4749"),
                  
                  dark = c("#1a2f37",
                           #"#203b46",
                           "#329589",
                           "#7faa71",
                           "#e6bd59",
                           "#ecce84",
                           "#edaa54",
                           "#f3974f",
                           "#e56140",
                           "#e76e50",
                           "#af4042")
                  )


# Generate the palettes
#~~~~~~~~~~~~~~~~~~~~~~
# Function that generates an actual colour 
# palette from the simple list of colours. 

## Function items:
# - name of the colour palette we want to use,
# - list of colour palettes we want to extract our choice from,
# - how many colours from it we want to use
# - discrete or continuous colour palette


my_palettes <- function(name, 
                        n, 
                        all_palettes = my_colors, 
                        type =c("discrete", "continuous")){
  
  palette = all_palettes[[name]]
  
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}



# Adapt color function to be used in ggplot
## A function for each type is needed (continuous/discrete)

### Discrete scale
scale_colour_my_color_d = function(name) {
  ggplot2::scale_colour_manual(values = my_palettes(name,
                                                    type = "discrete"))
}

scale_fill_my_color_d = function(name) {
  ggplot2::scale_fill_manual(values = my_palettes(name,
                                                   type = "discrete"))
}


### Continuous scale
scale_colour_my_color_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = my_palettes(name = name,
                                                         type = "continuous"))
}


scale_fill_my_color_c = function(name) {
  ggplot2::scale_fill_gradientn(colours = my_palettes(name = name,
                                                       type = "continuous"))
}
