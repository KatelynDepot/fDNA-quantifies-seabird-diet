
# color palette from c(viridis::viridis(n = 17, option = "H"))
combined_palette <- c("Copepod" = "#30123BFF",
                      "Jellyfish" = "#4040A2FF",
                      "Invert" = "#466BE3FF",
                      "Capelin" = "#4293FFFF",
                      "Eulachon" = "#28BBECFF",
                      "Pollock" = "#18DCC3FF",
                      "Gadid" = "#25E7AEFF", # combined Pollock/Tomcod
                      "Tomcod" = "#31F299FF",
                      "Greenling" = "#6BFE64FF",
                      "Herring" = "#A2FC3CFF",
                      "Lingcod" = "#CCED34FF",
                      "Myctophid" = "#EDD03AFF",
                      "Prowfish" = "#FDAD35FF",
                      "Sablefish" = "#FB8022FF",
                      "Salmon" = "#EC520FFF",
                      "Sand lance" = "#D23105FF",
                      "Stichaeid" = "#AC1701FF",
                      "Other fish" = "#7A0403FF")

lightened_palette <- colorspace::lighten(combined_palette, 0.5)

combined_pattern <- c("Copepod" = "none",
                      "Jellyfish" = "stripe",
                      "Invert" = "none",
                      "Capelin" = "circle",
                      "Eulachon" = "none",
                      "Pollock" = "stripe",
                      "Gadid" = "none",
                      "Tomcod" = "circle",
                      "Greenling" = "none",
                      "Herring" = "stripe",
                      "Lingcod" = "none",
                      "Myctophid" = "circle",
                      "Prowfish" = "none",
                      "Sablefish" = "stripe",
                      "Salmon" = "none",
                      "Sand lance" = "circle",
                      "Stichaeid" = "none",
                      "Other fish" = "stripe")

combined_pattern_angle <- c("Copepod" = 0, #none
                      "Jellyfish" = 45, #stripe
                      "Invert" = 0, #none
                      "Capelin" = 45, #circle
                      "Eulachon" = 0, #none
                      "Pollock" = 135, #stripe
                      "Gadid" = 0, #none
                      "Tomcod" = 45, #circle
                      "Greenling" = 0, #none
                      "Herring" = 90, #stripe
                      "Lingcod" = 0, #none
                      "Myctophid" = 45, #circle
                      "Prowfish" = 0, #none
                      "Sablefish" = 45, #stripe
                      "Salmon" = 0, #none
                      "Sand lance" = 45, #circle
                      "Stichaeid" = 0, #none
                      "Other fish" = 135) #stripe
