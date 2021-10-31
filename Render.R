channelID <- unique(automationdata$data_channel)
output_file <- paste0(channelID, ".md")
params = lapply(channelID, FUN = function(x){list(data_channel = x)})
reports <- tibble(output_file, params)
library(rmarkdown)
apply(reports, MARGIN = 1, 
            FUN = function(x){
                render(input = "Project02.Rmd", output_file = x[[1]], params = x[[2]])
                })
