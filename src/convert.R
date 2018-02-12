
convert_one_file <- function(filename, year) {
    	result <- list()
	lines <- readLines(filename, encoding = "UTF-8")

	first <- T
	data <- ""

	for (ALine in lines) {
		if (length(grep("traekning-row2\"", ALine))) {
			if (first) { 
				first <- F
				next
			}
			else {
				result <- c(result, data)
			}
			data <- ""
			next
		}
		if (length(grep("traekning-col", ALine))) {
		    	if (length(grep("head", ALine))) next
			ALine <- sub(".*?>","",ALine)
			ALine <- sub("<.*","",ALine)
			ALine <- gsub(","," ",ALine)
			if (data == "") {
			    data <- paste0(year)
			} else {
			    data <- paste0(data, ",", ALine)
			}
			next
		}
	}
	result <- c(result, data)
	result
}

raw_path <- "../raw/"
filenames <- dir(path = raw_path, pattern = "onsdag", full.names = T)

output <- list()
#output <- c(output,'år\tuge\tlottotal\ttillægstal\t6rigtige\t5+rigtige\t5rigtige\t4rigtige\t3rigtige')
output <- c(output,'år,uge,lottotal,tillægstal,6rigtige,5+rigtige,5rigtige,4rigtige,3rigtige')
for (i in 1:length(filenames)) {
    year <- sub(".*20","20", filenames[i])
    year <- sub("\\.php","", year)
    returnCode <-  tryCatch({
	output <- c(output,convert_one_file(filenames[i], year))
    }, error = function(e) {
	cat(paste0(filenames[i]," failed:\n",e,"\n"))
    })
}


lotto_csv <- file("onsdagslotto.csv", "w+")
for (i in seq(output)) {
    cat(paste0(output[[i]],"\n"), file = lotto_csv)
}
close(lotto_csv)

