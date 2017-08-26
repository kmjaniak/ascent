library(dplyr)

#' Read Ascent CSVs
#'
#' This function reads one or more Ascent csv files into a single data frame.
#' @param batch_names List of batch names.
#' @keywords
#' @export
#' @examples
#' read_ascent_csv()



read_ascent_csv <- function(batch_names)  {
  #create a list of file paths from the batch names(csvs are stored in "data" folder)

  batch_paths <- lapply(batch_names,
                        function(x) paste("data/", x, "_alere_results.csv", sep = ""))


  #Read in all the .csv files to "data" (creates a list of data frames for each stream)

  data <- lapply(batch_paths, read.csv, stringsAsFactors = FALSE)

  #Combine the individual data frames into one data frame

  do.call("rbind", data)

}

clean_ascent_cols <- function(data, posix = FALSE) {
  data <- data %>%
    select(Batch,
           Index,
           Sample = Sample.Name,
           Type,
           Nominal = Nominal.Conc.,
           Analyte = Compound,
           Response,
           Conc = Calc..Conc.,
           ISQuant = Chrom.1.Name,
           Area = Chrom.1.Area,
           AcqTime = Acquisition.Time)

  data$Type <- factor(data$Type, levels = c("Standard", "Blank", "QC", "Unknown"))
  data$Batch <- as.factor(data$Batch)
  data$Analyte <- as.factor(data$Analyte)
  data$ISQuant <- as.factor(data$ISQuant)

  data$AcqTime <- gsub("T", " ", data$AcqTime)
  data$AcqTime <- trimws(gsub("\\.000Z", "", data$AcqTime))
  if(posix) {data$AcqTime <- as.POSIXct(data$AcqTime)}

  data
}

remove_standards <- function(data) {
  data %>%
    filter(!Type == "Standard")
}

add_stream_col <- function(data) {

  data <- data %>%
    mutate(Stream = ifelse(grepl("_A_", Batch), "Stream 1",
                           ifelse(grepl("_B_", Batch), "Stream 2", "Stream Unknown")))

  data$Stream <- as.factor(data$Stream)

  data
}
