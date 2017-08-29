
#' Clean Ascent CSVs
#'
#' Reads Ascent csvs, generates a data frame that is easy to manipulate.
#' @param batch_names List of batch names
#' @param inst Defaults to TRUE. If FALSE will not extract the instrument from the batch name
#' @param stream Defaults to TRUE. If FALSE will not extract the stream from the batch name
#' @param posix Defaults to TRUE. If FALSE will not convert AcqTime to type POSIXct
#' @param direct Defaults to FALSE. If TRUE will read batch_names input as file path, without adding folder/extension to
#' input string
#' @keywords ascent
#' @export
#' @examples
#' clean_ascent_csvs(batch_names)


clean_ascent_csvs <- function(batch_names, inst = TRUE, stream = TRUE,
                             posix = TRUE, direct = FALSE) {

  #create a list of file paths from the batch names(csvs are stored in "data" folder)

  if(shiny) {
    batch_paths <- batch_names
  } else {
    batch_paths <- lapply(batch_names,
                          function(x) paste("data/", x, "_alere_results.csv", sep = ""))
  }
  #Read in all the .csv files to "data" (creates a list of data frames for eachInstrument)

  data <- lapply(batch_paths, read.csv, stringsAsFactors = FALSE)

  #Combine the individual data frames into one data frame

  data <- do.call("rbind", data)

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

  if(stream){
    data <- data %>%
      mutate(Stream = ifelse(grepl("_A_", Batch), "Stream 1",
                             ifelse(grepl("_B_", Batch), "Stream 2", "Stream Unknown")))

    data$Stream <- as.factor(data$Stream)
  }
  if(inst) {
    instruments <- c("McCartney", "Hendrix", "Cash", "Janis",
                     "Willie", "Freddie", "Harrison", "Lennon",
                     "Bono", "Ringo", "Corgan",
                     "Jagger", "Morrison", "Axl",
                     "Jones,", "Casablancas", "Jett", "Dylan",
                     "Page", "Plant", "Berry", "Prince")

    lapply(instruments, function(x) {

      matched_rows <- grep(x, data$Batch, ignore.case = TRUE)
      data[matched_rows, "Instrument"] <<- x

    })

    data$Instrument <- as.factor(data$Instrument)
  }
  data

}
