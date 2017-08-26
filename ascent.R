require(dplyr)
require(roxygen2)

#' Clean Ascent CSV
#'
#' Reads Ascent csv, generates a data frame that is easy to manipulate.
#' @param batch_names List of batch names
#' @param inst Defaults to TRUE. If FALSE will not extract the instrument from the batch name
#' @param stream Defaults to TRUE. If FALSE will not extract the stream from the batch name
#' @param posix Defaults to TRUE. If FALSE will not convert AcqTime to type POSIXct
#' @param direct Defaults to FALSE. If TRUE will read batch_names input as file path, without adding folder/extension to
#' input string
#' @keywords ascent
#' @export
#' @examples
#' clean_ascent_csv(batch_names)


clean_ascent_csv <- function(batch_names, inst = TRUE, stream = TRUE,
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



# #' Clean Ascent CSV (from full batch paths as input)
# #'
# #' Reads Ascent csv, generates a data frame that is easy to manipulate.
# #' @param batch_names List of batch names
# #' @keywords ascent
# #' @export
# #' @examples
# #' clean_ascent_csv_direct(batch_names)
#
#
# clean_ascent_csv_direct <- function(batch_names, inst = TRUE, stream = TRUE,
#                              posix = TRUE, shiny = TRUE) {
#
#   #create a list of file paths from the batch names(csvs are stored in "data" folder)
#
#   if(shiny) {
#     batch_paths <- batch_names
#   } else {
#     batch_paths <- lapply(batch_names,
#                           function(x) paste("data/", x, "_alere_results.csv", sep = ""))
#   }
#   #Read in all the .csv files to "data" (creates a list of data frames for eachInstrument)
#
#   data <- lapply(batch_paths, read.csv, stringsAsFactors = FALSE)
#
#   #Combine the individual data frames into one data frame
#
#   data <- do.call("rbind", data)
#
#   data <- data %>%
#     select(Batch,
#            Index,
#            Sample = Sample.Name,
#            Type,
#            Nominal = Nominal.Conc.,
#            Analyte = Compound,
#            Response,
#            Conc = Calc..Conc.,
#            ISQuant = Chrom.1.Name,
#            Area = Chrom.1.Area,
#            AcqTime = Acquisition.Time)
#
#   data$Type <- factor(data$Type, levels = c("Standard", "Blank", "QC", "Unknown"))
#   data$Batch <- as.factor(data$Batch)
#   data$Analyte <- as.factor(data$Analyte)
#   data$ISQuant <- as.factor(data$ISQuant)
#
#   data$AcqTime <- gsub("T", " ", data$AcqTime)
#   data$AcqTime <- trimws(gsub("\\.000Z", "", data$AcqTime))
#   if(posix) {data$AcqTime <- as.POSIXct(data$AcqTime)}
#
#   if(stream){
#     data <- data %>%
#       mutate(Stream = ifelse(grepl("_A_", Batch), "Stream 1",
#                              ifelse(grepl("_B_", Batch), "Stream 2", "Stream Unknown")))
#
#     data$Stream <- as.factor(data$Stream)
#   }
#   if(inst) {
#     instruments <- c("McCartney", "Hendrix", "Cash", "Janis",
#                      "Willie", "Freddie", "Harrison", "Lennon",
#                      "Bono", "Ringo", "Corgan",
#                      "Jagger", "Morrison", "Axl",
#                      "Jones,", "Casablancas", "Jett", "Dylan",
#                      "Page", "Plant", "Berry", "Prince")
#
#     lapply(instruments, function(x) {
#
#       matched_rows <- grep(x, data$Batch, ignore.case = TRUE)
#       data[matched_rows, "Instrument"] <<- x
#
#     })
#
#     data$Instrument <- as.factor(data$Instrument)
#   }
#   data
#
# }


#' Read Ascent CSVs
#'
#' This function reads one or more Ascent csv files into a single data frame.
#' @param batch_names List of batch names.
#' @keywords ascent
#' @export
#' @examples
#' read_ascent_csv()
#'
library(dplyr)


read_ascent_csv <- function(batch_names)  {
  #create a list of file paths from the batch names(csvs are stored in "data" folder)

  batch_paths <- lapply(batch_names,
                        function(x) paste("data/", x, "_alere_results.csv", sep = ""))


  #Read in all the .csv files to "data" (creates a list of data frames for eachInstrument)

  data <- lapply(batch_paths, read.csv, stringsAsFactors = FALSE)

  #Combine the individual data frames into one data frame

  do.call("rbind", data)

}

#' Clean Ascent Columns
#'
#' This function takes a data frame generated from Ascent csv files and
#' selects only the relevant columns, and renames the columns so that they
#' are easier to access. Removes extraneous characters from the "Time" column.
#' Coerces various columns to factors.
#' @param data Data frame of Ascent data generated from read_ascent_csv()
#' with all columns present and column names unmodified.
#' @param posix Optionally converts AcqTime to POSIXct. Default is FALSE.
#' @keywords ascent
#' @export
#' @examples
#' clean_ascent_cols()

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

#' Add Stream Column
#'
#' This function adds a "Stream" column to an existing Ascent data frame.
#' This column is a factor with two levels, "Stream 1" and "Stream 2".
#' "Stream 1" is assigned if the batch name contains "_A_", and "Stream 2"
#' is assigned if the batch name contains "_B_".
#' @param data Data frame of Ascent data, generated from read_ascent_csv()
#' @keywords ascent
#' @export
#' @examples
#' add_stream_col(data)


add_stream_col <- function(data) {

  data <- data %>%
    mutate(Stream = ifelse(grepl("_A_", Batch), "Stream 1",
                           ifelse(grepl("_B_", Batch), "Stream 2", "Stream Unknown")))

  data$Stream <- as.factor(data$Stream)

  data
}

#' Add Instrument Column
#'
#' Adds an "Instrument" column to a data frame resulting from read_ascent_csv().
#' @param data Data frame of Ascent data, generated from read_ascent_csv()
#' @keywords ascent
#' @details Creates a new column called "Instrument" which contains
#' a match found between "Batch" and a pre-defined list of instruments.
#' @export
#' @examples
#' add_inst_col(data)


add_inst_col <- function(data) {

  instruments <- c("McCartney", "Hendrix", "Cash", "Janis",
                  "Willie", "Freddie", "Harrison", "Lennon",
                  "Bono", "Ringo", "Corgan",
                  "Jagger", "Morrison", "Axl",
                  "Jones,", "Casablancas", "Jett", "Dylan",
                  "Page", "Plant", "Berry", "Prince")

  lapply(instruments, function(x) {

    matched_rows <- grep(x, data$Batch)
    data[matched_rows, "Instrument"] <<- x

  })

  # data <- data %>%
  #   mutate(Instrument = word(as.character(Batch), -1, sep = "_"))

  data$Instrument <- as.factor(data$Instrument)

  data

}


#' Plot Internal Standard
#'
#' Generates a plot of IS peak area vs acquisition time.
#' @param data Data frame of Ascent data, generated from read_ascent_csv(), cleaned with
#' clean_ascent_cols(posix = TRUE)
#' @keywords ascent
#' @export
#' @examples
#' plot_IS(data)


plot_IS <- function(data) {
  data %>%
    filter(ISQuant == "IS") %>%
    ggplot(aes(x = AcqTime, y = Area)) +
    geom_point(aes(col = Type)) +
    theme(axis.text.x = element_blank()) +
    labs(x = "Acquisition Time", y = "IS Peak Area") +
    facet_wrap(~Analyte)
}

#' Identify Outliers
#'
#' Updates the "Include" column of an Ascent data frame with FALSE if the point is an outlier
#' Points are marked as outliers if they are less than Q1 - 1.5 * IQR or greater than Q3 + 1.5*IQR
#' @param data Data frame processed by clean_ascent_csv()
#' @keywords ascent
#' @export
#' @examples
#' evaluate_outliers(data)

evaluate_outliers <- function(data) {
  data <- data %>%
    group_by(Analyte, Sample) %>%
    mutate(Q1 = quantile(Conc, 0.25),
           Q3 = quantile(Conc, 0.75),
           IQR = IQR(Conc),
           min_out = Q1 - 1.5*IQR,
           max_out = Q3 + 1.5*IQR) %>%
    ungroup()

  data$Outlier <- ifelse(data$Conc < data$min_out | data$Conc > data$max_out, TRUE, FALSE)

  data <- data %>%
    select(-c(Q1, Q3, IQR, min_out, max_out))

  return(data)
}

#' Create Summary Table of QC Verification Data
#'
#' Creates a summary table including Mean, SD, %CV, %Dev from Target, and n for each QC level for each analyte.
#' @param data Data frame processed by clean_ascent_csv() and evaluate_outliers()
#' @param exclude_outliers defaults to TRUE. Will not include data in summary if data$Outlier is TRUE (as determined by IQR)
#' @keywords ascent
#' @export
#' @examples
#' summarize_QC(data)

summarize_QC <- function(data, exclude_outliers = TRUE) {

  if(exclude_outliers) {
    use_data <- data %>%
      filter(!Outlier)
  } else
    use_data <- data

  summaryTable <- use_data %>%
  group_by(Analyte, Sample, Target) %>%
  summarize("Mean" = mean_round(Conc),
            "SD" = sd_round(Conc),
            "%CV" = cv_round(Conc),
            "%Dev" = acc(Conc, Target),
            "n" = n()) %>%
  ungroup()

  return(summaryTable)
}


#' Create Summary Table of Calibrator Verification Data
#'
#' Creates a summary table including Mean, SD, %CV, %Dev from Target, and n for each calibrator level for each analyte.
#' @param data Data frame processed by clean_ascent_csv() and evaluate_outliers()
#' @param exclude_outliers defaults to TRUE. Will not include data in summary if data$Outlier is TRUE (as determined by IQR)
#' @keywords ascent
#' @export
#' @examples
#' summarize_cals(data)

summarize_cals <- function(data, exclude_outliers = TRUE) {

  if(exclude_outliers) {
    use_data <- data %>%
      filter(!Outlier)
  } else
    use_data <- data

  summaryTable <- use_data %>%
  group_by(Analyte, Sample, Target) %>%
    summarize("Mean" = mean_round(Conc),
              "SD" = sd_round(Conc),
              "%CV" = cv_round(Conc),
              "%Dev" = acc(Conc, Target),
              "n" = n()) %>%
    ungroup()

  return(summaryTable)
}




#' Testing
#'
#' Creates a summary table including Mean, SD, %CV, %Dev from Target, and n for each calibrator level for each analyte.
#' @param data Data frame processed by clean_ascent_csv() and evaluate_outliers()
#' @param exclude_outliers defaults to TRUE. Will not include data in summary if data$Outlier is TRUE (as determined by IQR)
#' @keywords ascent
#' @export
#' @examples
#' testdocument(data)

testdocument <- function(data, exclude_outliers = TRUE) {

  if(exclude_outliers) {
    use_data <- data %>%
      filter(!Outlier)
  } else
    use_data <- data

  summaryTable <- use_data %>%
    group_by(Analyte, Sample, Target) %>%
    summarize("Mean" = mean_round(Conc),
              "SD" = sd_round(Conc),
              "%CV" = cv_round(Conc),
              "%Dev" = acc(Conc, Target),
              "n" = n()) %>%
    ungroup()

  return(summaryTable)
}




