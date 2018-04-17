

###########################################################

## R Tools to use SimulO output Data

## E. Leymarie

####################### readSimulO
#' readSimulO : read SimulO result file
#'
#' @description
#' readSimulO : read SimulO result file
#'
#' @param filename filename of the file to open
#'
#' @return list including  all data
#' @export
#'

readSimulO <- function(filename) {
  cat("open:", filename, "\n")
  dataSimulO <- scan(filename, what = character(0), sep = "\n")

  SimulO <- list()

  ind <- grep("^Photons_count=", dataSimulO)
  indObjects <- grep("^Compteurs Primitive :", dataSimulO)
  indSources <- grep("^Source Primitive :", dataSimulO)

  ### general
  for (i in 2:(ind - 1)) {
    temp <- strsplit(dataSimulO[i], split = "=")[[1]]
    SimulO[[temp[1]]] <- temp[2]
  }
  for (i in ind:(indObjects[1] - 1)) {
    temp <- strsplit(dataSimulO[i], split = "=")[[1]]
    SimulO[[temp[1]]] <- as.numeric(temp[2])
  }

  ### Compteur Primitive
  SimulO$objects <- list()
  for (i in indObjects) {
    primname <- strsplit(dataSimulO[i], split = " : ")[[1]][2]
    # ind<-grep(primname,dataSimulO)
    ind <- which(stringi::stri_detect(dataSimulO, regex = primname))
    ind <- ind[(ind > i) & (ind < min(indSources))]
    SimulO$objects[[primname]] <- list()
    for (j in ind) {
      temp <- strsplit(dataSimulO[j], split = "=")[[1]]
      SimulO$objects[[primname]][[temp[1]]] <- as.numeric(temp[2])
    }
  }

  ### Compteur source
  SimulO$sources <- list()
  for (i in indSources) {
    primname <- strsplit(dataSimulO[i], split = " : ")[[1]][2]
    ind <- grep(paste("^", primname, sep = ""), dataSimulO)
    ind <- ind[(ind > i)]
    SimulO$sources[[primname]] <- list()
    for (j in ind) {
      temp <- strsplit(dataSimulO[j], split = "=")[[1]]
      SimulO$sources[[primname]][[temp[1]]] <- as.numeric(temp[2])
    }
  }

  ### DataPrimitive
  indPrim <- grep(paste("^", "Primitive :", sep = ""), dataSimulO)
  if (length(indPrim) > 0) {
    SimulO$detectors <- list()
    for (i in indPrim) {
      primname <- strsplit(dataSimulO[i], split = " : ")[[1]][2]
      primname <- paste(primname, strsplit(dataSimulO[i + 1], split = " : ")[[1]][2], sep = "_")
      SimulO$detectors[[primname]] <- list()
      NbX <- as.numeric(strsplit(dataSimulO[i + 2], split = " : ")[[1]][2])
      NbY <- as.numeric(strsplit(dataSimulO[i + 3], split = " : ")[[1]][2])
      data <- dataSimulO[(i + 5):(i + 4 + NbX)]
      # data<-unlist(strsplit(data,split="\t"))
      data <- unlist(stringi::stri_split(data, regex = "\\\t", omit_empty = TRUE))
      data <- matrix(as.numeric(data), ncol = NbY, byrow = T)
      SimulO$detectors[[primname]] <- data
    }
  }

  ### Script
  indScript <- grep(paste("^", "SimulOVersion=", sep = ""), dataSimulO)
  SimulO$script <- dataSimulO[-(1:(indScript - 1))]

  return(SimulO)
}

####################### MergeSimulO
#' MergeSimulO : Merge SimulO result files
#'
#' @description
#' MergeSimulO : Merge SimulO result files created by several simulations and/or CPU
#'
#' @param fileslist vector of file to open
#' @param TestConfig Flag to merge data only if the configuration script is the same.
#'
#' @return list including  all data
#' @export
#'

MergeSimulO <- function(fileslist, TestConfig=TRUE) {
  MergedData <- readSimulO(fileslist[1])
  MergedData$fileslist <- fileslist[1]
  cat("NPhoton=", MergedData$Photons_count, "\n")

  ## Annulation des metadata sources car difficile a additionner
  for (i in (1:length(MergedData$sources))) {
    MergedData$sources[[i]] <- MergedData$sources[[i]][1]
  }

  ## Merge
  if (length(fileslist) > 1) {
    for (filename in fileslist[-1]) {
      NewData <- readSimulO(filename)

      ## verification que NewData est bieb la meme simul que MergedData
      if (TestConfig) {
        TestConfig.flag <- all(MergedData$script == NewData$script)
      }
      else {
        TestConfig.flag <- TRUE
      }

      if (TestConfig.flag) {
        ## Merging
        cat("Add NPhoton=", NewData$Photons_count, "\n")

        MergedData$fileslist <- c(MergedData$fileslist, filename)

        ### general moyennage des resultat moyen ("Mean_time(ms)","Average_pathLength","Mean_ScattEventCount")
        for (variable in c("Mean_time(ms)", "Average_pathLength", "Mean_ScattEventCount")) {
          MergedData[[i]] <- (MergedData$Photons_count * MergedData[[variable]] + NewData$Photons_count * NewData[[variable]]) / (MergedData$Photons_count + NewData$Photons_count)
        }

        ### general somme simple
        for (variable in c(
          "Photons_count", "Total_time(s)", "Photon_count_with_0_ScattEvent", "Photon_count_with_1_ScattEvent", "Photon_count_with_2_ScattEvent"
          , "Photon_count_with_3_ScattEvent", "Photon_count_withMore_3_ScattEvent"
        )) {
          MergedData[[variable]] <- MergedData[[variable]] + NewData[[variable]]
        }

        ## Merge objects
        for (i in (1:length(MergedData$objects))) {
          for (j in (1:length(MergedData$objects[[i]]))) {
            MergedData$objects[[i]][[j]] <- MergedData$objects[[i]][[j]] + NewData$objects[[i]][[j]]
          }
        }

        ## Merge source
        for (i in (1:length(MergedData$sources))) {
          MergedData$sources[[i]][[1]] <- MergedData$sources[[i]][[1]] + NewData$sources[[i]][[1]]
        }

        ## Merge detector
        for (i in (1:length(MergedData$detectors))) {
          MergedData$detectors[[i]] <- MergedData$detectors[[i]] + NewData$detectors[[i]]
        }
      }
      else {
        warning(filename, " is not the same configuration")
      }
    } # for (filename in fileslist[-1]){
  } # if (length(fileslist)>1){

  return(MergedData)
}
