## ---------------------------
## Script name: report_functions_internal.R
##
## Purpose of script: Does as it says on the tin.
##                    Various functions needed for running
##                    forecast_script_internal.Rmd
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021-11-03
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
## Notes: This script is made public as
##        a companion piece for practical
##        implementation of methods explored
##        in a publication.
##
## ---------------------------

read_DW <- function(INPUT, PROD) {
  # Reads DW data from INPUT

  # argument validity is checked on upper level!

  # ---
  # data reading and compilation ----
  # ---
  filenames <- list.files(path = INPUT, pattern = "DWSALES_*")  # Character vector of file names

  file_list <- list()
  for (i in 1:length(filenames)) {
  filename <- filenames[i] # get filename
  file_content <- fread(paste0(INPUT, filename), colClasses = c("Date", "numeric", "factor", "character", "character", "NULL")) # read file
  file_list[[i]] <- file_content # add to list
  }

  complete_data <- as.data.frame(rbindlist(file_list, fill = TRUE)) # compile list into a data frame

  # ---
  # data manipulation ----
  # ---
  if (PROD == "RBC") {

    deliveries <- complete_data %>%
      filter(V5 == "Punasoluvalmiste" & V2 > 0) %>% # RBCs are found by tag "Punasoluvalmiste" | quantity > 0 is a delivery, q < 0 a return
      select(V1, V2) %>% # we want date and quantity
      rename(date = V1, deliveries = V2)

    returns <- complete_data %>%
      filter(V5 == "Punasoluvalmiste" & V2 < 0) %>% # RBCs are found by tag "Punasoluvalmiste" | quantity > 0 is a delivery, q < 0 a return
      select(V1, V2) %>% # we want date and quantity
      rename(date = V1, returns = V2)

    deliv_daily <- aggregate(deliveries$deliveries, by = list(deliveries$date), sum) %>%
      rename(date = Group.1, deliveries = x)
    retur_daily <- aggregate(returns$returns, by = list(returns$date), sum) %>%
      rename(date = Group.1, returns = x)

    # Important! Check that the series are continuous (no missing dates). Impute with zeros if needed.
    all_dates <- seq.Date(min(c(deliv_daily$date, retur_daily$date)), max(c(deliv_daily$date, retur_daily$date)), "day")

    deliv <- merge(x = data.frame(date = all_dates), y = deliv_daily, all.x = TRUE) # continuous delivery series (missing date -> NA)
    deliv[is.na(deliv)] <- 0 # impute with zeros

    retur <- merge(x = data.frame(date = all_dates), y = retur_daily, all.x = TRUE) # continuous returns series (missing date -> NA)
    retur[is.na(retur)] <- 0

    # Create sales
    sales <- data.frame(date = all_dates, sales = deliv$deliveries + retur$returns) # sales: deliveries minus returns | '+' is used, cause returns are negative

    out <- cbind(sales, deliveries = deliv$deliveries, returns = abs(retur$returns)) # compile for returning

  }

  if (PROD == "PLAT") {

    deliveries <- complete_data %>%
      filter(V5 == "Trombosyyttivalmiste" & V2 > 0) %>% # Plats are found by tag "Trombosyyttivalmiste" | quantity > 0 is a delivery, q < 0 a return
      select(V1, V2) %>% # we want date and quantity
      rename(date = V1, deliveries = V2)

    returns <- complete_data %>%
      filter(V5 == "Trombosyyttivalmiste" & V2 < 0) %>% # Plats are found by tag "Trombosyyttivalmiste" | quantity > 0 is a delivery, q < 0 a return
      select(V1, V2) %>% # we want date and quantity
      rename(date = V1, returns = V2)

    deliv_daily <- aggregate(deliveries$deliveries, by = list(deliveries$date), sum) %>%
      rename(date = Group.1, deliveries = x)
    retur_daily <- aggregate(returns$returns, by = list(returns$date), sum) %>%
      rename(date = Group.1, returns = x)

    # Important! Check that the series are continuous (no missing dates). Impute with zeros if needed.
    all_dates <- seq.Date(min(c(deliv_daily$date, retur_daily$date)), max(c(deliv_daily$date, retur_daily$date)), "day")

    deliv <- merge(x = data.frame(date = all_dates), y = deliv_daily, all.x = TRUE) # continuous delivery series (missing date -> NA)
    deliv[is.na(deliv)] <- 0 # impute with zeros

    retur <- merge(x = data.frame(date = all_dates), y = retur_daily, all.x = TRUE) # continuous returns series (missing date -> NA)
    retur[is.na(retur)] <- 0

    # Create sales
    sales <- data.frame(date = all_dates, sales = deliv$deliveries + retur$returns) # sales: deliveries minus returns | '+' is used, cause returns are negative

    out <- cbind(sales, deliveries = deliv$deliveries, returns = abs(retur$returns)) # compile for returning

  }

  if (PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {

    deliveries <- complete_data %>%
      filter(V3 == PROD & V5 == "Punasoluvalmiste" & V2 > 0) %>% # Plats are found by tag "Punasoluvalmiste" | quantity > 0 is a delivery, q < 0 a return
      select(V1, V2) %>% # we want date and quantity
      rename(date = V1, deliveries = V2)

    returns <- complete_data %>%
      filter(V3 == PROD & V5 == "Punasoluvalmiste" & V2 < 0) %>% # Plats are found by tag "Punasoluvalmiste" | quantity > 0 is a delivery, q < 0 a return
      select(V1, V2) %>% # we want date and quantity
      rename(date = V1, returns = V2)

    deliv_daily <- aggregate(deliveries$deliveries, by = list(deliveries$date), sum) %>%
      rename(date = Group.1, deliveries = x)
    retur_daily <- aggregate(returns$returns, by = list(returns$date), sum) %>%
      rename(date = Group.1, returns = x)

    # Important! Check that the series are continuous (no missing dates). Impute with zeros if needed.
    all_dates <- seq.Date(min(c(deliv_daily$date, retur_daily$date)), max(c(deliv_daily$date, retur_daily$date)), "day")

    deliv <- merge(x = data.frame(date = all_dates), y = deliv_daily, all.x = TRUE) # continuous delivery series (missing date -> NA)
    deliv[is.na(deliv)] <- 0 # impute with zeros

    retur <- merge(x = data.frame(date = all_dates), y = retur_daily, all.x = TRUE) # continuous returns series (missing date -> NA)
    retur[is.na(retur)] <- 0

    # Create sales
    sales <- data.frame(date = all_dates, sales = deliv$deliveries + retur$returns) # sales: deliveries minus returns | '+' is used, cause returns are negative

    out <- cbind(sales, deliveries = deliv$deliveries, returns = abs(retur$returns)) # compile for returning

  }

  # ---
  # return ----
  # ---
  return(out)
}

read_FACS <- function(INPUT, PROD) {
    # Reads older FACS data from INPUT

    # argument validity is checked on upper level!

    # ---
    # data reading and compilation ----
    # ---
    filenames <- list.files(INPUT, pattern = "FAC0091_*")  # character vector of file names

    file_list <- list()
    for (i in filenames) { # compile a dataframe by going over all files
      file <- read.delim(file = paste0(INPUT, "/", i), header = FALSE, sep = ";", stringsAsFactors = FALSE, colClasses = 'character')

      if (length(file) == 26) { # special case
        file <- file[, !(names(file) %in% c("V10"))]  # the column numbers unfortunately vary between files, so we'll adjust
        }

      colnames(file) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13",
                          "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25")  # this is done so as to have easier column handling later

      file_list[[i]] <- file
    }

    complete_data <- as.data.frame(rbindlist(file_list, fill = TRUE)) # stack a data frame from the list of files

    # ---
    # data manipulation ----
    # ---

    complete_re <- complete_data %>% # recode for easier handling and less code redundancy
      mutate(V20 = recode(V20, "O +" = "O+", "O -" = "O-", "A +" = "A+", "A -" = "A-", "B +" = "B+", "B -" = "B-"))
    # NB! returns aren't coded by type in FACS data, so we don't need to account for the column shift here


    if (PROD == "RBC") {

        filtr <- c("REMOVED", "FROM", "PUBLIC", "VERSION") # filter to RBC by these

        deliveries <- complete_re %>%
          filter(V1 == "P") %>%
          select(V12, V14, V18) %>% # we want date, product code, quantity
          rename(date = V12, product = V14, deliveries = V18) %>% # rename columns
          transmute(date = dmy(date), product = as.character(product), deliveries = as.numeric(deliveries)) %>% # ensure column class
          filter(product %in% filtr) # filter by product

        returns <- complete_re %>% # ATTENTION: the column order is slightly different with returns!
          filter(V1 == "R") %>%
          select(V4, V5, V7) %>% # we want date, product code, quantity
          rename(date = V4, product = V5, returns = V7) %>% # rename columns
          transmute(date = dmy(date), product = as.character(product), returns = as.numeric(returns)) %>% # ensure column class
          filter(product %in% filtr) # filter by product

        deliv_daily <- aggregate(deliveries$deliveries, by = list(deliveries$date), sum) %>% # create a daily series from both by aggregating
          rename(date = Group.1, deliveries = x)
        retur_daily <- aggregate(returns$returns, by = list(returns$date), sum) %>%
          rename(date = Group.1, returns = x)

        # Important! Check that the series are continuous (no missing dates). Impute with zeros if needed.
        all_dates <- seq.Date(min(c(deliv_daily$date, retur_daily$date)), max(c(deliv_daily$date, retur_daily$date)), "day") # range of dates from start to end

        cont_deliv_with_nas <- merge(x = data.frame(date = all_dates), y = deliv_daily, all.x = TRUE) # continuous delivery series (missing date -> NA)
        cont_deliv_with_nas[is.na(cont_deliv_with_nas)] <- 0 # impute with zeros

        cont_retur_with_nas <- merge(x = data.frame(date = all_dates), y = retur_daily, all.x = TRUE) # continuous returns series (missing date -> NA)
        cont_retur_with_nas[is.na(cont_retur_with_nas)] <- 0

        # Important! There was a significant demand level change before 2014 (demand decreased).
        # Forecasts will improve when cutting to time after 2014, even though it limits the number of data points.
        deliv <- cont_deliv_with_nas %>%
          filter(date >= as.Date("2014-01-01"))
        retur <- cont_retur_with_nas %>%
          filter(date >= as.Date("2014-01-01"))

        out <- cbind(deliv, returns = retur$returns) # combine for returning

        }

    if (PROD == "PLAT") {

        filtr <- c("REMOVED", "FROM", "PUBLIC", "VERSION") # filter to PLAT by these

        deliveries <- complete_re %>%
          filter(V1 == "P") %>%
          select(V12, V14, V18) %>% # we want date, product code, quantity
          rename(date = V12, product = V14, deliveries = V18) %>% # rename columns
          transmute(date = dmy(date), product = as.character(product), deliveries = as.numeric(deliveries)) %>% # ensure column class
          filter(product %in% filtr) # filter by product

        returns <- complete_re %>% # ATTENTION: the column order is slightly different with returns!
          filter(V1 == "R") %>%
          select(V4, V5, V7) %>% # we want date, product code, quantity
          rename(date = V4, product = V5, returns = V7) %>% # rename columns
          transmute(date = dmy(date), product = as.character(product), returns = as.numeric(returns)) %>% # ensure column class
          filter(product %in% filtr) # filter by product

        deliv_daily <- aggregate(deliveries$deliveries, by = list(deliveries$date), sum) %>% # create a daily series from both by aggregating
          rename(date = Group.1, deliveries = x)
        retur_daily <- aggregate(returns$returns, by = list(returns$date), sum) %>%
          rename(date = Group.1, returns = x)

        # Important! Check that the series are continuous (no missing dates). Impute with zeros if needed.
        all_dates <- seq.Date(min(c(deliv_daily$date, retur_daily$date)), max(c(deliv_daily$date, retur_daily$date)), "day") # range of dates from start to end

        cont_deliv_with_nas <- merge(x = data.frame(date = all_dates), y = deliv_daily, all.x = TRUE) # continuous delivery series (missing date -> NA)
        cont_deliv_with_nas[is.na(cont_deliv_with_nas)] <- 0 # impute with zeros

        cont_retur_with_nas <- merge(x = data.frame(date = all_dates), y = retur_daily, all.x = TRUE) # continuous returns series (missing date -> NA)
        cont_retur_with_nas[is.na(cont_retur_with_nas)] <- 0

        # Important! There was a significant demand level change before 2014 (demand decreased).
        # Forecasts will improve when cutting to time after 2014, even though it limits the number of data points.
        deliv <- cont_deliv_with_nas %>%
          filter(date >= as.Date("2014-01-01"))
        retur <- cont_retur_with_nas %>%
          filter(date >= as.Date("2014-01-01"))

        out <- cbind(deliv, returns = retur$returns) # combine for returning

    }

    if (PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {

      filtr <- c("REMOVED", "FROM", "PUBLIC", "VERSION") # filter to RBC by these

        deliveries <- complete_re %>%
          filter(V1 == "P" & V20 == PROD) %>%
          select(V12, V14, V18) %>% # we want date, product code, quantity
          rename(date = V12, product = V14, deliveries = V18) %>% # rename columns
          transmute(date = dmy(date), product = as.character(product), deliveries = as.numeric(deliveries)) %>% # ensure column class
          filter(product %in% filtr) # filter by product

        # ATTENTION: RETURNS ARE NOT LOGGED BY TYPE IN THE FACS DATA!
        # SO UP UNTIL 2020, WE DON'T HAVE RETURNS DATA (SAVED AS ZEROS)

        deliv_daily <- aggregate(deliveries$deliveries, by = list(deliveries$date), sum) %>% # create a daily series from both by aggregating
          rename(date = Group.1, deliveries = x)

        # Important! Check that the series are continuous (no missing dates). Impute with zeros if needed.
        all_dates <- seq.Date(min(c(deliv_daily$date)), max(c(deliv_daily$date)), "day") # range of dates from start to end

        cont_deliv_with_nas <- merge(x = data.frame(date = all_dates), y = deliv_daily, all.x = TRUE) # continuous delivery series (missing date -> NA)
        cont_deliv_with_nas[is.na(cont_deliv_with_nas)] <- 0 # impute with zeros

        # Important! There was a significant demand level change before 2014 (demand decreased).
        # Forecasts will improve when cutting to time after 2014, even though it limits the number of data points.
        deliv <- cont_deliv_with_nas %>%
          filter(date >= as.Date("2014-01-01"))

        out <- cbind(deliv, returns = 0) # combine for returning
    }

    # ---
    # return ----
    # ---
    out <- data.frame(date = out$date, sales = out$deliveries - out$returns, deliveries = out$deliveries, returns = out$returns) # conform with read_DW output
    return(out)
    }

read_files <- function(input = RAWDATADIR, include_older = FALSE) {

  ######                                ######
  #     THIS FUNCTION IS UGLY DON'T LOOK     #
  ######                                ######
  if (include_older) {
    # Get all the OLDER files
    files <- list.files(path = RAWDATADIR, pattern = "FAC0091_*")  # Character vector of file names

    # Compile a dataframe by going over all files
    dlist <- list()
    for (i in files) {
      # Read a single file to a df called d
      d <- read.delim(file = paste0(RAWDATADIR, "/", i), header = FALSE, sep = ";", stringsAsFactors = FALSE, colClasses = 'character')

      if (length(d) == 26) {
        d <- d[, !(names(d) %in% c("V10"))]  # The column numbers unfortunately vary between files, so we'll adjust
        }

      colnames(d) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10",
                       "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20",
                       "V21", "V22", "V23", "V24", "V25")  # This is done so as to have easier column handling later on
      dlist[[i]] <- d
    }

    d <- as.data.frame(rbindlist(dlist, fill = TRUE))

    # Divide into distributions (P) and returns (R)
    P <- d[d$V1 == "P", ]
    R <- d[d$V1 == "R", ]

    # For distributions, we'll keep Distribution date, Quantity, ABO type, Volume, Exp date
    keep <- c("V12", "V14", "V18", "V20", "V22", "V24")
    distr <- P[keep]
    colnames(distr) <- c("date", "product", "quantity", "ABO", "volume", "exp")

    # For returns we keep the return date and quantity
    keep <- c("V4", "V5", "V7")
    retrn <- R[keep]
    colnames(retrn) <- c("date", "product", "quantity")

    # Datify
    distr$date <- dmy(distr$date); distr$exp <- dmy(distr$exp)
    retrn$date <- dmy(retrn$date)

    # Numerify
    distr$quantity <- as.numeric(distr$quantity); distr$volume <- as.numeric(distr$volume)
    retrn$quantity <- as.numeric(retrn$quantity)

    # Product codes for red cell products
    red_codes <- c("REMOVED", "FROM", "PUBLIC", "VERSION")

    red_distr <- distr[distr$product %in% red_codes, ]
    red_retrn <- retrn[retrn$product %in% red_codes, ]

    # Product codes for platelets
    plat_codes <- c("REMOVED", "FROM", "PUBLIC", "VERSION")

    plat_distr <- distr[distr$product %in% plat_codes, ]
    plat_retrn <- retrn[retrn$product %in% plat_codes, ]

    # Create a full sequence of dates for imputation purposes
    all_dates <- (seq.Date(min(red_distr$date),
                           max(red_distr$date),
                           "day"))
    ###           ###
    #   RED CELLS   #
    ###           ###
    all_red <- aggregate(red_distr$quantity, by = list(red_distr$date), sum); colnames(all_red) <- c("date", "pcs")
    # Merge into a whole set with NAs
    all_red <- merge(x = data.frame(date = all_dates),
                     y = all_red,
                     all.x = TRUE)
    # Replace with zeroes
    all_red[is.na(all_red)] <- 0
    # Cut to time after 2014
    all_red <- all_red[all_red$date >= as.Date("2014-01-01"), ]

    ###           ###
    #  RED RETURNS  #
    ###           ###

    all_red_retrn <- aggregate(red_retrn$quantity, by = list(red_retrn$date), sum); colnames(all_red_retrn) <- c("date", "pcs")
    # Merge into a whole set with NAs
    all_red_retrn <- merge(x = data.frame(date = all_dates),
                     y = all_red_retrn,
                     all.x = TRUE)
    # Replace with zeroes
    all_red_retrn[is.na(all_red_retrn)] <- 0
    # Cut to time after 2014
    all_red_retrn <- all_red_retrn[all_red_retrn$date >= as.Date("2014-01-01"), ]

    ###           ###
    #   PLATELETS   #
    ###           ###
    all_plat <- aggregate(plat_distr$quantity, by = list(plat_distr$date), sum); colnames(all_plat) <- c("date", "pcs")
    # Merge into a whole set with NAs
    all_plat <- merge(x = data.frame(date = all_dates),
                      y = all_plat,
                      all.x = TRUE)
    # Replace with zeroes
    all_plat[is.na(all_plat)] <- 0
    # Cut to time after 2014
    all_plat <- all_plat[all_plat$date >= as.Date("2014-01-01"), ]

    ###           ###
    # PLAT RETURNS  #
    ###           ###
    all_plat_retrn <- aggregate(plat_retrn$quantity, by = list(plat_retrn$date), sum); colnames(all_plat_retrn) <- c("date", "pcs")
    # Merge into a whole set with NAs
    all_plat_retrn <- merge(x = data.frame(date = all_dates),
                      y = all_plat_retrn,
                      all.x = TRUE)
    # Replace with zeroes
    all_plat_retrn[is.na(all_plat_retrn)] <- 0
    # Cut to time after 2014
    all_plat_retrn <- all_plat_retrn[all_plat_retrn$date >= as.Date("2014-01-01"), ]

    older_red <- data.frame(date = all_red[, 1], sales = all_red[, 2], deliveries = all_red[, 2] + all_red_retrn[, 2], returns = all_red_retrn[, 2])
    older_plat <- data.frame(date = all_plat[, 1], sales = all_plat[, 2], deliveries = all_plat[, 2] + all_plat_retrn[, 2], returns = all_plat_retrn[, 2])
  }

  # Get all the NEWER files
    files <- list.files(path = input, pattern = "DWSALES_*")  # Character vector of file names

    ## Read, define columns, omit last column
    dlist <- list()
    for (i in 1:length(files)) {
    filename <- files[i]
    file_content <- fread(paste0(input, filename),
                          colClasses = c("Date", "numeric", "factor", "character", "character", "NULL"))
    dlist[[i]] <- file_content

    }
    data <- as.data.frame(rbindlist(dlist, fill = TRUE)); data$V1 <- as.Date(data$V1)
    # Dataset filtered by tags
    red_deliv <- data %>%
        filter(V5 == "Punasoluvalmiste" & V2 > 0) %>%
        select(c("date" = V1, "pcs" = V2, "type" = V3)) %>%
        arrange(date)

    red_retur <- data %>%
        filter(V5 == "Punasoluvalmiste" & V2 < 0) %>%
        select(c("date" = V1, "pcs" = V2, "type" = V3)) %>%
        arrange(date)

    plat_deliv <- data %>%
        filter(V5 == "Trombosyyttivalmiste" & V2 > 0) %>%
        select(c("date" = V1, "pcs" = V2, "type" = V3)) %>%
        arrange(date)

    plat_retur <- data %>%
        filter(V5 == "Trombosyyttivalmiste" & V2 < 0) %>%
        select(c("date" = V1, "pcs" = V2, "type" = V3)) %>%
        arrange(date)

    # Daily aggregation
    red_deliv_daily <- aggregate(red_deliv$pcs, by = list(red_deliv$date), sum); colnames(red_deliv_daily) <- c("date", "pcs")
    red_retur_daily <- aggregate(red_retur$pcs, by = list(red_retur$date), sum); colnames(red_retur_daily) <- c("date", "pcs")
    plat_deliv_daily <- aggregate(plat_deliv$pcs, by = list(plat_deliv$date), sum); colnames(plat_deliv_daily) <- c("date", "pcs")
    plat_retur_daily <- aggregate(plat_retur$pcs, by = list(plat_retur$date), sum); colnames(plat_retur_daily) <- c("date", "pcs")

    # SALES NUMBER = DELIVERIES - RETURNS
    # To make sales, we need to ensure equal continuous time windows for deliv and retur series
    red_temp <- merge(red_deliv_daily, red_retur_daily, by = "date", all.x = TRUE)
    # Do new merge to ensure intact date series
    red_m <- merge(red_temp, data.frame(date = seq.Date(min(red_deliv_daily$date), max(red_deliv_daily$date), by = "day")), by = "date", all.y = TRUE)
    red_m[is.na(red_m)] <- 0
    red <- red_m %>%
        transmute(date = date, sales = pcs.x + pcs.y, deliveries = pcs.x, returns = -1 * pcs.y)
    # Now repeat for plats
    plat_temp <- merge(plat_deliv_daily, plat_retur_daily, by = "date", all.x = TRUE)
    # Do new merge to ensure intact date series
    plat_m <- merge(plat_temp, data.frame(date = seq.Date(min(plat_deliv_daily$date), max(plat_deliv_daily$date), by = "day")), by = "date", all.y = TRUE)
    plat_m[is.na(plat_m)] <- 0
    plat <- plat_m %>%
        transmute(date = date, sales = pcs.x + pcs.y, deliveries = pcs.x, returns = -1 * pcs.y)

    if (include_older) {
      # APPEND NEWER TO OLDER (AND OVERWRITE IF NEEDED)
      newer_red_first_date <- red[1, 1]
      cut_old_red <- older_red %>% filter(date < newer_red_first_date)
      full_red <- rbind(cut_old_red, red)

      newer_plat_first_date <- plat[1, 1]
      cut_old_plat <- older_plat %>% filter(date < newer_plat_first_date)
      full_plat <- rbind(cut_old_plat, plat)

      return(list(red = full_red, plat = full_plat))
    } else {return(list(red = red, plat = plat))}


}

update_data <- function(INPUT, OUTPUT, PROD) {

  # ---
  # checks ----
  # ---
  possible_products <- c("RBC", "PLAT", "O+", "O-", "A+", "A-", "B+", "B-", "AB-", "AB+")
  if (!(PROD %in% possible_products)) {
    stop(paste0("Argument PROD needs to be one of these: RBC, PLAT, O+, O-, A+, A-, B+, B-, AB+, AB-!"))
  } # check if PROD is valid
  if (!dir.exists(OUTPUT)) {
    stop(paste0("OUTPUT path: ", OUTPUT, " does not exist. Please edit file: config.R"))
  } # check if OUTPUT exists
  if (!dir.exists(INPUT)) {
    stop(paste0("INPUT path: ", INPUT, " does not exist. Please edit file: config.R"))
  } # check if INPUT exists

  # ---
  # read and update data ----
  # ---
  if (PROD == "RBC") {

    if (!file.exists(paste0(OUTPUT, PROD, "_daily.csv"))) { # if daily series data does not exist, create it
      FACS_data <- read_FACS(INPUT, PROD)
      DW_data <- read_DW(INPUT, PROD)

      # ATTENTION: the old data and new data overlap briefly in May 2020.
      # the newer data is presumably of better quality, so we'll use that one
      cutoff_date <- DW_data$date[1]
      data <- rbind(FACS_data[FACS_data$date < cutoff_date, ], DW_data)

      write.table(data, paste0(OUTPUT, PROD, "_daily.csv"), sep = ",", row.names = F, col.names = T)
    } else { # if daily series file exists
        file <- read.csv(paste0(OUTPUT, PROD, "_daily.csv"), colClasses = c("Date", "numeric", "numeric", "numeric")) # read it
        DW_data <- read_DW(INPUT, PROD) # read new data

        # check if we need to read FACS at all
        # need to satisfy: first_date == 2014-01-01, last_date >= 2020-05-04, continuous == TRUE
        first_date <- as.Date("2014-01-01") # should be
        last_date <- file$date[nrow(file)]
        if (file$date[1] == first_date & file$date[nrow(file)] >= last_date & is_continuous(file$date)) {
          # skip loading FACS and append new data directly if needed
          if (DW_data$date[nrow(DW_data)] > last_date) {
            new_data <- DW_data %>%
              filter(date > last_date)
            write.table(new_data, file = paste0(OUTPUT, PROD, "_daily.csv"), append = T, col.names = F, row.names = F)

            data <- rbind(file, new_data)
          } else {data <- file}

        }
    }
}

  if (PROD == "PLAT") {

    if (!file.exists(paste0(OUTPUT, PROD, "_daily.csv"))) { # if daily series data does not exist, create it
      FACS_data <- read_FACS(INPUT, PROD)
      DW_data <- read_DW(INPUT, PROD)

      # ATTENTION: the old data and new data overlap briefly in May 2020.
      # the newer data is presumably of better quality, so we'll use that one
      cutoff_date <- DW_data$date[1]
      data <- rbind(FACS_data[FACS_data$date < cutoff_date, ], DW_data)

      write.table(data, paste0(OUTPUT, PROD, "_daily.csv"), sep = ",", row.names = F, col.names = T)
    } else { # if daily series file exists
        file <- read.csv(paste0(OUTPUT, PROD, "_daily.csv"), colClasses = c("Date", "numeric", "numeric", "numeric")) # read it
        DW_data <- read_DW(INPUT, PROD) # read new data

        # check if we need to read FACS at all
        # need to satisfy: first_date == 2014-01-01, last_date >= 2020-05-04, continuous == TRUE
        first_date <- as.Date("2014-01-01") # should be
        last_date <- file$date[nrow(file)]
        if (file$date[1] == first_date & file$date[nrow(file)] >= last_date & is_continuous(file$date)) {
          # skip loading FACS and append new data directly if needed
          if (DW_data$date[nrow(DW_data)] > last_date) {
            new_data <- DW_data %>%
              filter(date > last_date)
            write.table(new_data, file = paste0(OUTPUT, PROD, "_daily.csv"), append = T, col.names = F, row.names = F)

            data <- rbind(file, new_data)
          } else {data <- file}

        }
    }
  }

  if (PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {
    if (!file.exists(paste0(OUTPUT, PROD, "_daily.csv"))) { # if daily series data does not exist, create it
      FACS_data <- read_FACS(INPUT, PROD)
      DW_data <- read_DW(INPUT, PROD)

      # ATTENTION: the old data and new data overlap briefly in May 2020.
      # the newer data is presumably of better quality, so we'll use that one
      cutoff_date <- DW_data$date[1]
      data <- rbind(FACS_data[FACS_data$date < cutoff_date, ], DW_data)

      write.table(data, paste0(OUTPUT, PROD, "_daily.csv"), sep = ",", row.names = F, col.names = T)
    } else { # if daily series file exists
        file <- read.csv(paste0(OUTPUT, PROD, "_daily.csv"), colClasses = c("Date", "numeric", "numeric", "numeric")) # read it
        DW_data <- read_DW(INPUT, PROD) # read new data

        # check if we need to read FACS at all
        # need to satisfy: first_date == 2014-01-01, last_date >= 2020-05-04, continuous == TRUE
        first_date <- as.Date("2014-01-01") # should be
        last_date <- file$date[nrow(file)]
        if (file$date[1] == first_date & file$date[nrow(file)] >= last_date & is_continuous(file$date)) {
          # skip loading FACS and append new data directly if needed
          if (DW_data$date[nrow(DW_data)] > last_date) {
            new_data <- DW_data %>%
              filter(date > last_date)
            write.table(new_data, file = paste0(OUTPUT, PROD, "_daily.csv"), append = T, col.names = F, row.names = F)

            data <- rbind(file, new_data)
          } else {data <- file}

        }
    }
}

  # ---
  # return for use ----
  # ---
  return(data)
}

workday_adjustment <- function(df, reverse = FALSE) {

  # Assumes date is first column
  beginning <- df[1, 1]
  colnam <- names(df)
  workdays <- as.numeric(bizdays(ts(1:nrow(df), start = c(year(beginning), month(beginning)), frequency = 12), FinCenter = "Zurich"))

  if (reverse) {
    adj_series <- data.frame(date = df[, 1], (df[, -1] * workdays))
  } else {
    adj_series <- data.frame(date = df[, 1], (df[, -1] / workdays))
    }

  colnames(adj_series) <- colnam

  return(adj_series)
}

find_resolution <- function(dates, format = "label"){

  # Find (approximate) temporal resolution of the series
  # for any function that requires it.
  # Can only find daily, weekly, and yearly resolution.
  # (Because those are ones we currently need)
  # Output depends on format: label or frequency

  # ---
  # check argument validity ----
  # ---
  if (!(class(dates) == "Date")) {stop("Supplied dates are not in Date format!")}
  if (!(format %in% c("label", "frequency"))) {stop("format should be either 'label' or 'frequency'")}

  diff <- abs(dates[1] - dates[2])

  # if label ----
  if (format == "label") {
    if (diff == 1) {return("daily")}
    if (diff > 1 & diff < 8) {return("weekly")}
    if (diff > 7 & diff < 32) {return("monthly")}
    if (diff > 31) {return("yearly")}
  }
  # if frequency ----
  if (format == "frequency") {
    if (diff == 1) {return(365.25)}
    if (diff > 1 & diff < 8) {return(52.17857)}
    if (diff > 7 & diff < 32) {return(12)}
    if (diff > 31) {return(1)}
  }

}

set_resolution <- function(daily, RES, at_end = FALSE, ECON, PROD, FIX = TRUE) {
  # aggregates daily series to weekly, monthly or yearly
  # at_end determines if we use the last day of each
  # week/month/year with date labels

  # NB! Special case for ECON analysis:
  # If PROD is either RBC or PLAT (the only two possible, really)
  # we require monthly data from 2004.
  # We will read it from OUTPUT and append
  # to our monthly aggregates!

  # ---
  # checks ----
  # ---
  if (ECON & PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {
    stop("Typed forecasts aren't currently possible for ECON = TRUE.")
  }
  if (ECON & RES %in% c("daily", "weekly")) {
    stop("ECON = TRUE allows only monthly or yearly resolution.")
  }

  # ---
  # COVID imputation (Spring 2020)----
  # ---
  if (FIX) {
    daily[daily$date >= as.Date("2020-03-01") & daily$date < as.Date("2020-07-01"), c(2, 3, 4)] <- daily[daily$date >= as.Date("2019-03-01") & daily$date < as.Date("2019-07-01"), c(2, 3, 4)]
  }

  # ---
  # aggregate ----
  # ---
  if (at_end) { # TODO: consider removing the possibility to have "at end" series

    if (RES == "weekly") {
      agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ week(date) + year(date), data = daily, FUN = sum)
      daterange <- ceiling_date(seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "week"), "week")
      agg <- cbind(date = daterange, agg_raw[, 3:5])
    }
    if (RES == "monthly") {
      if (ECON & PROD == "RBC") {
        # Load old data for ECON analysis
        file_old <- read.table(paste0(OUTPUT, "monthly_from_2004.txt"))[2:121, 2] # From Jan 2004 to Dec 2013
        old_daterange <- ceiling_date(seq.Date(from = as.Date("2004-01-01"), along.with = file_old, by = "month"), "month") - days(1)
        old_data <- data.frame(date = old_daterange, sales = as.numeric(file_old), deliveries = as.numeric(file_old), returns = 0)

        agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ month(date) + year(date), data = daily, FUN = sum)
        daterange <- ceiling_date(seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "month"), "month") - days(1)

        agg <- rbind(old_data, cbind(date = daterange, agg_raw[, 3:5]))
      }
      if (ECON & PROD == "PLAT") {
        # Load old data for ECON analysis
        file_old <- read.table(paste0(OUTPUT, "monthly_from_2004.txt"))[2:121, 3] # From Jan 2004 to Dec 2013
        old_daterange <- ceiling_date(seq.Date(from = as.Date("2004-01-01"), along.with = file_old, by = "month"), "month") - days(1)
        old_data <- data.frame(date = old_daterange, sales = as.numeric(file_old), deliveries = as.numeric(file_old), returns = 0)

        agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ month(date) + year(date), data = daily, FUN = sum)
        daterange <- ceiling_date(seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "month"), "month") - days(1)

        agg <- rbind(old_data, cbind(date = daterange, agg_raw[, 3:5]))
      }
      if (!ECON) {
        agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ month(date) + year(date), data = daily, FUN = sum)
        daterange <- ceiling_date(seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "month"), "month") - days(1)

        agg <- cbind(date = daterange, agg_raw[, 3:5])
      }
    }
    if (RES == "yearly") {
      agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ year(date), data = daily, FUN = sum)
      daterange <- ceiling_date(seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "year"), "year") - days(1)
      agg <- cbind(date = daterange, agg_raw[, 3:4])
    }

  } else {
    if (RES == "weekly"){
      agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ week(date) + year(date), data = daily, FUN = sum)
      agg <- cbind(date = seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "week"), agg_raw[, 3:5])
    }
    if (RES == "monthly"){
      if (ECON & (PROD == "RBC")) {
        # Load old data for ECON analysis
        file_old <- read.table(paste0(OUTPUT, "monthly_from_2004.txt"))[2:121, 2] # From Jan 2004 to Dec 2013
        old_daterange <- seq.Date(from = as.Date("2004-01-01"), along.with = file_old, by = "month")
        old_data <- data.frame(date = old_daterange, sales = as.numeric(file_old), deliveries = as.numeric(file_old), returns = 0)

        agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ month(date) + year(date), data = daily, FUN = sum)
        daterange <- seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "month")

        agg <- rbind(old_data, cbind(date = daterange, agg_raw[, 3:5]))
      }
      if (ECON & (PROD == "PLAT")) {
        # Load old data for ECON analysis
        file_old <- read.table(paste0(OUTPUT, "monthly_from_2004.txt"))[2:121, 3] # From Jan 2004 to Dec 2013
        old_daterange <- seq.Date(from = as.Date("2004-01-01"), along.with = file_old, by = "month")
        old_data <- data.frame(date = old_daterange, sales = as.numeric(file_old), deliveries = as.numeric(file_old), returns = 0)

        agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ month(date) + year(date), data = daily, FUN = sum)
        daterange <- seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "month")

        agg <- rbind(old_data, cbind(date = daterange, agg_raw[, 3:5]))
      }
      if (!ECON) {
        agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ month(date) + year(date), data = daily, FUN = sum)
        daterange <- seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "month")

        agg <- cbind(date = daterange, agg_raw[, 3:5])
      }
    }
    if (RES == "yearly"){
      agg_raw <- aggregate(cbind(sales, deliveries, returns) ~ year(date), data = daily, FUN = sum)
      agg <- cbind(date = seq.Date(from = daily$date[1], along.with = agg_raw$sales, by = "year"), agg_raw[, 3:4])
    }
  }

  # NB!
  # we never return the last row, as this is the running week/month/year (and may get aggregated before it finishes)
  # thus, we always forecast the running week/month/year!
  return(agg[-nrow(agg), ])
}

is_continuous <- function(dates, resolution = "daily") {

  # Checks whether a series of dates is continuous
  # on the desired level. Requires class to be 'Date'.
  # Assumes series are dated by level-end (weekly dates are Sundays, monthly dates are 31st, 28th, 31st, 30th...)

  # ---
  # check argument validity ----
  # ---
  if (!(class(dates) == "Date")) { stop(paste0("class(dates) should be Date but is: ", class(dates)))}
  if (!(resolution %in% c("daily", "weekly", "monthly", "yearly"))) { stop("possible levels: daily, weekly, monthly, yearly") }

  # ---
  # check continuity by level ----
  # ---
  if (resolution == "daily") {
    first_date <- dates[1]
    true_range <- seq.Date(from = first_date, along.with = dates, by = "day")
    if (all.equal(true_range, dates)) { return(TRUE) } else { return(FALSE) }
  }
  if (resolution == "weekly") {
    first_date <- dates[1]
    true_range <- seq.Date(from = first_date, along.with = dates, by = "week")
    if (all.equal(true_range, dates)) { return(TRUE) } else { return(FALSE) }
  }
  if (resolution == "monthly") {
    first_date <- dates[1]
    true_range <- seq.Date(from = first_date, along.with = dates, by = "month")
    if (all.equal(true_range, dates)) { return(TRUE) } else { return(FALSE) }
  }
  if (resolution == "yearly") {
    first_date <- dates[1]
    true_range <- seq.Date(from = first_date, along.with = dates, by = "year")
    if (all.equal(true_range, dates)) { return(TRUE) } else { return(FALSE) }
  }


}

ensure_window_parity <- function(x, y) {
  # Ensure that both series span the same time period
  #
  # ARGUMENTS
  # x: first series
  # y: second series
  #
  # RETURN
  # Both series, trimmed to identical date ranges

  # Assumes date column is first
  if (x[1, 1] > y[1, 1]) start <- x[1, 1] else start <- y[1, 1]
  if (x[nrow(x), 1] < y[nrow(y), 1]) end <- x[nrow(x), 1] else end <- y[nrow(y), 1]
  x <- x[x[, 1] >= start & x[, 1] <= end, ]
  y <- y[y[, 1] >= start & y[, 1] <= end, ]

  return(list(first = x, second = y))
}

find_trend <- function(data, window_size = 24, out = "vec"){
  # assumes data is df: date | value

  clean <- tail(na.omit(data), window_size)
  slope <- lm(clean[, 2] ~ clean[, 1])$coefficients[2]

  if (out == "vec") {
    res <- find_resolution(clean[, 1], format = "label")

    if (res == "daily") {scale <- 1; param <- "days"}
    if (res == "weekly") {scale <- 7; param <- "weeks"}
    if (res == "monthly") {scale <- 30; param <- "months"}
    if (res == "yearly") {scale <- 365; param <- "years"}

    slopevec <- cumsum(rep(slope * scale, window_size)) - cumsum(rep(slope * scale, window_size))[(floor(window_size/2))]
    pivot <- mean(clean[, 2])
    trendline <- slopevec + pivot

    return(data.frame(date = clean[, 1], trend = round(trendline)))
  }
  if (out == "slope") {
    return(slope)
  }


}

get_errors <- function(target, estimate, type = "APE"){

  # Computes specified errors from given series
  # Assumes that target is a df of: date | value
  # and that estimate is a df of: date | value1 | value2 | ...
  # They need not be equal in length, errors will
  # only be calculated from the overlapping time.

  # Percentage errors should not be used if target series
  # contains zero or near zero values. This function
  # DOES NOT TRUST THE USER. It will check for near zero values
  # and then default to Squared Error if it finds any.

  if (!(type %in% c("RAW", "APE", "PE", "SE"))) {stop("Invalid error type :( Use either RAW, APE, PE or SE.")}

  # Check for near zero ----
  # TODO: find out if we need to communicate this to the end user
  if (any(target[, 2] < 1, na.rm = T) | any(estimate[, 2] < 1, na.rm = T)) {type = "SE"}

  out <- ensure_window_parity(target, estimate)
  target <- out$first
  estimate <- out$second

  if (type == "RAW") {
    # Difference of estimate from target in original units ----
    err <- estimate[, -1] - target[, 2]
  }
  if (type == "APE") {
    # Mean Absolute Percentage Error ----
    err <- 100 * abs(estimate[, -1] - target[, 2]) / target[, 2]
  }
  if (type == "PE") {
    # Mean Percentage Error ----
    err <- 100 * (estimate[, -1] - target[, 2]) / target[, 2]
  }
  if (type == "SE") {
    # Squared Error ----
    err <- (estimate[, -1] - target[, 2])**2
  }
  return(data.frame(date = target[, 1], error = err))
}

wavg_selection <- function(history, alpha = 0.5, single = TRUE) {
  # Get WAVG configuration
  #
  # ARGUMENTS
  # history: a dataframe of method accuracies wo a date column
  # alpha: coefficient for exp. decay, default 0.5
  # single: boolean, whether the user wants a conf for one forecast, or for the whole history
  #
  # RETURNS
  # A named list: method name, coefs for weighting, order of methods for weighting
  #
  # NOTES: get_forecast expects that selected_method is either just a method name
  #        or a named list object with method name, and any other info needed to
  #        run the method.
  alphavec <- alpha ** (1:12) # 1:(Number of methods) | Remember, we exclude AVG and WAVG, so we don't start w.averaging averages
  if (single) {
    last <- history[nrow(history), c(1:12)]
    return(list(method = "wavg", coef = alphavec, order = order(last)))
  } else {
      return(list(method = "wavg", coef = alphavec/sum(alphavec), order = t(apply(history[, c(1:12)], 1, order))))
    }

}

bootstrap_interval <- function(series, fcast_history, h = 1, sampleN = 10000) {
  # Bootstrap prediction intervals using past residuals
  # (no need for normality assumption)
  #
  # ARGUMENTS
  # series: time series of interest, df: date | value
  # fcast_history: fcast history of selected method, df: date | value
  # length: how many steps into the future
  #
  # RETURN
  # df: date | 80lo| 80hi | 95lo | 95hi |

  # Ensure parity ----
  out <- ensure_window_parity(series, fcast_history) # make sure both have date column as first
  series <- out$first
  fcast_history <- out$second

  # Get residuals (to sample from) ----
  residuals_seed <- series[, 2] - fcast_history[, 2]
  residuals <- c(residuals_seed, -residuals_seed) # force symmetry

  # Generate sampleN alternate realities ----
  realities <- matrix(, nrow = sampleN, ncol = h)
  for (i in 1:sampleN) {
    # Last real obs
    y <- tail(series, 1)[, 2]
    alt_reality <- c()
    for (j in 1:h) {
      Y <- y + sample(residuals, 1)
      y <- Y
      alt_reality[j] <- Y
    }
    realities[i, ] <- alt_reality
  }

  # Get quantiles ----
  quants <- t(apply(realities, 2, function(x) quantile(x, c(0.20, 0.80, 0.05, 0.95))))
  df <- data.frame(quants)

  # return ----
  return(df)

}

simulate_selection_history <- function(data, S_R = "S", PROD, RES, ECON, TEST_LEN, OUTPUT) {
  if (ECON) {
    if (S_R == "S") {
      in_suffix <- paste0(PROD, "_sales_ECON_method_forecasts.csv")
      out_suffix <- paste0(PROD, "_sales_ECON_forecasts.csv")
    } else {
        in_suffix <- paste0(PROD, "_returns_ECON_method_forecasts.csv")
        out_suffix <- paste0(PROD, "_returns_ECON_forecasts.csv")
      }
  } else {
      if (S_R == "S") {
        in_suffix <- paste0(PROD, "_", RES, "_sales_OPER_method_forecasts.csv")
        out_suffix <- paste0(PROD, "_", RES, "_sales_OPER_forecasts.csv")
      } else {
          in_suffix <- paste0(PROD, "_", RES, "_returns_OPER_method_forecasts.csv")
          out_suffix <- paste0(PROD, "_", RES, "_returns_OPER_forecasts.csv")
        }
    }

  method_histories <- read.table(paste0(OUTPUT, in_suffix), header = TRUE, sep = ",")

  depth <- nrow(method_histories) - TEST_LEN
  simulated <- data.frame(date = rep(as.Date("2000-01-01"), depth), method = rep("NaN", depth), forecast = rep(0, depth)) # prepare a df
  for (i in 1:depth) {
    method <- select_method(data[data$date <= method_histories$date[i + TEST_LEN - 1], ], PROD, S_R, OUTPUT, ECON, TEST_LEN)
    forecast <- method_histories[(i + TEST_LEN), method]
    date <- method_histories[(i + TEST_LEN), "date"]
    simulated[i, ] <- c(date, method, forecast)
  }

  write.table(simulated, paste0(OUTPUT, out_suffix), sep = ",", row.names = FALSE, col.names = TRUE)

}

generate_method_history <- function(data, S_R, PROD, RES, ECON, TRAIN_LEN, HORIZON, OUTPUT){

  # ---
  # arguments to variables ----
  # ---
  if (ECON) {
    if (S_R == "S") {
      suffix <- paste0(PROD, "_sales_ECON_method_forecasts.csv")
      target <- data[, c(1, 2)]
    } else {
        suffix <- paste0(PROD, "_returns_ECON_method_forecasts.csv")
        target <- data[, c(1, 4)]
      }
    HORIZON <- as.integer(24) # this is different than normal ECON functionality (where HORIZON=60), because our method selection benchmark is "sum of next 24 months"
  } else {
      if (S_R == "S") {
        suffix <- paste0(PROD, "_", RES, "_sales_OPER_method_forecasts.csv")
        target <- data[, c(1, 2)]
      } else {
          suffix <- paste0(PROD, "_", RES, "_returns_OPER_method_forecasts.csv")
          target <- data[, c(1, 4)]
        }
  }

  resolution <- find_resolution(data[, 1], format = "frequency")
  train_len <- round(TRAIN_LEN * resolution) # years into correct units
  if (ECON | HORIZON == as.integer(1)) {
    if (resolution == 365.25) {
      first_fdate <- data[train_len, 1] + days(HORIZON) # because when ECON, our forecasts are ROLLSUMS of 1:HORIZON forecasts
      fdepth <- nrow(data[data$date >= first_fdate, ]) # in English: "length of data after the first_fdate"
      forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "day")
    }
    if (resolution == 52.17857) {
      first_fdate <- data[train_len, 1] + weeks(HORIZON)
      fdepth <- nrow(data[data$date >= first_fdate, ])
      forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "week")
    }
    if (resolution == 12) {
      first_fdate <- data[train_len, 1] + months(HORIZON)
      fdepth <- nrow(data[data$date >= first_fdate, ])
      forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "month")
    }
    if (resolution == 1) {
      first_fdate <- data[train_len, 1] + years(HORIZON)
      fdepth <- nrow(data[data$date >= first_fdate, ])
      forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "year")
    }
  } else {
      if (resolution == 365.25) {
        first_fdate <- data[train_len, 1] + days(1) # when NOT ECON, our forecasts start after the end of first training period
        fdepth <- nrow(data[data$date >= first_fdate, ])
        forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "day")
      }
      if (resolution == 52.17857) {
        first_fdate <- data[train_len, 1] + weeks(1)
        fdepth <- nrow(data[data$date >= first_fdate, ])
        forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "week")
      }
      if (resolution == 12) {
        first_fdate <- data[train_len, 1] + months(1)
        fdepth <- nrow(data[data$date >= first_fdate, ])
        forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "month")
      }
      if (resolution == 1) {
        first_fdate <- data[train_len, 1] + years(1)
        fdepth <- nrow(data[data$date >= first_fdate, ])
        forecast_dates <- seq.Date(from = first_fdate, length.out = fdepth, by = "year")
      }
    }
  # ---
  # generate forecasts ----
  # ---
  methods <- c("snaive", "ma5", "ma7", "ma9", "ma12", "ets", "stl", "stlf", "tbats", "arimax", "dynreg", "nn")
  method_fs <- list()
  for (i_m in 1:length(methods)) { # iterate over all methods
    forecasts <- c()
    for (i in 1:fdepth) { # and all available data
      window <- i:(train_len + i - 1)
      cdata <- data[window, ]
      fcast <- get_forecast(cdata, S_R, method = methods[i_m], hilo = F, PROD, TRAIN_LEN, HORIZON, OUTPUT, ECON)
      if (ECON) { # ECON needs 24mo rolling sums (fcasts)
        forecasts[i] <- sum(fcast[, 2])
      } else { # others are just single-point forecasts
          forecasts[i] <- fcast[1, 2]
        }
    }
    method_fs[[i_m]] <- forecasts
  }

  master_fs <- t(do.call(rbind, method_fs))
  master <- data.frame(date = forecast_dates, master_fs); colnames(master) <- c("date", methods)
  avg <- round(rowMeans(master_fs)) # create avg

  # create wavg
  if (ECON) { # ECON needs 24mo rolling sums (data)
    rollsums <- c()
    dates <- seq.Date(from = target[24, 1], length.out = (nrow(target) - 24), by = "month")
    for (i in 1:(nrow(target) - 24)){
      rollsums[i] <- sum(target[i:(i + 23), 2])
    }
    target <- data.frame(date = dates, value = rollsums)
  }

  acc_history <- get_errors(target, master)
  wavg_obj <- wavg_selection(acc_history[, -1], alpha = 0.5, single = FALSE) # this gets us everything we need
  wavg_v <- c()
  fs <- master[, -1]
  for (i in 1:(nrow(fs) - 1)) {
    wavg <- sum(wavg_obj$coef * fs[(i + 1), wavg_obj$order[i, ]])
    wavg_v[i] <- wavg
  }

  savethis <- cbind(master[-1, ], avg = avg[-1], wavg = round(wavg_v)) # prepare for saving

  write.table(savethis, file = paste0(OUTPUT, suffix), sep = ",", row.names = F) # save
}

select_method <- function(data, PROD, S_R = "S", OUTPUT, ECON, TEST_LEN) {

  # ---
  # check arguments ----
  # ---
  if (is.null(data)) {stop(paste0("Data is: ", NULL, "!")) }
  if (!is.integer(TEST_LEN)) {stop(paste0("Argument TEST_LEN: expected an integer, got ", TEST_LEN, " (", class(TEST_LEN), ") instead."))}

  # ---
  # arguments to variables ----
  # ---
  if (ECON) {
    if (S_R == "S") {
      suffix <- "_sales_ECON_method_forecasts.csv"
      target <- data[, c(1, 2)]
    } else {
        suffix <- "_returns_ECON_method_forecasts.csv"
        target <- data[, c(1, 4)]
      }
  } else {
      RES <- find_resolution(data[, 1], format = "label")
      if (S_R == "S") {
        suffix <- paste0("_", RES, "_sales_OPER_method_forecasts.csv")
        target <- data[, c(1, 2)]
    } else {
        suffix <- paste0("_", RES, "_returns_OPER_method_forecasts.csv")
        target <- data[, c(1, 4)]
      }
  }

  # ---
  # handy internal variables ----
  # ---
  methods <- c("snaive", "ma5", "ma7", "ma9", "ma12", "ets", "stl", "stlf", "tbats", "arimax", "dynreg", "nn", "avg", "wavg")

  # ---
  # load history file ----
  # ---
  histories <- read.table(paste0(OUTPUT, PROD, suffix), sep = ",", header = T)


  # ---
  # compute accuracies ----
  # ---
  if (ECON) {
    rollsums <- c()
    dates <- seq.Date(from = target[24, 1], length.out = (nrow(target) - 23), by = "month")
    for (i in 1:(nrow(target) - 23)){
      rollsums[i] <- sum(target[i:(i + 23), 2])
    }
    target <- data.frame(date = dates, value = rollsums)
  }
  acc <- get_errors(target, histories)


  # ---
  # select method ----
  # ---
  mincol <- which.min(colMeans(tail(acc[, -1], TEST_LEN)))
  return(methods[mincol])
}

get_forecast <- function(data, S_R = "S", method = "ets", hilo = TRUE, PROD, TRAIN_LEN, HORIZON, OUTPUT, ECON, save = FALSE) {

  # 'data' should ALWAYS be a DF of:
  # date | sales | deliveries | returns
  # 'S_R' tells us whether we are using $sales or $deliveries
  # 'HORIZON' comes from config.R and defines the length of the fcast
  # 'method' defines the forecasting method to be used. Defaults to ETS.

  # Note about 'R'eturn forecasts: the return series regularly contains zeros
  # especially with typed series (like O+). In addition, typed series have recorded
  # returns only from May 2020 onwards. Thus, the prediction intervals *will* dip below 0.
  # They are currently not needed with returns, luckily. However, we should ALWAYS check
  # for subzero forecasts.

  # ---
  # check arguments ----
  # ---
  if (is.null(data)) {stop(paste0("Data is: ", NULL, "!"))}
  if (!(S_R %in% c("S", "R"))) {stop(paste0("S_R needs to be either 'S' (sales) or 'R' (returns)"))}
  if (!is.integer(HORIZON)) {stop(paste0("Argument HORIZON: expected an integer, got ", HORIZON, " (", class(HORIZON), ") instead."))}
  if (!is.integer(TRAIN_LEN)) {stop(paste0("Argument TRAIN_LEN: expected an integer, got ", TRAIN_LEN, " (", class(TRAIN_LEN), ") instead."))}

  # ---
  # arguments to variables ----
  # ---
  if (HORIZON < 6) {HORIZON <- as.integer(6)} # for ECON this doesn't matter, but for OPER we want to plot at least 6 next (we still save only the very next)
  resolution <- find_resolution(data[, 1], format = "frequency") # temporal resolution of data
  train_len <- round(TRAIN_LEN * resolution) # TRAIN_LEN is in years, so convert to correct resolution
  visible <- tail(data, train_len) # the length of the immediate history the methods can see for fitting purposes
  if (S_R == "S") {coln <- 2} else {coln <- 4} # sales or returns
  genesis <- visible[1, 1] # first date in data
  terminus <- visible[nrow(visible), 1] # last date in data
  ts_start <- c(year(genesis), month(genesis))
  if (resolution == 365.25) {
    res <- "daily"
    forecast_dates <- seq.Date(from = terminus + days(1), along.with = 1:HORIZON, by = "day")
  }
  if (resolution == 52.17857) {
    res <- "weekly"
    forecast_dates <- seq.Date(from = terminus + weeks(1), along.with = 1:HORIZON, by = "week")
  }
  if (resolution == 12) {
    res <- "monthly"
    forecast_dates <- seq.Date(from = terminus + months(1), along.with = 1:HORIZON, by = "month")
    visible <- workday_adjustment(visible)
  }
  if (resolution == 1) {
    res <- "yearly"
    forecast_dates <- seq.Date(from = terminus + years(1), along.with = 1:HORIZON, by = "year")
  }
  if (ECON) {
    if (S_R == "S") {
      suffix <- paste0(PROD, "_sales_ECON_method_forecasts.csv")
      target <- data[, c(1, 2)]
    } else {
        suffix <- paste0(PROD, "_returns_ECON_method_forecasts.csv")
        target <- data[, c(1, 4)]
      }
  } else {
    if (S_R == "S") {
      suffix <- paste0(PROD, "_", res, "_sales_OPER_method_forecasts.csv")
      target <- data[, c(1, 2)]
    } else {
        suffix <- paste0(PROD, "_", res, "_returns_OPER_method_forecasts.csv")
        target <- data[, c(1, 4)]
      }
  }
  if (method == "wavg" | (method %in% c("dynreg", "nn", "avg") & hilo == TRUE)) {file <- read.table(paste0(OUTPUT, suffix), header = TRUE, sep = ",")}
  if (method %in% c("avg", "wavg")) {avg_these <- c("snaive", "ma5", "ma7", "ma9", "ma12", "ets", "stl", "stlf", "tbats", "arimax", "dynreg", "nn")}



  # 'forecast' package expects time series objects, so we will comply
  # NB! The series representation given by ts() is only interested in the
  # year and a secondary specifier of days. So a combination of 'start = c(2004, 2), frequency = 365'
  # will start counting the values as days starting from Jan 2st 2004, whereas 'start = c(2004, 2), frequency = 12'
  # will begin Feb 1st 2004. MEANING THAT 'start = c(2004, 13), frequency = 12' would begin at Jan 1st 2005!
  series <- ts(visible[, coln], start = ts_start, frequency = resolution)

  # ---
  # train / fit ----
  # ---
  if (method == "ma5") {fit <- ma(series, order = 5)}
  if (method == "ma7") {fit <- ma(series, order = 7)}
  if (method == "ma9") {fit <- ma(series, order = 9)}
  if (method == "ma12") {fit <- ma(series, order = 12)}
  if (method == "ets") {fit <- ets(series)}
  if (method == "stl") {fit <- stl(series, s.window = 7)}
  if (method == "tbats") {fit <- tbats(series)}
  if (method == "nn") {fit <- nnetar(series)}
  # handle special fit cases
  if (method == "arimax") {
    # Create xregs for ARIMAX
    nrow <- length(series)
    onehotyear <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                       ncol = 11,
                       byrow = TRUE)

    month.m <- matrix(, nrow = nrow, ncol = 11)
    for (j in 1:nrow) {
      if (res == "daily") {
        month.m[j,] <- onehotyear[month(genesis + days(j - 1)), ]
      }
      if (res == "weekly") {
        month.m[j,] <- onehotyear[month(genesis %w+% weeks(j - 1)), ]
      }
      if (res == "monthly") {
        month.m[j,] <- onehotyear[month(genesis %m+% months(j - 1)), ]
      }
      if (res == "yearly") {
        month.m[j,] <- onehotyear[month(genesis %y+% years(j - 1)), ]
      }
    }
    fit <- auto.arima(series, xreg = month.m)}
  if (method == "dynreg") {
    fit_lm <- tslm(series ~ trend + season)
    fcast_lm <- forecast(fit_lm, h = HORIZON)

    fit_resid_arima <- auto.arima(fit_lm$residuals)
    fcast_resid_arima <- forecast(fit_resid_arima, h = HORIZON)
  }

  # ---
  # create forecasts  ----
  # ---
  if (method %in% c("snaive", "stlf", "arimax", "dynreg", "nn", "avg", "wavg")) {   # handle special fcast cases
    if (method == "snaive") {fcast <- data.frame(snaive(series, h = HORIZON))}
    if (method == "stlf") {fcast <- data.frame(stlf(series, h = HORIZON))}
    if (method == "arimax") {
      horizon.m <- matrix(, nrow = HORIZON, ncol = 11)
      k <- 0
      for (j in nrow:(nrow + HORIZON - 1)) {
        k <- k + 1
        if (res == "daily") {
          horizon.m[k,] <- onehotyear[month(genesis + days(j)), ]
        }
        if (res == "weekly") {
          horizon.m[k,] <- onehotyear[month(genesis + weeks(j)), ]
        }
        if (res == "monthly") {
          horizon.m[k,] <- onehotyear[month(genesis %m+% months(j)), ]
        }
        if (res == "yearly") {
          horizon.m[k,] <- onehotyear[month(genesis %y+% years(j)), ]
        }
      }
      fcast <- data.frame(forecast(fit, xreg = horizon.m, h = HORIZON))
    }
    if (method == "dynreg") {
      y <- as.numeric(fcast_lm$mean)
      x <- as.numeric(fcast_resid_arima$mean)
      fcast <- data.frame(x + y)
    }
    if (method == "nn") {fcast <- data.frame(as.numeric(forecast(fit, h = HORIZON)$mean))} # this is ugly but mandatory
    if (method == "avg") {
      fsum <- rep(0, HORIZON)
      for (i_m in avg_these) {
        fsum <- fsum + get_forecast(data, S_R, method = i_m, hilo = FALSE, PROD, TRAIN_LEN, HORIZON, OUTPUT, ECON)[, 2]
      }
      fcast <- data.frame(round(fsum/length(avg_these)))
    }
    if (method == "wavg") {
      if (ECON) {
        rollsums <- c()
        dates <- seq.Date(from = target[24, 1], length.out = (nrow(target) - 24), by = "month")
        for (i in 1:(nrow(target) - 24)){
          rollsums[i] <- sum(target[i:(i + 23), 2])
        }
        target <- data.frame(date = dates, value = rollsums)
      }
      acc <- get_errors(target, file)
      wavg_obj <- wavg_selection(acc)
      fm <- matrix(, nrow = HORIZON, ncol = length(avg_these))
      for (i_m in 1:length(avg_these)) {
        fm[, i_m] <- get_forecast(data, S_R, method = avg_these[i_m], hilo = FALSE, PROD, TRAIN_LEN, HORIZON, OUTPUT, ECON)[, 2]
      }
      fcast <- data.frame(rowSums(t(t(fm[, wavg_obj$order]) * wavg_obj$coef)))

    }
  } else {
    fcast <- data.frame(forecast(fit, h = HORIZON))
  }

  if (hilo) {
    # handle special hilo cases (bootstrap)
    if (method %in% c("dynreg", "nn", "avg", "wavg")) {
      if (ECON) {file[, -1] <- round(file[, -1] / 24)}
      if (method == "dynreg") {
        history <- data.frame(date = file[, 1], value = file$dynreg)
        hilo <- bootstrap_interval(data[, c(1, coln)], history, h = HORIZON, sampleN = 10000)
      }
      if (method == "nn") {
        history <- data.frame(date = file[, 1], value = file$nn)
        hilo <- bootstrap_interval(data[, c(1, coln)], history, h = HORIZON, sampleN = 10000)
      }
      if (method == "avg") {
        history <- data.frame(date = file[, 1], value = file$avg)
        hilo <- bootstrap_interval(data[, c(1, coln)], history, h = HORIZON, sampleN = 10000)
      }
      if (method == "wavg") {
        history <- data.frame(date = file[, 1], value = file$wavg)
        hilo <- bootstrap_interval(data[, c(1, coln)], history, h = HORIZON, sampleN = 10000)
      }

      # build a df to return
      df <- data.frame(date = forecast_dates, forecast = fcast[, 1], Lo80 = hilo[, 1], Hi80 = hilo[, 2], Lo95 = hilo[, 3], Hi95 = hilo[, 4])
    } else {
        df <- data.frame(date = forecast_dates, forecast = fcast[, 1], Lo80 = fcast[, 2], Hi80 = fcast[, 3], Lo95 = fcast[, 4], Hi95 = fcast[, 5])
      }
  } else {
      df <- data.frame(date = forecast_dates, forecast = fcast[, 1])
    }

  # ---
  # transforms (if needed) ----
  # ---
  if (resolution == 12 & !(method %in% c("wavg", "avg"))) {
    if (method %in% c("nn", "dynreg")) {
      df[, c(1, 2)] <- workday_adjustment(df[, c(1, 2)], reverse = TRUE)
    } else {
        df <- workday_adjustment(df, reverse = TRUE)
      }
    df[, -1] <- round(df[, -1])
    # force out subzeroes
    # NB! this will ruin the probability mass distribution in the intervals if they have subzeroes
    # typical data transformations would guarantee correct PI, alas they do not work with zeroes
    df[df < 0] <- 0
  } else {
    df[, -1] <- round(df[, -1])
    df[df < 0] <- 0
    }

  if (method %in% c("wavg", "avg", "nn", "dynreg") & dim(df)[2] == 6) {
    df[, c(3, 4, 5, 6)] <- df[, c(3, 4, 5, 6)] * df[, 2]/df[1, 2]
  }
  # ---
  # save ----
  # ---
  if (save) {
    fdate <- df[1, 1]
    if (ECON) {
      if (S_R == "S") {
        savepath <- paste0(OUTPUT, PROD, "_sales_ECON_forecasts.csv")
        file <- read.table(savepath, header = T, sep = ","); file[, 1] <- as.Date(file[, 1])
        if (file[nrow(file), 1] < df[1, 1]) {
          datasum <- sum(tail(data[, 2], 23))
          rollfcast <- datasum + df[1, 2]
          tobesaved <- data.frame(date = fdate, method = method, forecast = rollfcast)
          write.table(tobesaved, file = savepath, append = T, sep = ",", col.names = F, row.names = F)
        }

      } else {
          savepath <- paste0(OUTPUT, PROD, "_returns_ECON_forecasts.csv")
          file <- read.table(savepath, header = T, sep = ","); file[, 1] <- as.Date(file[, 1])
          if (file[nrow(file), 1] < df[1, 1]) {
            datasum <- sum(tail(data[, 4], 23))
            rollfcast <- datasum + df[1, 2]
            tobesaved <- data.frame(date = fdate, method = method, forecast = rollfcast)
            write.table(tobesaved, file = savepath, append = T, sep = ",", col.names = F, row.names = F)
          }
        }
    } else {
        if (S_R == "S") {
          savepath <- paste0(OUTPUT, PROD, "_", res, "_sales_OPER_forecasts.csv")
          file <- read.table(savepath, header = T, sep = ","); file[, 1] <- as.Date(file[, 1])
          if (file[nrow(file), 1] < df[1, 1]) {
            fcast <- df[1, 2]
            tobesaved <- data.frame(date = fdate, method = method, forecast = fcast)
            write.table(tobesaved, file = savepath, append = T, sep = ",", col.names = F, row.names = F)
          }
        } else {
            savepath <- paste0(OUTPUT, PROD, "_", res, "_returns_OPER_forecasts.csv")
            file <- read.table(savepath, header = T, sep = ","); file[, 1] <- as.Date(file[, 1])
            if (file[nrow(file), 1] < df[1, 1]) {
              fcast <- df[1, 2]
              tobesaved <- data.frame(date = fdate, method = method, forecast = fcast)
              write.table(tobesaved, file = savepath, append = T, sep = ",", col.names = F, row.names = F)
            }
          }

      }
  }


  # ---
  # return ----
  # ---
  return(df)

}

draw_forecast <- function(forecast, data, ECON, OUTPUT, selected_method, RES, PROD) {
  # Plotting helper for displaying forecasts w/series
  # ---
  # variables from arguments ----
  # ---
  if (ECON) { # load selected forecast history
    fh_suffix <- paste0(PROD, "_sales_ECON_forecasts.csv")
    mh_suffix <- paste0(PROD, "_sales_ECON_method_forecasts.csv")
    forecast <- head(forecast, -12) # there is 12 months too much for plotting purposes in ECON
  } else {
      fh_suffix <- paste0(PROD, "_", RES, "_sales_OPER_forecasts.csv")
      mh_suffix <- paste0(PROD, "_", RES, "_sales_OPER_method_forecasts.csv")
    }
  forecast_history <- read.table(paste0(OUTPUT, fh_suffix), header = TRUE, sep = ",", colClasses = c("Date", "character", NA))[, c(1, 3)] # we use type detection for the numerics (=NA), because, for some reason, it doesn't understand it if we put it in explicitly??

  if (PROD == "RBC") {titlefix <- "Punasolut"}
  if (PROD == "PLAT") {titlefix <- "Trombosyytit"}
  if (!(PROD %in% c("RBC", "PLAT"))) {titlefix <- PROD}

  if (selected_method == "snaive") {method <- "kausittainen naivi"; coln <- 2}
  if (selected_method == "ma5") {method <- "liukuva keskiarvo (5)"; coln <- 3}
  if (selected_method == "ma7") {method <- "liukuva keskiarvo (7)"; coln <- 4}
  if (selected_method == "ma9") {method <- "liukuva keskiarvo (9)"; coln <- 5}
  if (selected_method == "ma12") {method <- "liukuva keskiarvo (12)"; coln <- 6}
  if (selected_method == "ets") {method <- "eksponentiaalinen tasoitus"; coln <- 7}
  if (selected_method == "stl") {method <- "kausitettu eksponentiaalinen tasoitus"; coln <- 8}
  if (selected_method == "stlf") {method <- "monikausitettu eksponentiaalinen tasoitus"; coln <- 9}
  if (selected_method == "tbats") {method <- "TBATS"; coln <- 10}
  if (selected_method == "arimax") {method <- "ARIMAX"; coln <- 11}
  if (selected_method == "dynreg") {method <- "dynaaminen regressio"; coln <- 12}
  if (selected_method == "nn") {method <- "neuroverkko"; coln <- 13}
  if (selected_method == "avg") {method <- "metodikeskiarvo"; coln <- 14}
  if (selected_method == "wavg") {method <- "painotettu metodikeskiarvo"; coln <- 15}
  method_history <- read.table(paste0(OUTPUT, mh_suffix), header = TRUE, sep = ",")[, c(1, coln)]; method_history[, 1] <- as.Date(method_history[, 1])
  if (RES == "daily") {
    past_length <- 365
    h <- tail(data[, 1], past_length)
    data_trend_len_long <- 180
    fcast_trend_len <- nrow(forecast)
    titl <- paste0(titlefix, ", ", nrow(forecast), " päivän ennuste")
    daterange <- seq.Date(from = h[1], length.out = (length(h) + nrow(forecast)), by = "day")
  }
  if (RES == "weekly") {
    past_length <- 6 * 52
    h <- tail(data[, 1], past_length)
    data_trend_len_long <- 156
    fcast_trend_len <- nrow(forecast)
    titl <- paste0(titlefix, ", ", nrow(forecast), " viikon ennuste")
    daterange <- seq.Date(from = h[1], length.out = (length(h) + nrow(forecast)), by = "week")
  }
  if (RES == "monthly") {
    past_length <- 10 * 12
    h <- tail(data[, 1], past_length)
    data_trend_len_long <- 60
    fcast_trend_len <- nrow(forecast)
    titl <- paste0(titlefix, ", ", nrow(forecast), " kuukauden ennuste")
    daterange <- seq.Date(from = h[1], length.out = (length(h) + nrow(forecast)), by = "month")
  }
  if (RES == "yearly") {
    past_length <- 15
    h <- tail(data[, 1], past_length)
    data_trend_len_long <- 5
    fcast_trend_len <- nrow(forecast)
    titl <- paste0(titlefix, ", ", nrow(forecast), " vuoden ennuste")
    daterange <- seq.Date(from = h[1], length.out = (length(h) + nrow(forecast)), by = "year")
  }
  n <- length(daterange)

  daterange_df <- data.frame(date = daterange) # So that we can left_join everything
  series_v <- tail(data[, c(1, 2)], past_length) # cut to view
  if (ECON) { # scaleback the 24mo ROLLSUMS
    forecast_history[, -1] <- forecast_history[, -1] / 24
    method_history[, -1] <- method_history[, -1] / 24
  }

  master_df <- daterange_df %>%
    left_join(series_v, by = "date") %>%
    left_join(forecast, by = "date") %>%
    left_join(forecast_history, by = "date") %>%
    left_join(method_history, by = "date")

  colnames(master_df) <- c("date", "sales", "fcast", "lo80", "hi80", "lo95", "hi95", "fhistory", "mhistory")
  master_df$fcast <- coalesce(master_df$fcast, master_df$fhistory)

  trend_l <- find_trend(master_df[, c(1, 2)], window_size = data_trend_len_long)
  trend_f <- find_trend(master_df[, c(1, 3)], window_size = fcast_trend_len)

  master_df <- master_df %>%
    left_join(trend_l, by = "date") %>%
    left_join(trend_f, by = "date")
  colnames(master_df) <- c("date", "sales", "fcast", "lo80", "hi80", "lo95", "hi95", "fhistory", "mhistory", "ltrend", "ftrend")

  past_error <- round(mean(get_errors(master_df[, c("date", "sales")], master_df[, c("date", "fhistory")])[, 2], na.rm = T), 2)
  method_error <- round(mean(get_errors(master_df[, c("date", "sales")], master_df[, c("date", "mhistory")])[, 2], na.rm = T), 2)


  ggplot(data = master_df) +
    geom_line(aes(x = date, y = sales, colour = "Data"), size = 1.2) +
    geom_ribbon(aes(x = date, ymin = lo95, ymax = hi95), fill = "#D5DBFF") +
    geom_ribbon(aes(x = date, ymin = lo80, ymax = hi80), fill = "#596DD5") +
    geom_line(aes(x = date, y = fcast, colour = "Ennuste"), size = 1.2) +
    #geom_segment(aes(x = daterange[36], xend = daterange[37],
    #            y = sales[36], yend = fcast[37]), linetype = "dashed") +
    geom_line(aes(x = date, y = ltrend, colour = "5v trendi (data)"), size = 1.2) +
    geom_line(aes(x = date, y = ftrend, colour = "Mallin trendi"), size = 1.2) +
    annotate("text", x = mean.Date(daterange), y = max(master_df$hi95, na.rm = T), label = paste0("Valittu menetelmä: ", method, ". Menetelmän keskivirhe: ", method_error, " %."), size = 5) +
    scale_colour_manual(values = c("Data" = "#000000", "Ennuste" = "#FF6757", "5v trendi (data)" = "#F1D302", "Mallin trendi" = "#E3E3E3")) +
    theme_minimal() +
    labs(title = titl,
         subtitle = paste0("Ennustimen keskivirhe: ", past_error, " %."),
         x = "Aika",
         y = "Tuotetta",
         color = "Väri") +
    scale_x_date(date_labels = "%m/%Y") +
    theme(text = element_text(size = 20),
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "transparent")
    )
}

table_current_year <- function(data, comb_forecast, PROD) {

  if (PROD == "RBC") {prefix <- "Punasolut"}
  if (PROD == "PLAT") {prefix <- "Trombosyytit"}
  if (PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {prefix <- PROD}

  last_year_in_data <- year(data[nrow(data), 1])
  data_part <- data %>% filter(year(date) == last_year_in_data) %>% mutate(date = paste0("Data\n", month(date), "/", year(date)))
  rotated_data <- data_part %>%
    gather(sales, deliveries, -date) %>%
    spread(date, deliveries) %>%
    select(data_part$date); rownames(rotated_data) <- c("Toimitukset", "Palautukset", "Myynti")
  fcast_part <- comb_forecast[, c(1, 2, 7, 8)] %>% filter(year(date) == last_year_in_data) %>% mutate(date = paste0("Ennuste\n", month(date), "/", year(date)))
  colnames(fcast_part) <- c("date", "sales", "deliveries", "returns")
  rotated_fcast <- fcast_part %>%
    gather(sales, deliveries, -date) %>%
    spread(date, deliveries) %>%
    select(fcast_part$date); rownames(rotated_fcast) <- c("Toimitukset", "Palautukset", "Myynti")
  this_year_df <- cbind(rotated_data, rotated_fcast)
  sums <- c(sum(this_year_df[1, ]), sum(this_year_df[2, ]), sum(this_year_df[3, ]))
  this_year_table <- cbind(this_year_df, Yhteensä = sums)
  datatable(this_year_table, rownames = TRUE, filter = "none", extensions = 'Buttons', options = list(pageLength = 5,
                                                                                                      scrollX = T,
                                                                                                      dom = 'tB',
                                                                                                      buttons = list("copy",
                                                                                                                     list(extend = "excel",
                                                                                                                          filename = paste0(prefix, "_TE_", last_year_in_data),
                                                                                                                          title = paste0(prefix, " | Toteuma ja ennuste | ", last_year_in_data)),
                                                                                                                     list(extend = "csv",
                                                                                                                          filename = paste0(prefix, "_TE_", last_year_in_data)),
                                                                                                                     list(extend = "pdf",
                                                                                                                          filename = paste0(prefix, "_TE_", last_year_in_data),
                                                                                                                          title = paste0(prefix, " | Toteuma ja ennuste | ", last_year_in_data),
                                                                                                                          pageSize = "LEGAL",
                                                                                                                          orientation = "landscape")),
                                                                                                      fixedColumns = list(leftColumns = 1)))
}

table_yearly <- function(data, forecast, PROD, S_R = "S"){
  # This function is currently ECON specific, so we hardcode everything

  if (PROD == "RBC") {prefix <- "Punasolut"}
  if (PROD == "PLAT") {prefix <- "Trombosyytit"}
  if (PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {prefix <- PROD}

  if (S_R == "S") {
    infix <- "myynti"
    data <- data[, c(1, 2)]
    forecast <- forecast[, c(1, 2)]
  }
  if (S_R == "R") {
    infix <- "palautukset"
    data <- data[, c(1, 4)]
    forecast <- forecast[, c(1, 8)]
  }

  dataslope <- find_trend(data, window_size = 60, out = "slope") * 30
  dtrend <- data.frame(date = forecast$date, value = round(cumsum(rep(dataslope, 72)) + data[nrow(data), 2]))
  ftrend  <- find_trend(forecast, window_size = nrow(forecast), out = "vec")

  colnames(forecast) <- c("date", "value")
  colnames(ftrend) <- c("date", "value")

  agg_f <- aggregate(value ~ year(date), data = forecast, FUN = sum)
  agg_dt <- aggregate(value ~ year(date), data = dtrend, FUN = sum)
  agg_ft <- aggregate(value ~ year(date), data = ftrend, FUN = sum)
  agg_df <- data.frame(year = agg_f[-1, 1], dtrend = agg_dt[-1, 2], fcast = agg_f[-1, 2], ftrend = agg_ft[-1, 2])
  tabled <- t(agg_df[, 2:4]); colnames(tabled) <- agg_df[, 1]; rownames(tabled) <- c("Datan trendi (5v)", "Ennuste", "Ennusteen trendi")

  if (agg_f[nrow(agg_f), 1] > (year(data[nrow(data), 1]) + 5)) {tabled <- tabled[, -dim(tabled)[2]]} # if we go over 5 years, cut the last column

  datatable(tabled, rownames = TRUE, filter = "none", extensions = 'Buttons', options = list(pageLength = 5,
                                                                                             scrollX = T,
                                                                                             dom = 'tB',
                                                                                             buttons = list("copy",
                                                                                                            list(extend = "excel",
                                                                                                                 filename = paste0(prefix, "_", infix, "5y_TRND"),
                                                                                                                 title = paste0(prefix, " | ", infix, " | Ennuste ja trendit | 5v")),
                                                                                                            list(extend = "csv",
                                                                                                                 filename = paste0(prefix, "_", infix, "5y_TRND")),
                                                                                                            list(extend = "pdf",
                                                                                                                 filename = paste0(prefix, "_", infix, "5y_TRND"),
                                                                                                                 title = paste0(prefix, " | Ennuste ja trendit | 5v"),
                                                                                                                 pageSize = "LEGAL",
                                                                                                                 orientation = "landscape")),
                                                                                             fixedColumns = list(leftColumns = 1)))
}

table_generic <- function(comb_forecast, PROD) {
  # This function is currently OPER specific, so we hardcode everything

  if (PROD == "RBC") {prefix <- "Punasolut"}
  if (PROD == "PLAT") {prefix <- "Trombosyytit"}
  if (PROD %in% c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-")) {prefix <- PROD}

  res <- find_resolution(comb_forecast[, 1], format = "label")
  if (res == "daily") {cnames <- format(comb_forecast[, 1], "%d %b %Y")}
  if (res == "weekly") {cnames <- paste("Viikko", week(comb_forecast[, 1]))}
  if (res == "monthly") {cnames <- paste(month(comb_forecast[, 1], label = TRUE), year(comb_forecast[, 1]))}
  if (res == "yearly") {cnames <- year(comb_forecast[, 1])}

  sales <- t(comb_forecast[, 2])
  deliv <- t(comb_forecast[, 7])
  ret <- t(comb_forecast[, 8])
  df <- rbind(sales, ret, deliv); rownames(df) <- c("Myynti", "Palautukset", "Toimitukset"); colnames(df) <- cnames

  datatable(df, rownames = TRUE, filter = "none", extensions = 'Buttons', options = list(pageLength = 5,
                                                                                                      scrollX = T,
                                                                                                      dom = 'tB',
                                                                                                      buttons = list("copy",
                                                                                                                     list(extend = "excel",
                                                                                                                          filename = paste0(prefix, "_E_", today()),
                                                                                                                          title = paste0(prefix, " | Ennutsennuste | ", today())),
                                                                                                                     list(extend = "csv",
                                                                                                                          filename = paste0(prefix, "_E_", today())),
                                                                                                                     list(extend = "pdf",
                                                                                                                          filename = paste0(prefix, "_E_", today()),
                                                                                                                          title = paste0(prefix, " | Ennuste | ", today()),
                                                                                                                          pageSize = "LEGAL",
                                                                                                                          orientation = "landscape")),
                                                                                                      fixedColumns = list(leftColumns = 1)))
}
