############################################ 1
# take path to data
# if no arg specific dir, then the function return path to data directory

path_to_data_dir <- function(){
    path_to_source <- getwd()
    files  <- list.files(path_to_source)

    for (file in files) {
        file_in <- toupper(file)
        condition <- grepl("DATA", file_in, fixed = TRUE)
        if (condition == TRUE) {
            path_to_data_dir <- paste0(getwd(), "/", file)
        }
    }

    return(path_to_data_dir)
}
############################################ 2
# take GDM data needed
take_gdm_df <- function(files) {
    for (file in files) {
        file_in <- toupper(strsplit(file, "\\.")[[1]][1])
        # here will insert condition to search diferent GDM domain
        condition <- grepl("DIAG", file_in, fixed = TRUE)

        if (condition == TRUE) {
            gdm_df <- read_sas(paste0(path_to_data_dir, "/",file))
        }
    }
    return(gdm_df)
}


########################################## 3
# make copy of GDM data
copy_gdm <- function(gdm_df) {
    copy_gdm  <- data.frame(gdm_df)

    return(copy_gdm)
}


########################################### 4
# create doain with one column DOMAIN
create_domain_variable <- function(domain, n_row) {
    vector <- rep(domain, n_row)
    df <- data.frame(DOMAIN = vector)
    len <- n_row
    uni <- unique(df$DOMAIN)
    msg <- paste("Created DM variable. \nLenght is: ", len, "\nUniques values: ", uni)
    wrtie_message(msg, f)

    return(df)
}
######################################### 5
# create TEXT_OUTPUT function
path_output_dir <- function() {
    files            <- list.files(getwd())

    if (!("TEXT_OUTPUT" %in% files)) {

    dir.create("TEXT_OUTPUT")
    print("The directory TEXT_OUTPUT was created")
    } else {
        "EXISITING"
    }

    path_output_dir <- paste0(getwd(), "/", "TEXT_OUTPUT")

    return(path_output_dir)
}

########################################## 6
path_text_file <- function(domain) {

    path_dir <- paste0(getwd(), "/TEXT_OUTPUT/")
    files   <- list.files(path_dir)
    new_file <- paste0(domain, "_text_output.txt")

    if (!(new_file %in% files)) {
        file.create(paste0(path_dir, new_file))
        print("The file was created !!!")
        } else {
            print("The file is already available !!!")
        }

    path_text_file <- paste0(path_dir, "/", new_file)
    return(path_text_file)
    }

##############################################

























