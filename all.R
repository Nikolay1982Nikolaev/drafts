
rm(list=ls())
library(haven)
library(dplyr)
library(assertthat)
library(tidyverse)
library(stringr)

###############################  extract DIAG data form folder data ###############################
dir_path <- paste0(getwd(), "/data_MO16432")
files  <- list.files(dir_path)

for (file in files) {

    f <- toupper(strsplit(file, "\\.")[[1]][1])
    print(f)
    g <- grepl("DIAG", f, fixed = TRUE)
    print(g)

    if (g == TRUE) {
        print("OK===")

        df_gdm <- read_sas(paste0(dir_path, "/",file))

        df_copy <- data.frame(df_gdm)
    }
}





################# extract metaqdata ###############################################################

names_diag <- names(df_copy)                            # names DIAG

dom <- "MH"                                             # create domain


###################################################################################################
# if missing directory TEXT_OUTPUT, will be created. There we will store the text files for
# diferent processed domain
dir_path <- getwd()
files  <- list.files(dir_path)

if ("TEXT_OUTPUT" %in% files) {
    print("The file TEXT_OUTPUT exisiting in current directory")
} else {
    dir.create("TEXT_OUTPUT")
}

#################################################################################
dir_path <- paste0(getwd(), "/TEXT_OUTPUT/")
files  <- list.files(dir_path)
new_file <- paste0(dom, "_text_output.txt")
length(files)

if (length(files) == 0) {
    file.create(paste0(dir_path, new_file))
    curren_time <- Sys.time()
    message_01 <- paste("The file was created at", curren_time)
    write(message_01, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
} else {
    print("The file is already available !!!")
}

################################################################################
#### DOMAIN variable
# check existance of variable name
var <- "DOMAIN"
if (var %in% names(df_copy)) {
    print("The var already exist")
} else {
    print("The var NOT exist yet !!!")
}


df_copy$DOMAIN <- dom

if (var %in% names(df_copy)) {
    print("The var already exist")
} else {
    print("The var NOT exist yet !!!")
}

print(unique(df_copy[[var]]))


message1 <- "Was created the variable DOMAIN"
message2 <- paste("The uniq values are: ", unique(df_copy[var]))

write(message1, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write(message2, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)



############################## STUDYID ############################################
var <- "PROTO"
if (var %in% names(df_copy)) {
    print("The var already exist")
    bool <- TRUE
} else {
    print("The var NOT exist yet !!!")
    bool <- FALSE
}



col_values <- as.vector(df_copy$PROTO)

if (bool == TRUE) {
    df_copy$STUDYID <- col_values
}
uniques_values <- unique(df_copy$STUDYID)

message1 <- "Was created the variable STUDYID"
message2 <- paste("The uniques values are:", uniques_values)
write(message1, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write(message2, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)


##########################################################################
# USUBJID
# derive from PROTO, CRTN, PT
var_new_col <- "USUBJID"
vars <- c("PROTO", "CRTN1", "PT_0")
ex_vars <- c()
for (var in vars) {
    if (var %in% names(df_copy)) {
    print("The var already exist")
    ex_vars <- append(ex_vars,var)
} else {
    print("The var NOT exist yet !!!")
}
}


df_copy[var_new_col] <- ""

message1 <- "Was created the variable USUBJID"
write(message1, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)

if (length(ex_vars) == 3) {
    df_copy <- df_copy %>%  unite(USUBJID,
        c(PROTO, CRTN, PT), sep = "-", remove = FALSE)

    message1 <- "There was all three needed vars: PROTO, PT, CRTN. All is fine."
    write(message1, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)

} else if (length(ex_vars) == 2) {
    var1 <- ex_vars[1]
    var2 <- ex_vars[2]

    df_copy <- df_copy %>%  unite(
        USUBJID,
        c(var1, var2), sep = "-", remove = FALSE)

    message1 <- paste("There was two from three vars:", ex_vars)
    write(message1, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)

} else if (length(ex_vars) == 1) {
    col_values <- pull(df_copy[ex_vars])
    attributes(col_values) <- NULL

    df_copy$USUBJID <- col_values

    message1 <- paste("There was only ONE from three vars. That is not correct. Please check that:", ex_vars)
    write(message1, file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("===========================================================",
    file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)
    write("", file="TEXT_OUTPUT/MH_text_output.txt", append=TRUE)

}


############################# 07.02.2023 ##########################
# Target: MHSPID / MHTERM / MHDECOD / MHPTCD / MHCAT / MHPRESP

# 1. MHSPID : Sponsor def identifier
# chr:200 / NO - core var / upper case / no duplicate check /
# diag.DGPAGE


# finish with PAGE - will be function

# check variable  === function
available_var <- FALSE
ex_var <- ""
for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -4) == "PAGE") {
    available_var <- TRUE
    ex_var <- var

    }
}
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}
available_var
ex_var
df_copy[ex_var]

new_var <- "MHSPID"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}

df_copy[new_var] <- toupper(col_values)


###################################################################
# 2. MHTERM: Reported Term for the Medical History
# char: 200 / core_var=Y / uppercase -Y / dup_check -Y /
# anonymization - conditional remove /
# DGPT
available_var <- FALSE
ex_var <- ""
for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -2) == "PT") {
    available_var <- TRUE
    ex_var <- var

    }
}
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}
available_var
ex_var
df_copy[ex_var]

new_var <- "MHTERM"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}

df_copy[new_var] <- toupper(col_values) # uppercase


#####################################################################
# 3: MHDECOD: Dictionary-Derived Term
# chr:200 / core-N / uppervase-N / dup check - Y
# DGPT
available_var <- FALSE
ex_var <- ""
for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -2) == "PT") {
    available_var <- TRUE
    ex_var <- var

    }
}
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}
available_var
ex_var
df_copy[ex_var]

new_var <- "MHDECOD"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}

df_copy[new_var] <- col_values # uppercase NO


########################################################################
# MHPTCD: Preferred Term Code
# num: 8 / medDRA / core -N /
# DGPT_C : JV16284 and MO16432

available_var <- FALSE
ex_var <- ""
names(df_gdm)
for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -4) == "PT_C") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var

if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var

new_var <- "MHPTCD"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}
new_var
df_copy[new_var] <- col_values # uppercase NO
sapply(df_copy["MHPTCD"], class)

if (sapply(df_copy["MHPTCD"], class) == "numeric") {
    print("OK. The data type is NUMERIC")
}  else {
    print("WRONG data type !!!")
}

################################### WARNING UP #####################################################



####################################################################################################
# MHCAT : Category for Medical History
# chr:100 / do not curate / core-Y / uppercase-Y / dup check - Y
# DIAG.DGTYP
# may be alwasy to UPPERCASE
df_copy["DGTYP"][1:5,]
available_var <- FALSE
ex_var <- ""
names(df_gdm)
for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "TYP") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var

if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var


new_var <- "MHCAT"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["MHCAT"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
}


#############################################################################
# MHPRESP : Medical History Event Pre-Specified
#
# DIAG.DGTYP - derived: ifelse(DIAG.DGTYP == "PRIMARY", 'Y', '')

unique(df_copy$DGTYP) # derived put the message about that

############################################################################
# MHOCCUR	Medical History Occurrence
# DIAG.DGTYP
# derived

###########################################################################
# MHBODSYS	Body System or Organ Class	N	direct	DIAG.DGSCT
# chr: 200 / core - N / upper -N /
# curation guide line: follow up with coding ??????
# DIAG.DGSCT
df_copy["DGSCT"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "SCT") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var

if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var


new_var <- "MHBODSYS"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["MHBODSYS"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }

##########################################################################
# MHBDSYCD	Body System or Organ Class Code	- 	direct
# num: 8, medDRA, upper- N / dup-N
# cur guideline : follow up with coding
# DIAG.DGSCT_C
df_copy["DGSCT_C"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "SCT") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var

if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var


new_var <- "MHBDSYCD"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["MHBODSYS"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }

###############################################################
# MHSOC	Primary System Organ Class Code		direct
# Char	200	/ MedDRA / upper -N / dup check - N
# follow up with coding
# DIAG.DGSCT
df_copy["DGSCT"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "SCT") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var

if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var


new_var <- "MHSOC"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["MHBODSYS"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }


############################################################
# MHSOCCD	Primary System Organ Class Code

# DIAG.DGSCT_C

##########################################################
# VISITNUM	Visit Number
#
#DIAG.DGACE
df_copy["DGACE"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "ACE") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var

if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var


new_var <- "VISITNUM"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["VISITNUM"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }

############################################################
# VISIT	Visit Number	Y	direct
# chr: 200 core-T dup-Y upper -Y

# DIAG.DGCPE
df_copy["DGCPE"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "CPE") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var
available_var
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}

available_var
ex_var


new_var <- "VISIT"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["VISIT"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }


##############################################################
# MHSTDTC	Start Date/Time of Medical History Event	Y	direct
# chr:25 ISO 8601 / core-Y /upper-Y / dup_check - Y /
# DIAG.DGBEGDT
df_copy["DGBEGDT"][1:5,]

# empty column
if (any(is.na(df_copy[,"DGBEGDT"]))) {
    print("Is EMPTY")
}


#########################################################################
# MHSTRTPT	Start Relative to Reference Time Point	N	derived
# chr:20 / core-N / upper-Y / dups -Y /
#DIAG.DGTYP
df_copy["DGTYP"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "TYP") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var
available_var
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}


new_var <- "MHSTRTPT"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["VISIT"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }


#############################################################
# MHSTTPT	Start Reference Time Point
# MHSTRTPT


new_var <- "MHSTTPT"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["VISIT"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }


############################################################
# MHENRTPT	End Relative to Reference Time Point	Y	derived
# Char	20 / core-Y / upper - Y / dup_check - Y
# DIAG.DGACT
df_copy["DGACT"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -3) == "ACT") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var
available_var
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}


new_var <- "MHENRTPT"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["VISIT"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }

#########################################################
# MHENTPT	End Reference Time Point	N	derived
#
# MHENRTPT

df_copy["MHENRTPT"][1:5,]
available_var <- FALSE
ex_var <- ""

for (var in names(df_copy)) {
    #print(var)
    if (str_sub(var, start = -6) == "ENRTPT") {
    available_var <- TRUE
    ex_var <- var

    }
}
ex_var
available_var
if (available_var == TRUE) {
    print("OK. The variable is available in the dataset")
} else {
    print("The variable missing in the dataset !!!")
}


new_var <- "MHENTPT"
df_copy[new_var] <- ""

if (available_var == TRUE) {
    col_values <- pull(df_copy[ex_var])
    attributes(col_values) <- NULL
}


df_copy[new_var] <- col_values # uppercase NO
df_copy[new_var][1:5,]
d_type <- sapply(df_copy["VISIT"], class)
d_t <- d_type[[1]]
if (d_type[[1]]  == "numeric") {
    print("OK. The data type is NUMERIC")
} else if  (d_type[[1]]  == "character") {
    print("OK. IS CHARACTER")
    } else {
    print("ERROR")
    }



#############################################################

