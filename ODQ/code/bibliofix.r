dsApi2df <- function(P, format = "bibliometrix"){

  query <- P$query
  item <- P$item
  P <- P$data

  switch(item,
         publications={
           df <- pub2df(P,format)
         },
         grants={
           df <- grants2df(P)
         },
         patents={
           df <- patents2df(P)
         },
         clinical_trials={
           df <- clinicaltrials2df(P)
         },
         policy_documents={
           df <- policydocuments2df(P)
         })

  return(df)

}

#### Publications ####
pub2df <- function(P, format){

  n <- length(P)


  ### Data Conversion

  df <- data.frame(AU=rep(NA,n), AF="NA",TI="NA", SO="NA", SO_LIST=NA, LA="English", DT=NA,DE=NA,ID=NA,AB="NA",C1=NA,RP=NA,OI=NA,FU=NA,CR=NA,
                   ALT=NA, TC=NA, TCR=NA,PU=NA,SN=NA, J9=NA, JI=NA, PY=NA, VL=NA, IS=NA, DI=NA, PG=NA, SC=NA, OA=NA, URL=NA, DB="DIMENSIONS",
                   AU_UN=NA, AU1_UN=NA, AU_CO=NA, AU1_CO=NA, SR_FULL=NA,  stringsAsFactors = FALSE)

  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, char = "=")

  for (i in 1:n) {
    #if (i%%100==0 | i==n) cat("Documents converted  ",i,"of",n, "\n")
    #print(i)
    utils::setTxtProgressBar(pb, i)
    if (P[[i]]$type %in% c("article", "chapter", "preprint")){
      a <- list2char(P[[i]])

      items<- names(a)
      ## Document Type
      df$DT[i] <- a["type"]

      df$AB[i] <- a["abstract"]

      ## Title
      df$TI[i] <- a["title"]

      ## Publication Year
      df$PY[i] <- a["year"]

      ### Co-Authors
      AU_last_ind <- which(items == "authors.last_name")
      AU_first_ind <- which(items == "authors.first_name")
      name <-  paste(a[AU_last_ind], a[AU_first_ind], sep=", ")
      df$AF[i] <- paste(name, collapse = ";")

      ## Countries
      CO_ind <- which(items == "authors.affiliations.country")
      country <- a[CO_ind]

      ## Affiliations
      Aff_name_ind <- which(items == "authors.affiliations.name")
      Affiliations <- a[Aff_name_ind]

      Aff_city_ind <- which(items == "authors.affiliations.city")
      city <- a[Aff_city_ind]

      df$C1[i] <- paste(Affiliations, country, sep=", ", collapse=";")

      ## Author's countries
      df$AU_CO[i] <- paste(country, collapse = ";")

      ## Author's Affilaiiton standardized
      df$AU_UN[i] <- paste(Affiliations, collapse = ";")

      ## Corresponding Author
      AU_corr <- which(items =="authors.corresponding")
      j <- which(a[AU_corr]=="TRUE")[1]
      if (is.na(j)) j <- 1

      df$RP[i] <- paste(Affiliations[j],country[j],sep=",",collapse=";")
      df$AU1_UN[i] <- Affiliations[j]
      df$AU1_CO[i] <- country[j]



      ## Subject categories
      SC_ind <- which(items == "category_for.name")
      df$SC[i] <- trimws(gsub('[[:digit:]]+', '', paste(a[SC_ind], collapse =";")))


      ## Keywords
      ID_ind <- which(regexpr("concepts",items)>-1)
      df$ID[i] <- df$DE[i] <- paste(a[ID_ind],collapse=";")

      ## Journals

      SO_ind <- which(items %in% c("journal.title", "book_title"))
      df$SO[i] <- a[SO_ind[1]]


      ## Doi
      df$DI[i] <- a["doi"]

      ## Journal List
      SO_list_ind <- which(regexpr("journal_lists",items)>-1)
      df$SO_LIST[i] <- paste(a[SO_list_ind],collapse=";")

      ## URL
      df$URL[i] <- a["linkout"]

      ## Total Citations
      df$TC[i] <- a["times_cited"]

      ## Altmetrics
      df$ALT[i] <- a["altmetric"]

      ## Recent TC
      df$TCR[i] <- a["recent_citations"]


      ## References
      CR_ind <- which(regexpr("reference_ids",items)>-1)
      df$CR[i] <- paste(a[CR_ind], collapse = ";")

      ## ISSN
      df$SN[i] <- a["issn"]

      ## Pages
      df$PG[i] <- a["pages"]

      ## Founders
      FU_name_ind <- which(regexpr("funders.name",items)>-1)
      FU_acronym_ind <- which(regexpr("funders.acronym",items)>-1)
      FU_city <- which(regexpr("funders.city_name",items)>-1)
      FU_country <- which(regexpr("funders.country_name",items)>-1)
      df$FU[i] <- paste(a[FU_name_ind],a[FU_acronym_ind],a[FU_city],a[FU_country],sep=",",collapse=";")

      ## Publisher
      df$PU[i] <- a["publisher"]

      ## Volume
      df$VL[i] <- a["volume"]

      ## Issue
      df$IS[i] <- a["issue"]

      ## Orcid ID
      OI_orcid_ind <- which(items == "researchers.orcid_id")

      df$OI[i] <- paste(a[OI_orcid_ind],collapse=";")

      ## Open Access
      oa_values <- c(a["open_access"], a["open_access1"], a["open_access2"])[!is.na(c(a["open_access"], a["open_access1"], a["open_access2"]))]
      df$OA[i] <- paste0(oa_values, collapse=";")


    }
  }


  if (format == "bibliometrix") {
    DI <- df$DI
    URL <- df$URL
    df <- data.frame(lapply(df, toupper), stringsAsFactors = FALSE)
    df$DI <- DI
    df$URL <- URL
  }

  ### PY
  df$PY <- as.numeric(df$PY)

  ### TC and TCR
  df$TCR <- as.numeric(df$TCR)
  df$TCR[is.na(df$TCR)] <- 0
  df$TC <- as.numeric(df$TC)
  df$TC[is.na(df$TC)] <- 0

  ###  remove empy rows
  df=df[!is.na(df$DT),]

  ### Author AU

  df$AU <- df$AF

  df$AU <- gsub("\\s+", " ", df$AU)
  df$AU <- trimws(gsub("\\(|\\)","",df$AU))

  listAU <- strsplit(df$AU, ";")

  AU <- lapply(listAU, function(l) {
    lastname <- trimws(gsub(",.*", "", l))
    firstname <- strsplit(trimws(gsub(".*,", "", l)), " ")
    i <- which(nchar(lastname)<2)
    if (length(i)>0){
      lastname <- lastname[-i]
      firstname <- firstname[-i]
    }
    firstname <- lapply(firstname, function(x) {
      if (length(x) > 0) {
        x <- paste(substr(x, 1, 1), sep = "", collapse = "")
      } else {
        x = ""
      }
      return(x)
    })
    AU <- paste(lastname,
                unlist(firstname),
                sep = " ",
                collapse = ";")
    return(AU)
  })

  df$AU <- unlist(AU)
  df$AU[df$AU=="NA N"] <- NA

  #### To Add in convert2df
  ### SR field creation
  #suppressWarnings(df <- metaTagExtraction(df, Field="SR"))

  #row.names(df) <- df$SR
  close(pb)

  return(df)

}

