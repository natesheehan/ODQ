###########################################################################
###########################################################################
###                                                                     ###
###                 BUILD DATAFRAME OF RE3 REPOSITOIRES                 ###
###                                                                     ###
###########################################################################
###########################################################################
# Step 1: Fetch XML content from the URL
re3_url_repos = "https://www.re3data.org/api/v1/repositories"
re3_url_repo = "https://www.re3data.org/api/v1/repository/"

# Step 2: Parse data from URL
raw_data = xml2::read_xml(re3_url_repos)

# Step 3: Collect Name and ID
name = xml2::xml_text(xml2::xml_find_all(raw_data, xpath = "//name"))
id = xml2::xml_text(xml2::xml_find_all(raw_data, xpath = "//id"))

# Step 4: Merge into table
re3_ids = tibble::tibble(name = name, id = id)

# Step 5: Save data
saveRDS(re3_ids,"data/re3_ids.RDS")

# Step 6: Set up core cluster
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
clusterExport(cl, list("re3_ids", "re3_url_repo", "read_xml", "xml_text", "xml_find_all"))
clusterEvalQ(cl, library(xml2))

# Step 7: Create Function to process metadata
process_id <- function(i) {
  cat(paste0("OK! HERE WE GO. Printing for ", re3_ids$name[i]), "\n")

  repo_data = read_xml(paste0(re3_url_repo, re3_ids$id[i]))
  repo_names = xml_text(xml_find_all(repo_data, xpath = "//r3d:additionalName"))
  repo_access = xml_text(xml_find_all(repo_data, xpath = "//r3d:databaseAccessType"))

  list(
    access = repo_access,
    names = paste(repo_names, collapse = ",")
  )
}

# Step 8 Merge Data
results <- parLapply(cl, 1:nrow(re3_ids), process_id)
for (i in 1:nrow(re3_ids)) {
  re3_ids$access[i] = results[[i]]$access
  re3_ids$names[i] = results[[i]]$names
}

# Step 9: Save data
saveRDS(re3_ids,"data/re3_ids.RDS")

# Step 5: Save data
saveRDS(re3_ids,"data/re3_ids.RDS")
