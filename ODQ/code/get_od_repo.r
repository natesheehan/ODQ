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


test = read_xml(paste0(re3_url_repo,re3_ids$id[69]))

xml_text(xml_find_all(test, xpath = "//r3d:additionalName"))
xml_text(xml_find_all(test, xpath = "//r3d:dataAccessType"))
