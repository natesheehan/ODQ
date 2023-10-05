###########################################################################
###########################################################################
###                                                                     ###
###                    FETCH DATA FROM DIMENSIONS.AI                    ###
###                                                                     ###
###########################################################################
###########################################################################

# Step 1: Set up auth
token <- dsAuth(key = dimkey)


# Step 2: Create query
query <- dsQueryBuild(item = "publications",
                      words = re3_ids$name[27],
                      type = "article",
                      full.search = TRUE,
                      start_year = 2013, end_year = 2023,output_fields = "basics + extras + categories")
query

# Step 3: Fetch data
res <- dsApiRequest(token = token, query = query, limit = 0, verbose = TRUE)




res$total_count

D <- dsApiRequest(token = token, query = query, step = 200, limit = res$total_count)

M <- dsApi2df(D)

