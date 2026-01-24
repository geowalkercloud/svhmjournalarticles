## =====================================================================
## scripts/fetch_pubmed.R
## =====================================================================

library(rentrez)
library(dplyr)
library(readr)
library(tibble)
library(stringr)

## ---------------------------------------------------------------------
## 1. NCBI polite use
## ---------------------------------------------------------------------
Sys.setenv(ENTREZ_EMAIL = "humphrey.walker@svhm.org.au")  # change if needed
Sys.setenv(ENTREZ_KEY   = "")  # optional; can leave blank


## ---------------------------------------------------------------------
## 2. PubMed query
##    Logic: (Affiliation block) OR (Author list)
## ---------------------------------------------------------------------
query <- "
(
  (
    \"St Vincent's Hospital\"[Affiliation] OR
    \"St Vincents Hospital\"[Affiliation] OR
    \"St Vincent's\"[Affiliation] OR
    \"St Vincents\"[Affiliation] OR
    \"St Vincent's Hospital, Melbourne\"[Affiliation] OR
    \"St Vincents Hospital, Melbourne\"[Affiliation] OR
    \"St Vincent's Hospital Melbourne\"[Affiliation] OR
    \"St Vincents Hospital Melbourne\"[Affiliation] OR
    \"St Vincent Hospital, Melbourne\"[Affiliation]
  )
  AND
  (
    \"Intensive Care\"[Affiliation] OR
    \"Department of Critical Care\"[Affiliation] OR
    \"Department Critical Care\"[Affiliation] OR
    ICU[Affiliation]
  )
)
AND
(
  Brown A[Author] OR
  Walker H[Author] OR
  Williams D[Author] OR
  Tobin A[Author] OR
  Ghani M[Author] OR
  Haydon T[Author] OR
  Dixon B[Author] OR
  Santamaria J[Author] OR
  Santamaria JD[Author] OR
  Hurune P[Author] OR
  Sakurai K[Author] OR
  Mora JC[Author] OR
  Musca S[Author] OR
  O'Brien Y[Author] OR
  Smit C[Author] OR
  Holmes J[Author] OR
  Luk V[Author] OR
  Reid D[Author] OR
  Smith R[Author]
)
"

cat("PubMed query:\n", query, "\n\n")

## ---------------------------------------------------------------------
## 3. Search PubMed
## ---------------------------------------------------------------------
message("Searching PubMed...")
search_res <- entrez_search(
  db     = "pubmed",
  term   = query,
  retmax = 500  # keep at 500 as agreed
)

if (length(search_res$ids) == 0) {
  message("No results found. Exiting.")
  quit(save = "no")
}

pmids <- search_res$ids
message("Found ", length(pmids), " PMIDs.")


## ---------------------------------------------------------------------
## 4. Fetch summaries for those PMIDs
## ---------------------------------------------------------------------
message("Fetching summaries for ", length(pmids), " PMIDs...")
summ <- entrez_summary(db = "pubmed", id = pmids)


## ---------------------------------------------------------------------
## 5. Convert summaries -> simple tibble
##    Fields: sortpubdate, pubdate, first_author, title, pmid, pubmed_url
## ---------------------------------------------------------------------
safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
}

extract_simple <- function(s) {
  uid <- safe_chr(s$uid)
  
  tibble(
    sortpubdate  = safe_chr(s$sortpubdate),      # good for sorting
    pubdate      = safe_chr(s$pubdate),          # human-readable
    first_author = safe_chr(s$sortfirstauthor),  # first author only
    title        = safe_chr(s$title),
    pmid         = uid,
    pubmed_url   = if (!is.na(uid)) paste0("https://pubmed.ncbi.nlm.nih.gov/", uid, "/")
    else NA_character_
  )
}

df <- bind_rows(lapply(as.list(summ), extract_simple)) %>%
  arrange(desc(sortpubdate), title) %>%
  mutate(pmid = as.character(pmid)) 

## At this point df is your “current run” list:
## sortpubdate, pubdate, first_author, title, pmid, pubmed_url


## ---------------------------------------------------------------------
## 6. Save cumulative + latest-run CSVs
## ---------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)

csv_path <- "data/pubs_all.csv"

if (file.exists(csv_path)) {
  old <- read_csv(csv_path, show_col_types = FALSE) %>%
    mutate(pmid = as.character(pmid))
  combined <- old %>%
    bind_rows(df) %>%
    arrange(desc(sortpubdate), title) %>%
    distinct(pmid, .keep_all = TRUE)
} else {
  combined <- df %>%
    arrange(desc(sortpubdate), title) %>%
    distinct(pmid, .keep_all = TRUE)
}

write_csv(combined, csv_path)
write_csv(df, "data/pubs_latest.csv")

message("Done. Written ", nrow(combined), " unique records to ", csv_path)
