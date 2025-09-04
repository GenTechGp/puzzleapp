#' Inheritance Utility
#'
library(data.table)

check_pedigree_sanity <- function(pedigree) {
  issues <- list()
  # check required fields exist for all entries
  required_fields <- c("sample_id", "kinship", "status", "sex", "code")
  for (i in seq_along(pedigree)) {
    missing_fields <- setdiff(required_fields, names(pedigree[[i]]))
    if (length(missing_fields) > 0) {
      issues <- c(issues, paste0("Sample ", i, " missing fields: ", paste(missing_fields, collapse = ", ")))
    }
  }
  # collect kinship info
  kinships <- vapply(pedigree, function(x) x$kinship, character(1))
  sample_ids <- vapply(pedigree, function(x) x$sample_id, character(1))
  # proband count
  if (sum(kinships == "proband") != 1) {
    issues <- c(issues, paste("Expected exactly 1 proband, found", sum(kinships == "proband")))
  }
  # father/mother uniqueness
  if (sum(kinships == "father") > 1) {
    issues <- c(issues, paste("More than one father found (", sum(kinships == "father"), ")", sep=""))
  }
  if (sum(kinships == "mother") > 1) {
    issues <- c(issues, paste("More than one mother found (", sum(kinships == "mother"), ")", sep=""))
  }
  # duplicate sample_id
  if (anyDuplicated(sample_ids)) {
    issues <- c(issues, "Duplicate sample_id values found")
  }
  if (length(issues) > 0) {
    cat("Pedigree sanity check found issues:\n")
    for (msg in issues) {
      cat(" -", msg, "\n")
    }
    return(FALSE)
  }
  cat("Pedigree sanity check passed.\n")
  TRUE
}

convert_samples_to_pedigree <- function(samples_list) {
  cat("Converting", length(samples_list), "samples to pedigree list.\n")
  pedigree <- lapply(samples_list, function(s) {
    list(
      sample_id = s$sample_id,
      kinship   = s$kinship,
      status    = s$status,
      sex       = s$sex,
      code      = s$code
    )
  })
  print(pedigree)
  check_pedigree_sanity(pedigree)
  pedigree
}

# --- Helper: compute allele count per sample ---
alleleCount <- function(inher, status, sex) {
  if (status == "NA") return("")  # special case

  if (inher == "Homozygous Recessive") {
    if (status == "affected") "2" else "0-1"
  } else if (inher == "Dominant/De Novo") {
    if (status == "affected") "1-2" else "0"
  } else if (inher == "Compound Heterozygous") {
    if (status == "affected") "1" else "0-1"
  } else if (inher == "X-Linked Recessive") {
    if (sex == "male") {
      if (status == "affected") "1" else "0"
    } else {
      if (status == "affected") "2" else "0-1"
    }
  } else {
    ""
  }
}

# --- Compute allele counts for standard inheritance models as a named list ---
compute_allele_table <- function(pedigree, inher) {
  if (length(pedigree) == 0 || inher == "") return(list())
  res <- list()
  for (sample in pedigree) {
    sid <- sample$sample_id
    res[[sid]] <- alleleCount(inher, sample$status, sample$sex)
  }
  res
}