#' Internal inheritance utilities
#'
#' Helper functions used internally for pedigree validation and allele
#' count inference. Not part of the public API.
#'
#' @keywords internal
#' @noRd
NULL

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
  # print(pedigree)
  puzzlecore_check_pedigree_sanity(pedigree)
  pedigree
}

