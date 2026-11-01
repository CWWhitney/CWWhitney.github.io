# Import talks/conference contributions dropped into bib_inbox/ and
# regenerate the Activities (content/event) pages.
#
# Usage (from the repository root):
#   1. Export the entries from Zotero as BibTeX.
#   2. Rename the file to the event name, e.g. "Tropentag 2026.bib", and
#      put it in bib_inbox/. The file name is used as the event name
#      (booktitle) for any entry that does not already have one.
#      Prefix the name with "lecture_" (e.g. "lecture_Kunming 2026.bib")
#      to file the entries as invited talks instead of conference
#      contributions.
#   3. Rscript scripts/import_activities.R
#
# New entries (by citation key) are appended to content/event/conferences.bib
# or content/event/lectures.bib, the inbox file is moved to
# bib_inbox/processed/, and content/event/bibtex_2event.R is re-run.

library(stringr)

inbox     <- "bib_inbox"
processed <- file.path(inbox, "processed")
dir.create(processed, showWarnings = FALSE)

# split raw bib text into entries, one string per "@type{key, ...}"
split_entries <- function(lines) {
  starts <- grep("^\\s*@", lines)
  if (length(starts) == 0) return(character(0))
  ends <- c(starts[-1] - 1, length(lines))
  mapply(function(s, e) paste(lines[s:e], collapse = "\n"), starts, ends)
}
entry_key <- function(entry) str_match(entry, "^\\s*@\\w+\\s*\\{\\s*([^,\\s]+)\\s*,")[, 2]

bibs <- list.files(inbox, pattern = "\\.bib$", full.names = TRUE)
if (length(bibs) == 0) message("No .bib files in ", inbox, "/")

for (bib in bibs) {
  name      <- tools::file_path_sans_ext(basename(bib))
  is_talk   <- str_detect(name, regex("^lecture_", ignore_case = TRUE))
  event     <- str_trim(str_remove(name, regex("^lecture_", ignore_case = TRUE)))
  target    <- if (is_talk) "content/event/lectures.bib" else "content/event/conferences.bib"
  # Zotero's default export name says nothing about the event
  if (str_detect(event, regex("^Exported Items", ignore_case = TRUE))) {
    warning(basename(bib), ": rename the file to the event name (e.g. 'Tropentag 2026.bib') ",
            "so entries without a booktitle get one; leaving the event name blank.")
    event <- NA
  }

  existing <- entry_key(split_entries(readLines(target, encoding = "UTF-8")))
  entries  <- split_entries(readLines(bib, encoding = "UTF-8", warn = FALSE))
  keys     <- entry_key(entries)
  new      <- entries[!keys %in% existing]

  # drop local Zotero attachment paths, they would be published on the site
  new <- str_remove_all(new, "\\n\\s*file\\s*=\\s*\\{[^\\n]*\\},?")
  if (!is.na(event)) {
    no_booktitle <- !str_detect(new, regex("\\n\\s*booktitle\\s*=", ignore_case = TRUE))
    new[no_booktitle] <- str_replace(new[no_booktitle], "^(\\s*@\\w+\\s*\\{[^,]+,)",
                                     paste0("\\1\n\tbooktitle = {", event, "},"))
  }

  if (length(new) > 0) {
    write(paste0("\n", new, collapse = "\n"), target, append = TRUE)
  }
  message(basename(bib), ": added ", length(new), " to ", target,
          if (any(keys %in% existing)) paste0(" (skipped existing: ",
                                              paste(keys[keys %in% existing], collapse = ", "), ")"))
  file.rename(bib, file.path(processed, paste0(format(Sys.Date()), "_", basename(bib))))
}

source("content/event/bibtex_2event.R")
