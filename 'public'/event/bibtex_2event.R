# Same idea as content/publication/bibtex_2academic.R, adapted for the
# event (talk/conference) content type and its YAML front matter.
# Run with
# source("content/event/bibtex_2event.R")
# serve with
# blogdown::serve_site()
# build with
# blogdown::build_site()


bibtex_2event <- function(bibfile,
                          outfold,
                          abstract = FALSE,
                          overwrite = FALSE,
                          category = NULL,
                          pdf_bib = bibfile) {
  # category (e.g. "Conference" or "Invited Talk") is written into the
  # "tags" front matter so entries from different bib files can share one
  # activities timeline while remaining filterable by type.
  # pdf_bib is a bib file with Zotero "file" fields (e.g. the master CV bib);
  # the first PDF attached to each entry is copied into its page folder.

  require(RefManageR)
  require(dplyr)
  require(stringr)
  require(anytime)
  require(tibble)

  # Import the bibtex file and convert to data.frame
  # check = FALSE: with "warn", RefManageR silently drops entries that lack
  # a "required" field (e.g. @inproceedings without booktitle, as in Zotero
  # exports of conference abstracts), so those talks never appeared.
  mytalks   <- ReadBib(bibfile, check = FALSE, .Encoding = "UTF-8") %>%
    as.data.frame() %>%
    rownames_to_column() %>% # retain rownames (as labels for bibtex re-export)

    mutate_all(~str_remove_all(.,"[{}\"]")) %>%   ### remove {}" from bibtext entries
    mutate_all(~str_replace_all(.,'\\\\([%&_#$])', '\\1')) %>%  ### unescape LaTeX \%, \&, \_, \#, \$ for markdown/YAML
    select(-any_of("file"))   ### local Zotero attachment paths must not be published in cite.bib

  # citation key -> path of the first PDF attachment, read from the raw bib
  # text because Zotero escapes ";" in file names as "\\;" in the bib
  pdf_paths <- local({
    lines <- if (file.exists(pdf_bib)) readLines(pdf_bib, encoding = "UTF-8", warn = FALSE) else character(0)
    keys  <- str_match(lines, "^\\s*@\\w+\\s*\\{\\s*([^,\\s]+)\\s*,")[, 2]
    for (i in seq_along(keys)) if (is.na(keys[i]) && i > 1) keys[i] <- keys[i - 1]
    is_file <- str_detect(lines, "^\\s*file\\s*=\\s*\\{")
    files <- str_match(lines[is_file], "^\\s*file\\s*=\\s*\\{(.*)\\},?\\s*$")[, 2]
    first_pdf <- vapply(files, function(f) {
      parts <- str_split(str_replace_all(f, fixed("\\;"), "\u0001"), fixed(";"))[[1]]
      parts <- str_replace_all(parts, fixed("\u0001"), ";")
      pdfs  <- str_match(parts, "^[^:]*:(/.*\\.pdf):application/pdf$")[, 2]
      pdfs  <- pdfs[!is.na(pdfs)]
      if (length(pdfs) > 0) pdfs[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
    setNames(first_pdf, keys[is_file])
  })

  # make bibtype the name of the type column (default for WriteBib)
  if (has_name(mytalks, "document_type") & !(has_name(mytalks, "bibtype"))) {
    mytalks <- mytalks %>% rename(bibtype = document_type)
  }

  # create a function which populates the md template based on the info
  # about a talk/conference contribution
  create_md <- function(x) {

    # bib fields such as "address"/"booktitle"/"abstract"/"url" are absent
    # entirely (not just NA) from x when no entry in the whole file has them
    get_field <- function(field) if (field %in% names(x)) x[[field]] else NA
    # colons/quotes in free text break unquoted YAML scalars, so quote them
    yaml_str <- function(value) {
      value <- str_squish(str_replace_all(value, fixed("\\"), "\\\\"))
      paste0("\"", str_replace_all(value, '"', '\\\\"'), "\"")
    }

    # define a date and create filename by appending date and start of title
    # use the bib "month" field when present, so talks sort within the year
    month <- match(str_sub(tolower(get_field("month")), 1, 3), tolower(month.abb))
    if (!is.na(x[["year"]])) {
      x[["date"]] <- paste0(x[["year"]], "-", sprintf("%02d", ifelse(is.na(month), 1, month)), "-01")
    } else {
      x[["date"]] <- "2999-01-01"
    }

    # folder name keeps the year-01-01 prefix (not the month) so existing
    # folders are reused rather than duplicated
    foldername <- paste(str_sub(x[["date"]], 1, 4) %>% paste0("-01-01"), x[["title"]] %>%
                          str_replace_all(fixed(" "), "_") %>%
                          str_remove_all(fixed(":")) %>%
                          str_sub(1, 20), sep = "_")

    dir.create(file.path(outfold, foldername), showWarnings = FALSE)
    filename = "index.md"
    outsubfold = paste(outfold, foldername, sep="/")

    # Zotero PDF -> <folder>/<folder>.pdf; the theme shows a "PDF" button
    # for a bundle file named after the page folder
    pdf_from <- unname(pdf_paths[x[["rowname"]]])
    if (length(pdf_from) == 1 && !is.na(pdf_from) && file.exists(pdf_from)) {
      file.copy(pdf_from, file.path(outsubfold, paste0(foldername, ".pdf")), overwrite = TRUE)
    }

    if (!file.exists(file.path(outsubfold, filename)) | overwrite) {
      fileConn <- file.path(outsubfold, filename)
      write("---", fileConn)

      # Title (of the talk/paper) and conference/event name
      write(paste0("title: ", yaml_str(x[["title"]])), fileConn, append = T)
      write("", fileConn, append = T)
      if (!is.na(get_field("booktitle"))) {
        write(paste0("event: ", yaml_str(get_field("booktitle"))), fileConn, append = T)
      } else {
        write("event: \"\"", fileConn, append = T)
      }
      write("event_url: \"\"", fileConn, append = T)
      write("", fileConn, append = T)

      # Location. Kept as a single free-text field; address sub-fields left blank.
      if (!is.na(get_field("address"))) {
        write(paste0("location: ", yaml_str(get_field("address"))), fileConn, append = T)
      } else {
        write("location: \"\"", fileConn, append = T)
      }
      write("address:", fileConn, append = T)
      write("  street: ", fileConn, append = T)
      write("  city: ", fileConn, append = T)
      write("  region: ", fileConn, append = T)
      write("  postcode: ", fileConn, append = T)
      write("  country: ", fileConn, append = T)
      write("", fileConn, append = T)

      write(paste0("summary: ", yaml_str(x[["title"]])), fileConn, append = T)
      if (abstract & !is.na(get_field("abstract"))) {
        write(paste0("abstract: ", yaml_str(get_field("abstract"))), fileConn, append = T)
      } else {
        write("abstract: \"\"", fileConn, append = T)
      }
      write("", fileConn, append = T)

      # Talk date is unknown from the bib entry, use the publish date for sorting.
      write(paste0("date: \"", anydate(x[["date"]]), "T00:00:00Z\""), fileConn, append = T)
      write(paste0("publishDate: \"", anydate(x[["date"]]), "T00:00:00Z\""), fileConn, append = T)
      write("", fileConn, append = T)

      # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
      auth_hugo <- str_replace_all(x["author"], " and ", "\", \"")
      auth_hugo <- stringi::stri_trans_general(auth_hugo, "latin-ascii")
      write(paste0("authors: [\"", auth_hugo, "\"]"), fileConn, append = T)
      if (!is.null(category)) {
        write(paste0("tags: [\"", category, "\"]"), fileConn, append = T)
      } else {
        write("tags: []", fileConn, append = T)
      }
      write("", fileConn, append = T)

      write("featured: false", fileConn, append = T)
      write("", fileConn, append = T)

      write("links: []", fileConn, append = T)
      write(paste0("url_pdf: \"", ifelse(is.na(get_field("url")), "", get_field("url")), "\""), fileConn, append = T)
      write("url_code: \"\"", fileConn, append = T)
      write("url_slides: \"\"", fileConn, append = T)
      write("url_video: \"\"", fileConn, append = T)
      write("", fileConn, append = T)

      write("projects: []", fileConn, append = T)
      write("---", fileConn, append = T)
    }
    # convert entry back to data frame
    df_entry = as.data.frame(as.list(x), stringsAsFactors=FALSE) %>%
      column_to_rownames("rowname")

    # write cite.bib file to outsubfolder
    WriteBib(as.BibEntry(df_entry[1,]), paste(outsubfold, "cite.bib", sep="/"))
  }
  # apply the "create_md" function over the talks list to generate
  # the different "md" files.

  apply(mytalks, FUN = function(x) create_md(x), MARGIN = 1)
}


# Running the function for conference contributions
#
# The master CV bib (exported from Zotero, with PDF attachment paths) is the
# source when it exists on this machine. A copy without the local "file"
# paths is kept in the repo so the site can be rebuilt elsewhere.

master_bibfile <- "~/Library/CloudStorage/Dropbox/_profile/_master/curriculum_vitae/bib/conferences.bib"
my_bibfile     <- "content/event/conferences.bib"
out_fold       <- "content/event"
if (file.exists(master_bibfile)) {
  master_lines <- readLines(master_bibfile, encoding = "UTF-8", warn = FALSE)
  writeLines(master_lines[!grepl("^\\s*file\\s*=\\s*\\{", master_lines)], my_bibfile, useBytes = TRUE)
} else {
  master_bibfile <- my_bibfile
}
bibtex_2event(
  bibfile   = my_bibfile,
  outfold   = out_fold,
  abstract  = TRUE,
  overwrite = TRUE,
  category  = "Conference",
  pdf_bib   = path.expand(master_bibfile)
)

# Running the function for invited talks/lectures, if that bib file exists.
# Entries land in the same content/event timeline, tagged "Invited Talk" so
# they can be told apart from conference contributions.

lectures_bibfile <- "content/event/lectures.bib"
if (file.exists(lectures_bibfile)) {
  bibtex_2event(
    bibfile   = lectures_bibfile,
    outfold   = out_fold,
    abstract  = TRUE,
    overwrite = TRUE,
    category  = "Invited Talk"
  )
}
