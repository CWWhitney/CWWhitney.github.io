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
                          category = NULL) {
  # category (e.g. "Conference" or "Invited Talk") is written into the
  # "tags" front matter so entries from different bib files can share one
  # activities timeline while remaining filterable by type.

  require(RefManageR)
  require(dplyr)
  require(stringr)
  require(anytime)
  require(tibble)

  # Import the bibtex file and convert to data.frame
  mytalks   <- ReadBib(bibfile, check = "warn", .Encoding = "UTF-8") %>%
    as.data.frame() %>%
    rownames_to_column() %>% # retain rownames (as labels for bibtex re-export)

    mutate_all(~str_remove_all(.,"[{}\"]")) %>%   ### remove {}" from bibtext entries
    mutate_all(~str_replace_all(.,'\\\\%', '%'))  ### some replace double escaped % for markdown

  # make bibtype the name of the type column (default for WriteBib)
  if (has_name(mytalks, "document_type") & !(has_name(mytalks, "bibtype"))) {
    mytalks <- mytalks %>% rename(bibtype = document_type)
  }

  # create a function which populates the md template based on the info
  # about a talk/conference contribution
  create_md <- function(x) {

    # define a date and create filename by appending date and start of title
    if (!is.na(x[["year"]])) {
      x[["date"]] <- paste0(x[["year"]], "-01-01")
    } else {
      x[["date"]] <- "2999-01-01"
    }

    foldername <- paste(x[["date"]], x[["title"]] %>%
                          str_replace_all(fixed(" "), "_") %>%
                          str_remove_all(fixed(":")) %>%
                          str_sub(1, 20), sep = "_")

    dir.create(file.path(outfold, foldername), showWarnings = FALSE)
    filename = "index.md"
    outsubfold = paste(outfold, foldername, sep="/")

    if (!file.exists(file.path(outsubfold, filename)) | overwrite) {
      fileConn <- file.path(outsubfold, filename)
      write("---", fileConn)

      # Title (of the talk/paper) and conference/event name
      write(paste0("title: ", x[["title"]]), fileConn, append = T)
      write("", fileConn, append = T)
      if (!is.na(x[["booktitle"]])) {
        write(paste0("event: ", x[["booktitle"]]), fileConn, append = T)
      } else {
        write("event: \"\"", fileConn, append = T)
      }
      write("event_url: \"\"", fileConn, append = T)
      write("", fileConn, append = T)

      # Location. Kept as a single free-text field; address sub-fields left blank.
      if (!is.na(x[["address"]])) {
        write(paste0("location: ", x[["address"]]), fileConn, append = T)
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

      write(paste0("summary: ", x[["title"]]), fileConn, append = T)
      if (abstract & !is.na(x[["abstract"]])) {
        write(paste0("abstract: \"", x[["abstract"]], "\""), fileConn, append = T)
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
      write(paste0("url_pdf: \"", ifelse(is.na(x[["url"]]), "", x[["url"]]), "\""), fileConn, append = T)
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

my_bibfile <- "content/event/conferences.bib"
out_fold   <- "content/event"
bibtex_2event(
  bibfile  = my_bibfile,
  outfold   = out_fold,
  abstract  = FALSE,
  overwrite = TRUE,
  category  = "Conference"
)

# Running the function for invited talks/lectures, if that bib file exists.
# Entries land in the same content/event timeline, tagged "Invited Talk" so
# they can be told apart from conference contributions.

lectures_bibfile <- "content/event/lectures.bib"
if (file.exists(lectures_bibfile)) {
  bibtex_2event(
    bibfile   = lectures_bibfile,
    outfold   = out_fold,
    abstract  = FALSE,
    overwrite = TRUE,
    category  = "Invited Talk"
  )
}
