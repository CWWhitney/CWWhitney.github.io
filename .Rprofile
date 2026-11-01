# Use the Apple Silicon Hugo installed by blogdown::install_hugo("0.81.0", extended = TRUE)
options(blogdown.hugo.version = "0.81.0")

# /usr/local/bin/go is Intel-only and no longer runs, so point Hugo at the cached
# wowchemy modules instead of letting it call `go mod download`.
# Remove this block once an Apple Silicon Go is installed (e.g. `brew install go`).
local({
  mods <- Sys.glob(path.expand(
    "~/Library/Caches/hugo_cache/modules/filecache/modules/pkg/mod/github.com/wowchemy/wowchemy-hugo-modules/*@*"
  ))
  if (length(mods)) {
    names <- sub("@.*", "", basename(mods))
    Sys.setenv(HUGO_MODULE_REPLACEMENTS = paste0(
      "github.com/wowchemy/wowchemy-hugo-modules/", names, " -> ", mods, collapse = ","
    ))
  }
})
