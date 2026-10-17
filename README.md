<img src="content/authors/admin/avatar.jpg" alt="Cory Whitney" align="right" width = "25%" height="25%"/>

You have landed on my github.io page. Please visit [my
website](https://cory-whitney.com/).

| Quick Links |
|:---|
| [**ORCID: 0000-0003-4988-4583**](https://orcid.org/0000-0003-4988-4583) |
| [**Scholar**](https://scholar.google.de/citations?user=YPIcAA4AAAAJ&hl=en) |
| [**ResearchGate**](https://www.researchgate.net/profile/Cory_Whitney) |
| [**Academia.edu**](https://bonn.academia.edu/CoryWhitney) |
| [**LinkedIn**](https://www.linkedin.com/in/corywhitney/?originalSubdomain=de) |
| [**Center for Development Research (ZEF)**](https://www.zef.de/index.php?id=2232&tx_zefportal_staff_info%5Bref%5D=2252&tx_zefportal_staff_info%5Buid%5D=1799&tx_zefportal_staff_profile%5Buid%5D=1799&no_cache=1) |
| [**University of Bonn**](https://www.gartenbauwissenschaften.uni-bonn.de/en/staff/cory-whitney) |
| [Cory Whitney website](https://cory-whitney.com/) |

Run the site with these from `blogdown`, `RefManageR` and `anytime`
libraries with `bibtex_2academic.R`:

- publications: export the publications collection from Zotero (BibTeX
  only), save it as `content/publication/papers.bib` and run
  `source("content/publication/bibtex_2academic.R")`

- activities (talks and conferences): export the conferences collection
  from Zotero (BibTeX only), save it as `content/event/conferences.bib`
  (invited talks: `content/event/lectures.bib`) and run
  `source("content/event/bibtex_2event.R")`. Fill in the conference name
  in Zotero ("Proceedings Title") so it shows as the event name.

- preview the site locally: `blogdown::serve_site()`

- check the full build: `blogdown::build_site()` (writes to `public/`,
  which is not committed)

- publish: commit and push the source. Netlify builds the live site
  from it; `public/` is never pushed

- needs blogdown >= 1.24 (older versions create a stray `'public'`
  folder)

# build with Netlify

\[![Netlify
Status](https://api.netlify.com/api/v1/badges/235c4b03-c385-4045-917f-e631560f77a4/deploy-status)
