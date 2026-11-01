# Activities inbox

Drop Zotero BibTeX exports of talks and conference contributions here,
renamed to the event name (e.g. `Tropentag 2026.bib`; prefix with
`lecture_` for invited talks), then run from the repository root:

    Rscript scripts/import_activities.R

New entries are added to `content/event/conferences.bib` (or `lectures.bib`),
the pages under Activities → Talks & Conferences are regenerated, and the
dropped file is moved to `processed/`.
