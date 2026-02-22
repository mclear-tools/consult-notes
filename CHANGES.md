# Changelog

## 0.8

### Org-Roam

- **Configurable open function**: New `consult-notes-org-roam-open-function`
  defcustom (default `org-roam-node-visit`). Set to `org-roam-node-open` to
  restore previous window behavior. (#80)
- **New node on match failure**: When input does not match an existing node,
  `consult-notes` now creates a new org-roam node via `org-roam-capture-`,
  matching the behavior of `org-roam-node-find` and the denote backend. (#80)
- **Respect org-roam-node-display-template**: consult-notes no longer overrides
  `org-roam-node-display-template`. Customize that variable directly for display
  control. (#79)
- **Duplicate title handling**: Nodes with identical titles are disambiguated
  with an ID suffix rather than raising an error. (#19)
- **Empty title fallbacks**: Nodes with empty or whitespace-only titles fall
  back to filename or node ID.
- **Runtime annotation lookup**: Annotation functions are wrapped in lambdas
  so that `consult-notes-org-roam-annotate-function` changes take effect
  without re-enabling the mode.

### Denote

- **Fixed title column width**: New `consult-notes-denote-title-width` defcustom.
  When set to a number, titles are truncated or padded to that exact width for
  consistent column alignment. When nil (default), auto-computes from the widest
  title. (#82)
- **Configurable file listing**: `consult-notes-denote-files-function` controls
  which files are listed (all files, denote-only files, or a custom regex).
- **Configurable keyword display**: `consult-notes-denote-display-keywords-function`,
  `consult-notes-denote-display-keywords-indicator`, and
  `consult-notes-denote-display-keywords-width` control keyword formatting.
- **Configurable directory display**: `consult-notes-denote-display-dir-function`
  controls how directory names appear.

### General

- **Exclude dailies**: New `consult-notes-org-roam-exclude-dailies` option to
  hide org-roam dailies from `consult-notes` while keeping them searchable via
  `consult-notes-search-in-all-notes`. (#58)
- **Hidden sources**: `consult-notes-file-dir-sources` entries accept `:hidden t`
  to hide a source from the default list while keeping it accessible via its
  narrowing key.
- **UTF-8 encoding**: Added encoding declarations to all source files. (#75)
- **Embark + org-headings**: Fixed embark integration for org-headings source. (#48)
