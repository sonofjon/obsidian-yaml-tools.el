#+TITLE: obsidian-tools.el

#+PROPERTY: LOGGING nil

# Note: This readme works with the org-make-toc <https://github.com/alphapapa/org-make-toc> package, which automatically updates the table of contents.

[[https://melpa.org/#/package-name][file:https://melpa.org/packages/package-name-badge.svg]] [[https://stable.melpa.org/#/package-name][file:https://stable.melpa.org/packages/package-name-badge.svg]]

Helper functions for Obsidian in Emacs.

* Screenshots

.

* Contents                                                         :noexport:
:PROPERTIES:
:TOC:      :include siblings
:END:
:CONTENTS:
  -  [[#installation][Installation]]
  -  [[#usage][Usage]]
  -  [[#changelog][Changelog]]
  -  [[#credits][Credits]]
  -  [[#development][Development]]
  -  [[#license][License]]
:END:

* Installation
:PROPERTIES:
:TOC:      :depth 0
:END:

** MELPA

Not available yet.

** Manual

  Install these required packages:

  + =yaml=

  Then put this file in your load-path, and put this in your init file:

  #+BEGIN_SRC elisp
(require 'package-name)
  #+END_SRC

* Usage
:PROPERTIES:
:TOC:      :depth 0
:END:

  Run one of these commands:

  + =obsidian-tools-file-to-front-matter-title= : Change the title in the YAML
  front matter of the current buffer to be identical to the filename of the
  buffer.

  + =obsidian-tools-front-matter-title-to-file=' : Rename the current file
  using the title in the front matter as filename.

** Tips

+ None.

* Changelog
:PROPERTIES:
:TOC:      :depth 0
:END:

** 0.1

Initial release.

* Credits

  This package was inspired by: [[https://github.com./licht1stein/obsidian.el]][obsidian.el].

* Development

Bug reports, feature requests, suggestions, are welcome.

* License

GPLv3

# Local Variables:
# eval: (require 'org-make-toc)
# before-save-hook: org-make-toc
# org-export-with-properties: ()
# org-export-with-title: t
# End:
