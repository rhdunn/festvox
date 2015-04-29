# Change Log

## [2.1-1] - 2015-05-02 Maintenance

  * Build improvements from [1.1-1](#1.1-1).
  * Merged the ANNOUNCE-2.1 text into the CHANGELOG.md and README.md files.

## [2.1] - 2007-01-21 Release

  * General bug fixes and improvements.
  * Better clunits general voice support.
  * Clustergen Statistical Parametric Synthesis (HTS like) that is
    easier to use and relaible on multiple voices and languages.
  * A new autoamtic phoneme label EHMM, which is easier to use and
    give better results that our previous methods.
  * VC voice conversion module (as standalone and integrated into Festival).
  * Full support under Cygwin under windows for voice building.
  * Support for finding "nice" prompts in large databases of texts in
    new languages.

## [2.0-1] - 2015-05-02 Maintenance

  * Build improvements from [1.1-1](#1.1-1).
  * Remove the derivable autoconf and automake files: config.{guess,sub},
    configure, install-sh, missing and mkinstalldirs.
  * Remove configure.in as this was added as configure.ac in [1.1-1](#1.1-1).
  * Merged the ANNOUNCE-2.0 text into the CHANGELOG.md file.

## [2.0] - 2003-01-28 Release

  * General bug fixes and improvements.
  * Better clunits general voice support.
  * Using CMU Sphinx and SphinxTrain for building acoustic models
    for labelling databases.
  * DOCBOOK version of the documentation, with more general background
    documentation.
  * Initial support for Mac OS X.
  * `configure` support to match Edinburgh Speech Tools.

## [1.6-1] - 2015-05-02 Maintenance

  * Build improvements from [1.1-1](#1.1-1).

## [1.6] - 2001-06-10 Release

Upstream version 1.6.

## [1.4-1] - 2015-05-02 Maintenance

  * Build improvements from [1.1-1](#1.1-1).

## [1.4] - 2000-12-31 Release

Upstream version 1.4.

## [1.3-1] - 2015-05-02 Maintenance

  * Build improvements from [1.1-1](#1.1-1).
  * Merged the ANNOUNCE-1.2 text into the CHANGELOG.md file.

## [1.3] - 2000-09-06 Release

Upstream version 1.3.

## [1.2-1] - 2015-05-01 Maintenance

  * Build improvements from [1.1-1](#1.1-1).
  * Merged the ANNOUNCE-1.2 text into the CHANGELOG.md file.

## [1.2] - 2000-07-10 Beta

  * Better support for generating scheme files from skeletons.
  * Clear walkthroughs for standard tasks.
  * Improved overall documentation and support.
  * The limited domain code has substantially improved.
  * PointyClicky (a GUI interface) has be added.
  * Prosody modelling has better support.
  * The document has more content (with corrections) though still needs work.

## [1.1-1] - 2015-05-01 Maintenance

Add and modernize the standard project files:

  * Added a CHANGELOG.md file to track the project changes.
  * Converted the README file to markdown and restructured it to be more
    readable.
  * Merged the ANNOUNCE-1.1 text into the README.md and CHANGELOG.md files.
  * Added a .gitignore file to ignore the build output.
  * Support building HTML versions of the markdown files with `kramdown`.

Support the GNU standard project layout:

  * Added a COPYING file, taking the license text from `src/Makefile`.
  * Added an AUTHORS file.
  * Link NEWS and README to other files in the project.

Add autotools support:

  * Added an autogen.sh script to setup the configure script.
  * Backport configure.in from 2.0 as configure.ac.
  * Link to the automake files needed to run the configure script.
  * Use `autoconf` to generate the configure script.
  * Backport the `config/` changes from 2.0 to support more systems.

## [1.1] - 2000-02-24 Beta

  * Better support for generating scheme files from skeletons.
  * Clear walkthroughs for standard tasks.
  * Improved overall documentation and support.

## [1.0] - 1999-02-11 Release

Upstream version 1.0.

__NOTE:__ This release is designed for festival 1.3.1.
