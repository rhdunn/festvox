# Change Log

## [1.1-1]

Add and modernize the standard project files:

  * Added a CHANGELOG.md file to track the project changes.
  * Added a .gitignore file to ignore the build output.
  * Support building HTML versions of the markdown files with `kramdown`.

Support the GNU standard project layout:

  * Added a COPYING file, taking the license text from `src/Makefile`.
  * Added an AUTHORS file.
  * Link NEWS to another file in the project.

Add autotools support:

  * Added an autogen.sh script to setup the configure script.
  * Backport configure.in from 2.0 as configure.ac.
  * Link to the automake files needed to run the configure script.
  * Use `autoconf` to generate the configure script.
  * Backport the `config/` changes from 2.0 to support more systems.

## [1.1]

Upstream version 1.1.

## [1.0]

Upstream version 1.0.

__NOTE:__ This release is designed for festival 1.3.1.
