# FestVox: Building Voices in Festival

- [Warning](#warning)
- [Requirements](#requirements)
- [Environment Variables](#environment-variables)
- [Compilation](#compilation)
- [License](#license)

----------

The [festvox](http://www.festvox.org) project, based at Carnegie Mellon
University, distributes documentation, scripts and examples that should
be sufficient for an interested person to build their own synthetic voices
in currently supported languages or new languages in the University of
Edinburgh's Festival Speech Synthesis System.  The quality of the result
depends much on the time and skill of the builder.  For English it may be
possible to build a new voice in a couple of days work, a new language may
take months or years to build.  It should be noted that even the best
voices in Festival (or any other speech synthesis system for that matter)
are still nowhere near perfect quality.

This distribution includes:

  * Support for designing, recording and autolabelling diphone databases.
  * Support for designing, recording and autolabelling unit selection dbs.
  * Support for designing, recording and autolabelling statistical parametric
    synthesis voices.
  * Building simple limited domain synthesis engines.
  * Support for building rule driven and data driven prosody models
    (duration, intonation and phrasing).
  * Support for building rule driven and data driven text analysis.
  * Lexicon and building Letter to Sound rule support.
  * Predefined scripts for building new US (and UK) English voices.
  * Scripts for designing and selecting prompts to record in arbitrary
    languages.

## Warning

This is not a plug and play program to build new voices. It is
instructions with discussion on the problems and an attempt to
document the expertise we have gained in building other voices.
Although we have tried to automate the task as much as possible this
is no substitute for careful correction and understanding of the
processes involved.  There are significant pointers into the
literature throughout the document that allow for more detailed study
and further reading.

## Requirements

1.  A POSIX-compatible system

    Either a Unix machine -- although there is nothing inheritantly Unix
    about the scripts -- or a Windows machine running the cygwin environment.

2.  Festival 2.0.95 and Edinburgh Speech Tools 2.0.95

    This uses speech tools programs and festival itself at various
    stages in builidng voices as well as (of course) for the final
    voices.  Festival and the Edinburgh Speech Tools are available from
    [http://www.cstr.ed.ac.uk/projects/festival/](http://www.cstr.ed.ac.uk/projects/festival/)
    or
    [http://www.festival.org/festival/latest/](http://www.festival.org/festival/latest/).

    It is recommended that you compile your own versions of these
    as you will need the libraries and include files to build some
    programs in this festvox.

3.  [EMU Labeller](http://www.shlrc.mq.edu.au/emu/)

    The University of Macquarie's Speech Hearing and Language Research
    Centre distribute labelling tools for speech databases.  We use
    it here for viewing speech, as spectrograms, F0s phone labels etc.
    Other waveform labeller/viewers exist and you find them more convinient
    to use but we include support for emulabel as it meets our requirements
    and is freely available.

4.  Patience and understanding

    Building a new voice is a lot of work, and something will probably
    go wrong which may require the repetition of some long boring and
    tedious process.  Even with lots of care a new voice still might 
    just not work.  In distributing this document we hope to increase the
    basic knowledge of synthesis out there and hopefully find people 
    who can improve on this making the processing easier and more reliable
    in the future.

If you need to build the document itself, you will need a working
version of the docbook tools, which may (or may not) already
be installed on your system

## Environment Variables

The following environment variables are expected in order to build the
festvox scripts and programs:

  * `ESTDIR` -- The path to the *Edinburgh Speech Tools* build directory.

The following environment variables are expected in order to run the
festvox scripts and programs:

  * `ESTDIR` -- The path to the *Edinburgh Speech Tools* build directory.

  * `FESTVOXDIR` -- The top-level directory of this project.

These can be set in `sh`-based shell (e.g. `bash`, `zsh`, `ksh` or `sh`) using:

    export ESTDIR=/home/awb/projects/2.0.95/speech_tools
    export FESTVOXDIR=/home/awb/projects/festvox

or a `csh`/`tcsh` shell using:

    setenv ESTDIR /home/awb/projects/2.0.95/speech_tools
    setenv FESTVOXDIR /home/awb/projects/festvox

Remember to set these to where *your* installations are, not *ours*.

## Compilation

The `festvox` project uses a standard autogen-based build system. It can
be build using the following commands:

    ./autogen.sh
    ./configure --prefix=/usr
    make
    sudo make install

The configuration basically tries to find your version of 
the Edinburgh Speech Tools and uses its configuration to set
compiler type etc.  So you must have that installed.  If configure
fails try expliciting setting your `ESTDIR` environment variable
to point ot your compiled version of the Speech Tools.

A pre-generated version of the document in [html](html/index.html) and
[postscript](html/festvox.ps.gz) distributed in the [html](html) directory.

To build the documenation, run the following commands:
   
    cd docbook
    make doc

Note that even if the documentation build fails you can still use all
the scripts and programs.

## License

The `festvox` project is released under a [4-clause BSD license](COPYING)
with the following copyright:

    Carnegie Mellon University and
    Alan W Black and Kevin A. Lenzo
    Copyright (c) 1998-2010
    All Rights Reserved.

No claims are made by the authors of this work, Carnegie Mellon University
(or the University of Edinburgh), on the voices that you generate with the
scripts and techniques described within this distribution.

The changes to the project are described in the [CHANGELOG.md](CHANGELOG.md)
file in order to comply with clause 2 of the BSD license.
