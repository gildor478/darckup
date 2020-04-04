# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog], and this project adheres to [Semantic Versioning].

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html

## [0.0.15] - 2020-04-05
* Setup dispakan.

## [0.0.14] - 2018-02-23
* Exclude .done files when using --only_catalogs or --only_volumes in the
  file list.

## [0.0.13] - 2018-02-16
* Add .done files with the matching archive.

## [0.0.12] - 2018-02-15
* Include files ending with .done in archive.

## [0.0.10] - 2017-05-17
* Allow to create an initial version even if always_incremental is set.

## [0.0.7] - 2015-11-03
* Add --no_terminal to get rid of message "No terminal found for user
  interaction." by using -Q on the command line.

## [0.0.6] - 2015-10-22
* Use the right order for dar command line options, so that "reference:"
  section in the dar configuration file will take into account the
  incremental archive.

## [0.0.5] - 2015-10-20
* Flush stderr when logging.

## [0.0.4] - 2015-10-18
* Allow to produce and use inline catalogs for dar (use_catalog=true).
* Allow to only produce incremental archives (always_incremental=true).
* Misc:
  * Fix negative number of archive to remove in the info log.
  * Refactor test suite to make it easier to read.

## [0.0.3] - 2015-10-14
* Minor correction for the location of the default configuration files, it
  should be in sysconfig/darckup/darckup.ini.

## [0.0.2] - 2015-10-12
* First full working implementation.

## [0.0.1] - 2015-08-24
* Initial version 0.0.1
