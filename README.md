# ansistyle - Stylish Terminal Output

[![Build Status](https://travis-ci.org/brodieG/ansistyle.png?branch=master)](https://travis-ci.org/brodieG/ansistyle)
[![Code Coverage](https://codecov.io/github/brodieG/ansistyle/coverage.svg?branch=master)](https://codecov.io/github/brodieG/ansistyle?branch=master)

**THIS PACKAGE IS NO LONGER UNDER DEVELOPMENT**

Ended up needing too much of the functionality built-into `crayon`, so instead figured out that removing depenencies form `crayon` was not too difficult.

## Overview

Stripped down dependency-less version of the [`crayon`][1] package by Gábor Csárdi.  Modifies character vectors by adding ANSI escape sequences to render text with style in terminals that support ANSI style escape sequences.

This package allows other packages to implement basic ANSI styles with a minimal dependency footprint.  For most purposes you are probably better off using [`crayon`][1] rather than this package since [`crayon`][1] implements all the functionality available here and quite a bit more.

## Acknowledgements

Gábor Csárdi for [`crayon`][1] and for adding a new dimension to the R experience.

[1]: https://github.com/gaborcsardi/crayon

