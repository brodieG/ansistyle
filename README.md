# ansistyle - Stylish Terminal Output

[![Build Status](https://travis-ci.org/brodieG/ansistyle.png?branch=master)](https://travis-ci.org/brodieG/ansistyle)
[![Code Coverage](https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=master)](https://codecov.io/github/brodieG/unitizer?branch=master)

## Overview

Stripped down dependency-less version of the `[crayon][1]` package by Gábor Csárdi.  Modifies character vectors by adding ANSI escape sequences to render text with style in terminals that support ANSI style escape sequences.

This package allows other packages to implement basic ANSI styles with a minimal dependency footprint.  For most purposes you are probably better off using `[crayon][1]` rather than this package since `[crayon][1]` implements all the functionality available here and quite a bit more.  Most of the code in this package comes directly from `[crayon][1]`.

## Acknowledgements

Gábor Csárdi for `[crayon][1]` and for adding a new dimension to the R experience.

[1]: https://github.com/gaborcsardi/crayon

