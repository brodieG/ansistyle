# ansistyle - Stylish Terminal Output

## Overview

Stripped down dependency-less version of the `crayon` package by Gábor Csárdi.  Modifies character vectors by adding ANSI escape sequences to render text with style in terminals that support ANSI style escape sequences.

This package allows other packages to implement basic ANSI styles with a minimal dependency footprint.  For most purposes you are probably better off using `crayon` than this package since `crayon` implements all the functionality available here and quite a bit more.  Most of the code in this package comes directly from `crayon`.

## Acknowledgements

Gábor Csárdi for `crayon` and for adding a new dimension to the R experience.
