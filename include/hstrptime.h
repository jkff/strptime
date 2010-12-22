/*
 A modified version of code from R (www.r-project.org)
 repackaged as a stand-alone .c file, with a couple of 
 R-specific bits removed .

 We use this version because of its support for input
 of fractional seconds.

 Modified by Eugene Kirpichov <ekirpichov@gmail.com>, 2010.

 */

/* For inclusion by datetime.c. 

   A modified version of code from the GNU C library with locale
   support removed and wchar support added.
*/

/* Convert a string representation of time to a time value.
   Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1996.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   a copy is available at http://www.r-project.org/licenses/
*/
/* XXX This version of the implementation is not really complete.
   Some of the fields cannot add information alone.  But if seeing
   some of them in the same format (such as year, week and weekday)
   this is enough information for determining the date.  */

#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>  /* for strncasecmp */
#endif

