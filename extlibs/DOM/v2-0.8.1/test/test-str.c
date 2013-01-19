/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-str.c
 *
 * Copyright (C) 2001 Paolo Casarini <paolo@casarini.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <gdome.h>

void
dot() {
	printf(".");
	fflush(stdout);
}

int
main (int argc, char **argv)
{
  GdomeDOMString *str1, *str2, *str3, *str4;
  gboolean ret = TRUE;
  int i;

  /* Test equal */
  str1 = gdome_str_mkref ("Test String");
  str2 = gdome_str_mkref ("Test String");
  if (!gdome_str_equal (str1, str2) ||
      !gdome_str_equal (str2, str1))
    fprintf (stderr, "\nDOMString.equal: failed"), ret = FALSE;
  gdome_str_unref (str2);
  dot();

  /* Test equalIgnoreCase */
  str2 = gdome_str_mkref ("test string");
  if (!gdome_str_equalIgnoreCase (str1, str2) ||
      !gdome_str_equalIgnoreCase (str2, str1))
    fprintf (stderr, "\nDOMString.equalIgnoreCase: failed"), ret = FALSE;
  gdome_str_unref (str2);
  dot();

  /* Test charAt */
  for (i=0; i<strlen(str1->str) && ret; i++)
    if (gdome_str_charAt (str1, i) != str1->str[i])
      fprintf (stderr, "\nDOMString.charAt(%d): failed", i), ret = FALSE;
  dot();

  /* Text concat */
  str2 = gdome_str_mkref (" Test String");
  str3 = gdome_str_concat (str1, str2);
  str4 = gdome_str_mkref ("Test String Test String");
  if (!gdome_str_equal (str3, str4))
    fprintf (stderr, "\nDOMString.concat: failed"), ret = FALSE;
  gdome_str_unref (str2);
  gdome_str_unref (str3);
  gdome_str_unref (str4);
  dot();

  /* Test endsWith */
  str2 = gdome_str_mkref ("ring");
  str3 = gdome_str_mkref ("rin");
  if (!gdome_str_endsWith (str1, str2) ||
      !gdome_str_endsWith (str1, NULL) ||
      gdome_str_endsWith (str1, str3))
    fprintf (stderr, "\nDOMString.endsWith: failed"), ret = FALSE;
  gdome_str_unref (str2);
  gdome_str_unref (str3);
  dot();

  /* Test length */
  if (gdome_str_length (str1) != strlen (str1->str))
    fprintf (stderr, "\nDOMString.length: failed"), ret = FALSE;
  dot();

  /* Test startsWith */
  str2 = gdome_str_mkref ("Test");
  str3 = gdome_str_mkref ("est");
  if (!gdome_str_startsWith (str1, str2) ||
      !gdome_str_startsWith (str1, NULL) ||
      gdome_str_startsWith (str1, str3))
    fprintf (stderr, "\nDOMString.startsWith: failed"), ret = FALSE;
  gdome_str_unref (str2);
  gdome_str_unref (str3);
  gdome_str_unref (str1);
  dot();

  printf ("test_str\n");

  if (ret)
    return 0;
  else
    return -1;
}

