#!/bin/csh -f

#set FILES=`grep -l 'manual/variables/chap3.shtml' *.html`
set FILES=`grep -l 'chap.\.php' *.html`

#../help_and_documentation/manual/variables/chap3.php

if ( ! -e mandir_redo ) mkdir mandir_redo

foreach FILE ($FILES)
   echo FILE $FILE
   cat $FILE | \
      sed 's@../../help_and_documentation/manual/decoders/chap6.php@/cgi-bin/gempak/manual/decoders_index@g' | \
      sed 's@../../help_and_documentation/manual/programs/chap4.php@/cgi-bin/gempak/manual/variables_index@g' | \
	sed 's@../../help_and_documentation/manual/variables/chap3.php@/cgi-bin/gempak/manual/programs_index@g' > $FILE.$$

   #sed 's@manual/variables/chap3.shtml@manual/variables/chap3.php@g' > $FILE.$$
   mv $FILE mandir_redo
   mv $FILE.$$ $FILE
end
