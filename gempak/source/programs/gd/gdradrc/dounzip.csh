#!/bin/csh -f

set DIRS=`ls -d */N1R`
foreach DIR ($DIRS)
   cd $DIR
   set FILES=`ls N1R*`
   foreach FILE ($FILES)
      if ( ! -z $FILE ) then
         mv $FILE $FILE.gz
         gunzip $FILE.gz
      endif
   end
   cd ../..
end

