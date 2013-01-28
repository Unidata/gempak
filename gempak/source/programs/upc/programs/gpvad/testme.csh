#!/bin/csh

set DIRS=`ls -d $RAD/NIDS/*`

foreach DIR ($DIRS)
   set SITE=$DIR:t
   echo SITE $SITE
   gpvad << EOF
      restore
      RADFIL = NEXRIII|${SITE}|NVW
      TITLE = 1/-2/${SITE} VAD DISPLAY ~
      r

      e
EOF
end
