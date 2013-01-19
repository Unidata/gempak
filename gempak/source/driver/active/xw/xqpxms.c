#include "geminc.h"
#include "gemprm.h"
#include "xwcmn.h"

Pixmap xqpxms(int loop, int npix )

{
    return(gemwindow[current_window].pxms[loop][npix]);
}
