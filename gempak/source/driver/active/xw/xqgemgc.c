#include "geminc.h"
#include "gemprm.h"
#include "xwcmn.h"

GC xqgemgc(void)
{
	return(gemwindow[current_window].gc);
}
