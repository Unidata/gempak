#include "gui.h"


/*ARGSUSED*/
void ExitCb ( Widget widget, int tag, XtPointer cb_data )
{
  SaveObsChanges();
  exit(EXIT_SUCCESS);
}
