#include "mcsubs.h"
#include "arasubs.h"

void araget_(int *fd, int *offset, int *nbyte, int *buf)
{
    int rc;
    rc = araget(*fd, *offset, *nbyte, buf);
    return;
}
