/* Convert double to nearest integer */

#include "f2c.h"
#include "util.h"

int dblnint(double dvalue)
{
	int ivalue;
	ivalue = i_dnnt(&dvalue);
	return ivalue;
}
