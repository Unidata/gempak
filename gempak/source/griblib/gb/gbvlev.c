#include "gbcmn.h"

void gb_vlev ( int *ilevel, int *iret )
/************************************************************************
 * gb_vlev								*
 *									*
 * This routine determines the vertical levels for a grid.		*
 *									*
 * gb_vlev ( ilevel, iret )						*
 *									*
 * Output parameters:							*
 *	*ilevel 	int		Level(s)			*
 *	*iret		int		Return value			*
 **									*
 * Log:									*
 * Chiz/Unidata		 3/00	Created from gb_gpds			*
 * M. Li/GSC		 5/00	Added iret and cleaned up		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = 0;
	switch ( pds.vcoord ) {
	    case 101:
	    case 104:
	    case 106:
	    case 108:
	    case 110:
	    case 112:
	    case 116:
	    case 120:
		    ilevel[0] = pds.level_1;
		    ilevel[1] = pds.level_2;
		    break;
	    case 100:
	    case 103:
	    case 105:
	    case 107:
	    case 109:
	    case 111:
	    case 113:
	    case 115:
	    case 117:
	    case 119:
	    case 125:
	    case 200:
	    case 201:
		    ilevel[0] = pds.level;
		    ilevel[1] = -1;
		    break;
	    case 114:
                    ilevel[0] = 475 - pds.level_1;
                    ilevel[1] = 475 - pds.level_2;
                    break;
	    case 121:
		    ilevel[0] = 1100 - pds.level_1;
		    ilevel[1] = 1100 - pds.level_2;
		    break;
	    case 128:
		    ilevel[0] = 1100 - pds.level_1;
		    ilevel[1] = 1100 - pds.level_2;
		    break;
	    case 141:
		    ilevel[0] = pds.level_1 * 10;
		    ilevel[1] = 1100 - pds.level_2;
		    break;
	    case 160:
		    ilevel[0] = pds.level;
		    ilevel[1] = -1;
		    break;
	    default:
		    ilevel[0] = 0;
		    ilevel[1] = -1;
		    break;
	}

}
