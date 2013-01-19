#include "gb2def.h"

void gb2_proc( gribfield *gfld, char *param )
/************************************************************************
 * gb2_proc                                                             *
 *									*
 * This function adds generating process info to the GEMPAK parameter 	*
 * string where necessary to indentify parameters.			*
 *									*
 * gb2_proc( gfld, param )						*
 *									*
 * Input parameters:							*
 *	*gfld		struct gribfield	Decoded GRIB2 structure *
 *									*
 * Input/Output parameters:						*
 *      *param          char            GEMPAK Parameter string         *
 **									*
 * Log:									*
 * S. Chiswell/Unidata	 8/06	Created					*
 ***********************************************************************/
{
        char	cpds[32];
        int	ext_flag, num;


	ext_flag = 0;
	cpds[0] = '\0';

        switch ( gfld->ipdtnum ) {

	    case  0 :	/* Analysis or forecast at a level or point */
		ext_flag = 1;
		switch ( gfld->ipdtmpl[2] ) {
		    case 6 :
			sprintf( cpds, "FERR");
			break;
		    case 7 :
			sprintf( cpds, "AERR");
			break;
		}
		break;
        }

	if ( ext_flag == 1 ) {
            num = 12 - strlen(param);
            strncat ( param, cpds, (size_t)num );
        }

	return;

}
