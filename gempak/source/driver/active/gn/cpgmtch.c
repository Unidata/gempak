#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


void pg_mtch ( FILE *fp, char *srch_str, char *srch_str2, char *prod_str )
/************************************************************************
 * pg_mtch								*
 *									*
 * This function searches for a matching product string in the product	*
 * table.								*
 *									*
 * pg_mtch ( fp, srch_str, srch_str2, prod_str )			*
 *									*
 * Input parameters:							*
 *	*fp		FILE	Pointer to open file to search		*
 *	*srch_str	char 	First string to match in file		*
 *	*srch_str2	char 	Second criteria string to match		*
 *									*
 * Output parameters:							*
 *	*prod_str	char	Matching product string			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96	Created					*
 * E. Wehner/EAi	12/96	Remove hardcode string passing		*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * T. Piper/GSC		10/98	Prolog update				*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    int found = 0;
    int eof = 0;
    int ipos;
    int iret;
    char tmp_str[100];

    
    while ((!found) && (!eof))
    {
        tmp_str[0] = '\0';
        /* retrieve the next line from the file */
        cfl_rdln(fp, 99, tmp_str,  &iret);


        if (iret == 0)
        {
            /* look for match, first on "srch_str", then on "srch_str2" if
               it is used */
            if (strlen(srch_str) > (size_t)1)
            {
              cst_srch(0, (int)strlen(srch_str)-1,srch_str, tmp_str, &ipos, &iret);  
              if (iret == 0)
              {
                if (strlen(srch_str2) > (size_t)2)
                {
                    cst_srch((int)strlen(srch_str), (int)strlen(tmp_str)-1, 
					srch_str2, tmp_str, &ipos,
                                           &iret);
                    if (iret == 0)
                    {
                        found = 1;
                    }
                }
                else
                {
                    found=1;
                }
              }
          }
        }
        else
        {
            eof = 1;

        }

    }
    if (!found)
        prod_str[0] = '\0';
    else
	strcpy(prod_str, tmp_str);

    return;
}


