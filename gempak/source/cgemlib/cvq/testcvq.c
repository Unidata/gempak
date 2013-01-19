#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

int main ( void )
/************************************************************************
 * TESTCVQ								*
 *									*
 * This program tests the CGEMLIB "CVQ" functions.			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * E. Wehner/EAi	 7/97	Fixed typecasting of group type		*
 * E. Wehner/EAi	 7/97	Added cvq_higrp.			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/SAIC	11/01   Added maxoff to cvq_scangrp		*
 ***********************************************************************/
{
	int		cont, iret, numsub, i, ier;
	int		flag, level, elevel, ebuff;
	int		eflag;
	int		ingrp, maxoff;
	long		size;
	char		select[LLSCRN], filnam[LLSCRN];
	char		grp[4], path[133]; 
	VG_DBStruct     el;
	int		vgstart;
	char		gptyp;		/* group type */
	int		gpnum;		/* group number */
	int		members[10];
	int		fpos;
	int		tgtyp;		/* temporary group type */

/*---------------------------------------------------------------------*/
    iret = 0;
    size = 0;
    cont = G_FALSE;
    flag = G_FALSE;
    eflag = G_FALSE;
    strcpy(grp, "CVQ");
    level = 0;
    elevel = 2;
    ebuff = 0;

    in_bdta(&ier);
    er_stat(&elevel, &ebuff, &eflag, &ier);

    vgstart = 0;
    cvg_rdrec ( filnam, vgstart, &el, &iret );

    while ( cont == G_FALSE ) 
    {
	numsub = 0;
	printf ( "\n\n" );
	printf ("  1 = CVQ_SCANGRP   2 = CVQ_GETGINF   3 = CVQ_NXTGNM\n");
	printf ("  4 = CVQ_HIGP   \n");
	printf ( "\n" );
	printf ( "Select a function number or type EXIT: " );
	scanf ( " %s", select );

	switch ( select[0] ) 
	{
	    case 'e':
	    case 'E':
		cont = G_TRUE;
		break;
	    default:
		numsub = atoi ( select );
		break;
        }

/*---------------------------------------------------------------------*/
	switch (numsub)
	{
/*
 *	----------------------------------------------------------
 *	       CVQ_SCANGRP - retrieve the members of a group
 *	----------------------------------------------------------
 */
	    case 1:

		printf ( "Enter the VG file name to scan:\n" );
		scanf ( " %s", filnam );
		printf( "Enter Group type to retrieve:\n");
		scanf( " %i", &tgtyp);
		printf( "Enter Group number to retrieve: \n");
		scanf( " %i", &gpnum);
		printf( "Enter maximum number of offsets to find: \n"); 
		scanf( " %i", &maxoff);

		gptyp = (char)tgtyp;

		cvq_scangrp( filnam, gptyp, gpnum, maxoff, members, 
		             &ingrp, &iret);

		printf ( "\nCVQ_SCANGRP: iret = %d\n\n", iret );
		printf(" Extracted %i elements in group: %i/%i  \n", ingrp,
				gptyp, gpnum);

		for (i = 0;  i< ingrp; i++)
		{
                        /* read header */
                    cfl_inqr ( filnam, " ", &size, path, &ier );
		    if (members[i] < size)
		    {

                        cvg_rdrec ( filnam, members[i], &el, &iret );
                        cvg_dump ( el, el.hdr.recsz, el.hdr.recsz, 
					flag, &iret );

		    }

		}

		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;
/*
 *	----------------------------------------------------------
 *	       CVQ_GETGINF - Return group type and number from elem.
 *	----------------------------------------------------------
 */
	    case 2:
                printf ( "Enter the VG file name to examine:\n" );
                scanf ( " %s", filnam );
                printf( "Enter file location of element in question:\n");
                scanf( " %i", &fpos);

                cvq_getginf( filnam, fpos, &gptyp, &gpnum, &iret);

                printf ( "\nCVQ_GETGINF: iret = %d\n\n", iret );
                printf(" Element is in group %c %i \n", gptyp, gpnum);

                er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
                  strlen(filnam) );
		break;
/*
 *	----------------------------------------------------------
 *	       CVQ_NXTGNM - Get next group number used in file 
 *	----------------------------------------------------------
 */
	    case 3:
                printf ( "Enter the VG file name to examine:\n" );
                scanf ( " %s", filnam );
                printf( "Enter the group type to retrieve from:\n");
                scanf( " %i", &tgtyp);
		gptyp = (char)tgtyp;
	
		printf( "Locate the next group number above: \n");
		scanf( " %i", &gpnum);

                cvq_nxtgnm( filnam, gptyp, &gpnum, &iret);

                printf ( "\nCVQ_NXTGNM: iret = %d\n\n", iret );
                printf(" The next group number of group type %c is %i \n", 
				gptyp, gpnum);

                er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
                  strlen(filnam) );

		break;

/*
 *	----------------------------------------------------------
 *	       CVQ_HIGP - Get highest group number used in file 
 *	----------------------------------------------------------
 */
	    case 4:
                printf ( "Enter the VG file name to examine:\n" );
                scanf ( " %s", filnam );
	
                cvq_higp( filnam, &gpnum, &iret);

                printf ( "\nCVQ_HIGP: iret = %d\n\n", iret );
                printf(" The highest group number is %i \n", 
				gpnum);

                er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
                  strlen(filnam) );

		break;


	}
    }
    return 0;
}
