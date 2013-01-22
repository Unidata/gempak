#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cescmn.h"
#include "cesgtcmn.h"
#include "pgprm.h"

void ces_clist  ( int *iret );
void ces_eldmp  ( int, VG_DBStruct*, int *iret );
void ces_list   ( int *iret );
void ces_gtlist ( int *iret );
void ces_qset   ( int*, VG_DBStruct*, int *iret );
void ces_trln   ( FILE *fptr, int bufsiz, char *buffer, int *iret);

#define	LENBUF	256

static Boolean	groups_initialized = FALSE;


int main ( void )
/************************************************************************
 * TESTCES								*
 *									*
 * This program tests the CGEMLIB "CES" functions.			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi    8/97    Created					*
 * C. Lin/EAi	    9/97    Modified to test default table in ces_rstng	*
 * F.J.Yen/NCEP     4/98    New ces names. Add lists and cesset.Clean up*
 * F.J.Yen/NCEP     5/98    Added DARR_ELM and HASH_ELM.		*
 * I. Durham/GSC    5/98    Changed underscore decl. to an include	*
 * W. Li/EAI	    07/98   Added txt_attrib's text value setting	*
 * C. Lin/EAI	    09/98   Added smoothing level for line/front	*
 * S. Jacobs/NCEP   12/98   Fixed typo					*
 * A. Hardy/GSC     12/98   Added CLASS_CIRCLE and CIRCLE_ELM           *
 * F. J. Yen/NCEP    5/99   Fixed spacing to print in columns		*
 * S. Jacobs/NCEP    5/99   Added CLASS_TRACKS and TRKSTORM_ELM		*
 * S. Jacobs/NCEP    5/99   Swapped TRACK and CIRCLE in print statements*
 * J. Wu/GSC	    02/01   Modified 'unused1' in VG to 'smooth'	*
 * H. Zeng/EAI      02/01   Added new ces routines.                     *
 * H. Zeng/EAI      03/01   Added more ces routines.                    *
 * D.W.Plummer/NCEP  3/01   Added rdgrptbl flag				*
 * S. Jacobs/NCEP	 3/01	Removed ces_gtgcolr			*
 * E. Safford/SAIC	02/02	add cesgtcmn.h include			*
 * E. Safford/SAIC	04/02	correct ces_gtggrps() calling seq.	*
 * H. Zeng/EAI          04/02   added new ces routines                  *
 * H. Zeng/EAI          05/02   added new ces routines                  *
 * J. Wu/SAIC           11/02   add class LIST		  		*
 * H. Zeng/XTRIA	03/03   added ces_getflag() calling seq.	*
 * J. Wu/SAIC           09/03   add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC           01/04   add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC		02/04   added CLASS_MET -> TCA_ELM		*
 * B. Yin/SAIC		03/04   fixed a bug in option 4 for tca		*
 * F. J. Yen/NCEP	04/07	Fixed print alignmnt & spelling of GROUP*
 * L. Hinson/AWC        01/12   Add CLASS_MET -> SGWX_ELM               *
 ***********************************************************************/
{
int		cont, iret, numsub, ngrp, nlbl, ier;
int		level, elevel, ebuff, eflag, ii;
int		index, iclass, ivgtyp, subtyp, grpid, grp_avid;
char		select[LLSCRN], logstr[10], filnam[25];
char		grp[4], grpnam[20], *names = NULL, lbls[256], ppid[32];
int		rdgrptbl = 0;
Boolean         incl_dev, layer_flag;
VG_DBStruct     el;

/*---------------------------------------------------------------------*/
    iret = 0;
    strcpy(grp, "CES");
    level = 0;

    elevel = 2;
    ebuff = 0;
    eflag = G_FALSE;
    in_bdta(&ier);
    er_stat(&elevel, &ebuff, &eflag, &ier);


    cont = G_FALSE;
    while ( cont == G_FALSE ) 
    {
	numsub = 0;
	printf ( "\n\n" );
	printf ("======================================================="
		"================\n\n");
	printf ("  1 = CES_RTBL    2 = CES_GETINX   3 = CES_GET   "
		"  4 = CES_SET\n  5 = List init setting table    "
		"  6 = List current settings\n"
                "  7 = CES_GTRTBL  8 = CES_GTGID    9 = CES_GTGNAM\n"
                " 10 = CES_GTGGRPS 11= CES_GTGLBLS \n"
		" 12 = Dump current group structure \n"
                " 13 = CES_GTGAVID 14= CES_GTGMSID  \n"
                " 15 = CES_GTGMGRPS                 \n"
                " 16 = CES_GETFLAG		    \n" );
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
	    case 1:
		sprintf (filnam, "%s", SETTING_TBL);
                printf ( "Reading the setting table %s...\n", filnam );
		ces_rtbl(&iret);
		printf ( "\nCES_RTBL: iret = %d\n\n", iret );
		
    		break;
	 
	    case 2:
    		printf ("Enter VG_TYPE ( 1 LINE       2 FRONT    4 CIRCLE  5 WX SYM"
            		"  6 WBOX    8 WND BARB\n"
			"                9 WND ARRW  10 CTSYM   11 ICSYM  12 PTSYM " 
			" 13 PWSYM  14 SKSYM\n"
			"               15 SPSYM     16 TBSYM   17 TEXT   18 TEXTC "
			" 20 SPLN   21 SPTX\n"
			"               23 DARR      24 HASH    25 COMBO  26 TRACK "
			" 34 LIST   37 JET\n"
			"               38 GFA       39 TCA ):\n");
                scanf( " %i", &ivgtyp);
		if ( ivgtyp == 39 ) {
		   printf( "\nTCA is not supported at the moment!\n" );
		   break;
	        }
		el.hdr.vg_type = (char)ivgtyp;
		printf("Enter VG_CLASS ( 1 FRONTS   2 WATCHES   3 LINES   4 SYMBOLS   "
		       "5 TEXT   6 WINDS\n"
		       "                10 TRACK   11 CIRCLE   14 LIST   15 MET ):\n");
		scanf(" %i", &iclass);
		el.hdr.vg_class = (char)iclass;
		printf("Enter SUBTYP: \n");
		scanf(" %i", &subtyp);

		ces_getinx(&el, subtyp, &index, &iret);
                printf ( "\nCES_GETINX: iret = %d\n\n", iret );

		if (iret == 0) {
		  printf("Settings are located at position %i \n", index);
		}
		else {
		  printf ("Unable to obtain index for subtyp %i\n", subtyp);
		}		
		break;

	    case 3:
    		printf ("Enter VG_TYPE ( 1 LINE       2 FRONT   4 CIRCLE   5 WX SYM"
            		"  6 WBOX    8 WND BARB\n"
			"                9 WND ARRW  10 CTSYM  11 ICSYM  12 PTSYM " 
			" 13 PWSYM  14 SKSYM\n"
			"               15 SPSYM     16 TBSYM  17 TEXT   18 TEXTC "
			" 20 SPLN   21 SPTX\n"
			"               23 DARR      24 HASH   25 COMBO  26 TRACK "
			" 34 LIST   37 JET\n"
			"               38 GFA       39 TCA ): \n");
                scanf( " %i", &ivgtyp);
		if ( ivgtyp == 39 ) {
		   printf( "\nTCA is not supported at the moment!\n" );
		   break;
	        }
		el.hdr.vg_type = (char)ivgtyp;
		printf("Enter VG_CLASS ( 1 FRONTS   2 WATCHES   3 LINES   4 SYMBOLS   "
		       "5 TEXT   6 WINDS\n"                  
		       "                10 TRACK   11 CIRCLE   14 LIST   15 MET ):\n");
		scanf(" %i", &iclass);
		el.hdr.vg_class = (char)iclass;
		printf("Enter SUBTYP: \n");
		scanf(" %i", &subtyp);

		ces_get(subtyp, &el, &iret);
                printf ( "\nCES_GET: iret = %d\n\n", iret );

		if ( iret == 0 ) {
		  ces_eldmp(subtyp, &el, &iret);
		}
		break;

	    case 4:
		    
		ces_qset (&subtyp, &el, &iret);

		if (iret == -3) {
		  printf ("\nCES_QSET: iret = %d\n", iret);
		  sprintf(logstr, "%i", subtyp);
		  er_lmsg ( &level, grp, &iret, logstr, &ier, strlen(grp),
		      strlen(logstr) );
		}
		else if ( iret == -4 ) {
       		  printf( "\nTCA is not supported at the moment!\n" );
		}
		else {
		  ces_set (subtyp, &el, &iret);
                  printf ( "\nCES_SET: iret = %d\n\n", iret );
		}
		break;

	    case 5:

		sprintf (filnam, "%s", SETTING_TBL);
                printf ( "\n\nListing the setting table %s...\n", filnam );

		ces_list(&iret);
		printf ( "\nCES_LIST: iret = %d\n\n", iret );
		break;

	    case 6:

		ces_clist(&iret);
		printf ( "\nCES_CLIST: iret = %d\n\n", iret );
		break;

	    case 7:
		sprintf (filnam, "%s", GRPTYPE_TBL);
                printf ( "Reading the group type table %s...\n", filnam );

		ces_gtrtbl(&iret);
		printf ( "\nCES_GTRTBL: iret = %d\n\n", iret );
		rdgrptbl = 1;

		if ( iret >= 0 ) {
		    groups_initialized = TRUE;
		}
    		break;

	    case 8:
		if ( rdgrptbl == 0 )  {
		    printf("Must read group table first.\n");
		    break;
		}
    		printf ("Enter GROUP NAME: \n");
                scanf( " %s", grpnam);
		cst_lcuc ( grpnam, grpnam, &iret );
		ces_gtgid(grpnam, &grpid, &iret);
                printf ( "\nCES_GTGID: iret = %d\n\n", iret );

		if (iret == 0) {
		  printf("The GROUP TYPE ID = %i \n", grpid);
		}
		else {
		  printf ("Unable to obtain group type id for %s\n", grpnam);
		}		
		break;

	    case 9:
		if ( rdgrptbl == 0 )  {
		    printf("Must read group table first.\n");
		    break;
		}
    		printf ("Enter GROUP ID: \n");
                scanf( " %i", &grpid);
		ces_gtgnam(grpid, grpnam, &iret);
                printf ( "\nCES_GTGNAM: iret = %d\n\n", iret );

		if (iret == 0) {
		  printf("The GROUP TYPE NAME = %s \n", grpnam);
		}
		else {
		  printf ("Unable to obtain group type name for %i\n", grpid);
		}		
		break;

            case 10:
                ces_gtggrps(&ngrp, &names, &iret);
                printf ( "\nCES_GTGGRPS: iret = %d\n\n", iret );

                if (iret == 0) {
                  printf("The # OF GROUP TYPES = %d \n", ngrp);
                  printf("GROUP TYPE NAMES = %s \n", names);
                }
                else {
                  printf ("Unable to obtain total # of group types.\n");
                }

		if ( names != NULL ) free (names);

                break;

            case 11:
                printf ("Enter GROUP ID: \n");
                scanf( " %i", &grpid);
                ces_gtglbls(grpid, &nlbl, lbls, &iret);
                printf ( "\nCES_GTGLBLS: iret = %d\n\n", iret );

                if (iret == 0) {
                  printf("The # OF LABELS =%d \n", nlbl);
                  printf("The LABEL NAMES =%s \n", lbls);
                }
                else {
                  printf ("Unable to obtain label strings for %i\n", grpid);
                }
                break;

            case 12:
		printf ("\n");
		if ( groups_initialized ) {
	 	    ces_gtlist( &iret );
		}
		else {
		    printf ("	Group structures not loaded.\n"
		            "	Call ces_gtrtbl first.\n");
		}
		break;

	    case 13:
		if ( rdgrptbl == 0 )  {
		    printf("Must read group table first.\n");
		    break;
		}
    		printf ("Enter GROUP ID: \n");
                scanf( " %i", &grpid);
		grp_avid = ces_gtgavid(grpid);
		printf("The GROUP TYPE AV ID = %i \n", grp_avid);
		
		break;

	    case 14:
		if ( rdgrptbl == 0 )  {
		    printf("Must read group table first.\n");
		    break;
		}
    		printf ("Enter GROUP TYPE AV ID: \n");
                scanf( " %i", &grp_avid);
		grpid = ces_gtgmsid(grp_avid);
		printf("The GROUP TYPE ID = %i \n", grpid);
		
		break;

            case 15:
    		printf ("Enter 0 to exclude the developmental types\n"
                        "Or    1 otherwise. \n" );
                scanf( " %i", &ii);
                incl_dev = (Boolean)ii;
                ces_gtgmgrps(incl_dev, &ngrp, &names, &iret);
                printf ( "\nCES_GTGMGRPS: iret = %d\n\n", iret );

                if (iret == 0) {
                  printf("The # OF GROUP TYPES = %d \n", ngrp);
                  printf("GROUP TYPE NAMES = %s \n", names);
                }
                else {
                  printf ("Unable to obtain total # of group types.\n");
                }

		if ( names != NULL ) free (names);

                break;

	    case 16:
    		printf ("Enter VG_TYPE ( 1 LINE       2 FRONT    4 CIRCLE  5 WX SYM"
            		"  6 WBOX    8 WND BARB\n"
			"                9 WND ARRW  10 CTSYM   11 ICSYM  12 PTSYM " 
			" 13 PWSYM  14 SKSYM\n"
			"               15 SPSYM     16 TBSYM   17 TEXT   18 TEXTC "
			" 20 SPLN   21 SPTX\n"
			"               23 DARR      24 HASH    25 COMBO  26 TRACK "
			" 34 LIST   37 JET\n"
			"               38 GFA       39 TCA ): \n");
                scanf( " %i", &ivgtyp);
		if ( ivgtyp == 39 ) {
		   printf( "\nTCA is not supported at the moment!\n" );
		   break;
	        }
		el.hdr.vg_type = (char)ivgtyp;
		printf("Enter VG_CLASS ( 1 FRONTS   2 WATCHES   3 LINES   4 SYMBOLS   "
		       "5 TEXT   6 WINDS\n"
		       "                10 TRACK   11 CIRCLE   14 LIST   15 JET ):\n");
		scanf(" %i", &iclass);
		el.hdr.vg_class = (char)iclass;
		printf("Enter SUBTYP: \n");
		scanf(" %i", &subtyp);

		layer_flag = ces_getflag(subtyp, &el, &iret);
                printf ( "\nCES_GETFLAG: iret = %d\n\n", iret );

		if (iret == 0) {
		  printf("Layer flag for the VG element is %u \n", layer_flag);
		}
		else {
		  printf ("Unable to obtain layer flag for subtyp %i\n", subtyp);
		}
		
		break;

	    case 17:
    		printf ( "Enter VG_TYPE ( 1 LINE       2 FRONT    4 CIRCLE  5 WX SYM"
            		 "  6 WBOX    8 WND BARB\n"
			 "                9 WND ARRW  10 CTSYM   11 ICSYM  12 PTSYM " 
			 " 13 PWSYM  14 SKSYM\n"
			 "               15 SPSYM     16 TBSYM   17 TEXT   18 TEXTC "
			 " 20 SPLN   21 SPTX\n"
			 "               23 DARR      24 HASH    25 COMBO  26 TRACK "
			 " 34 LIST   37 JET\n"
			 "               38 GFA       39 TCA ): \n" );
                scanf ( " %i", &ivgtyp );
		if ( ivgtyp == 39 ) {
		    printf ( "\nTCA is not supported at the moment!\n" );
		    break;
	        }
		
		el.hdr.vg_type = (char)ivgtyp;
		printf ( "Enter VG_CLASS ( 1 FRONTS   2 WATCHES   3 LINES   4 SYMBOLS   "
		         "5 TEXT   6 WINDS\n"
		         "                10 TRACK   11 CIRCLE   14 LIST   15 JET ):\n" );
		scanf ( " %i", &iclass );
		el.hdr.vg_class = (char)iclass;
		printf ( "Enter SUBTYP: \n" );
		scanf ( " %i", &subtyp );

		if ( iret == 0 ) {
		    printf ( "Post-processing setting for the VG element is %s \n", ppid );
		}
		else {
		    printf ( "Unable to obtain post-processing setting for subtyp %i\n", subtyp );
		}
		
		break;
	}
    }
    return(0);
}

/*=====================================================================*/

void ces_clist ( int *iret )
/************************************************************************
 * ces_clist								*
 *									*
 * This function lists current settings.				*
 *									*
 * void ces_clist ( iret )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * F. J. Yen/NCEP	 4/98	Created					*
 * W. Li/EAI	03/99	added latitude/longitude for symbols		*
 * F. J. Yen/NCEP	 5/99	Add SMTH for fronts/lines; fix spacing/ *
 *				typo in CLASS_CIRCLE and _SYMBOLS lists.*
 * D.W.Plummer/NCEP	 3/01	Added SIGMET processing;		*
 *				Added default: to case statements	*
 * J. Wu/SAIC           11/02   add class LIST		  		*
 * H. Zeng/XTRIA	01/03   added watch marker info.		*
 * J. Wu/SAIC           08/03   add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC           01/04   add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC          02/04   added CLASS_MET -> TCA_ELM		*
 * J. Wu/SAIC           05/04   add barb/hash color in JET_ELM attr.	*
 * J. Wu/SAIC		05/04   add post-processing setting for text	*
 * J. Wu/SAIC           09/04   add text type for jet barb text		*
 * J. Wu/SAIC		10/04   remove line width from GFA_ELM		*
 * J. Wu/SAIC		11/04   fix error on listing TCA_ELM settings	*
 * T. Piper/SAIC	12/05	redone for new Setting_t structure	*
 ***********************************************************************/
{
    int		idx;
    char	curvgtype, curvgclass;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    idx = 0;
    curvgclass = 99;
    curvgtype = 99; 

    printf ("\nVGCLASS      VGTYPE SUBTYP");

    while ( idx < num_set )
    {
        switch (set[idx].vg_class)
	{
	  case CLASS_FRONTS:
	    printf ("\n\t\t\t  MAJ MIN SMTH SIZE DIR STRENGTH\n");
	    curvgclass = set[idx].vg_class;
	    while (idx < num_set && set[idx].vg_class == curvgclass) 
	    {
	      printf ("CLASS_FRONTS\t%3i  %03i   %2i  %2i  %2i   %3.1f  %2i   %2i\n",
		    set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		    set[idx].min_col, set[idx].smooth,
		    ((float)set[idx].info.frt->fpipsz/100.0F),
		    set[idx].info.frt->fpipdr, set[idx].info.frt->fwidth);
	      idx++;
	    }
	    break;

	  case CLASS_WATCHES:
	    printf ("\n\t\t\t  MAJ MIN W_TYPE M_TYP M_SIZ M_WID\n");
	    curvgclass = set[idx].vg_class;
	    while (idx < num_set && set[idx].vg_class == curvgclass) 
	    {
	      printf ("CLASS_WATCHES\t%3i  %3i   %2i  %2i    %2i    %2i  %5.1f    %2i\n",
		    set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		    set[idx].min_col, set[idx].info.wbx->w_type,
                    set[idx].info.wbx->w_mrktyp, set[idx].info.wbx->w_mrksiz,
                    set[idx].info.wbx->w_mrkwid);
	      idx++;
	    }
	    break;

	  case CLASS_TRACKS:
	    printf ("\n\t\t\t  MAJ MIN LN1 LN2 MK1 MK2 WDTH INCR\n");
	    curvgclass = set[idx].vg_class;
	    while (idx < num_set && set[idx].vg_class == curvgclass)
	    {
		  printf ("CLASS_TRACKS\t%3i  %3i   %2i  %2i  %2i  %2i  %2i"
		      "  %2i  %3i  %3i\n",  
		      set[idx].vg_type, set[idx].subtyp,
		      set[idx].maj_col, set[idx].min_col,
		      set[idx].info.trk->ltype1, set[idx].info.trk->ltype2,
		      set[idx].info.trk->mtype1, set[idx].info.trk->mtype2,
		      set[idx].info.trk->width, set[idx].info.trk->incr);
	          idx++;
	        }
	    break;

	  case CLASS_LINES:
            switch (set[idx].vg_type)
	    {
	      case LINE_ELM:
	        printf ("\n\t\t\t  MAJ MIN SMTH LINTYP LTHW WIDTH LWHW\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		  printf ("CLASS_LINES\t%3i  %3i   %2i  %2i   %2i   %2i    %2i"
				"    %3i  %3i\n",  
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].smooth,
		      set[idx].info.lin->lintyp, set[idx].info.lin->lthw,
		      set[idx].info.lin->width, set[idx].info.lin->lwhw);
	          idx++;
	        }
	        break;
	      case SPLN_ELM:
		printf ("\n\t\t\t  MAJ MIN SMTH LTYP DIR SIZE WIDTH\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		  printf ("CLASS_LINES\t%3i  %3i   %2i  %2i  %2i %3i  %3i  "
		      "%4.1f  %3i\n",
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].smooth,
		      set[idx].info.spl->spltyp, set[idx].info.spl->spldir,
		      set[idx].info.spl->splsiz, set[idx].info.spl->splwid);
	          idx++;
	        }
	        break;
	      default:
		printf("Unknown vg_type (%d) in CLASS_LINES\n", set[idx].vg_type );
		idx++;
		break;
	    }
	    break;
 
	  case CLASS_SYMBOLS:
	    printf ("\n\t\t\t  MAJ MIN WIDTH SIZE LATITUDE LONGITUDE\n");
	    curvgclass = set[idx].vg_class;
	    while (idx < num_set && set[idx].vg_class == curvgclass) 
	    {
	      printf ("CLASS_SYMBOLS\t%3i  %3i   %2i  %2i   %3i %4.1f   %3.2f      %3.2f\n",
		    set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		    set[idx].min_col, set[idx].info.sym->info.width, set[idx].info.sym->info.size,
		    set[idx].info.sym->data.latlon[0], set[idx].info.sym->data.latlon[1]);
	      idx++;
	    }
	    break;

	  case CLASS_WINDS:
            switch (set[idx].vg_type)
	    {
              case ARROW_ELM:
	      case DARR_ELM:
		printf ("\n\t\t\t  MAJ MIN WIDTH SIZE HDSIZE\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		  printf ("CLASS_WINDS\t%3i  %3i   %2i  %2i   %2i  %4.1f  %5.1f\n",
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].info.wnd->width,
		      set[idx].info.wnd->size, set[idx].info.wnd->hdsiz);
	          idx++;
	        }
	        break;
              case BARB_ELM:
	      case HASH_ELM:
		printf ("\n\t\t\t  MAJ MIN WIDTH SIZE\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		  printf ("CLASS_WINDS\t%3i  %3i   %2i  %2i   %2i  %4.1f\n",
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].info.wnd->width,
		      set[idx].info.wnd->size);
	          idx++;
	        }
	        break;
	      default:
		printf("Unknown vg_type (%d) in CLASS_WINDS\n", set[idx].vg_type );
		idx++;
		break;
	    }
	    break;

	  case CLASS_TEXT:
            switch (set[idx].vg_type)
	    {
              case TEXT_ELM:
              case TEXTC_ELM:
		printf ("\n\t\t\t  MAJ MIN ROT SIZE FONT WIDTH ALIGN\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		  printf ("CLASS_TEXT\t%3i  %3i   %2i  %2i %3.0f %4.1f  %2i  "
			  "%3i   %3i\n",
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].info.txt->info.rotn,
		      set[idx].info.txt->info.sztext, set[idx].info.txt->info.itxfn,
		      set[idx].info.txt->info.iwidth, set[idx].info.txt->info.ialign);
	          idx++;
	        }
	        break;
              case SPTX_ELM:
		printf ("\n\t\t\t  MAJ MIN ROT SIZE TURB FONT HW WIDTH ALIGN\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		  printf ("CLASS_TEXT\t%3i  %3i   %2i  %2i %3.0f %4.1f  %2i   %2i"
			  "  %2i %3i   %3i \n",
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].info.spt->text.info.rotn,
		      set[idx].info.spt->text.info.sztext, set[idx].info.spt->text.info.turbsym,
		      set[idx].info.spt->text.info.itxfn, set[idx].info.spt->text.info.ithw,
		      set[idx].info.spt->text.info.iwidth, set[idx].info.spt->text.info.ialign);
	          idx++;
	        }
	        break;
	      default:
		printf("Unknown vg_type (%d) in CLASS_TEXT\n", set[idx].vg_type );
		idx++;
		break;
	    }
	    break;

	  case CLASS_CIRCLE:
	    printf ("\n\t\t\t  MAJ MIN LINTYP LTHW WIDTH LWHW\n");
	    curvgtype = set[idx].vg_type;
	    while (idx < num_set && set[idx].vg_type == curvgtype)
	    {
		printf ("CLASS_CIRCLE\t%3i  %3i   %2i  %2i    %2i    %2i   %3i"
		      "  %3i\n",  
		      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		      set[idx].min_col, set[idx].info.cir->lintyp,
		      set[idx].info.cir->lthw, set[idx].info.cir->width,
		      set[idx].info.cir->lwhw);
		idx++;
	    }
	    break;

	  case CLASS_SIGMETS:
	    curvgclass = set[idx].vg_class;
	    switch (set[idx].vg_type)
	    {
	      case SIGCCF_ELM:
		printf ("\n\t\t\t  SMTH\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype) 
	        {
		    printf ("CLASS_SIGMETS\t%3i  %3i   %2i\n", 
				set[idx].vg_type, set[idx].subtyp, set[idx].smooth);
		    idx++;
		}
		break;
	      case ASHCLD_ELM:
		printf ("\n\t\t\t  MAJ MIN SMTH\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		    printf ("CLASS_SIGMETS\t%3i  %3i   %2i  %2i   %2i\n",
			set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
			set[idx].min_col, set[idx].smooth);
		    idx++;
		}
	        break;
	      case VOLC_ELM:
		printf ("\n\t\t\t  MAJ MIN WIDTH SIZE\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)             
                {
		    printf ("CLASS_SIGMETS\t%3i  %3i   %2i  %2i   %2i  %4.1f\n",
			set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
			set[idx].min_col, set[idx].info.vol->width, 
			set[idx].info.vol->size);
		    idx++;
		}
		break;
	      default:
		printf ("\n\t\t\t  MAJ MIN SMTH LINTYP LTHW\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)             
                {
		    printf ("CLASS_SIGMETS\t%3i  %3i   %2i  %2i   %2i   %2i   %3i\n",
                      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
                      set[idx].min_col, set[idx].smooth,
		      set[idx].info.sig->lintyp, set[idx].info.sig->linwid );
		    idx++;
		}
		break;
	    }
	    break;
	  
	  case CLASS_LIST:
	    printf ("\n\t\t\t  MAJ MIN SUBTYP MRKTYP MRKSIZ MRKWID\n");
	    curvgclass = set[idx].vg_class;
	    while (idx < num_set && set[idx].vg_class == curvgclass) 
	    {
	      printf ("CLASS_LIST\t%3i  %3i   %2i  %2i    %3i    %2i     %3.1f   %3i\n",
                      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
                      set[idx].min_col,
		      set[idx].info.lst->subtyp, set[idx].info.lst->mrktyp,
		      set[idx].info.lst->mrksiz, set[idx].info.lst->mrkwid );
	      idx++;
	    }
	    break;
	  
	  case CLASS_MET:
            switch (set[idx].vg_type)
	    {
              case JET_ELM:
	        printf ("\n\t\t\t  MAJ MIN SMTH  LINE LINE LINE");
	        printf (" BARB BARB BARB TEXT TEXT TEXT TEXT TEXT TEXT HASH HASH HASH GRP\n");
	        printf ("\t\t\t\t\tTYPE SIZE WDTH COLR");
	        printf (" SIZE WDTH TYPE SIZE FONT ITHW WDTH ALGN COLR SIZE WDTH\n");
		curvgtype = set[idx].vg_type;
		while (idx < num_set && set[idx].vg_type == curvgtype)
		{
		    printf ( "CLASS_MET\t%3i  %3i   %2i  %2i  %2i    %2i   %3.1f"
			     "  %2i   %3i  %3.1f  %2i  %3i  %3.1f  %2i   %2i"
			     "  %2i   %2i    %3i  %3.1f   %2i %5s\n",
			set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
		        set[idx].min_col, set[idx].smooth,
 		        set[idx].info.jet->line.spltyp, 
 		        set[idx].info.jet->line.splsiz, 
			set[idx].info.jet->line.splwid,
			set[idx].info.jet->barb[0].wndcol,		        
			set[idx].info.jet->barb[0].wnd.info.size,
		        set[idx].info.jet->barb[0].wnd.info.width,
			set[idx].info.jet->barb[0].spt.info.sptxtyp,
			set[idx].info.jet->barb[0].spt.info.sztext,
		        set[idx].info.jet->barb[0].spt.info.itxfn,
		        set[idx].info.jet->barb[0].spt.info.ithw,
		        set[idx].info.jet->barb[0].spt.info.iwidth,
		        set[idx].info.jet->barb[0].spt.info.ialign,
		        set[idx].info.jet->hash[0].wndcol,
		        set[idx].info.jet->hash[0].wnd.info.size,
		        set[idx].info.jet->hash[0].wnd.info.width, 
			set[idx].grp_typ );
			 
	            idx++;
	        
		}
		break;
	      case GFA_ELM:
	        printf ("\n\t\t\t  MAJ MIN\n");
	        curvgclass = set[idx].vg_class;
	        curvgtype = set[idx].vg_type;
                while (idx < num_set && set[idx].vg_type == curvgtype) 
	        {
	            printf ("CLASS_MET\t%3i  %3i   %2i  %2i\n",
                      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
                      set[idx].min_col );
	            idx++;
	        }
		break;
                
              case SGWX_ELM:
                printf ("\n\t\t\t  MAJ MIN\n");
                curvgclass = set[idx].vg_class;
                curvgtype = set[idx].vg_type;
                while (idx < num_set && set[idx].vg_type == curvgtype) 
	        {
	            printf ("CLASS_MET\t%3i  %3i   %2i  %2i\n",
                      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
                      set[idx].min_col );
	            idx++;
	        }
		break;
                
	      case TCA_ELM:	        
	        curvgclass = set[idx].vg_class;
	        curvgtype = set[idx].vg_type;
                while (idx < num_set && set[idx].vg_type == curvgtype) 
	        {
	            printf ("CLASS_MET\t%3i  %3i   %2i  %2i ",
                      set[idx].vg_type, set[idx].subtyp, set[idx].maj_col,
                      set[idx].min_col );
	            idx++;
	        }
		printf("	<Note: These are not used at the moment>\n" );
	        idx++;
	        break;
	      default:
		printf("Unknown vg_type (%d) in CLASS_MET\n", set[idx].vg_type );
		idx++;
		break;
	    }
	    break;

          default:
	    idx++;
            break;
        }
    }
}

/*=====================================================================*/

void ces_gtlist ( int *iret )
/************************************************************************
 * ces_gtlist								*
 *									*
 * This function lists current group type information.			*
 *									*
 * void ces_gtlist ( iret )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * E. Safford/SAIC	02/02	initial coding				*
 ***********************************************************************/
{
    int		ii, jj;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    printf ("_grpTbl.mstr[] contains %d records:\n", _grpTbl.nmstr);	
    for (ii=0; ii<_grpTbl.nmstr; ii++) {
	printf ("	_grpTbl.mstr[%d].name = %s\n", 
					ii, _grpTbl.mstr[ii].name);
	printf ("		_grpTbl.mstr[%d].type = %d\n",
					ii, _grpTbl.mstr[ii].type);
    }
  
    printf ("\n"); 
    printf ("_grpTbl.grps[] contains %d records:\n", _grpTbl.ngrp);
    for (ii=0; ii<_grpTbl.ngrp; ii++) {
	printf ("	_grpTbl.grps[%d].name = %s\n",
					ii, _grpTbl.grps[ii].name);
	printf ("		_grpTbl.grps[%d].type = %d\n",
					ii, _grpTbl.grps[ii].type);

	for (jj=0; jj < _grpTbl.grps[ii].nitems; jj++) {
	    printf ("		label[%d] = %s\n", jj, _grpTbl.grps[ii].label[jj]);
	    printf ("		info[%d]  = %s\n", jj, _grpTbl.grps[ii].info[jj]);
	}

    } 

}

/*=====================================================================*/

void ces_eldmp ( int subtyp, VG_DBStruct *el, int *iret )
/************************************************************************
 * ces_eldmp								*
 *									*
 * This function dumps settings for a type of element to the screen.	*
 *									*
 * ces_eldmp ( subtyp, el, iret )					*
 *									*
 * Input parameters:							*
 *	subtyp		int		GEMPAK element type (if present)*
 *									*
 * Input/Output parameters:						*
 *	*el		VG_DBStruct	Element as mask and returned	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * F.J. Yen/NCEP	 4/98	Clean up.  Switch on class.  Add fields.*
 * C. Lin/EAi	 	 9/98	add smoothing level for line/font	*
 * A. Hardy/GSC         12/98   Added CLASS_CIRCLE and CIRCLE_ELM       *
 * J. Wu/SAIC           11/02   add class LIST		  		*
 * H. Zeng/XTRIA	01/03   added watch marker info.		*
 * J. Wu/SAIC           08/03   add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC           08/03   add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC		02/04   added CLASS_MET -> TCA_ELM		*
 * J. Wu/SAIC           05/04   add barb/hash color in JET_ELM attr.	*
 * J. Wu/SAIC           09/04   add text type JET_ELM attr.		*
 * J. Wu/SAIC		10/04   remove line width from GFA_ELM		*
 ***********************************************************************/
{
    *iret = G_NORMAL;

    printf("Dumping settings in element header \n");
    printf("   SUBTYP:  %i \n",subtyp);

    switch (el->hdr.vg_class)
    {
      case CLASS_FRONTS:
	  printf("FRONT_ELM CLASS_FRONTS 	fcode=%i smooth= %d "
			"fpipsz= %3.2f fpipdr=%i fwidth=%i \n",
		el->elem.frt.info.fcode,
		el->hdr.smooth,
		(float)el->elem.frt.info.fpipsz/100.0F,
		el->elem.frt.info.fpipdr,
		el->elem.frt.info.fwidth);
	  break;

      case CLASS_WATCHES:
	  printf("WBOX_ELM CLASS_WATCHES 	wtype=%i mrktyp=%i "
		     "mrksiz=%f mrkwid=%i \n",
		  el->elem.wbx.info.w_type,
		  el->elem.wbx.info.w_mrktyp,
		  el->elem.wbx.info.w_mrksiz,
		  el->elem.wbx.info.w_mrkwid );
	  break;

      case CLASS_TRACKS:
	  switch (el->hdr.vg_type)
	  {
	    case TRKSTORM_ELM:
	      printf("TRKSTORM_ELM CLASS_TRACKS ltype1=%i ltype2=%i "
			"mtype1=%i mtype2=%i width=%i incr=%i \n",
		el->elem.trk.info.ltype1,
		el->elem.trk.info.ltype2,
		el->elem.trk.info.mtype1,
		el->elem.trk.info.mtype2,
		el->elem.trk.info.width,
		el->elem.trk.info.incr);
	      break;
	  }
	  break;

      case CLASS_LINES:
	  switch (el->hdr.vg_type)
	  {
	    case LINE_ELM:
	      printf("LINE_ELM CLASS_LINES	lintyp=%i smooth=%i "
			"lthw=%i width=%i lwhw=%i \n",
		el->elem.lin.info.lintyp,
		el->hdr.smooth,
		el->elem.lin.info.lthw,
		el->elem.lin.info.width,
		el->elem.lin.info.lwhw);
	      break;
	    case SPLN_ELM:
	      printf("SPLN_ELM CLASS_LINES 	spltyp=%i smooth=%i "
				"spldir=%i splsiz=%f splwid=%i \n",
		el->elem.spl.info.spltyp,
		el->hdr.smooth,
		el->elem.spl.info.spldir,
		el->elem.spl.info.splsiz,
		el->elem.spl.info.splwid);
	      break;
	  }
	  break;
 
      case CLASS_SYMBOLS:
	  printf("SYM_ELM CLASS_SYMBOLS 	vg_type=%i width=%i size=%f \n",
		el->hdr.vg_type,
		el->elem.sym.info.width,
		el->elem.sym.info.size);
	  break;

      case CLASS_CIRCLE:
	      printf("CIRCLE_ELM CLASS_CIRCLE  lintyp=%i smooth=%i "
			"lthw=%i width=%i lwhw=%i \n",
		el->elem.cir.info.lintyp,
		el->hdr.smooth,
		el->elem.cir.info.lthw,
		el->elem.cir.info.width,
		el->elem.cir.info.lwhw);
	      break;

      case CLASS_WINDS:

	  switch (el->hdr.vg_type)
	  {
            case ARROW_ELM:
  	      printf("ARROW_ELM CLASS_WINDS	width=%i size=%f wndtyp=%i hdsiz=%f \n",
		el->elem.wnd.info.width,
		el->elem.wnd.info.size,
		el->elem.wnd.info.wndtyp,
		el->elem.wnd.info.hdsiz);
	      break;
            case BARB_ELM:
	      printf("BARB_ELM CLASS_WINDS	width=%i size=%f wndtyp=%i \n",
		el->elem.wnd.info.width,
		el->elem.wnd.info.size,
		el->elem.wnd.info.wndtyp);
	      break;
            case DARR_ELM:
  	      printf("DARR_ELM CLASS_WINDS	width=%i size=%f wndtyp=%i hdsiz=%f \n",
		el->elem.wnd.info.width,
		el->elem.wnd.info.size,
		el->elem.wnd.info.wndtyp,
		el->elem.wnd.info.hdsiz);
	      break;
            case HASH_ELM:
	      printf("HASH_ELM CLASS_WINDS	width=%i size=%f wndtyp=%i \n",
		el->elem.wnd.info.width,
		el->elem.wnd.info.size,
		el->elem.wnd.info.wndtyp);
	      break;
          }

	  break;

      case CLASS_TEXT:

	  switch (el->hdr.vg_type)
	  {
            case TEXT_ELM:
            case TEXTC_ELM:
	      printf("TEXT_ELM CLASS_TEXT	rot=%f size=%f font=%i ithw=%i "
				         	"width=%i align=%i \n",
		el->elem.txt.info.rotn,
		el->elem.txt.info.sztext,
		el->elem.txt.info.itxfn,
		el->elem.txt.info.ithw,
		el->elem.txt.info.iwidth,
		el->elem.txt.info.ialign);
	      break;
            case SPTX_ELM:
	      printf("SPTX_ELM CLASS_TEXT	subtyp=%i rot=%f size=%f turbsym=%i \n"
		     "                  font=%i ithw=%i width=%i align=%i \n",
		el->elem.spt.info.sptxtyp,
		el->elem.spt.info.rotn,
		el->elem.spt.info.sztext,
		el->elem.spt.info.turbsym,
		el->elem.spt.info.itxfn,
		el->elem.spt.info.ithw,
		el->elem.spt.info.iwidth,
		el->elem.spt.info.ialign);
	      break;
	  }
	  break;

      case CLASS_LIST:

	  printf("LIST_ELM CLASS_LIST	subtyp=%i mrktyp=%i mrksiz=%3.1f mrkwid=%i \n",
	      el->elem.lst.info.subtyp,
	      el->elem.lst.info.mrktyp,
	      el->elem.lst.info.mrksiz,
	      el->elem.lst.info.mrkwid );
	  break;

      case CLASS_MET:

	  switch (el->hdr.vg_type)
	  {
            case JET_ELM:
	      printf("JET_ELM CLASS_MET smooth=%i spltyp=%i splsiz=%3.1f splwid=%i barb_sptxtyp=%i\n"
	             "                  barb_colr=%i barb_size=%3.1f barb_width=%i barb_txtcol=%i\n"
	             "                  barb_sztxt=%3.1f barb_itxfn=%i barb_ithw=%i barb_iwidth=%i\n"
	             "                  barb_ialign=%i hash_colr=%i hash_size=%3.1f hash_width=%i\n",
 	      
	      el->hdr.smooth,
	      el->elem.jet.line.spl.info.spltyp, 
 	      el->elem.jet.line.spl.info.splsiz,
	      el->elem.jet.line.spl.info.splwid,
	      el->elem.jet.barb[0].spt.info.sptxtyp,

	      el->elem.jet.barb[0].wndcol,
	      el->elem.jet.barb[0].wnd.info.size,
	      el->elem.jet.barb[0].wnd.info.width,
	        
	      el->elem.jet.barb[0].sptcol,
	      el->elem.jet.barb[0].spt.info.sztext,
	      el->elem.jet.barb[0].spt.info.itxfn,
	      el->elem.jet.barb[0].spt.info.ithw,
	      el->elem.jet.barb[0].spt.info.iwidth,
	      el->elem.jet.barb[0].spt.info.ialign,
		
	      el->elem.jet.hash[0].wndcol,
	      el->elem.jet.hash[0].wnd.info.size,
	      el->elem.jet.hash[0].wnd.info.width );
	    
	    break;

            case GFA_ELM:
	    
	    break;
          
            case TCA_ELM:
	      printf("TCA settings are not used at this moment.\n" );
	  }
	  
	  break;
	  
      default:
          break;
    }
    printf ("                              maj color %i    min color %i\n",
	    el->hdr.maj_col, el->hdr.min_col);

}

/*=====================================================================*/

void ces_trln ( FILE *fptr, int bufsiz, char *buffer, int *iret )
/************************************************************************
 * ces_trln								*
 *									*
 * This function reads a line of data from a GEMPAK setting file into	*
 * BUFFER.  It terminates the line after the last non-blank character	*
 * with a null character.  It returns comment records also. 	 	*
 *									*
 * Since this is adopted from cfl_trln and is used only internally by	*
 * testces, the error return codes uses table cfl.err.			*
 *									*
 * void ces_trln ( fptr, bufsiz, buffer, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE	File pointer				*
 *	bufsiz		int	Size of buffer				*
 *									*
 * Output parameters:							*
 *	*buffer		char	Text string from file			*
 *	*iret		int	Return code				*
 *				  0 = Normal; full record returned	*
 *				  1 = Record length exceeds passed-in	*
 *					buffer size; 			*
 *					partial record returned		*
 *				  4 = EOF reached			*
 *				 -3 = Read failure			*
 *				 -6 = No file has been opened		*
 *				-24 = Record > internal buffer size	*
 **									*
 * F.J. Yen/NCEP	 4/98	Adopted from cfl_trln.			*
 ***********************************************************************/
{
int	found, lenbuf, nonblank, b1, ier;
char	readbuf[LENBUF], *string, tchar;
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}

	/*
	 *  Read records until a data record is found.
	 */
	found = 0;
	while ( found == 0 ) {

	    /*
	     *  Read the record and check for errors.
	     */
	    string = fgets ( readbuf, LENBUF, fptr );

	    if ( feof(fptr) && !ferror(fptr) )  {
		*iret = 4;
		return;
	    }
	    if ( string == NULL || ferror(fptr) ) {
		*iret = -3;
		return;
	    }

	    /*
	     *  Check the length of returned record.
	     */
	    lenbuf = strlen (readbuf);
	    if ( lenbuf >= LENBUF )  {
		*iret = -24;
		return;
	    }

	    /*
	     *  Check first non-blank character.  
	     *  If valid, copy string and return; otherwise continue.
	     */
	    nonblank = strspn (readbuf, " \t");
	    tchar = readbuf[nonblank];
	    if ( tchar != '\n' && tchar != '\0' ) {
		    
		found = 1;
		if ( lenbuf < bufsiz )  {
		    strcpy (buffer, readbuf);
		    cst_lstr( buffer, &b1, &ier );
		    buffer[b1] = '\0';
		}
		else  {
		    *iret = 1;
		    b1 = bufsiz - 1;
		    strncpy( buffer, readbuf, b1 );
		    buffer[b1] = '\0';
		}
	    }


	}
}

/*=====================================================================*/

void ces_list ( int *iret )
/************************************************************************
 * ces_list								*
 *									*
 * This function reads and prints the setting table.			*
 *									*
 * ces_list ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  2 = Bad read.  Ignore record. *
 *					  1 = Setting array siz exceeded*
 *					 -1 = No setting table		*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 4/98	Adopted from ces_rtbl.			*
 * F. J. Yen/NCEP	 5/98	Excluded comment records from count.	*
 * F. J. Yen/NCEP	 5/99	Increase size of teststr for setting.tbl*
 ***********************************************************************/
{
char	tstr[256], teststr[78], grp[4];
int	istart, loglev, quit, ier1, ier, j;
FILE	*fp;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    /*
     * Convert input string to caps if applicable.  Open the setting
     * table.  If not found, return an error.
     */

    if ( (fp = (FILE * )cfl_tbop(SETTING_TBL, "pgen", &ier)) == NULL) {
        *iret = -1;
	loglev = 2;
	strcpy(grp, "CES");
	er_lmsg (&loglev, grp, iret, SETTING_TBL, &ier1, strlen(grp),
		 strlen(SETTING_TBL));
        return;
    }

    /* 
     * For every line in the setting table list, read in the record,
     */
    istart = 0;
    j = 0;
    quit = G_FALSE;
    while ( ( !quit ) && ( j < MAX_SET ) ) {
	ces_trln ( fp, sizeof(tstr), tstr, &ier );

	if ( ier == 4 ) {
	    /*
             * Here for end of file.
	     */
	    quit = G_TRUE;
	}

	else if ( ier != 0 ) {
	    /*
	     * Here for a bad read; record is ignored.
	     */
	    loglev = 2;
    	    strcpy(grp, "CFL");
	    ier = 2;
	    er_lmsg ( &loglev, grp, &ier, SETTING_TBL, &ier1,
		      strlen(grp), strlen(SETTING_TBL) );
	}

	else {
	    /*
	     * Here to process a good record.
	     */
	    sscanf(tstr, "%s", teststr);

	    if ( strcmp(teststr, "!VGCLASS" ) == 0 ) {
		istart =1;
	    }
	    if (istart == 1) {
   		printf ("%-80s\n", tstr);
	    }
	    if (tstr[0] != '!')
		j++;

	}
    }

    if ( !quit ) {
	/*
	 * Setting array (set) size exceeded.  MAX_SET should
	 * be increased.
	 */
	*iret = 1;
    }
    if ( *iret != 0 ) {

	loglev = 2;
	strcpy(grp, "CES");
	er_lmsg ( &loglev, grp, iret, "\0", &ier1,
		  strlen(grp), strlen("\0") );
    }
    
    /* close the settings file */
    cfl_clos(fp, &ier);

    num_set = j;
}

/*=====================================================================*/

void ces_qset ( int *subtyp, VG_DBStruct *el, int *iret )
/************************************************************************
 * ces_qset								*
 *									*
 * This function querries user for new settings for an element.		*
 *									*
 * ces_qset ( subtyp, el, iret )					*
 *									*
 * Output parameters:							*
 *	*subtyp		int		Subtype				*
 *	*el		VG_DBStruct	Element with new settings	*
 *	*iret		int		Return code			*
 *					   0 = element set		*
 *					  -3 = vg type or class unknown *
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 4/98	Created					*
 * F. J. Yen/NCEP	 5/98	Added DARR_ELM and HASH_ELM		*
 * W. Li/EAI		06/98	Changed front pip size to float		*
 * A. Hardy/GSC          7/98   Gave meaningful names to special lines  *
 * F. J. Yen/NCEP	 9/98	Added SPLN20 and SPLN21.		*
 * C. Lin/EAI		09/98	add smoothing level to line/front	*
 * A. Hardy/GSC         12/98   Added CLASS_CIRCLE and CIRCLE_ELM       *
 * F. J. Yen/NCEP	 5/99	Fixed bus error. Removed sgi86 warnings.*
 * F. J. Yen/NCEP	 5/99	Fixed setting for smoothing and line dir*
 * H. Zeng/EAI          02/01   Added group type info.                  *
 * J. Wu/SAIC           11/02   add class LIST		  		*
 * H. Zeng/XTRIA        01/03   added watch marker info.		*
 * J. Wu/SAIC           08/03   add CLASS_MET->JET_ELM		  	*
 * J. Wu/SAIC           08/03   add CLASS_MET->GFA_ELM		  	*
 * B. Yin/SAIC		02/04   added CLASS_MET->TCA_ELM	  	*
 * B. Yin/SAIC		03/04   fixed a bug for tca (return -4)		*
 * J. Wu/SAIC           05/04   add barb/hash color in JET_ELM attr.	*
 * J. Wu/SAIC           09/04   add text type JET_ELM attr.		*
 * J. Wu/SAIC		10/04   remove line width from GFA_ELM		*
 ***********************************************************************/
{
    float	pipsize;
    int		ivgtyp, iclass, type, itemp, grpid, ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    printf ("Enter VG_TYPE ( 1 LINE       2 FRONT    4 CIRCLE  5 WX SYM"
            "  6 WBOX    8 WND BARB\n"
            "                9 WND ARRW  10 CTSYM   11 ICSYM  12 PTSYM " 
	    " 13 PWSYM  14 SKSYM\n"
	    "               15 SPSYM     16 TBSYM   17 TEXT   18 TEXTC "
	    " 20 SPLN   21 SPTX\n"
	    "               23 DARR      24 HASH    25 COMBO  26 TRACK "
	    " 34 LIST   37 JET\n"
	    "               38 GFA       39 TCA ): \n");
    scanf( " %i", &ivgtyp);
    if ( ivgtyp == 39 ) {
       *iret = -4;
       return;
    }
    el->hdr.vg_type = (char)ivgtyp;
    printf("Enter VG_CLASS ( 1 FRONTS   2 WATCHES   3 LINES   4 SYMBOLS   "
	   "5 TEXT   6 WINDS\n"                  
	   "                10 TRACK   11 CIRCLE   14 LIST   15 MET):\n");
    scanf  (" %i", &iclass);
    el->hdr.vg_class = (char)iclass;
    printf ("Enter subtype: \n");
    scanf  (" %i", subtyp);

    printf ("Enter major color: \n");
    scanf  (" %i", &el->hdr.maj_col);
    printf ("Enter minor color: \n");
    scanf  (" %i", &el->hdr.min_col);
    printf ("Enter group type id: \n");
    scanf  (" %d", &grpid);
    el->hdr.grptyp = (char)grpid;

    switch (el->hdr.vg_class)
    {
      case CLASS_FRONTS:

	  el->elem.frt.info.fcode = *subtyp;
          printf ("Enter smoothing level: \n");
	  scanf  (" %d", &itemp);
	  el->hdr.smooth = (char)itemp;
          printf ("Enter pip size: \n");
	  scanf  (" %f", &pipsize);
	  el->elem.frt.info.fpipsz = (int)(pipsize * 100.0F);
  	  printf ("Enter pip direction: \n");
	  scanf  (" %i", &el->elem.frt.info.fpipdr);
 	  printf ("Enter front code: \n");
	  scanf  (" %i", &el->elem.frt.info.fcode);
	  printf ("Enter width: \n");
	  scanf  (" %i", &el->elem.frt.info.fwidth);
	  break;

      case CLASS_WATCHES:
	  
          printf ("Enter w_type (1 for Svr T-Storm,  2 for Tornado): \n");
	  scanf  (" %i", &el->elem.wbx.info.w_type);
	  scanf  (" %i", &el->elem.wbx.info.w_mrktyp);
	  scanf  (" %f", &el->elem.wbx.info.w_mrksiz);
	  scanf  (" %i", &el->elem.wbx.info.w_mrkwid);          
	  break;

      case CLASS_LINES:

	  switch (el->hdr.vg_type)
	  {
	    case LINE_ELM:

	      printf ("Enter line type: \n");
	      scanf  (" %i", &el->elem.lin.info.lintyp);
              printf ("Enter smoothing level: \n");
	      scanf  (" %d", &itemp);
	      el->hdr.smooth = (char)itemp;
	      printf ("Enter line type flag: \n");
	      scanf  (" %i", &el->elem.lin.info.lthw);
	      printf ("Enter line width size multiplier: \n");
	      scanf  (" %i", &el->elem.lin.info.width);
	      printf ("Enter line width flag: \n");
	      scanf  (" %i", &el->elem.lin.info.lwhw);
	      break;

	    case SPLN_ELM:

	      printf ("Enter special line type: \n\n");
	      printf ("\t 1 = Ballchain       2 = ZigZag        3 = Scallop\n"
		      "\t 4 = PntArrow        5 = AltAngTicks   6 = FillArrow\n"
		      "\t 7 = BoxCircles      8 = Two-Xs        9 = FillCircs\n"
		      "\t10 = LnFillCircLn   11 = TickMarks    12 = Ln-X-Ln\n"
		      "\t13 = Fill-OpenBox   14 = FillCirc-X   15 = Box-X\n"
		      "\t16 = LnCircLn       17 = LnCaretLn    18 = Ln Caret Ln\n"
		      "\t19 = SineCurve      20 = Arrow dashed 21 = Fill Arr dsh\n");
	      scanf  (" %i", &el->elem.spl.info.spltyp);
              printf(" Enter the special line direction indicator:\n");
              scanf ( " %i", &el->elem.spl.info.spldir );
              printf ("Enter smoothing level: \n");
	      scanf  (" %d", &itemp);
	      el->hdr.smooth = (char)itemp;
              printf ( "Enter the special line size:\n" );
              scanf ( " %f", &el->elem.spl.info.splsiz );
              printf ( "Enter the special line width:\n" );
              scanf ( " %i", &el->elem.spl.info.splwid );
	      break;

	    default:

	      *iret = -3;
	      break;
	  }
	  break;
 
      case CLASS_SYMBOLS:

          printf("Enter one of the following symbol types:\n");
          printf ( "Weather\t %2d\n", WXSYM_ELM );
          printf ( "Cloud\t %2d\n", CTSYM_ELM );
          printf ( "Icing\t %2d\n", ICSYM_ELM );
          printf ( "Prs Tnd\t %2d\n", PTSYM_ELM );
          printf ( "Past Wx\t %d\n", PWSYM_ELM );
          printf ( "Sky Cvr\t %2d\n", SKSYM_ELM );
          printf ( "Special\t %2d\n", SPSYM_ELM );
          printf ( "Turbul\t %2d\n\n", TBSYM_ELM );
          scanf ( " %i", &type );
          if ( type == WXSYM_ELM || type == CTSYM_ELM ||
               type == ICSYM_ELM || type == PTSYM_ELM ||
               type == PWSYM_ELM || type == SKSYM_ELM ||
               type == SPSYM_ELM || type == TBSYM_ELM )
	    el->hdr.vg_type = type;
          else
	    el->hdr.vg_type = 99;
          printf ( "Enter the symbol width:\n" );
          scanf ( " %i", &el->elem.sym.info.width );
          printf ( "Enter the symbol size:\n" );
          scanf ( " %f", &el->elem.sym.info.size );
          printf ( "Enter the symbol ityp:\n" );
          scanf ( " %i", &el->elem.sym.info.ityp );

	  break;

      case CLASS_WINDS:

	  switch (el->hdr.vg_type)
	  {
            case ARROW_ELM:
            case DARR_ELM:

              printf ( "Enter the arrow width:\n" );
	      scanf ( " %i", &el->elem.wnd.info.width);
              printf ( "Enter the arrow size:\n" );
	      scanf ( " %f", &el->elem.wnd.info.size);
              printf ( "Enter the wind type:\n" );
	      scanf ( " %i", &el->elem.wnd.info.wndtyp);
              printf ( "Enter the head size:\n" );
	      scanf (" %f", &el->elem.wnd.info.hdsiz);
	      break;

            case BARB_ELM:
            case HASH_ELM:

              printf ( "Enter the barb width:\n" );
	      scanf ( " %i", &el->elem.wnd.info.width);
              printf ( "Enter the barb size:\n" );
	      scanf ( " %f", &el->elem.wnd.info.size);
              printf ( "Enter the wind type:\n" );
	      scanf ( " %i", &el->elem.wnd.info.wndtyp);
	      break;

	    default:

	      *iret = -3;
	      break;
          }
	  break;

      case CLASS_TEXT:

	  switch (el->hdr.vg_type)
	  {
            case TEXT_ELM:
            case TEXTC_ELM:

              printf ( "Enter the text rotation:\n" );
	      scanf (" %f", &el->elem.txt.info.rotn);
              printf ( "Enter the text size:\n" );
	      scanf (" %f", &el->elem.txt.info.sztext);
              printf ( "Enter the text font:\n" );
	      scanf (" %i", &el->elem.txt.info.itxfn);
              printf ( "Enter the text width:\n" );
	      scanf (" %i", &el->elem.txt.info.iwidth);
              printf ( "Enter the text alignment:\n" );
	      scanf (" %i", &el->elem.txt.info.ialign);
	      if (*subtyp == -99) {
	        el->elem.txt.info.ithw = 2 ;
	      }
	      else {
	        el->elem.txt.info.ithw = *subtyp;
	      }
	      break;

            case SPTX_ELM:

              printf ( "Enter the special text type\n" );
 	      scanf (" %i", &el->elem.spt.info.sptxtyp);
              printf ( "Enter the special text rotation:\n" );
	      scanf (" %f", &el->elem.spt.info.rotn);
              printf ( "Enter the special text size:\n" );
	      scanf (" %f", &el->elem.spt.info.sztext);
              printf ( "Enter the turbulence symbol:\n" );
	      scanf (" %i", &el->elem.spt.info.turbsym);
              printf ( "Enter the special text font:\n" );
	      scanf (" %i", &el->elem.spt.info.itxfn);
              printf ( "Enter the special text hw flag:\n" );
	      scanf (" %i", &el->elem.spt.info.ithw);
              printf ( "Enter the special text width:\n" );
	      scanf (" %i", &el->elem.spt.info.iwidth);
              printf ( "Enter the special text alignment:\n" );
	      scanf (" %i", &el->elem.spt.info.ialign);
	      break;

	    default:

	      *iret = -3;
	      break;
	  }
	  break;
	  
      case CLASS_CIRCLE:

	  printf ("Enter line type: \n");
	  scanf  (" %i", &el->elem.cir.info.lintyp);
	  printf ("Enter line type flag: \n");
	  scanf  (" %i", &el->elem.cir.info.lthw);
	  printf ("Enter line width size multiplier: \n");
	  scanf  (" %i", &el->elem.cir.info.width);
	  printf ("Enter line width flag: \n");
	  scanf  (" %i", &el->elem.cir.info.lwhw);
	  break;

      case CLASS_LIST:

	  el->elem.lst.info.subtyp = *subtyp;
	  printf ("Enter marker type: \n");
	  scanf  (" %i", &el->elem.lst.info.mrktyp);
	  printf ("Enter marker size: \n");
	  scanf  (" %f", &el->elem.lst.info.mrksiz);
	  printf ("Enter marker width: \n");
	  scanf  (" %i", &el->elem.lst.info.mrkwid);
	  break;
      
      case CLASS_MET:

	  switch (el->hdr.vg_type)
	  {
	    case JET_ELM:
              printf ("Enter smoothing level: \n");
	      scanf  (" %d", &itemp);
	      el->hdr.smooth = (char)itemp;
	      printf ("Enter jet line type: \n\n");
	      printf ("\t 1 = Ballchain       2 = ZigZag        3 = Scallop\n"
		      "\t 4 = PntArrow        5 = AltAngTicks   6 = FillArrow\n"
		      "\t 7 = BoxCircles      8 = Two-Xs        9 = FillCircs\n"
		      "\t10 = LnFillCircLn   11 = TickMarks    12 = Ln-X-Ln\n"
		      "\t13 = Fill-OpenBox   14 = FillCirc-X   15 = Box-X\n"
		      "\t16 = LnCircLn       17 = LnCaretLn    18 = Ln Caret Ln\n"
		      "\t19 = SineCurve      20 = Arrow dashed 21 = Fill Arr dsh\n");
	      scanf  (" %i", &el->elem.jet.line.spl.info.spltyp);
              printf ( "Enter the jet line size:\n" );
              scanf ( " %f", &el->elem.jet.line.spl.info.splsiz );
              printf ( "Enter the jet line width:\n" );
              scanf ( " %i", &el->elem.jet.line.spl.info.splwid );
	      
              printf ( "Enter the jet barb color:\n" );
	      scanf ( " %i", &el->elem.jet.barb[0].wndcol );
              printf ( "Enter the jet barb size:\n" );
	      scanf ( " %f", &el->elem.jet.barb[0].wnd.info.size);
              printf ( "Enter the jet barb width:\n" );
	      scanf ( " %i", &el->elem.jet.barb[0].wnd.info.width);
              
	      printf ( "Enter the jet text color:\n" );
	      scanf (" %i", &el->elem.jet.barb[0].sptcol );
	      printf ( "Enter the jet text type:\n" );
	      scanf (" %i", &el->elem.jet.barb[0].spt.info.sptxtyp);
	      printf ( "Enter the jet text size:\n" );
	      scanf (" %f", &el->elem.jet.barb[0].spt.info.sztext);
              printf ( "Enter the jet text font:\n" );
	      scanf (" %i", &el->elem.jet.barb[0].spt.info.itxfn);
              printf ( "Enter the jet text hw flag:\n" );
	      scanf (" %i", &el->elem.jet.barb[0].spt.info.ithw);
              printf ( "Enter the jet text width:\n" );
	      scanf (" %i", &el->elem.jet.barb[0].spt.info.iwidth);
              printf ( "Enter the jet text alignment:\n" );
	      scanf (" %i", &el->elem.jet.barb[0].spt.info.ialign);
	      
              printf ( "Enter the jet hash color:\n" );
	      scanf ( " %i", &el->elem.jet.hash[0].wndcol );
              printf ( "Enter the jet hash size:\n" );
	      scanf ( " %f", &el->elem.jet.hash[0].wnd.info.size);
              printf ( "Enter the jet hash width:\n" );
	      scanf ( " %i", &el->elem.jet.hash[0].wnd.info.width);	      
	      
	      for ( ii = 1; ii < MAX_JETPTS; ii++ ) {
	          el->elem.jet.barb[ii].wndcol = el->elem.jet.barb[0].wndcol;
	          el->elem.jet.barb[ii].wnd.info.size = el->elem.jet.barb[0].wnd.info.size;
	          el->elem.jet.barb[ii].wnd.info.width = el->elem.jet.barb[0].wnd.info.width ;
	        
		  el->elem.jet.barb[ii].sptcol = el->elem.jet.barb[0].sptcol;
		  el->elem.jet.barb[ii].spt.info.sptxtyp = el->elem.jet.barb[0].spt.info.sptxtyp;
		  el->elem.jet.barb[ii].spt.info.sztext = el->elem.jet.barb[0].spt.info.sztext;
	          el->elem.jet.barb[ii].spt.info.itxfn = el->elem.jet.barb[0].spt.info.itxfn;
	          el->elem.jet.barb[ii].spt.info.ithw = el->elem.jet.barb[0].spt.info.ithw;
	          el->elem.jet.barb[ii].spt.info.iwidth = el->elem.jet.barb[0].spt.info.iwidth;
	          el->elem.jet.barb[ii].spt.info.ialign = el->elem.jet.barb[0].spt.info.ialign;
		
		  el->elem.jet.hash[ii].wndcol = el->elem.jet.hash[0].wndcol;
		  el->elem.jet.hash[ii].wnd.info.size = el->elem.jet.hash[0].wnd.info.size;
	          el->elem.jet.hash[ii].wnd.info.width = el->elem.jet.hash[0].wnd.info.width;
	      }
	    
	    break;
	    
	    case GFA_ELM:
            break;	    
	    
	    case TCA_ELM:
	      printf ("\nTCA settings are not used at this moment! \n");
            break;	    
          }
          
	  break;

      default:

	  *iret = -3;
          break;
    }

}
