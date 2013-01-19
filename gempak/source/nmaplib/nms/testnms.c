#include "nmscmn.h"

#define MXSTRM	20

void dumpcmn ( int *iret );

int main ( void )
/************************************************************************
 * TESTNMS								*
 *									*
 * This program test the NMS library of routines.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * A. Hardy/GSC		10/99	Added nms_ghrn				*
 * S. Jacobs/NCEP	10/99	Added MXSTRM to nms_ghrn call		*
 * S. Jacobs/NCEP	11/99	Added nms_rtbl				*
 * S. Jacobs/NCEP	11/99	Changed arrays of info to structures	*
 * S. Jacobs/NCEP	12/99	Updated data structures			*
 * S. Jacobs/NCEP	 3/00	Updated data structures			*
 * M. Li/GSC		 7/00	Added NMS_SAVE and NMS_REST		*
 * M. Li/SAIC		 5/03	Added color 2 to NMS_DSPL		*
 * T. Piper/SAIC	02/04	Removed unused variable iextr		*
 * F. J. Yen/NCEP	 6/04	Added arrw.ityp				* 
 * T. Piper/SAIC	12/05	Added cds_init; removed from nms_dspl	*
 * H. Zeng/SAIC		06/07	Added ionoff[MAXPLT] for nms_dspl	*
 * T. Piper/SAIC	10/07	Removed nms_ghrn - not used		*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ***********************************************************************/
{
	int	cont, ier, iret, numsub, id, icnt;
	char	ergrp[4], erstr[81], select[LLSCRN];

	int	iindex, jindex, ititl,
		mode, istat, isbcat, ii, ionoff[MAXPLT],
		iclrs[MAXPLT], iclrs2[MAXPLT], iflgs[MAXPLT],
		ntype, nflag;
	char	alias[81], filnam[81],
		garea[81], proj[5], panel[81], dattim[21], device[81],
		map[21], ltln[21], ans[9];

	float	fvalu[MAXPLT],
		fline[MAXPLT*3], farrw[MAXPLT*4],
		fsym1[MAXPLT*3], fsym2[MAXPLT*3];

	char	blank[] = " ";

	NMS_types	types[MAXPLT];
	NMS_flags	flags[MAXPLT];

/*---------------------------------------------------------------------*/

	in_bdta ( &ier );
	gd_init ( &ier );

	mode = 1;
	ginitp ( &mode, &istat, &ier );

	printf ( "Enter full DEVICE string:\n" );
	scanf ( " %s", device );

	gg_sdev ( device, &ier, strlen ( device ) );

	strcpy ( ergrp, "NMS" );

	cds_init( &ier );

	cont = G_TRUE;

	while ( cont ) {
	    printf ( "\n\n" );
	    printf ( "   1 = NMS_INIT   2 = NMS_SATT   3 = NMS_QATT\n" );
	    printf ( "   4 =            5 = NMS_DSPL   6 = NMS_RTBL\n" );
	    printf ( "   7 = NMS_SAVE   8 = NMS_REST \n\n" );
	    printf ( "  10 = Dump common\n\n" );
	    printf ( "  20 = Change device\n\n" );
	    printf ( "\n" );
	    printf ( "Select a subroutine number or type EXIT: " );
	    scanf ( " %s", select );
	    switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = G_FALSE;
		default:
			numsub = atoi ( select );
			break;
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 1 )  {
		nms_init ( &iret );

		printf ( "iret = %d\n", iret );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 2 )  {
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );
		printf ( "Enter the data alias (e.g., WTCH, VGF):\n" );
		scanf  ( " %s", alias );
		printf ( "Enter data subcategory number:\n" );
		scanf  ( " %d", &isbcat );
		printf ( "Enter the data file name, or NONE:\n" );
		scanf  ( " %s", filnam );

		printf ( "Enter the number of attribute types:\n" );
		scanf  ( " %d", &ntype );
		for ( icnt = 0; icnt < ntype; icnt++ )  {
		    printf ( "Enter name %d:\n", icnt );
		    scanf  ( " %[^\n]", types[icnt].name );
		    printf ( "Enter on_off, color and value %d:\n", icnt );
		    scanf  ( " %d %d %f", &(types[icnt].ionoff),
					  &(types[icnt].icolr),
		    			  &(types[icnt].value) );

		    printf ( "Are there LINE attributes (y/n)\n" );
		    scanf  ( " %s", ans );
		    if  ( ans[0] == 'N' || ans[0] == 'n' )  {
		      types[icnt].line.size = -1.0F;
		      types[icnt].line.iwid = -1;
		    }
		    else {
		      printf ( "Enter LINE attributes (size width) %d:\n", icnt );
		      scanf  ( " %f %d",
				&(types[icnt].line.size),
				&(types[icnt].line.iwid) );
		    }

		    printf ( "Are there SYM1 attributes (y/n)\n" );
		    scanf  ( " %s", ans );
		    if  ( ans[0] == 'N' || ans[0] == 'n' )  {
		      types[icnt].symb[0].code = -1.0F;
		      types[icnt].symb[0].size = -1.0F;
		      types[icnt].symb[0].iwid = -1;
		    }
		    else {
		      printf ( "Enter SYM1 attributes (code size width) %d:\n", icnt );
		      scanf  ( " %f %f %d",
				&(types[icnt].symb[0].code),
				&(types[icnt].symb[0].size),
				&(types[icnt].symb[0].iwid) );
		    }

		    printf ( "Are there SYM2 attributes (y/n)\n" );
		    scanf  ( " %s", ans );
		    if  ( ans[0] == 'N' || ans[0] == 'n' )  {
		      types[icnt].symb[1].code = -1.0F;
		      types[icnt].symb[1].size = -1.0F;
		      types[icnt].symb[1].iwid = -1;
		    }
		    else {
		      printf ( "Enter SYM2 attributes (code size width) %d:\n", icnt );
		      scanf  ( " %f %f %d",
				&(types[icnt].symb[1].code),
				&(types[icnt].symb[1].size),
				&(types[icnt].symb[1].iwid) );
		    }

		    printf ( "Are there ARRW attributes (y/n)\n" );
		    scanf  ( " %s", ans );
		    if  ( ans[0] == 'N' || ans[0] == 'n' )  {
		      types[icnt].arrw.size = -1.0F;
		      types[icnt].arrw.hdsz = -1.0F;
		      types[icnt].arrw.iwid = -1;
		      types[icnt].arrw.ityp = -1;
		    }
		    else {
		      printf ( "Enter ARRW attributes (size hdsz width type) %d:\n", icnt );
		      scanf  ( " %f %f %d %d",
				&(types[icnt].arrw.size),
				&(types[icnt].arrw.hdsz),
				&(types[icnt].arrw.iwid),
				&(types[icnt].arrw.ityp) );
		    }
		}

		printf ( "Enter the number of flags:\n" );
		scanf  ( " %d", &nflag );
		for ( icnt = 0; icnt < nflag; icnt++ )  {
		    printf ( "Enter name %d:\n", icnt );
		    scanf  ( " %[^\n]", flags[icnt].name );
		    printf ( "Enter flag %d:\n", icnt );
		    scanf  ( " %d", &(flags[icnt].iflg) );
		}

		nms_satt ( iindex, alias, isbcat, filnam,
			   ntype, types, nflag, flags,
			   &jindex, &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "jindex = %d\n", jindex );

                if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 3 )  {
                printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );

		nms_qatt ( iindex, alias, &isbcat, filnam,
			   &ntype, types, &nflag, flags,
			   &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "alias  = %s\n", alias );
		printf ( "isbcat = %d\n", isbcat );
		printf ( "filnam = %s\n", filnam );

		for ( icnt = 0; icnt < ntype; icnt++ )  {
		    printf ( "types[%d]\n    name = %s\n",
			     icnt, types[icnt].name );
		    printf ( "    ionoff = %d\n", types[icnt].ionoff );
		    printf ( "    icolr  = %d\n", types[icnt].icolr );
		    printf ( "    value  = %.2f\n", types[icnt].value );
		    if  ( types[icnt].line.size >= 0.0F &&
			  types[icnt].line.iwid >= 0 )  {
			printf ( "    LINE = %.2f / %d\n",
				types[icnt].line.size,
				types[icnt].line.iwid );
		    }
		    if  ( types[icnt].symb[0].code >= 0.0F &&
			  types[icnt].symb[0].size >= 0.0F &&
			  types[icnt].symb[0].iwid >= 0 )  {
			printf ( "    SYM1 = %.2f / %.2f / %d\n",
				types[icnt].symb[0].code,
				types[icnt].symb[0].size,
				types[icnt].symb[0].iwid );
		    }
		    if  ( types[icnt].symb[1].code >= 0.0F &&
			  types[icnt].symb[1].size >= 0.0F &&
			  types[icnt].symb[1].iwid >= 0 )  {
			printf ( "    SYM2 = %.2f / %.2f / %d\n",
				types[icnt].symb[1].code,
				types[icnt].symb[1].size,
				types[icnt].symb[1].iwid );
		    }
		    if  ( types[icnt].arrw.size >= 0.0F &&
			  types[icnt].arrw.hdsz >= 0.0F &&
			  types[icnt].arrw.iwid >= 0 &&
		 	  types[icnt].arrw.ityp >= 0 )  {
			printf ( "    ARRW = %.2f / %.2f / %d / %d\n",
				types[icnt].arrw.size,
				types[icnt].arrw.hdsz,
				types[icnt].arrw.iwid,
				types[icnt].arrw.ityp );
		    }
		}

		for ( icnt = 0; icnt < nflag; icnt++ ) {
		    printf ( "flags[%d]\n    name = %s\n    iflg = %d\n",
			     icnt, flags[icnt].name, flags[icnt].iflg );
		}

                if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 4 )  {
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 5 )  {
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );

		for ( icnt = 0; icnt < MAXPLT; icnt++ )  {
		    ionoff[icnt] = -1;

		    iclrs[icnt] = -1;
		    iclrs2[icnt] = -1;

		    iflgs[icnt] = -1;
		}

		nms_qatt ( iindex, alias, &isbcat, filnam,
			   &ntype, types, &nflag, flags,
			   &iret );

		for ( icnt = 0; icnt < ntype; icnt++ )  {
		    if  ( types[icnt].ionoff == 1 )  {
		        ionoff[icnt]= 1;
			iclrs[icnt] = types[icnt].icolr;
			iclrs2[icnt] = types[icnt].icolr2;
		    }
		    else {
		        ionoff[icnt] = 0;
			iclrs[icnt] = 0;
			iclrs2[icnt] = 0;
		    }
		    fvalu[icnt] = types[icnt].value;

		    fline[icnt*2+0] = types[icnt].line.size;
		    fline[icnt*2+1] = (float)types[icnt].line.iwid;

		    fsym1[icnt*3+0] = types[icnt].symb[0].code;
		    fsym1[icnt*3+1] = types[icnt].symb[0].size;
		    fsym1[icnt*3+2] = (float)types[icnt].symb[0].iwid;

		    fsym2[icnt*3+0] = types[icnt].symb[1].code;
		    fsym2[icnt*3+1] = types[icnt].symb[1].size;
		    fsym2[icnt*3+2] = (float)types[icnt].symb[1].iwid;

		    farrw[icnt*4+0] = types[icnt].arrw.size;
		    farrw[icnt*4+1] = types[icnt].arrw.hdsz;
		    farrw[icnt*4+2] = (float)types[icnt].arrw.iwid;
		    farrw[icnt*4+3] = (float)types[icnt].arrw.ityp;
		}

		for ( icnt = 0; icnt < nflag; icnt++ )  {
		    iflgs[icnt] = flags[icnt].iflg;
		}

		printf ( "Enter GAREA:\n" );
		scanf  ( " %s", garea );

		printf ( "Default projection? (y/n)\n" );
		scanf  ( " %s", ans );
		if  ( ans[0] == 'N' || ans[0] == 'n' )  {
		    printf ( "Enter PROJ:\n" );
		    scanf  ( " %s", proj );
		}
		else {
		    strcpy ( proj, blank );
		}

		printf ( "Enter PANEL:\n" );
		scanf  ( " %s", panel );
		printf ( "Enter DATTIM:\n" );
		scanf  ( " %s", dattim );

		printf ( "Enter title line:\n" );
		scanf  ( " %d", &ititl );

		gg_maps ( proj, garea, blank, &id, &ier,
			  strlen ( proj ), strlen ( garea ),
			  strlen ( blank ) );

		gclear ( &ier );

		strcpy ( map, "1" );
		gg_map ( map, &ier, strlen ( map ) );

		strcpy ( ltln, "2" );
		gg_ltln ( ltln, &ier, strlen ( ltln ) );

		nms_dspl ( panel, dattim, alias, &isbcat, filnam,
			   &ntype, ionoff, iclrs, iclrs2, &nflag, iflgs,
			   fvalu, fline, fsym1, fsym2,
			   farrw, &ititl, &iret,
			   strlen ( panel ), strlen ( dattim ),
			   strlen ( alias ), strlen ( filnam ) );

		geplot ( &ier );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 6 )  {
		printf ( "Enter the data alias (e.g., WTCH, VGF):\n" );
		scanf  ( " %s", alias );

		nms_rtbl ( alias, &ntype, types, &nflag, flags, &iret );

		printf ( "For alias, %s, the attributes are:\n", alias );
		for ( ii = 0; ii < ntype; ii++ )  {
		  printf ( "%-24s %d / %d / %.2f\n",
				types[ii].name,
				types[ii].ionoff,
				types[ii].icolr,
				types[ii].value );
		  if  ( types[ii].line.size >= 0.0F &&
			types[ii].line.iwid >= 0 )  {
		      printf ( "  LINE: %.2f / %d\n",
				    types[ii].line.size,
				    types[ii].line.iwid );
		  }
		  if  ( types[ii].symb[0].code >= 0.0F &&
			types[ii].symb[0].size >= 0.0F &&
			types[ii].symb[0].iwid >= 0 )  {
		      printf ( "  SYM1: %.2f / %.2f / %d\n",
				    types[ii].symb[0].code,
				    types[ii].symb[0].size,
				    types[ii].symb[0].iwid );
		  }
		  if  ( types[ii].symb[1].code >= 0.0F &&
			types[ii].symb[1].size >= 0.0F &&
			types[ii].symb[1].iwid >= 0 )  {
		      printf ( "  SYM2: %.2f / %.2f / %d\n",
				    types[ii].symb[1].code,
				    types[ii].symb[1].size,
				    types[ii].symb[1].iwid );
		  }
		  if  ( types[ii].arrw.size >= 0.0F &&
			types[ii].arrw.hdsz >= 0.0F &&
			types[ii].arrw.iwid >= 0 &&
			types[ii].arrw.ityp >= 0 )  {
		      printf ( "  ARRW: %.2f / %.2f / %d / %d\n",
				    types[ii].arrw.size,
				    types[ii].arrw.hdsz,
				    types[ii].arrw.iwid,
				    types[ii].arrw.ityp );
		  }
		}
		printf ( "For alias, %s, the flags are:\n", alias );
		for ( ii = 0; ii < nflag; ii++ )  {
		    printf ( "%-24s %4d\n", flags[ii].name,
					    flags[ii].iflg );
		}

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
           if  ( numsub == 7 )  {

                nms_save ( &iret );

                printf ( "iret   = %d\n", iret );

		if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }

            }

/*---------------------------------------------------------------------*/
           if  ( numsub == 8 )  {

		nms_rest ( &iret );

                printf ( "iret   = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }

            }

/*---------------------------------------------------------------------*/

	    if  ( numsub == 10 )  {
		dumpcmn ( &iret );
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 20 )  {
		printf ( "Enter full DEVICE string:\n" );
		scanf ( " %s", device );

		gg_sdev ( device, &ier, strlen ( device ) );
	    }

	}
	return(0);
}

/*=====================================================================*/

void dumpcmn ( int *iret )
/************************************************************************
 * dumpcmn								*
 *									*
 * This routine dumps the contents of the NMS common variables to the	*
 * screen.								*
 *									*
 * dumpcmn ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Jacobs/NCEP	12/99	Updated data structures			*
 * T. Piper/SAIC        06/03   Replaced nmsc with MAXTMPLT             *
 ***********************************************************************/
{

	int		i, j;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for ( i = 0; i < MAXTMPLT; i++ )  {

	    printf ( "\nMisc Data Attributes: index = %d\n",
			indmsc[i] );
	    printf ( "    alias  = %s\n", mscdt[indmsc[i]].alias );
	    printf ( "    isbcat = %d\n", mscdt[indmsc[i]].isbcat );
	    printf ( "    filnam = %s\n", mscdt[indmsc[i]].filnam );

	    printf ( "  Types:\n" );
	    for ( j = 0; j < mscdt[indmsc[i]].numtyp; j++ )  {
		printf ( "    [%02d] %-24s %d / %d / %.2f\n",
			 j, mscdt[indmsc[i]].msctyp[j].name,
			    mscdt[indmsc[i]].msctyp[j].ionoff,
			    mscdt[indmsc[i]].msctyp[j].icolr,
			    mscdt[indmsc[i]].msctyp[j].value );
		if  ( mscdt[indmsc[i]].msctyp[j].line.size >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].line.iwid >= 0 )  {
		    printf ( "  LINE: %.2f / %d\n",
			    mscdt[indmsc[i]].msctyp[j].line.size,
			    mscdt[indmsc[i]].msctyp[j].line.iwid );
		}
		if  ( mscdt[indmsc[i]].msctyp[j].symb[0].code >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].symb[0].size >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].symb[0].iwid >= 0 )  {
		    printf ( "  SYM1: %.2f / %.2f / %d\n",
			    mscdt[indmsc[i]].msctyp[j].symb[0].code,
			    mscdt[indmsc[i]].msctyp[j].symb[0].size,
			    mscdt[indmsc[i]].msctyp[j].symb[0].iwid );
		}
		if  ( mscdt[indmsc[i]].msctyp[j].symb[1].code >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].symb[1].size >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].symb[1].iwid >= 0 )  {
		    printf ( "  SYM2: %.2f / %.2f / %d\n",
			    mscdt[indmsc[i]].msctyp[j].symb[1].code,
			    mscdt[indmsc[i]].msctyp[j].symb[1].size,
			    mscdt[indmsc[i]].msctyp[j].symb[1].iwid );
		}
		if  ( mscdt[indmsc[i]].msctyp[j].arrw.size >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].arrw.hdsz >= 0.0F &&
		      mscdt[indmsc[i]].msctyp[j].arrw.iwid >= 0 &&
		      mscdt[indmsc[i]].msctyp[j].arrw.ityp >= 0 )  {
		    printf ( "  ARRW: %.2f / %.2f / %d / %d\n",
			    mscdt[indmsc[i]].msctyp[j].arrw.size,
			    mscdt[indmsc[i]].msctyp[j].arrw.hdsz,
			    mscdt[indmsc[i]].msctyp[j].arrw.iwid,
			    mscdt[indmsc[i]].msctyp[j].arrw.ityp );
		}
	    }

	    printf ( "  Flags:\n" );
	    for ( j = 0; j < mscdt[indmsc[i]].numflg; j++ )  {
		printf ( "    [%02d] %-24s %4d\n",
			 j, mscdt[indmsc[i]].mscflg[j].name,
			    mscdt[indmsc[i]].mscflg[j].iflg );
	    }

	}

}
