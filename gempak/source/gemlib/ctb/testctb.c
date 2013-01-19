#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"


int main ( void )
/************************************************************************
 * TESTCTB								*
 *									*
 * This program tests the GEMLIB "CTB" functions.			*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 1/97						*
 * G. Krueger/EAI	 6/97	Added CTB_PRMT				*
 * S. Jacobs/NCEP	 7/97	Added CTB_PROD				*
 * S. Jacobs/NCEP	 7/97	Added indent value to CTB_PROD		*
 * S. Jacobs/NCEP	 8/97	Added reserved value to CTB_PROD	*
 * S. Jacobs/NCEP	 8/97	Added CTB_AFOS				*
 * G. Krueger/EAI	 9/97	Added CTB_RBUL, RDTYP, RMTYP, and RSTN	*
 * D.W.Plummer/NCEP	 3/98	Added CTB_PLRD				*
 * D.W.Plummer/NCEP	 3/98	Added CTB_DT* suite of functions	*
 * D.W.Plummer/NCEP	 3/98	Mod for CTB_PLSDEF			*
 * S. Jacobs/NCEP	 5/98	Modified call to CTB_PROD		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * C. Lin/EAI	 	 8/98	Added CTB_FSZRD, CTB_FSZNAM, CTB_FSZVAL,*
 *				 CTB_FSZXVL, CTB_FSZFND, CTB_FSZQN	*
 * S. Jacobs/NCEP	 9/98	Changed length of the path string	*
 * A. Hardy/GSC          9/98   Added CTB_AWDEF and CTB_AWMAP           *
 * S. Jacobs/NCEP	 2/99	Added CTB_TIFF				*
 * F. J. Yen/NCEP	 5/99	Fixed sgi86 warning messages for printf	*
 * S. Jacobs/NCEP	 8/99	Changed format of data.tbl (CTB_DT*)	*
 * A. Hardy/GSC		 4/00	Added font size multiplier to CTB_FSZXVL*
 * M. Li/GSC		10/00	Added CTB_TRKQN, CTB_TRKITV, CTB_TRKFND	*
 * S. Jacobs/NCEP	 1/01	Changed call to ctb_pllist		*
 * D.W.Plummer/NCEP	 3/01	Added ctb_ccrd and ctb_ccfind		*
 * J. Wu/GSC		06/01	add ctb_dcatstoi, ctb_dcatitos		*
 *				    ctb_dscatstoi, ctb_dscatitos 	*
 * S. Jacobs/NCEP	 8/01	Changed defdir from NULL to null char	*
 * S. Jacobs/NCEP	 9/01	Fixed format for ctb_astn output	*
 * J. Wu/SAIC		02/02	modify ctb_trkitv()			*
 * T. Lee/SAIC		 2/02	Added ctb_lyrd, ctblygetname,		*
 *				ctblygetfile, ctblygetcmode,		*
 *				ctblygetcolor, ctblygetfmode		*
 *				ctblygetgrptyp				*
 * S. Jacobs/NCEP	 4/02	Fixed variables in call to ctb_awdef	*
 * T. Lee/SAIC		 5/02	Added ctb_lygetdsply			*
 * E. Safford/SAIC	06/02	param change to ctb_lygetname		*
 * H. Zeng/EAI          08/02   added preference table related func.    *
 * R. Tian/SAIC		02/03	added ctb_rdcpf, ctb_wrcpf		*
 * D.W.Plummer/NCEP	 3/03	modify ctb_rdcpf,ctb_wrcpf calling seq	*
 * m.gamazaychikov/SAIC  5/03	added ctb_g2read, ctb_g2gnam, ctb_g2gnum*
 * m.gamazaychikov/SAIC  5/03	added parm to ctb_g2gnam and ctb_g2gnum	*
 * A. Hardy/NCEP	 7/03	added ctb_rdwou				*
 * T. Lee/SAIC		 2/04	Changed AVN to GFS			*
 * A. Hardy/NCEP	 3/04	added ctb_mzrd and ctb_mzgnm		*
 * M. Li/SAIC		 4/04	added ihzrmp, idrct to ctb_g2gnam	*
 * T. Lee/SAIC		 9/04	added ctb_dhrsitos, ctb_dhrsstoi;	*
 *				    added bin hours to ctb_dtget	*
 * B. Yin/SAIC		10/04	added ctb_gfa*** functions		*
 * A. Hardy/NCEP	10/04	added ctb_permccrd & ctb_permccfind	*
 * M. Li/SAIC           10/04   Replaced ctb_rdwou with ctb_rdprf	*
 * B. Yin/SAIC		11/04	added ctb_gfagcat function		*
 * m.gamazaychikov/SAIC 12/04	added ionoff for bin hours to functions *
 * B. Yin/SAIC		12/04	added ctb_gfagtemp function		*
 * A. Hardy/NCEP	02/05   changed example for option '63'		*
 * S. Jacobs/NCEP	03/05	Fixed typo for printing data category	*
 * E. Safford/SAIC	07/05	add ctb_getAirmetIssueTm,		* 
 *				  ctb_getAirmetCycleTms			*
 * J. Wu/SAIC		10/05	added ctb_gfaCmpSeverity		*
 * E. Safford/SAIC	11/05	added ctb_gfaiss (for AWC)		*
 * m.gamazaychikov/SAIC 01/06	Changed template string length to MXTMPL*
 * E. Safford/SAIC	04/06	added ctb_gfaGetWorstIssTyp		*
 * E. Safford/SAIC	04/06	added ctb_gfaCombineIFRTypes		*
 * m.gamazaychikov/SAIC 05/06	added dtmmtch to CS for ctb_dtget	*
 * B. Yin/SAIC		06/06	added ctb_gfagid			*
 * B. Yin/SAIC		06/06	added ctb_gfagdesk			*
 * B. Yin/SAIC		07/06	removed ctb_gfaGetWorstIssTyp		*
 * J. Wu/SAIC		09/06	added ctb_gfaWorstFL & ctb_gfaWorstIssue*
 * T. Piper/SAIC	09/07	Modified for ctb_rstn CSC		*
 * T. Piper/SAIC	09/07	declared LLSTFL variables as static	*
 * S. Jacobs/NCEP	10/07	Added Hershey Font functions		*
 * F. J. Yen/NCEP	 4/08	CSC to ctb_dtget, _dhrsitos, & _dhrsstoi*
 ***********************************************************************/
{
	int		i, ii, jj, cont, color, ionoff;
	int		ier, iret, numsub, lststn;
	int		num, lstals, ialias, ntoply, ly;
	int		maxstn,  istn,  nstn,  maxbul,  ibul,  nbul;
	int		maxdtyp, idtyp, ndtyp, maxmtyp, imtyp, nmtyp;

	char		select[LLSCRN], filnam[LLSCRN], ddname[LLSCRN],
			ans[LLSCRN], prmtyp[LLSCRN];
	char		*defdir;
	char		alias[1000][73], cly[10], vcord[1000][5];

	int		level[1000];

	static int	istnm[LLSTFL], ispri[LLSTFL];
	static float	slat[LLSTFL], slon[LLSTFL], selv[LLSTFL];
	static char	stid[LLSTFL][9], stnnam[LLSTFL][33],
			stat[LLSTFL][3], coun[LLSTFL][3],
			tbchrs[LLSTFL][21];
			
	static struct bulletin_list	bularr[LLSTFL];
	static struct datatype_list	dtyparr[LLSTFL];
	static struct maptype_list	mtyparr[LLSTFL];

	char		wheel[5], subset[5], subout[5][5], descr[5][41];
	int		ibit[5], ilin[5], irot[5], iind[5], irsv[5];
	int		nsub;

	char		pil[4];
	int		ix, iy, imap, itime, igscl;

	int		nele;

	char		plalias[16], pldtype[16], plparm[16], plstr[80];
	char		list[50][16], colcod;
	char		dtalias[16], path[26], template[MXTMPL], layout[80];
	int		catgry, subcat, nframe, range, intrvl;
	char		tcat[][9] = { "SCAT_NIL", "SCAT_SFC",
				      "SCAT_SHP", "SCAT_SFF",
				      "SCAT_FFG", "SCAT_SND",
				      "SCAT_SNF", "SCAT_FCT",
				      "SCAT_ANL" };
	char		ccat[][9] = { "CAT_NIL", "CAT_IMG",
				      "CAT_SFC", "CAT_SFF",
				      "CAT_SND", "CAT_SNF",
				      "CAT_GRD", "CAT_VGF",
				      "CAT_MSC" };

	char            awpid[11], kloc[5], flcls[2], prodid[11]; 
	char 	        extnd[7], wmo[7], bckgnd[7];
	int             irettim, ikmap, icrdflg, isclint, isclfac;
        int             iareacd, ilabel, icordfg, inrefpt, iktime;
        int             iimap;

        int             pick, *ikxsize, *ikysize; 
        int             ilat[4], ilon[4], iangles[3]; 

	int		index, nfsz, ifsz, imult;
	char		fsznam[20], **interval;
	float 		fszval;

	char		wmoid[7], tdesc[41];
	int		jbit, jlin, jrot;

	char		ccwfo[4], ccname[32], cctbl[128], info[80];
	char		pcwfo[4], pcname[32];
	int		fips, ncfips, cfips[50], nret;

	char		catgrynam[8], scatgrynam[9];
	int		catnum, scatnum;

        char            tag[MAX_PREF_STR], cval[MAX_PREF_STR], filename[25];
        Boolean         bval;
        int             rdpftbl = 0;
        int             rdhftbl = 0;

	int		np;
	float		cplat[100], cplon[100];

        int             discipline, category, idnumber, pdnumber;
        char            gemname[13];
	int		ihzrmp, idrct;

	StnLst			*stnarr;
	Clustcnty_t		cc;
	Permclust_t		pc;
        Marzon_t	        mznms;

	char		cpfname[128];

	char            value[120], stidmz[7], fulnam[257];

	int		hrsbfr, hraftr, mnsbfr, mnaftr, dtmmtch, mstrct;
	char		binhr[21];
	char		tblnam[72], dirsym[160];

	int 		nfhr, nhaz, ntags, ndesc, which, choice, nchoices;
	char		fhrlist [ 1024 ], sep [ 2 ] = ";", type [ 64 ];
	char		hazlist [ 2048 ], hazard [64 ], desc[ 64 ]; 
	char		taglist [ 1024 ], choiceStr[ 128 ], cat[ 32 ];
	char		fileName[ 128 ];

	char		issueTm[ 128 ], cycleTm[ 128 ], **times = NULL, dst[32];
	Boolean		isDST;
	int		ntimes, ndesks;
        
	char		value1[32], value2[32], worstCase[32];
	int		nopt;
	char		issOptList[128];
	char		type0[ 128 ], type1[ 128 ], IFRType[ 256 ];
	char		gfaid[ 8 ], desklist[ 1024 ];

        int        	topBot, nissue;
        char        	**issues, worstIssue[ 32 ];
	char		flightLevel0[32], flightLevel1[32];
	char		worstTop[32], worstBottom[32];
   	char		fzlTop0[32], fzlBottom0[32], fzlTop1[32], fzlBottom1[32];
   	char		worstBottomFZL[32], worstTopFZL[32];

#define MXCHPT 150
#define MXCHAR 200
	int		fontnum, maxpt, numch, ascval[MXCHPT],
			ixmin[MXCHPT], ixmax[MXCHPT], npnts[MXCHPT],
			ixc[MXCHPT*MXCHAR], iyc[MXCHPT*MXCHAR];
	int		ichr, ipnt, idx;
 /*---------------------------------------------------------------------*/
	cont = G_FALSE;

	while ( cont == G_FALSE ) {
	    printf ( "\n\n" );
	    printf ( "   1 = CTB_ASTN         2 = CTB_PRMT         3 = CTB_PROD\n" );
	    printf ( "   4 = CTB_AFOS         5 = CTB_RDTYP        6 = CTB_RMTYP\n" );
	    printf ( "   7 = CTB_RSTN         8 = CTB_RBUL         9 = CTB_PLRD\n" );
	    printf ( "  10 = CTB_PLDUMP      11 = CTB_PLGET       12 = CTB_PLSET\n" );
	    printf ( "  13 = CTB_PLGCC       14 = CTB_PLSDEF      15 = CTB_PLLIST\n" );
	    printf ( "  16 = CTB_DTRD        17 = CTB_DTDUMP      18 = CTB_DTGET\n" );
	    printf ( "  19 = CTB_DTPATH      20 = CTB_DTTMPL      21 = CTB_DTTIME\n" );
	    printf ( "  22 = CTB_DTCAT       23 =                 24 = CTB_TIFF\n" );
	    printf ( "  25 = CTB_FSZRD       26 = CTB_FSZQN       27 = CTB_FSZNAM\n" );
	    printf ( "  28 = CTB_FSZVAL      29 = CTB_FSZXVL      30 = CTB_FSZFND\n" );
	    printf ( "  31 = CTB_AWDEF       32 = CTB_AWMAP       33 = CTB_TRKQN\n" );
	    printf ( "  34 = CTB_TRKITV      35 = CTB_TRKFND      36 = CTB_CCRD\n" );
	    printf ( "  37 = CTB_CCFIND      38 = CTB_DCATSTOI    39 = CTB_DCATITOS\n" );
	    printf ( "  40 = CTB_DSCATSTOI   41 = CTB_DSCATITOS   42 = CTB_LYRD\n" );
	    printf ( "  43 = CTB_LYGETNAME   44 = CTB_LYGETFILE   45 = CTB_LYGETCMODE\n" );
	    printf ( "  46 = CTB_LYGETCOLOR  47 = CTB_LYGETFMODE  48 = CTB_LYGETGRPTYP\n" );
	    printf ( "  49 = CTB_LYGETDSPLY  50 = CTB_LYGETOUTFILE\n");
	    printf ( "  51 = CTB_PFREAD      52 = CTB_PFSTR \n" );
	    printf ( "  53 = CTB_PFBOOL      54 = CTB_RDCPF       55 = CTB_WRCPF\n" );
	    printf ( "  56 = CTB_G2READ      57 = CTB_G2GNAM      58 = CTB_G2GNUM\n" );
	    printf ( "  59 = CTB_RDPRF       60 = CTB_MZRD        61 = CTB_MZGNM\n" );
	    printf ( "  62 = CTB_DHRSITOS    63 = CTB_DHRSSTOI    64 = CTB_GFARD\n" );
            printf ( "  65 = CTB_GFAGFHR     66 = CTB_GFAGHAZ     67 = CTB_GFAGNDESC\n" );
            printf ( "  68 = CTB_GFAGDESC    69 = CTB_GFAGDC      70 = CTB_GFAGTAG\n" ); 
	    printf ( "  71 = CTB_GFAGCAT     72 = CTB_PERMCCRD    73 = CTB_PERMCCFIND\n" );
	    printf ( "  74 = CTB_GFAGTMP     75 = CTB_GFACMPSEVERITY\n");
	    printf ( "  76 = CTB_airmetGetIssueTm\n" );
	    printf ( "  77 = CTB_airmetGetCycleTms\n" );
	    printf ( "  78 = CTB_GFAISS      79 = \n" );
	    printf ( "  80 = CTB_gfaCombineIFRTypes               81 = CTB_GFAGID\n" );
	    printf ( "  82 = CTB_GFAGDESK\n" );
	    printf ( "  83 = CTB_GFAWORSTFL  84 = CTB_GFAWORSTISSUE\n" );
	    printf ( "  85 = CTB_HFREAD      86 = CTB_HFDUMP      87 = CTB_HFGETFONT\n\n" );
	    printf ( "\n" );
	    printf ( "Select a subroutine number or type EXIT: " );
	    scanf ( " %s", select );
	    switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = G_TRUE;
		default:
			numsub = atoi ( select );
			break;
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 1 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = ddname;
		    ddname[0] = CHNULL;
		}

		printf ( "Enter MAXSTN, the number of stns to read\n" );
		scanf ( " %d", &maxstn );

		ctb_astn ( filnam, defdir, &maxstn, &nstn, stid, stnnam,
			   istnm, stat, coun, slat, slon, selv,
			   ispri, tbchrs, &iret );

		if ( nstn > 0 && iret >= 0 ) {
		    printf ( "To list all of the stations read, enter 1:\n" );
		    lststn = 0;
		    scanf ( " %d", &lststn );
		    if ( lststn > 0 ) {
			for ( istn = 0; istn < nstn; istn++ )
			{
			    printf ( "%-8s %6d %-32s %-2s %-2s %6.2f %7.2f %8.2f %2d %s\n",
				     &stid[istn][0], istnm[istn],
				     &stnnam[istn][0], &stat[istn][0],
				     &coun[istn][0], slat[istn],
				     slon[istn], selv[istn],
				     ispri[istn], tbchrs[istn] );
			}
		    }
		}

		printf ( "\nCTB_ASTN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		printf ( "Enter the parameter type:\n" );
		scanf ( " %s", prmtyp );

		ctb_prmt ( prmtyp,  &num, alias, vcord, level, &iret );

		if ( num > 0 ) {
		    printf ( "To list all of the aliases read, enter 1:\n" );
		    lstals = 0;
		    scanf ( " %d", &lstals );
		    if ( lstals > 0 ) {
			for ( ialias = 0; ialias < num; ialias++ )
			{
			    printf ( "\n%-20.20s %s %d\n",
				     &alias[ialias][0],
				     &vcord[ialias][0],
				     level[ialias] );
			}
		    }
		}

		printf ( "\nCTB_PRMT: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		printf ( "Enter the wheel ID:\n" );
		scanf ( " %4s", wheel );
		printf ( "Enter the subset ID:\n" );
		scanf ( " %4s", subset );

		ctb_prod ( wheel, subset, 5, &nsub, subout, descr,
			   ibit, ilin, irot, iind, irsv, &iret );

		printf ( "\n" );
		for ( i = 0; i < nsub; i++ )  {
		    printf ( "%s %s %s %d %d %d %d %d\n",
			     wheel, subout[i], descr[i], ibit[i],
			     ilin[i], irot[i], iind[i], irsv[i] );
		}
		printf ( "\nCTB_PROD: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		printf ( "Enter the PIL ID:\n" );
		scanf ( " %3s", pil );

		ctb_afos ( pil, (char *)descr, &ix, &iy, &imap, &itime, &igscl,
			   &iret );

		printf ( "\n%s %s %d %d %d %d %d\n", wheel,
						     (char *)descr, ix, iy,
						     imap, itime,
						     igscl );
		printf ( "\nCTB_AFOS: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = ddname;
		    ddname[0] = CHNULL;
		}

		printf ( "Enter MAXDTYP, the number of data types to read\n" );
		scanf ( " %d", &maxdtyp );

		ctb_rdtyp ( filnam, defdir, &maxdtyp, &ndtyp, dtyparr, &iret );

		if ( ndtyp > 0 ) {
		    printf ( "To list all of the data types read, enter 1:\n" );
		    lststn = 0;
		    scanf ( " %d", &lststn );
		    if ( lststn > 0 ) {
			for ( idtyp = 0; idtyp < ndtyp; idtyp++ )
			{
			    printf ( "\n%-12s %-12s %1s %-40s %s\n",
				     dtyparr[idtyp].datatyp,
				     dtyparr[idtyp].loctbl,
				     dtyparr[idtyp].bsflag,
				     dtyparr[idtyp].datadir,
				     dtyparr[idtyp].filext );
			}
		    }
		}

		printf ( "\nCTB_RDTYP: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = ddname;
		    ddname[0] = CHNULL;
		}

		printf ( "Enter MAXMTYP, "
			 "the number of map projections to read\n" );
		scanf ( " %d", &maxmtyp );

		ctb_rmtyp ( filnam, defdir, &maxmtyp, &nmtyp, mtyparr, &iret );

		if ( nmtyp > 0 ) {
		    printf ( "To list all of the map projections read, "
			     "enter 1:\n" );
		    lststn = 0;
		    scanf ( " %d", &lststn );
		    if ( lststn > 0 ) {
			for ( imtyp = 0; imtyp < nmtyp; imtyp++ )
			{
			    printf ( "\n%-40s %s\n",
				     mtyparr[imtyp].name,
				     mtyparr[imtyp].garea );
			}
		    }
		}

		printf ( "\nCTB_RMTYP: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = ddname;
		    ddname[0] = CHNULL;
		}

		ctb_rstn ( filnam, defdir, &nbul, &stnarr, &iret );

		if ( nbul > 0 && iret >= 0 ) {
		    printf ( "To list all of the stations read, enter 1:\n" );
		    lststn = 0;
		    scanf ( " %d", &lststn );
		    if ( lststn > 0 ) {
			for ( ibul = 0; ibul < nbul; ibul++ ) {
			    printf ( "\n%s %d %-32s %s %s %6.2f %7.2f %4.0f "
				     "%d\n",
				     stnarr[ibul].stid,
				     stnarr[ibul].stnm,
				     stnarr[ibul].name,
				     stnarr[ibul].state,
				     stnarr[ibul].coun,
				     stnarr[ibul].rlat,
				     stnarr[ibul].rlon,
				     stnarr[ibul].elev,
				     stnarr[ibul].prior );
			}
		    }
		    G_FREE(stnarr, StnLst);
		}
		printf ( "\nCTB_RSTN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = ddname;
		    ddname[0] = CHNULL;
		}

		printf ( "Enter MAXBUL, the number of bulletins to read\n" );
		scanf ( " %d", &maxbul );

		ctb_rbul ( filnam, defdir, &maxbul, &nbul, bularr, &iret );

		if ( nbul > 0 && iret >= 0 ) {
		    printf ( "To list all of the bulletins read, enter 1:\n" );
		    lststn = 0;
		    scanf ( " %d", &lststn );
		    if ( lststn > 0 ) {
			for ( ibul = 0; ibul < nbul; ibul++ )
			{
			    printf ( "\n%-6s %-8s %-32s %s %s %6.2f %7.2f "
				     "%4.0f\n",
				     bularr[ibul].bullid,
				     bularr[ibul].stid,
				     bularr[ibul].name,
				     bularr[ibul].state,
				     bularr[ibul].coun,
				     bularr[ibul].rlat,
				     bularr[ibul].rlon,
				     bularr[ibul].elev );
			}
		    }
		}

		printf ( "\nCTB_RBUL: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {

		ctb_plrd ( &iret );

		printf ( "\nCTB_PLRD: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 10 ) {

		ctb_pldump( &iret );

		printf ( "\nCTB_PLDUMP: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 11 ) {

		printf ( "Enter a prmlst data type (eg., metar):\n" );
		scanf ( " %s", pldtype );
		printf ( "Enter a prmlst alias (eg., summer):\n" );
		scanf ( " %s", plalias );
		printf ( "Enter a parameter (eg., colors):\n" );
		scanf ( " %s", plparm );

		ctb_plget( pldtype, plalias, plparm, plstr, &iret );

		printf ( "\nCTB_PLGET: iret = %d\n", iret );
		printf ( "\tstring = %s\n\n", plstr );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 12 ) {

		printf ( "Enter a prmlst data type (eg., metar):\n" );
		scanf ( " %s", pldtype );
		printf ( "Enter a prmlst alias (eg., summer):\n" );
		scanf ( " %s", plalias );
		printf ( "Enter a parameter (eg., colors):\n" );
		scanf ( " %s", plparm );
		printf ( "Enter the parameter value (eg., 2;3;4;5):\n" );
		scanf ( " %s", plstr );

		ctb_plset( pldtype, plalias, plparm, plstr, &iret );

		printf ( "\nCTB_PLSET: iret = %d\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 13 ) {

		printf ( "Enter a prmlst data type (eg., metar):\n" );
		scanf ( " %s", pldtype );
		printf ( "Enter a prmlst alias (eg., summer):\n" );
		scanf ( " %s", plalias );

		ctb_plgcc( pldtype, plalias, &colcod, &iret );

		printf ( "\nCTB_PLGCC: iret = %d\n", iret );
		printf ( "color code = %d\n", colcod );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 14 ) {

                printf ( "Enter a prmlst data type (eg., metar):\n" );
                scanf ( " %s", pldtype );
                printf ( "Enter a prmlst alias (eg., summer):\n" );
                scanf ( " %s", plalias );
                printf ( "Enter a parameter (eg., colors or all):\n" );
                scanf ( " %s", plparm );

                ctb_plsdef( pldtype, plalias, plparm, &iret );

                printf ( "\nCTB_PLSDEF: iret = %d\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 15 ) {

		printf ( "Enter a prmlst data type (eg., metar):\n" );
		scanf ( " %s", pldtype );

		ctb_pllist( pldtype, 50, list, &nele, &iret );

		printf ( "\nCTB_PLLIST: iret = %d\n", iret );

		printf ( "Alias list for data type %s\n", pldtype );
		for ( i = 0; i < nele; i++ )  printf("%d  %s\n", i, list[i] );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 16 ) {

		ctb_dtrd ( &iret );

		printf ( "\nCTB_DTRD: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 17 ) {

		ctb_dtdump( &iret );

		printf ( "\nCTB_DTDUMP: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 18 ) {

		printf ( "Enter a data alias (eg., GFS):\n" );
		scanf ( " %s", dtalias );

		ctb_dtget( dtalias, path, template, 
			   &catgry, &subcat, &nframe,
			   &range, &intrvl, &ionoff, &hrsbfr, &mnsbfr,
			   &hraftr, &mnaftr, &mstrct, &dtmmtch, &iret );

		printf ( "\nCTB_DTGET: iret = %d\n", iret );
		printf ( "\tpath     = %s\n", path );
		printf ( "\ttemplate = %s\n", template );
		printf ( "\tcatgry   = %s (%d)\n", ccat[catgry], catgry );
		printf ( "\tsubcat   = %s (%d)\n", tcat[subcat], subcat );
		printf ( "\tnframe   = %d\n", nframe );
		printf ( "\trange    = %d\n", range );
		printf ( "\tintrvl   = %d\n", intrvl );
		printf ( "\tionoff   = %d\n", ionoff );
		printf ( "\thrsbfr   = %d\n", hrsbfr );
		printf ( "\tmnsbfr   = %d\n", mnsbfr );
		printf ( "\thraftr   = %d\n", hraftr );
		printf ( "\tmnaftr   = %d\n", mnaftr );
		printf ( "\tmstrct   = %d\n", mstrct );
	        printf ( "\tdtmmtch  = %d\n", dtmmtch );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 19 ) {

		printf ( "Enter a data alias (eg., GFS):\n" );
		scanf ( " %s", dtalias );

		ctb_dtpath( dtalias, path, &iret );

		printf ( "\nCTB_DTPATH: iret = %d\n", iret );
		printf ( "\tpath     = %s\n", path );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 20 ) {

		printf ( "Enter a data alias (eg., GFS):\n" );
		scanf ( " %s", dtalias );

		ctb_dttmpl( dtalias, template, &iret );

		printf ( "\nCTB_DTTMPL: iret = %d\n", iret );
		printf ( "\ttemplate = %s\n", template );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 21 ) {

		printf ( "Enter a data alias (eg., GFS):\n" );
		scanf ( " %s", dtalias );

		ctb_dttime( dtalias, &nframe, &range, &intrvl, &iret );

		printf ( "\nCTB_DTTIME: iret = %d\n", iret );
		printf ( "\tnframe   = %d\n", nframe );
		printf ( "\trange    = %d\n", range );
		printf ( "\tintrvl   = %d\n", intrvl );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 22 ) {

		printf ( "Enter a data alias (eg., GFS):\n" );
		scanf ( " %s", dtalias );

		ctb_dtcat( dtalias, &catgry, &subcat, &iret );

		printf ( "\nCTB_DTCAT: iret = %d\n", iret );
		printf ( "\tcatgry = %s (%d)\n", ccat[catgry], catgry );
		printf ( "\tsubcat = %s (%d)\n", tcat[subcat], subcat );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 24 ) {

		printf ( "Enter the WMO ID:\n" );
		scanf ( " %6s", wmoid );

		ctb_tiff ( wmoid, tdesc, &jbit, &jlin, &jrot, &iret );

		printf ( "\n" );
		printf ( "%s %s %d %d %d\n",
			  wmoid, tdesc, jbit, jlin, jrot );
		printf ( "\nCTB_TIFF: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 25 ) {

		ctb_fszrd( &iret );

		printf ( "\nCTB_FSZRD: iret = %d\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 26 ) {

		ctb_fszqn( &nfsz, &iret );

		printf ( "\nCTB_FSZQN: iret = %d\n", iret );
		printf ( "\tnfsz    = %d\n", nfsz );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 27 ) {

		printf ( "Enter a font size index:\n" );
		scanf ( " %d", &index );

		ctb_fsznam( index, fsznam, &iret );

		printf ( "\nCTB_FSZNAM: iret = %d\n", iret );
		if ( iret == 0 )
		    printf ( "\tfszname    = %s\n", fsznam );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 28 ) {

		printf ( "Enter a font size index:\n" );
		scanf ( " %d", &index );

		ctb_fszval( index, &fszval, &iret );

		printf ( "\nCTB_FSZVAL: iret = %d\n", iret );
		if ( iret == 0 )
		    printf ( "\tfszval    = %.3f\n", fszval );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 29 ) {

		printf ( "Enter a font size index:\n" );
		scanf ( " %d", &index );

		ctb_fszxvl( index, &ifsz, &imult, &iret );

		printf ( "\nCTB_FSZXVL: iret = %d\n", iret );
		if ( iret == 0 )
		    printf ( "\tifsz    = %d \timult   = %d\n",ifsz,imult );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 30 ) {

		printf ( "Enter a font size:\n" );
		scanf ( " %f", &fszval );

		ctb_fszfnd( fszval, &index, &iret );

		printf ( "\nCTB_FSZFND: iret = %d\n", iret );
		printf ( "\tfszinx    = %d\n", index );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 31 ) {

		printf ( "Enter the PIL ID:\n" );
		scanf ( " %3s", pil );

		ctb_awdef ( pil, awpid, kloc, flcls, prodid, extnd, wmo, 
		            bckgnd, &irettim, &ikmap, &icrdflg, &isclint, 
			    &isclfac, &iareacd, &ilabel, &icordfg, &inrefpt, 
                            &iktime, &iimap, &iret );     

                if ( iret == 0 )
                printf ( "\n%s %s %s %s %s %s %s %s %d %d %d %d %d %d %d %d %d %d %d\n",
                            pil, awpid, kloc, flcls, prodid, extnd,
                            wmo, bckgnd, irettim, ikmap, icrdflg, isclint, 
                            isclfac, iareacd, ilabel, icordfg, inrefpt, 
                            iktime, iimap);

		printf ( "\nCTB_AWDEF: iret = %d\n", iret );
                if ( iret == -1 )
		    printf ( "Table cannot be opened.  iret=%d\n\n",
			    iret );
                if ( iret == -2 )
		    printf ( "PIL entry not in table.  iret=%d\n\n",
			    iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 32 ) {

		printf ( "Enter the map setting number ( 1 or 2 ) :\n" );
		scanf ( "%d", &pick);

		ctb_awmap ( pick, ikxsize, ikysize, ilat, ilon, 
		            iangles, &iret ); 

                if ( iret == 0 )
                printf( "\n%d %d %d %d %d %d %d %d %d %d %d %d %d %d\n", 
                            pick, *ikxsize, *ikysize, 
                            ilat[0], ilon[0], ilat[1], ilon[1], 
			    ilat[2], ilon[2], ilat[3], ilon[3], 
			    iangles[0], iangles[1], iangles[2] );

		printf ( "\nCTB_AWMAP: iret = %d\n", iret );
                if ( iret == -1 )
		    printf ( "Table cannot be opened.  iret = %d\n\n",
			   iret );
                if ( iret == -2 )
		printf ( "Invalid choice. \n\n" );
	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 33 ) {

                ctb_trkqn(&num, &iret);

                printf ( "\nCTB_TRKQN: iret = %d\n", iret );
		printf ( "Total number of intervals : %d\n", num);


                if ( iret == -1 )
                    printf ( "Table cannot be opened.  iret = %d\n\n",
                           iret );
            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 34 ) {

		ctb_trkqn ( &num, &iret );

		interval = (char **) malloc( (size_t)num * sizeof( char *) ) ;
		for ( ii = 0; ii < num; ii++ ) {
		    *(interval+ii) = (char *) malloc( 6 * sizeof( char ) );      
		}

		ctb_trkitv( num, interval, &iret);

		printf ( "\nCTB_TRKITV: iret = %d\n\n", iret );

		if ( num > 0 && iret >= 0 ) {

 		    printf("Total number of %d intervals found:\n", num );
		    for ( ii = 0; ii < num; ii++) {
			printf("%s\n", interval[ii] );
		    }
		}
		else {
		    if ( iret == -1 ) {
			printf ( "Table cannot be opened. \n\n" );
		    }
		    else {
			printf ("No valid track intervals found.\n\n");
		    }
		}

		for ( ii = 0; ii < num; ii++ ) {
		    free( *(interval+ii) );
		}

		free( interval );
		}
/*---------------------------------------------------------------------*/
            if ( numsub == 35 ) {

                printf (" Enter the time interval (15, 20, 30 ...): \n");
                scanf ("%d", &pick);

                ctb_trkfnd(&pick, &index, &iret);

		printf ( "\nCTB_TRKFND: iret = %d\n", iret );
		printf ( "Index = %d\n", index );

                if ( iret == -1 )
                    printf ( "Table cannot be opened.  iret = %d\n\n",
                           iret );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 36 ) {

                printf (" Enter the county cluster file name: \n");
                scanf ("%s", cctbl );

                ctb_ccrd( cctbl, "stns", &cc, &iret );

		printf ( "\nCTB_CCRD: iret = %d\n", iret );

		for ( ii = 0; ii < cc.nclust; ii++ )  {
		    printf("Cluster %3d - %s - %s\n\t", 
			ii, cc.clust[ii].ccwfo, cc.clust[ii].ccname );
		    for ( jj = 0; jj < cc.clust[ii].ncc; jj++ )  {
		        printf("%d ", cc.clust[ii].cc[jj] );
		    }
		    printf("\n");
		}

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 37 ) {

                printf (" Enter the FIPS code to find cluster: \n");
                scanf ("%d", &fips );

                ctb_ccfind( fips, ccwfo, ccname, &ncfips, cfips, &iret );

		printf ( "\nCTB_CCFIND: iret = %d\n", iret );

		clo_init ( &iret );

		printf("CLUSTER %s (%s): \n", ccname, ccwfo );
		for ( ii = 0; ii < ncfips; ii++ )  {
		    clo_findnum ( "COUNTY", cfips[ii], sizeof(info),
			&nret, info, &iret );
		    printf("\t%d - %s\n", cfips[ii], info );
		}

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 38 ) {

                printf (" Enter the category name (CAT_IMG, CAT_SFC ...):\n");
                scanf ("%s", catgrynam );

                ctb_dcatstoi( catgrynam, &catnum, &iret );

		printf ( "\nCTB_DCATSTOI:  iret = %d\n", iret );
		printf ( "\nCategory name: %s", catgrynam );
		printf ( "\tCategory type: %d\n", catnum );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 39 ) {

                printf (" Enter the category type (0-8):\n");
                scanf ("%d", &catnum );

                ctb_dcatitos( &catnum, catgrynam, &iret );

		printf ( "\nCTB_DCATITOS:  iret = %d\n", iret );
		printf ( "\nCategory type: %d", catnum );
		printf ( "\tCategory name: %s\n", catgrynam );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 40 ) {

                printf (" Enter the sub-category name (SCAT_SFC ...):\n");
                scanf ("%s", scatgrynam );

                ctb_dscatstoi( scatgrynam, &scatnum, &iret );

		printf ( "\nCTB_DSCATSTOI:  iret = %d\n", iret );
		printf ( "\nSub-category name: %s", scatgrynam );
		printf ( "\tSub-category type: %d\n", scatnum );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 41 ) {

                printf (" Enter the sub-category type (0-8):\n");
                scanf ("%d", &scatnum );

                ctb_dscatitos( &scatnum, scatgrynam, &iret );

		printf ( "\nCTB_DSCATITOS:  iret = %d\n", iret );
		printf ( "\nSub-category type: %d", scatnum );
		printf ( "\tSub-category name: %s\n", scatgrynam );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 42 ) {

                printf (" Enter the layer file name:\n");
                scanf ("%s", filnam);

                ctb_lyrd ( filnam, &ntoply, &iret );

		if ( iret != 0 ) {
		    er_wmsg ( "CTB", &iret, filnam, &ier, 3, strlen (filnam) );
		}
		printf ( "\nCTB_LYRD:  iret = %d\n", iret );
		printf ( "\nTop layer: %d", ntoply );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 43 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetname ( ly, sizeof(layout), layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETNAME:  iret = %d\n", iret );
		printf ( "\nName of the layer: %s", layout);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 44 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetfile ( ly, layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETNAME:  iret = %d\n", iret );
		printf ( "\nName of the file: %s", layout);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 45 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetcmode ( ly, layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETCMODE:  iret = %d\n", iret );
		printf ( "\nColor mode: %s", layout);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 46 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetcolor ( ly, &color, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETCOLOR:  iret = %d\n", iret );
		printf ( "\nColor id: %d", color);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 47 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetfmode ( ly, layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETFMODE:  iret = %d\n", iret );
		printf ( "\nFill mode: %s", layout);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 48 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetgrptyp ( ly, layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETGRPTYP:  iret = %d\n", iret );
		printf ( "\nGroup type: %s", layout);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 49 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetdsply ( ly, layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETDSPLY:  iret = %d\n", iret );
		printf ( "\nDisplay mode: %s", layout);

            }
/*---------------------------------------------------------------------*/

            if ( numsub == 50 ) {

                printf (" Enter the layer number:\n");
                scanf ("%d", &ly);

                ctb_lygetoutfile ( ly, layout, &iret );

		if ( iret != 0 ) {
		    cst_inch ( ly, cly, &ier );
		    er_wmsg ( "CTB", &iret, cly, &ier, 3, strlen (cly) );
		}
		printf ( "\nCTB_LYGETOUTNAME:  iret = %d\n", iret );
		printf ( "\nName of the output file: %s", layout);

            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 51 ) {

		sprintf (filename, "%s", PREFS_TBL);
                printf  ("Reading the preferences table %s...\n", filename);

		ctb_pfread(&iret);
		printf ( "\nCTB_PFREAD: iret = %d\n\n", iret );
		rdpftbl = 1;

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 52 ) {

		if ( rdpftbl == 0 )  {
		    printf("Must read preference table first.\n");

		}
                else {
    		    printf ("Enter A TAG: \n");
                    scanf  ("%s",  tag);
		    ctb_pfstr(tag, cval, &iret);
                    printf ( "\nCTB_PFSTR: iret = %d\n\n", iret );

		    if (iret == 0) {
		        printf("The TAG VALUE = %s \n", cval);
		    }
		    else {
		        printf("Unable to obtain tag value for %s\n", tag);
		    }
                }

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 53 ) {

		if ( rdpftbl == 0 )  {
		    printf("Must read preference table first.\n");

		}
                else {
    		    printf ("Enter A TAG: \n");
                    scanf  ("%s",  tag);
		    ctb_pfbool(tag, &bval, &iret);
                    printf ( "\nCTB_PFBOOL: iret = %d\n\n", iret );

		    if (iret == 0) {
                  
		        if ( bval == TRUE ) {
		             printf("The BOOLEAN VALUE FOR THE TAG IS: TRUE\n");
                        }
                        else {
		             printf("The BOOLEAN VALUE FOR THE TAG IS: FALSE\n");
                        }
		    }
		    else {
		        printf("Unable to obtain boolean value for %s\n", tag);
		    }
                }

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 54 ) {
    		printf ("Enter cpf filename to read from: \n");
                scanf  ("%s",  cpfname);
	  	ctb_rdcpf ( cpfname, &np, cplat, cplon, &iret );
		if ( iret == 0 ) {
		    printf ( "Number of Cursor Points: %d\n", np );
		    for ( i = 0; i < np && i < 100; i++ ) {
			printf ( "%7.2f; %7.2f\n", cplat[i], cplon[i] );
		    }
		}
		else {
		    printf ( "Error encounted when reading CPF\n" );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 55 ) {
    		printf ("Enter cpf filename to write to: \n");
                scanf  ("%s",  cpfname);
		printf ( "Number of Cursor Points:" );
		scanf ( "%d", &np );
		printf ( "Enter lats and lons for %d points\n", np);
		printf ( "Enter each pair of values on a new line, separated by a space\n" );
		for (i = 0; i < np; i++ ) {
		    scanf ( "%f %f", &cplat[i], &cplon[i] );
		}

		ctb_wrcpf (cpfname, np, cplat, cplon, &ier );

		if (ier != 0 ) printf ( "Error encounted when writing CPF\n" );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 56 ) {

		ctb_g2read ( &iret );

		printf ( "\nCTB_G2READ: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 57 ) {

                printf ( "Enter the discipline:\n" );
                scanf ( " %d", &discipline );
                printf ( "Enter the category:\n" );
                scanf ( " %d", &category );
                printf ( "Enter the id number:\n" );
                scanf ( " %d", &idnumber );
                printf ( "Enter the pd number:\n" );
                scanf ( " %d", &pdnumber );

                ctb_g2gnam ( &discipline, &category, &idnumber, &pdnumber, 
                             gemname, &ihzrmp, &idrct, 
                             &iret );

                printf ( "\nGEMPAK NAME = %s\n", gemname );
		printf ( "\nIHZRMP = %d\n", ihzrmp );
		printf ( "\nIDRCT = %d\n", idrct );  

                printf ( "\nCTB_G2GNAM: iret = %d\n\n", iret );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 58 ) {

                printf ( "Enter the gemname:\n" );
                scanf ( " %s", gemname );

                ctb_g2gnum ( gemname, 
                             &discipline, &category, &idnumber, &pdnumber,
                             &iret );

                printf ( "\nDISCIPLINE = %d", discipline );
                printf ( "\nCATEGORY = %d",   category );
                printf ( "\nIDNUMBER = %d",   idnumber );
                printf ( "\nPDNUMBER = %d\n", pdnumber );

                printf ( "\nCTB_G2GNUM: iret = %d\n\n", iret );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 59 ) {


		printf ( "Enter the prefs table file:\n" );
                scanf ( " %s", tblnam);
                printf ( "Enter the lowest level directory name:\n" );
                scanf ( " %s", dirsym);
		printf ( "Enter the tag to be found:\n" );
                scanf ( " %s", tag);

		ctb_rdprf ( tblnam, dirsym, tag, value, &iret );     

                if ( iret == 0 ) {
                   printf ( "\n%s %s \n", tag, value );
		}

		printf ( "\nCTB_RDPRF: iret = %d\n", iret );
                if ( iret == -1 )
		    printf ( "Table cannot be opened.  iret=%d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 60 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = ddname;
		    ddname[0] = CHNULL;
		}

		ctb_mzrd ( filnam, defdir, &nbul, &mznms, &iret );

		if ( nbul > 0 && iret >= 0 ) {
		    printf ( "To list all of the stations read, enter 1:\n" );
		    lststn = 0;
		    scanf ( " %d", &lststn );
		    if ( lststn > 0 ) {
			for ( ibul = 0; ibul < nbul; ibul++ )
			{
			    printf ( "\n%s %s\n",
				     mznms.mzones[ibul].mzid,
				     mznms.mzones[ibul].name );
			}
		    }
		}

		printf ( "\nCTB_MZRD: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 61 ) {
                printf ( "Enter the marine zone station id (Eg. ANZ535)\n" );
		scanf ( " %s", stidmz );

               ctb_mzgnm ( stidmz, fulnam, &iret );

	       if ( iret == 0 ) {
                   printf("\nFull marine name: %s\n", fulnam);
	       }
		printf ( "\nCTB_MZGNM: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 62 ) {
                printf ( "Turn bin hours: OFF=0 ON=1 \n" );
		scanf ( " %d", &ionoff );
                printf ( "Enter hour and min (hh mm) before current time \n" );
		scanf ( " %d %d", &hrsbfr, &mnsbfr );
                printf ( "Enter hour and min (hh mm) before current time \n" );
		scanf ( " %d %d", &hraftr, &mnaftr );
                printf ( "Most recent only flag: 0 if OFF; 1 if ON \n" );
		scanf ( " %d", &mstrct );

		ctb_dhrsitos ( &ionoff, &hrsbfr, &mnsbfr, &hraftr, &mnaftr,
			&mstrct, binhr, &iret );

		if ( iret == 0 ) {
                   printf("\nBin hours: %s\n", binhr );
		}
		printf ( "\nCTB_DHRSITOS: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 63 ) {
                printf ( "Enter bin hours string (ON/5:15/2:30/ON):\n" );
		scanf ( " %s", binhr );
		
		ctb_dhrsstoi ( binhr, &ionoff, &hrsbfr, &mnsbfr, &hraftr,
			&mnaftr, &mstrct, &iret );

		if ( iret == 0 ) {
                   printf("\nBin hour on or off: %s\n", (ionoff==1)?"ON":"OFF");
                   printf("\nBin hour:min before current time: %d:%d\n",
			  hrsbfr, mnsbfr);
                   printf("\nBin hour:min after current time: %d:%d\n",
			  hraftr, mnaftr);
                   printf("\nMost recent flag on or off: %s\n",
			  (mstrct==1)?"ON":"OFF");
		}
		printf ( "\nCTB_DHRSSTOI: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 64 ) {

                ctb_gfard ( &iret );

                printf ( "\nCTB_GFARD: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 65 ) {

                nfhr = 0;
                fhrlist[ 0 ] = '\0';

                ctb_gfagfhr ( sep, &nfhr, fhrlist, &iret );

                if ( iret == 0 ) {
                   printf( "\nNumber of forecast hour choices: \t%d\n", nfhr );
                   printf( "List of forecast hour choices: \t\t%s\n", fhrlist );
                }

                printf ( "\nCTB_GFAGFHR: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 66 ) {

                nhaz = 0;
                hazlist[ 0 ] = '\0';

                ctb_gfaghaz ( sep, &nhaz, hazlist, &iret );

                if ( iret == 0 ) {
                   printf( "\nNumber of hazard choices: \t%d\n", nhaz );
                   printf( "List of hazard choices: \t%s\n", hazlist );
                }

                printf ( "\nCTB_GFAGHAZ: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 67 ) {

                printf ( "Enter a GFA hazard: \n" );
                scanf ( " %s",  hazard );

                ndesc = 0;
                ctb_gfagndesc ( hazard, &ndesc, &iret );

                if ( iret == 0 ) {
                   printf( "\nNumber of hazard descriptors: \t%d\n", ndesc );
                }

                printf ( "\nCTB_GFAGNDESC: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 68 ) {

                printf ( "Enter a GFA hazard: \n" );
                scanf ( " %s",  hazard );

                printf ( "Enter which descriptor: \n" );
                scanf ( " %d",  &which );

                desc[ 0 ] = '\0';
                type[ 0 ] = '\0';
                nchoices  = 0;

                ctb_gfagdesc ( hazard, &which, type, desc, &nchoices, &iret );

                if ( iret == 0 ) {
                   printf( "\nType: \t\t%s\n", type );
                   printf( "Descriptor: \t%s\n", desc );
                   printf( "Number of choices/columns: \t%d\n", nchoices );
                }

                printf ( "\nCTB_GFAGDESC: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 69 ) {


                printf ( "Enter a GFA hazard: \n" );
                scanf ( " %s",  hazard );

                printf ( "Enter which descriptor: \n" );
                scanf ( " %d",  &which );

                printf ( "Enter which choice: \n" );
                scanf ( " %d",  &choice );

                choiceStr[ 0 ] = '\0';

                ctb_gfagdc ( hazard, &which, &choice, choiceStr, &iret );

                if ( iret == 0 ) {
                   printf( "\nChoice: \t%s\n", choiceStr );
                }

                printf ( "\nCTB_GFAGDC: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 70 ) {

                ntags = 0;
                taglist[ 0 ] = '\0';

                ctb_gfagtag ( sep, &ntags, taglist, &iret );

                if ( iret == 0 ) {
                   printf( "\nNumber of tags: \t%d\n", ntags );
                   printf( "List of tags: \t\t%s\n", taglist );
                }

                printf ( "\nCTB_GFAGTAG: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 71 ) {

                printf ( "Enter a GFA hazard: \n" );
                scanf ( " %s",  hazard );

                ctb_gfagcat ( hazard, cat, &iret );

                if ( iret == 0 ) {
		   if ( strlen ( cat ) > (size_t)0 ) {
                      printf( "\nHazard category: \t%s\n", cat );
		   }
		   else {
		      printf( "\nHazard category: \tNone\n" );
		   }
                }
		else {
                   printf ( "\nNo hazard found!\n");
		}

                printf ( "\nCTB_GFAGCAT: iret = %d\n\n", iret );

            }

/*---------------------------------------------------------------------*/
	    if ( numsub == 72 ) {

                printf (" Enter the county cluster file name: \n");
                scanf ("%s", cctbl );

                ctb_permccrd( cctbl, "stns", &pc, &iret );

		printf ( "\nCTB_PERMCCRD: iret = %d\n", iret );

		for ( ii = 0; ii < pc.nclust; ii++ )  {
		    printf("Cluster %3d - %s - %s\n\t", 
			ii, pc.clust[ii].pcwfo, pc.clust[ii].pcname );
		    for ( jj = 0; jj < pc.clust[ii].npc; jj++ )  {
		        printf("%d ", pc.clust[ii].pc[jj] );
		    }
		    printf("\n");
		}

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 73 ) {

                printf (" Enter the FIPS code to find permanent cluster: \n");
                scanf ("%d", &fips );

                ctb_permccfind( fips, pcwfo, pcname, &ncfips, cfips, &iret );

		printf ( "\nCTB_PERMCCFIND: iret = %d\n", iret );

		clo_init ( &iret );

		printf("CLUSTER %s (%s): \n", pcname, pcwfo );
		for ( ii = 0; ii < ncfips; ii++ )  {
		    clo_findnum ( "MZ_CNTY", cfips[ii], sizeof(info),
			&nret, info, &iret );
		    printf("\t%d - %s\n", cfips[ii], info );
		}

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 74 ) {

                ctb_gfagtemp ( fileName, &iret );

                if ( iret == 0 ) {
                   printf( "\nAIRMET file name template: \t%s\n", fileName );
                }

                printf ( "\nCTB_GFAGTEMP: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 75 ) {

                printf ( "Enter a GFA hazard: \n" );
                scanf ( " %s",  hazard );

                printf ( "Enter a GFA descriptor name (i.e. Frequency or Severity): \n" );
                scanf ( " %s",  desc );

                printf ( "Enter the first description:\n" );
                scanf ( " %s",  value1 );

                printf ( "Enter the second description:\n" );
                scanf ( " %s",  value2 );

                ctb_gfaCmpSeverity ( hazard, desc, value1, value2, worstCase, &iret );

                if ( iret == 0 ) {
                   printf( "\nHazard: \t\t%s\n", hazard );
                   printf( "Descriptor: \t%s\n", desc );
                   printf( "Descriptions compared: \t%s\t%s\n", value1, value2 );
                   printf( "The more severe one is %s \n", worstCase );
                }

                printf ( "\nCTB_GFACMPSEVERITY: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 76 ) {
		printf(" Enter the input cycle time:\n");
		scanf( "%s", cycleTm );

		ctb_airmetGetIssueTm( cycleTm, issueTm, &iret );
		
		printf("\nCTB_GETAIRMETISSUETM:  iret = %d, issueTm = %s\n",
				iret, issueTm);  
	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 77 ) {
		isDST = False;

		printf("\nIs Daylight Savings in effect (y/n) ?" );
		scanf( "%s", dst );

		if( (dst[0] == 'y') || (dst[0] == 'Y') ) {
		    isDST = True;
                }

  		ctb_airmetGetCycleTms( isDST, &ntimes, &times, &iret ); 

		printf("\nCTB_AIRMETGETCYCLETMS:  iret = %d\n", iret );
		printf("                      ntimes = %d\n", ntimes );

		for( ii=0; ii<ntimes; ii++ ) {
		    if( ii==0 ) {
		        printf("             Cycle Times are:  %s\n", times[ii] );
		    }
		    else {
		        printf("                               %s\n", times[ii] );
		    }
		}


		for( ii=0; ii<ntimes; ii++ ) {
		    free( times[ii] );
		}
		free( times );
	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 78 ) {

		ctb_gfagiss( ";", &nopt, issOptList, &iret );

		printf("\nCTB_GFAGISS:  iret = %d\n", iret );
		printf("       num options = %d\n", nopt );
		printf("       issOptList  = %s\n", issOptList );

	    }
/*---------------------------------------------------------------------*/
            if ( numsub == (int )80 ) {

		printf("\nFirst IFR type (use underscores for blanks): "); 
		scanf( "%s", type0 );
		for( ii=0; ( unsigned int )ii < strlen( type0 ); ii++ ) {
		    if( type0[ii] == '_' ) {
			type0[ii] = ' ';
		    }
	        }

		printf("\nSecond IFR type (use underscores for blanks): "); 
		scanf( "%s", type1 );
		for( ii=0; ( unsigned int )ii < strlen( type1 ); ii++ ) {
		    if( type1[ii] == '_' ) {
			type1[ii] = ' ';
		    }
	        }

                ctb_gfaCombineIFRTypes( type0, type1, IFRType, &iret );

		printf("\nCTB_GFACOMBINEIFRTYPES:  iret = %d\n", iret );
		printf("    Combined IFRType = %s\n", IFRType );
	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 81 ) {

                printf ( "Enter a GFA hazard: \n" );
                scanf ( " %s",  hazard );

                ctb_gfagid ( hazard, gfaid, &iret );

                if ( iret == 0 ) {
		   if ( strlen ( gfaid ) > (size_t)0 ) {
                      printf( "\nHazard id: \t%s\n", gfaid );
		   }
		   else {
		      printf( "\nHazard id: \tNone\n" );
		   }
                }
		else {
                   printf ( "\nNo hazard id found!\n");
		}

                printf ( "\nCTB_GFAGCAT: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 82 ) {

                ndesks = 0;
                desklist[ 0 ] = '\0';

                ctb_gfagdesk ( sep, &ndesks, desklist, &iret );

                if ( iret == 0 ) {
                   printf( "\nNumber of desks: \t%d\n", ndesks );
                   printf( "List of desks: \t\t%s\n", desklist );
                }

                printf ( "\nCTB_GFAGDESK: iret = %d\n\n", iret );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 83 ) {
		
		printf ( "Enter the first flight level: \n");
                scanf ( " %s", flightLevel0  );

		printf ( "Enter the top freezing level on the first flight level: \n");
                scanf ( " %s", fzlTop0  );

		printf ( "Enter the bottom freezing level on the first flight level: \n");
                scanf ( " %s", fzlBottom0  );

		printf ( "Enter the second flight level: \n");
                scanf ( " %s", flightLevel1  );

		printf ( "Enter the top freezing level on the second flight level: \n");
                scanf ( " %s", fzlTop1  );

		printf ( "Enter the bottom freezing level on the second flight level: \n");
                scanf ( " %s", fzlBottom1  );
		
	        topBot = 0;
		ctb_gfaWorstFL ( topBot, flightLevel0, fzlBottom0, fzlTop0, 
		              flightLevel1, fzlBottom1, fzlTop1,
	 		      worstTop, worstBottomFZL, worstTopFZL, &iret );
	        
	        topBot = 1;
		ctb_gfaWorstFL ( topBot, flightLevel0, fzlBottom0, fzlTop0, 
		              flightLevel1, fzlBottom1, fzlTop1,
	 		      worstBottom, worstBottomFZL, worstTopFZL, &iret );

                if ( iret == 0 ) {
                   printf( "The worst flight level top is: \t%s\n", worstTop );
                   printf( "The worst flight level bottom is: \t%s\n", worstBottom );
                   printf( "The worst freezing level top is: \t%s\n", worstTopFZL );
                   printf( "The worst freezing level bottom is: \t%s\n", worstBottomFZL );
                }

                printf ( "\nCTB_GFAWORSTFL: iret = %d\n\n", iret );

            }

/*---------------------------------------------------------------------*/
            if ( numsub == 84 ) {
                
                printf ( "Enter the number of GFA issue types: \n" );
                scanf ( " %d",  &nissue );
                
		issues = (char **) malloc ( nissue * sizeof( char * ) );
                
		for ( ii = 0;  ii < nissue; ii++ ) {
		    issues[ ii ] = (char (*) ) malloc ( sizeof( char ) * 32 );
                    
		    printf ( "Enter the %d_th issue type: \n", ii+1 );
                    scanf ( " %s",  issues[ii] );
                }
		
                ctb_gfaWorstIssue ( nissue, issues, worstIssue, &iret );

                if ( iret == 0 ) {
		    printf( "The issue types: \t" );
		    for ( ii = 0;  ii < nissue; ii++ ) {
		        printf( "%s ",  issues[ ii ]);		        
			free ( issues[ ii ] );                    
		    }
                    
		    printf( "\n" );			
		    printf( "The worst issue is: \t\t%s\n", worstIssue );
                }

                printf ( "\nCTB_GFAWORSTISSUE: iret = %d\n\n", iret );
				
		free ( issues );

            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 85 ) {

                printf ( "\nReading the Hershey Font table...\n" );

		ctb_hfread ( &iret );
		printf ( "\nCTB_HFREAD: iret = %d\n\n", iret );
		rdhftbl = 1;

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 86 ) {

		if ( rdhftbl == 0 )  {
		    printf ( "\nMust read Hershey Font table first.\n" );
		}
		else {
		    printf ( "\nDumping the Hershey Font table...\n" );

		    ctb_hfdump ( &iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 87 ) {

		if ( rdhftbl == 0 )  {
		    printf ( "\nMust read Hershey Font table first.\n" );
		}
                else {
    		    printf ( "Enter the font number: \n" );
                    scanf  ( "%d",  &fontnum );

		    maxpt = MXCHPT;
		    ctb_hfgetfont ( &fontnum, &maxpt, &numch,
			   	    ascval, ixmin, ixmax, npnts,
				    ixc, iyc, &iret );

		    printf ( "\nCTB_HFGETFONT: iret = %d\n", iret );

		    if  ( iret == 0 )  {
			
			printf ( "\nFont id number = %d\n", fontnum );

			for ( ichr = 0; ichr < numch; ichr++ ) {

			    printf ( "ASCII Value = %d (%c) -- Number of points = %d\n",
				ascval[ichr], ascval[ichr], npnts[ichr] );
			    printf ( "        Min X = %d -- Max X = %d\n", 
				ixmin[ichr], ixmax[ichr] );

			    for ( ipnt = 0; ipnt < npnts[ichr]; ipnt++ ) {
				idx = (ichr * maxpt) + ipnt;
				printf ( "(%3d,%3d)  ",
					ixc[idx], iyc[idx] );
				if ( (ipnt+1) % 6 == 0 ) printf ( "\n" );
			    }
			    printf ( "\n" );
			}
		    }

                }

	    }
/*---------------------------------------------------------------------*/
	}
	return(0);
}
