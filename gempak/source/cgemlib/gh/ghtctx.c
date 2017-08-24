#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "ghcmn.h"

#define HURRICANE_WARNING       0
#define HURRICANE_WATCH         1
#define TROPICAL_STORM_WARNING  2
#define TROPICAL_STORM_WATCH    3

#define TROPICAL_STORM		0
#define HURRICANE		1
#define WATCH			0
#define WARNING			1

#define LISTSIZE		1000
#define WFOSIZE			200
#define BKPSIZE			605

#define BKPT_TBL		"TCA_BKPTS"

/*
 * private function to read a vg file
 */
static void gh_rdvg( char*, char*, char*, char*, char*, 
		     char*, char*, char*, int[], int[][ 4 ], int[], 
		     int[] [ 4 ], int[], int[][ 4 ], int[], int[] [ 4 ],
 		     int[], int[][ 4 ], int[], int [][ 4 ] );
 

void gh_tctx ( char * vgFile, char * prevVGF, char *stTcan, char *validTcan, 
               char *zonecan )  
/************************************************************************
 * gh_tctx                                                              *
 *                                                                      *
 * This subroutine generates the text message containing tropical       *
 * cyclone watch/warning and breakpoint information.                    *
 *                                                                      *
 * gh_tctx ( vgFile, prevVGF, stTcan, validTcan, zonecan ) 	        *
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgFile	       	char	Name of VGF file with breakpoint info	*
 *	*prevVGF	char	Name of previous VGF file 		*
 *  	*stTcan		char	Storm type for cancel			*
 *	*validTcan	char	GEMPAK time for cancel			*
 * 	*zonecan	char	Local time zone for cancel		*
 *                                                                      *
 * Output parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC 		 8/04   Created					*
 * B. Yin/SAIC 		12/04   Added timezone				*
 * D. Kidwell/NCEP	 2/05   CSC for gh_rdvg and gh_wwtx, added      *
 * 				calls to gh_wwbl, BKPSIZE 110 -> 275    *
 * M. Li/SAIC		03/05	Added stTcan, validTcan, and zonecan	*
 * B. Yin/SAIC		04/05	Modified gh_rdvg calling sequences	*
 * B. Yin/SAIC		04/05	Modified gh_wwtx calling sequences	*
 * D. Kidwell/NCEP	 4/05	Added Keys processing, BKPSIZE 275->605 *
 * D. Kidwell/NCEP	10/05	Added adNum to gh_rdvg and gh_wwtx      *
 * S. Gilbert/NCEP	01/06	Changed adNUm from int to char          *
 * S. Jacobs/NCEP	 8/08	Added New England islands		*
 ***********************************************************************/
{
   int 		iret, ii, jj, nSeg, nSegin;
   int 		vtec [ MAX_TCAWW ] [ 3 ], idxare [ 6 ];
   int 		numbus1 [ 4 ], bkus1 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbus2 [ 4 ], bkus2 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbvi1 [ 4 ], bkvi1 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbvi2 [ 4 ], bkvi2 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbpr1 [ 4 ], bkpr1 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbpr2 [ 4 ], bkpr2 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbwt1 [ 4 ], bkwt1 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbwt2 [ 4 ], bkwt2 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbky1 [ 4 ], bkky1 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbky2 [ 4 ], bkky2 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbpc1 [ 4 ], bkpc1 [ MAX_TCAWW * 2 ] [ 4 ];
   int 		numbpc2 [ 4 ], bkpc2 [ MAX_TCAWW * 2 ] [ 4 ];
   int		jusgec, jother, jprico, jwater, jkeys, jpacif;
   int          iadnm, iflag;

   char		stormName [ MAX_STORM_STR + 1 ], validTime [ DTTMSZ ];
   char		clist [ MAX_TCAWW ][ LISTSIZE ];
   char		zlist [ MAX_TCAWW ][ LISTSIZE ], wfoStr [ WFOSIZE ];
   char		bkpStr [ MAX_TCAWW ] [ BKPSIZE ];
   char 	stType [ 3 ], stormId [ 7 ], zone [ 4 ], status, adNum[ 5 ];

/*---------------------------------------------------------------------*/

   /*
    * Initialize
    */
   clo_init ( &iret );
   gh_bktb  ( &iret );

   jusgec = IUSGEC;
   jother = IOTHER;
   jprico = IPRICO;
   jwater = IWATER;
   jkeys  = IKEYS;
   jpacif = IPACIF;
   strcpy (adNum, "1");

   for ( ii = 0; ii < 4; ii++ ) {
       numbus1 [ ii ] = 0;
       numbus2 [ ii ] = 0;
       numbvi1 [ ii ] = 0;
       numbvi2 [ ii ] = 0;
       numbpr1 [ ii ] = 0;
       numbpr2 [ ii ] = 0;
       numbwt1 [ ii ] = 0;
       numbwt2 [ ii ] = 0;
       numbky1 [ ii ] = 0;
       numbky2 [ ii ] = 0;
       numbpc1 [ ii ] = 0;
       numbpc2 [ ii ] = 0;
       idxare [ ii ]  = 0;
   }
   idxare [ 4 ]  = 0;
   idxare [ 5 ]  = 0;

   for ( ii = 0; ii < MAX_TCAWW * 2; ii++ ) {
       for ( jj = 0; jj < 4; jj++ ) {
           bkus1 [ ii ][ jj ] = 0;
           bkus2 [ ii ][ jj ] = 0;
           bkvi1 [ ii ][ jj ] = 0;
           bkvi2 [ ii ][ jj ] = 0;
           bkpr1 [ ii ][ jj ] = 0;
           bkpr2 [ ii ][ jj ] = 0;
           bkwt1 [ ii ][ jj ] = 0;
           bkwt2 [ ii ][ jj ] = 0;
           bkky1 [ ii ][ jj ] = 0;
           bkky2 [ ii ][ jj ] = 0;
           bkpc1 [ ii ][ jj ] = 0;
           bkpc2 [ ii ][ jj ] = 0;
       }
   }

   /*
    * Read break point info
    */
   if ( strlen(prevVGF) != (size_t)0 ) {

      gh_rdvg ( prevVGF, stType, validTime, stormName, stormId, zone, 
      		&status, adNum, numbus1, bkus1, numbvi1, bkvi1, 
                numbpr1, bkpr1, numbwt1, bkwt1, numbky1, bkky1,
                numbpc1, bkpc1 ); 

   }

   if (  strlen (vgFile) != (size_t)0 ) {
       gh_rdvg ( vgFile, stType, validTime, stormName, stormId, zone, 
         	 &status, adNum, numbus2, bkus2, numbvi2, bkvi2, 
                 numbpr2, bkpr2, numbwt2, bkwt2, numbky2, bkky2,
                 numbpc2, bkpc2 ); 
   }
   else {
      strcpy ( stType, stTcan );
      strcpy ( validTime, validTcan );
      strcpy ( zone, zonecan );
      gh_advn ( adNum, &iadnm, &iflag, &iret );
      cst_inch ( ++iadnm, adNum, &iret );
   } 

   /*
    * append a space to the storm name, which is need in gh_wwtx
    */
   strcat ( stormName, " " );

   /*
    * Generate the text message.
    * The message is saved into a file "KNHCTCVAT#"
    */

   nSeg      = 0;
   gh_wwbk ( bkus2, numbus2, bkus1, numbus1, &jusgec, clist, zlist, 
	     vtec, bkpStr, &nSeg, wfoStr, &iret, 
	     LISTSIZE, LISTSIZE, BKPSIZE, WFOSIZE );
   if ( nSeg > 0 ) idxare [ 0 ] = 1;
   nSegin = nSeg;

   gh_wwbl ( bkvi2, numbvi2, bkvi1, numbvi1, &jother, clist, zlist, 
	     vtec, bkpStr, &nSeg, wfoStr, &iret, 
	     LISTSIZE, LISTSIZE, BKPSIZE, WFOSIZE );
   if ( nSeg > nSegin ) idxare [ 1 ] = nSegin + 1;
   nSegin = nSeg;

   gh_wwbl ( bkpr2, numbpr2, bkpr1, numbpr1, &jprico, clist, zlist, 
	     vtec, bkpStr, &nSeg, wfoStr, &iret, 
	     LISTSIZE, LISTSIZE, BKPSIZE, WFOSIZE );
   if ( nSeg > nSegin ) idxare [ 2 ] = nSegin + 1;
   nSegin = nSeg;

   gh_wwbl ( bkwt2, numbwt2, bkwt1, numbwt1, &jwater, clist, zlist, 
	     vtec, bkpStr, &nSeg, wfoStr, &iret, 
	     LISTSIZE, LISTSIZE, BKPSIZE, WFOSIZE );
   if ( nSeg > nSegin ) idxare [ 3 ] = nSegin + 1;
   nSegin = nSeg;

   gh_wwbk ( bkky2, numbky2, bkky1, numbky1, &jkeys, clist, zlist, 
	     vtec, bkpStr, &nSeg, wfoStr, &iret, 
	     LISTSIZE, LISTSIZE, BKPSIZE, WFOSIZE );
   if ( nSeg > nSegin ) idxare [ 4 ] = nSegin + 1;

   gh_wwbk ( bkpc2, numbpc2, bkpc1, numbpc1, &jpacif, clist, zlist, 
	     vtec, bkpStr, &nSeg, wfoStr, &iret, 
	     LISTSIZE, LISTSIZE, BKPSIZE, WFOSIZE );
   if ( nSeg > nSegin ) idxare [ 5 ] = nSegin + 1;

   wfoStr [ WFOSIZE - 1 ] = '\0';

   gh_wwtx ( stType, validTime, stormName, stormId, zone, &nSeg, wfoStr,
	     idxare, &status, adNum, clist, zlist, vtec, bkpStr, &iret,
 	     strlen ( stType ), strlen( validTime ), 
	     strlen ( stormName ), strlen ( stormId ), strlen ( zone ), 
	     strlen ( wfoStr ), 1, strlen ( adNum ), LISTSIZE, LISTSIZE, 
             BKPSIZE ); 
}
/*=====================================================================*/


static void gh_rdvg ( char * fileName, char * stType, char * validTime,
                      char * stormName, char * stormId, char * timezone,
		      char * status, char *adNum, int numbus[], 
		      int bkus[][ 4 ], int numbvi[], 
                      int bkvi[][ 4 ], int numbpr[], int bkpr[][ 4 ], 
                      int numbwt[], int bkwt[][ 4 ], int numbky[],
		      int bkky[][4], int numbpc[], int bkpc[][4] )
/************************************************************************
 * gh_rdvg                                                              *
 *                                                                      *
 * This subroutine reads the break point info from a vg file.	        *
 *                                                                      *
 * gh_rdvg ( fileName, stType, validTime, stormName, stormId, timezone, *
 *	     status, adNum, numbus, bkus, numbvi, bkvi, numbpr, bkpr,   *
 *	     numbwt, bkwt, numbky, bkky, numbpc, bkpc )                 *
 *                                                                      *
 * Input parameters:                                                    *
 *	*fileName	char	name of VGF file with breakpoint info	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*stType		char	storm type				*
 *                                  HU: Hurricane                       *
 *                                  TS: Tropical Storm                  *
 *                                  TD: Tropical Depression             *
 *                                  SS: Subtropical Storm               *
 *                                  SD: Subtropical Depression          *
 *                                  PT: Post-Tropical Cyclone           *
 *                                 PTC: Potential Tropical Cyclone      *
 *	*validTime	char	time string in gempak format		*
 *	*stormName	char	storm name				*
 *	*stormId	char	basin + storm number + year		*
 *	*timezone	char	time zone				*
 *	*status		char	issue status				*
 *	*adNum		char	advisory number                         *
 *	numbus[]	int	number of bkpt pairs in US		*
 *	bkus[][]	int	sequence numbers of bkpts in US		*
 *	numbvi[]	int	number of bkpts in islands		*
 *	bkvi[][]	int	sequence numbers of bkpts in islands    *
 *	numbpr[]	int	number of bkpt pairs in Puerto Rico     *
 *	bkpr[][]	int	sequence numbers of bkpts in Puerto Rico*
 *	numbwt[]	int	number of bkpts in water bodies         *
 *	bkwt[][]	int	sequence numbers of bkpts in water      *
 *	numbky[]	int	number of bkpt pairs in Florida Keys    *
 *	bkky[][]	int	sequence numbers of bkpts in FL Keys    *
 *	numbpc[]	int	number of bkpt pairs in Pacific coast   *
 *	bkpc[][]	int	sequence numbers of bkpts in Pacific cst*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC 		 8/04   Created					*
 * B. Yin/SAIC 		12/04   Added timezone				*
 * D. Kidwell/NCEP	 2/05   CSC, added processing for VI, PR, water *
 * B. Yin/SAIC		 4/05	Added issue status              	*
 * D. Kidwell/NCEP	 4/05   CSC, added processing for Florida Keys  *
 * D. Kidwell/NCEP	10/05   CSC to add adNum; added "DRY_ISLE" check*
 * S. Gilbert/NCEP	01/06   CHanged adNum from int to char          *
 * S. Gilbert/NCEP	10/07   Added return of pacific coast sbkpts    *
 * m.gamazaychikov/SAIC	12/07   Added return of Hawaiian islands bkpts  *
 * C. Lauer/NHC         03/11   Add PuertoRico, change US Virgin Islands*
 * S. Jacobs/NCEP	 3/13	Add check for storm type 5 = PT		*
 * M. Onderlinde/NHC     9/16   Add check for storm type 6 = PTC        *
 ***********************************************************************/
{
   int 		iret, stormNum, year, basin, stormType, wwNum;
   int		type, npt, ii, jj, kk, seqNum, areaID, seqCal, seqME;
   int		ibpt, iuscnt, imxcnt, iprcnt, ikycnt, ipccnt, icalcnt, imxwcnt;
   int          icncnt;
   int 		severity [ MAX_TCAWW * 2 ], adType [ MAX_TCAWW * 2 ]; 
   int 		geo[ MAX_TCAWW * 2 ], numBkp[  MAX_TCAWW * 2 ];

   float	lat [ MAX_TCAWW * 2 ], lon [ MAX_TCAWW * 2 ];

   char		bpStr [ MAX_TCAWW * 2 * MAX_BREAK_PT_STR ];
   char		stn [ 34 ], state[3], country[3];
/*---------------------------------------------------------------------*/

   gh_bkrv ( fileName, &stormNum, &year, &basin, adNum, &stormType,
	     stormName, validTime, &wwNum, severity, adType, geo,
	     lat, lon, bpStr, numBkp, timezone, status, &iret ); 
   ibpt = 0;
   type = TROPICAL_STORM_WATCH;

   for ( ii = 0; ii < wwNum; ii++ ) {

       if ( severity [ ii ] == HURRICANE && adType [ ii ] == WARNING )

          type = HURRICANE_WARNING;

        else if ( severity [ ii ] == HURRICANE && adType [ ii ] == WATCH )

          type = HURRICANE_WATCH;

        else if ( severity [ ii ] == TROPICAL_STORM && adType [ ii ]== WARNING )

          type = TROPICAL_STORM_WARNING;

        else if ( severity [ ii ] == TROPICAL_STORM && adType [ ii ] == WATCH )
  
          type = TROPICAL_STORM_WATCH;

       /*
        * get the sequence numbers
        */

       iuscnt = 0;
       imxcnt = 0;
       iprcnt = 0;
       ikycnt = 0;
       ipccnt = 0;
       icncnt = 0;
       icalcnt = 0;
       imxwcnt = 0;
       for ( jj = 0; jj < numBkp [ ii ]; jj++ ) {

           clo_tclosest ( BKPT_TBL, lat [ ibpt ], lon [ ibpt ], 1, 
                          &iret );
           clo_tgid ( BKPT_TBL, 1, sizeof( stn ), &npt, stn, &iret );

           /*
            * remove ";" from the stn string
            */
           kk = 0;
	   while ( stn [ kk ] != '\0' ) {
	         if ( stn[ kk ] == ';' ) {
	            stn[ kk ] = '\0';
	            break;
		 }
	         kk++;
	   }

           strncat( stn, "       ", 8 - strlen( stn ) );
           gh_bksq( stn, &seqNum, &areaID, &iret );

	   if ( areaID == IUSGEC ) {
	      bkus [ numbus [ type ] ] [ type ] = seqNum;
              numbus [ type ]++; 
              gh_bkloc( stn, state, country, &iret, strlen(stn), 2 , 2 );
              state[2]='\0';
              country[2]='\0';
              if ( strcmp ( country, "US" ) == 0 ) 
	         iuscnt++;
              else
                 icncnt++;
	   }
	   else if ( areaID == IMXCSA ) {
	      imxcnt++;
	   }
	   else if ( areaID == IOTHER ) {

	      /*
	       * Check for US VI, Hawaiian or Dry Tortugas Island.
	       * Lauer, 3/11, check for Vieques, Culebra, PR, etc.
	       */

	      if ( ( strcmp ( stn,"US_VRG_I" ) == 0 ) ||
                   ( strcmp ( stn,"ST_TH_JO" ) == 0 ) ||
                   ( strcmp ( stn,"ST_CROIX" ) == 0 ) ||
                   ( strcmp ( stn,"VIEQUES " ) == 0 ) ||
                   ( strcmp ( stn,"CULEBRA " ) == 0 ) ||
                   ( strcmp ( stn,"PUERTO_R" ) == 0 ) ||
                   ( strcmp ( stn,"HAWAII  " ) == 0 ) ||  
                   ( strcmp ( stn,"KAUAI   " ) == 0 ) ||  
                   ( strcmp ( stn,"MAUI    " ) == 0 ) ||  
                   ( strcmp ( stn,"OAHU    " ) == 0 ) ||  
                   ( strcmp ( stn,"BLOCK_IS" ) == 0 ) ||  
                   ( strcmp ( stn,"NANTUKCT" ) == 0 ) ||  
                   ( strcmp ( stn,"MRT_VYRD" ) == 0 ) ||  
                   ( strcmp ( stn,"DRY_ISLE" ) == 0 ) ) { 
	          bkvi [ numbvi [ type ] ] [ type ] = seqNum;
                  numbvi [ type ]++; 
  	      }
	   }
	   else if ( areaID == IPRICO ) {
	      bkpr [ numbpr [ type ] ] [ type ] = seqNum;
              numbpr [ type ]++; 
	      iprcnt++;
	   }
	   else if ( areaID == IWATER ) {
	      bkwt [ numbwt [ type ] ] [ type ] = seqNum;
              numbwt [ type ]++; 
	   }
	   else if ( areaID == IKEYS ) {
	      bkky [ numbky [ type ] ] [ type ] = seqNum;
              numbky [ type ]++; 
	      ikycnt++;
	   }
	   else if ( areaID == IPACIF ) {
	      bkpc [ numbpc [ type ] ] [ type ] = seqNum;
              numbpc [ type ]++; 
	      ipccnt++;
              gh_bkloc( stn, state, country, &iret, strlen(stn), 2 , 2 );
              state[2]='\0';
              country[2]='\0';
              if ( strcmp ( country, "US" ) == 0 ) 
                 icalcnt++;
              else
                 imxwcnt++;
	   }
           ibpt++;

       }


       /*
        * Check for East TX/MX border.  Treat Brownsville, TX as the border
        * for TCV purposes.
        */

       if ( ( iuscnt == 1 ) && ( imxcnt == 1 ) ) {
	   if ( bkus [ numbus [ type ] - 1 ] [ type ] != 1 ) {
	      bkus [ numbus [ type ] ] [ type ] = 1;
	      numbus [ type ]++;
           }
	   else {

              /*
               * One point is Brownsville, the other is in Mexico.
               * There is no TCV segment, so remove point.
               */

	      numbus [ type ]--;
	      bkus [ numbus [ type ] ] [ type ] = 0;
	   }
       }

       else if ( ( icalcnt == 1 ) && ( imxwcnt == 1 ) ) {

          /*
           * Check for West CA/MX border.  Treat the last California 
           * breakpoint in the table as the border for TCV purposes.
           */
           gh_bkcal( &seqCal, &iret );

	   if ( bkpc [ numbpc [ type ] - 1 ] [ type ] == seqCal ||
	        bkpc [ numbpc [ type ] - 2 ] [ type ] == seqCal ) {
              /*
               * One point is the last bkpt in California, the other is in
               * Mexico, There is no TCV segment, so remove both bkpts.
               */
              numbpc [ type ]--; 
              bkpc [ numbpc [ type ] ] [ type ] = 0;
              numbpc [ type ]--; 
              bkpc [ numbpc [ type ] ] [ type ] = 0;
           }
           else {
              /*
               * Set Mexico brkpt to the last one in California.
               */
              if ( bkpc [ numbpc [ type ] - 1 ] [ type ] > seqCal )
                   bkpc [ numbpc [ type ] - 1 ] [ type ] = seqCal;
              if ( bkpc [ numbpc [ type ] - 2 ] [ type ] > seqCal )
                   bkpc [ numbpc [ type ] - 2 ] [ type ] = seqCal;
           }

       }

       else if ( ( iuscnt == 1 ) && ( icncnt == 1 ) ) {

          /*
           * Check for Maine/Canada border.  Treat the last Maine 
           * breakpoint in the table as the border for TCV purposes.
           */
           gh_bkme( &seqME, &iret );
	   if ( bkus [ numbus [ type ] - 1 ] [ type ] == seqME ||
	        bkus [ numbus [ type ] - 2 ] [ type ] == seqME ) {
              /*
               * One point is the last bkpt in Maine, the other is in
               * Canada, There is no TCV segment, so remove both bkpts.
               */
              numbus [ type ]--; 
              bkus [ numbus [ type ] ] [ type ] = 0;
              numbus [ type ]--; 
              bkus [ numbus [ type ] ] [ type ] = 0;
           }
           else {
              /*
               * Set Canada brkpt to the last one in Maine.
               */
              if ( bkus [ numbus [ type ] - 1 ] [ type ] > seqME )
                   bkus [ numbus [ type ] - 1 ] [ type ] = seqME;
              if ( bkus [ numbus [ type ] - 2 ] [ type ] > seqME )
                   bkus [ numbus [ type ] - 2 ] [ type ] = seqME;
           }

       }

       else {

           /*
            * Check for singletons which should have been pairs, and 
            * remove the point(s).
            */

           if ( iuscnt == 1 ) {
	      numbus [ type ]--;
	      bkus [ numbus [ type ] ] [ type ] = 0;
	   }
           if ( iprcnt == 1 ) {
	      numbpr [ type ]--;
	      bkpr [ numbpr [ type ] ] [ type ] = 0;
	   }
           if ( ikycnt == 1 ) {
	      numbky [ type ]--;
	      bkky [ numbky [ type ] ] [ type ] = 0;
	   }
           if ( ipccnt == 1 ) {
	      numbpc [ type ]--;
	      bkpc [ numbpc [ type ] ] [ type ] = 0;
	   }
       }
   }

   /*
    * Get the number of pairs for U.S., Puerto Rico and Keys breakpoints.
    */

   for ( ii = 0; ii < 4; ii++ ) {
       numbus [ ii ] /= 2;
       numbpr [ ii ] /= 2;
       numbky [ ii ] /= 2;
       numbpc [ ii ] /= 2;
   }

   switch ( stormType ) {
	  
          case 0:
		
		strcpy ( stType, "HU" );
		break;

          case 1:
		
		strcpy ( stType, "TS" );
		break;

          case 2:
		
		strcpy ( stType, "TD" );
		break;

          case 3:
		
		strcpy ( stType, "SS" );
		break;

          case 4:
		
		strcpy ( stType, "SD" );
		break;

          case 5:
		
		strcpy ( stType, "PT" );
		break;

          case 6:

		strcpy ( stType, "PTC" );
		break;

	  default:
		
		strcpy ( stType, "HU" );
		break;

   }

   switch ( basin ) {
	
	  case 0:
	
		sprintf	( stormId, "AL%02d%02d", stormNum, year%100 );
		break;

	  case 1:
	
		sprintf	( stormId, "EP%02d%02d", stormNum, year%100 );
		break;

	  case 2:
	
		sprintf	( stormId, "CP%02d%02d", stormNum, year%100 );
		break;

	  case 3:
	
		sprintf	( stormId, "WP%02d%02d", stormNum, year%100 );
		break;

	  default:
	
		sprintf	( stormId, "??%02d%02d", stormNum, year%100 );
		break;
   }

}


