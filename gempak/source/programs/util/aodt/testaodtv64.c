#include <geminc.h>
#include <gemprm.h>
#include "AODT/v64/odtapi.h" 

#define	AODT_TRUE	1
#define AODT_FALSE	0

void aodtv64_exit (char *errormsg);

int main (void)
/************************************************************************
 * TESTAODTV64								*
 *									*
 * This program tests the AODTV64 library functions.			*
 *									*
 **									*
 * Log:									*
 * M. Li/SAIC		12/05	Modified from testaodtv63.c		*
 ***********************************************************************/
{
    int         init, cont, one, mode, imark, imkhw, imkwid, numsub;
    int         s_oland, s_osearch,g_oland, g_osearch,s_classf,g_classf;
    int         s_posf, s_domain, s_topof, s_irdate, s_irtime, s_irsat;
    int         g_posf, g_domain, g_topof, g_irdate, g_irtime, g_sat;
    int         iaodt, s_neweye, s_newcloud, s_oldeye, s_oldcloud;
    int         g_neweye, g_newcloud, g_oldeye, g_oldcloud, ldelete; 
    int         indx, n_records, h_deleted, h_modified, h_records;
    int         time_arr [5], jyear, jday, ier, istat, iret, recnumber;
    int         radius, irad, ii, jj, numx, numy, time1, time2, itype;
    float	szmark, s_class, g_class, fii, fjj, ifl;
    float       s_lat, s_lon, g_lat, g_lon, s_sstvalue, g_sstvalue;
    float       *ftmps, *flats, *flons, s_lon2, dx, dy;
    float       **temps, **lats, **lons;
    char 	select[8], s_dattim[14], historyfile[128];
    char	imagefile [128], bulletin[5000], date1[20], date2[20];
    char        latlon[32], device[32], clrbar[80], srcID[512];
    char        retmsg[10000]="\0", infomsg[5000]="\0", listing[5000]="\0";
    char	comment[128], g_csat[20];

    struct odtdata *historyrec;
/*---------------------------------------------------------------------*/
    cont   = AODT_FALSE;
    one    = 1;
    iaodt  = 0;
    mode   = 1;
    imark  = 11;
    imkhw  = 1;
    imkwid = 2;
    szmark = 3.0F;

    while ( cont == AODT_FALSE ) {
        printf ( "\n\n" );
        printf ( "  91 = AODT_Initialize        \n" );
        printf ( "  99 = AODT_FreeMemory\n" );
        printf ( "\n" );
        printf ( " Intensity Analysis Functions: \n" );
        printf ( "\n" );
        printf ( "   1 = AODT_SetMiscOptions     	 2 = AODT_GetMiscOptions\n" );
        printf ( "   3 = AODT_SetStartStr        	 4 = AODT_GetStartStr\n" );
        printf ( "   5 = AODT_SetHistoryFile     	 6 = AODT_GetHistoryFile\n" );
        printf ( "   7 = AODT_SetIRImageInfo     	 8 = AODT_GetIRImageInfo\n" );
        printf ( "   9 = AODT_SetLocation       	10 = AODT_GetLocation\n" );
        printf ( "  11 = AODT_SetDomain         	12 = AODT_GetDomain\n" );
        printf ( "  13 = AODT_SetSSTValue       	14 = AODT_GetSSTValue\n" );
        printf ( "  15 = AODT_SetTopoValue      	16 = AODT_GetTopoValue\n" );
        printf ( "  17 = AODT_LoadIRImage\n" );
        printf ( "  18 = AODT_SetEyeCloudTemp   	19 = AODT_SceneType\n" );
        printf ( "  20 = AODT_GetSceneType      	21 = AODT_SetSceneTypes\n" );
        printf ( "  22 = AODT_Intensity         	23 = AODT_BulletinOutput\n" );
        printf ( "\n" );
        printf ( " History File Management Functions: \n" );
        printf ( "\n" );
        printf ( "  30 = AODT_SetDateTime\n" );
        printf ( "  31 = AODT_HistoryRecordInsert  	32 = AODT_HistoryWriteFile\n" );
        printf ( "  33 = AODT_HistoryGetNextRec		34 = AODT_HistoryDeleteRec\n" );
        printf ( "  35 = AODT_HistoryListFmt		36 = AODT_HistoryBullFmt\n" );
        printf ( "  37 = AODT_HistoryAddcomment\n" );
        printf ( "\n" );
        printf ( "Select a subroutine number or type EXIT: " );
        scanf ( " %s", select );
        printf ( "\n" );
        switch ( select[0] ) {
                case 'e':
                case 'E':
                        cont = AODT_TRUE;
                        iaodt = 0;
                default:
                        numsub = atoi ( select );
                        break;
        }
/*---------------------------------------------------------------------*/
            if ( numsub == 91 ) {

                init=aodtv64_initialize ( );

                printf ( "\nAODT_Initialize iaodt =      %d\n\n", init );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 99 ) {

                aodtv64_freememory();

                printf ( "\nAODT_FreeMemory iaodt =            %d\n\n", 0 );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 1 ) {

                printf("Enter operation over land FLAG:\n");
                printf("        0 = don't allow\n");
                printf("        1 = allow\n");
                scanf ( " %d", &s_oland );
                printf("Enter log spiral search FLAG:\n");
                printf("        0 = no search (OFF)\n");
                printf("        1 = search    (ON )\n");
                scanf ( " %d", &s_osearch );

                iaodt=aodtv64_setmiscoptions ( s_oland, s_osearch );

                printf ( "\nAODT_SetMiscOptions iaodt =  %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 2 ) {

                iaodt=aodtv64_getmiscoptions ( &g_oland, &g_osearch );

                printf("Operation over land FLAG =      %d\n", g_oland);
                printf("Log spiral search FLAG   =      %d\n", g_osearch);

                printf ( "\nAODT_GetMiscOptions iaodt =  %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 3 ) {

                printf("Enter initial classification FLAG:\n");
                printf("        0 = value not set 	(OFF)\n");
                printf("        1 = value set 		(ON)\n");
                scanf ( " %d", &s_classf );
                printf("Enter initial classification value:\n");
                scanf ( " %f", &s_class );

                iaodt=aodtv64_setstartstr ( s_classf, s_class );

                printf ( "\nAODT_SetStartStr iaodt =  %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 4 ) {

                iaodt=aodtv64_getstartstr ( &g_classf, &g_class );

                printf("Initial classification FLAG  =      %d\n", g_classf);
                printf("Initial classification value =      %f\n", g_class);

                printf ( "\nAODT_GetMiscOptions iaodt =  %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 5 ) {

                printf ( "Enter history filename: ");
                scanf ( " %s", historyfile );

                iaodt=aodtv64_sethistoryfile ( historyfile );

                printf ( "\nAODT_SetHistoryFile iaodt =  %d\n\n", iaodt );

                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, historyfile, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 6 ) {

                iaodt=aodtv64_gethistoryfile ( historyfile );

                printf("History file is:                %s\n", historyfile );
                printf ( "\nAODT_GetHistoryFile iaodt =  %d\n\n", iaodt );

                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 7 ) {

                printf("Enter IR image date and time in GEMPAK format: \n");
                printf("(YYYYMMDD/HHMM) \n");
                scanf ( " %s", s_dattim );
                ti_ctoi ( s_dattim, time_arr, &ier, strlen(s_dattim));
                ti_itoj ( time_arr, &jyear, &jday, &ier );

                s_irdate = time_arr[0] * 1000 + jday;
                s_irtime = time_arr[3] * 10000 + time_arr[4] * 100;

		s_irsat = -1;
                iaodt = aodtv64_setIRimageinfo ( s_irdate, s_irtime, s_irsat );

                printf ( "\nAODT_SetIRImageInfo iaodt =  %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 8 ) {

                iaodt = aodtv64_getIRimageinfo ( &g_irdate, &g_irtime, &g_sat, g_csat );

                printf("IR Image Information in AODT library format: \n");
                printf("        Date =  %d \n", g_irdate);
                printf("        Time =  %d \n", g_irtime);

                printf ( "\nAODT_GetIRImageInfo iaodt =  %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 9 ) {

                printf("To load image, Enter the image file name\n");
                scanf ( " %s", imagefile );

                ginitp ( &mode, &istat, &iret );
                if ( iret != 0 )  {
                   printf("GINITP failed...iret=%d\n", iret );
                   exit (0);
                }
                im_simg ( "sat", imagefile, &ier, strlen("sat"), strlen(imagefile) );
                gsicmn ( &ier );
                sprintf( device, "gn|ODT-%s", imagefile);
                gg_sdev ( device, &iret, strlen(device) );
                if ( iret != 0 )  {
                   printf("GSDEVA failed...iret=%d\n", iret );
                   exit (0);
                }

                im_drop ( &ier );
                strcpy (clrbar, "1/V/LL/0;.05/.90");
                im_cbar ( clrbar, &ier, strlen(clrbar) );

                strcpy ( latlon, "32////10;10" );
                gg_ltln ( latlon, &ier, strlen(latlon) );
                geplot ( &ier );


                printf("Enter Coordinates of storm position\n");
                printf("        LATITUDE:\n");
                scanf ( " %f", &s_lat );
                printf("        LONGITUDE:\n");
                scanf ( " %f", &s_lon );

                gsmrkr ( &imark, &imkhw, &szmark, &imkwid, &ier );
                gmark ( sys_M, &one, &s_lat, &s_lon, &ier, strlen(sys_M) );
                geplot ( &ier );

                printf("Enter location positioning method FLAG:\n");
                printf("        1 = forecast interpolation\n");
                printf("        2 = laplacian technique\n");
                printf("        3 = warm spot location\n");
                printf("        4 = linear interpolation\n");
                scanf ( " %d", &s_posf );

                gendp ( &one, &ier);

                s_lon *= -1.0F;

                iaodt=aodtv64_setlocation ( s_lat, s_lon, s_posf );
                printf ( "\nAODT_SetLocation iaodt =     %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 10 ) {

                iaodt=aodtv64_getlocation ( &g_lat, &g_lon, &g_posf );

                printf("AODT Coordinates of storm position\n");
                printf("Storm position LATITUDE          =      %f \n", g_lat);
                printf("Storm position LONGITUDE         =      %f \n", g_lon);
                printf("Location positioning method FLAG =      %d \n", g_posf);

                printf ( "\nAODT_GetLocation iaodt =     %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 11 ) {

                printf("Enter domain FLAG: \n");
                printf("        0 = automatic determination\n");
                printf("        1 = manual selection (Atlantic)\n");
                printf("        2 = manual selection (Pacific)\n");
                scanf ( " %d", &s_domain );

                iaodt = aodtv64_setdomain ( s_domain );

                printf ( "\nAODT_SetDomain iaodt =       %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 12 ) {

                iaodt=aodtv64_getdomain ( &g_domain );

                printf("Domain FLAG =  %d \n", g_domain);

                printf ( "\nAODT_GetDomain iaodt =       %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 13 ) {

                printf("Enter SST value at storm center location \n");
                scanf ( " %f", &s_sstvalue );

                iaodt = aodtv64_setsstvalue ( s_sstvalue );

                printf ( "\nAODT_SetSSTValue iaodt =     %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 14 ) {

                iaodt=aodtv64_getsstvalue ( &g_sstvalue );

                printf("SST value at storm center location =  %f \n", g_sstvalue );

                printf ( "\nAODT_GetSSTValue iaodt =     %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 15 ) {

                printf("Enter topography FLAG: \n");
                printf("        1 = land \n");
                printf("        2 = ocean \n");
                scanf ( " %d", &s_topof );

                iaodt = aodtv64_settopovalue ( s_topof );

                printf ( "\nAODT_SetTopoValue iaodt =    %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 16 ) {

                iaodt=aodtv64_gettopovalue ( &g_topof );

                printf("Topography FLAG =  %d \n", g_topof);

                printf ( "\nAODT_GetTopoValue iaodt =    %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 17 ) {
               /*
                *   Convert center (lat,lon) into device coordinates.
                */
                s_lon2 = -1.0 * s_lon;
                gtrans ( sys_M, sys_D, &one, &s_lat, &s_lon2, &dx, &dy, &ier,
                         strlen(sys_M), strlen(sys_D) );
                dx = (float)G_NINT ( dx );
                dy = (float)G_NINT ( dy );
                radius = 190;
                irad = radius/4 + 5;
                numx = numy = irad*2 + 1;
                ftmps = (float *)malloc( (size_t)((numx)*(numy))* sizeof(float) );
                flats = (float *)malloc( (size_t)((numx)*(numy)) * sizeof(float) );
                flons = (float *)malloc( (size_t)((numx)*(numy)) * sizeof(float) );
               /*
                *   Retrieve temperatures from image.
                */
                im_gtmp ( imagefile, "dset", sys_D, &dx, &dy, &irad,
                          &numx, &numy, ftmps, flats, flons, &iret,
                          strlen(imagefile), strlen("dset"), strlen(sys_D) );
                if ( iret != 0 )  {
                   er_wmsg  ( "IM", &iret, " ", &ier, strlen("IM"), strlen(" ") );
                   exit (0);
                }
               /*
                *   Convert device (i,j) to earth (lat,lon).
                *   Also, ODT calculations expect longitude to be west positive.
                */
                for ( fjj = dy-(float)irad; fjj <= dy+(float)irad; fjj++ )  {
                    jj = (int)(fjj - (dy-(float)irad));
                    for ( fii = dx-(float)irad; fii <= dx+(float)irad; fii++ )  {
                        ii = (int)(fii - (dx-(float)irad));
                        indx = jj*(numx)+ii;
                        gtrans( sys_D, sys_M, &one, &fii, &fjj,
                                &(flats[indx]), &(flons[indx]), &ier,
                                strlen(sys_D), strlen(sys_M) );
                    }
                }

		ifl = (size_t)sizeof(float);
	        temps = (float **)calloc((size_t)numy, ifl);
         	lats  = (float **)calloc((size_t)numy, ifl);
         	lons  = (float **)calloc((size_t)numy, ifl);

         	for ( jj = 0; jj < numy; jj++ ) {
            	    temps[jj] = (float *)calloc((size_t)numx, ifl);
            	    lats[jj]  = (float *)calloc((size_t)numx, ifl);
            	    lons[jj]  = (float *)calloc((size_t)numx, ifl);
         	}

                for ( jj = 0; jj < (numy); jj++ )  {
                    for ( ii = 0; ii < (numx); ii++ )  {
                        indx = jj*(numy)+ii;
                        temps[jj][ii] = ftmps[indx];
                        lats [jj][ii] = flats[indx];
                        lons [jj][ii] = flons[indx];
                    }
                }
               /*
                *   Free up all malloc'd memory.
                */
                free ( flons );
                free ( flats );
                free ( ftmps );

                iaodt = aodtv64_loadIRimage ( temps, lats, lons, numx, numy );

                printf ( "\nAODT_LoadIRImage iaodt =     %d\n\n", iaodt );
		
		/*
         	 * Free memory.
         	 */
         	for ( jj = 0; jj < numy; jj++ ) {
             	    free ( temps[jj] );
             	    free ( lats[jj]  );
             	    free ( lons[jj]  );  
         	}
         	free( lons  );
         	free( lats  );
         	free( temps );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 18 ) {

                iaodt=aodtv64_seteyecloudtemp ( );

                printf ( "\nAODT_seteyecloudtemp iaodt =	%d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 19 ) {

                iaodt=aodtv64_scenetype ( );

                printf ( "\nAODT_SceneType iaodt =       %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 20 ) {

                iaodt=aodtv64_getscenetypes ( &g_neweye, &g_newcloud,
                                             &g_oldeye, &g_oldcloud );

                printf("NEW eye scene type:     %d \n", g_neweye);
                printf("NEW cloud scene type:   %d \n", g_newcloud);
                printf("OLD eye scene type:     %d \n", g_oldeye);
                printf("OLD cloud scene type:   %d \n", g_oldcloud);

                printf ( "\nAODT_GetSceneTypes iaodt =   %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 21 ) {
                printf("Enter NEW eye scene type:\n");
                printf("        0 = CLEAR\n");
                printf("        1 = PINHOLE\n");
                printf("        2 = LARGE CLEAR\n");
                printf("        3 = LARGE RAGGED\n");
                printf("        4 = RAGGED\n");
                printf("        5 = OBSCURED\n");
                printf("        6 = NONE\n");
                scanf ( " %d", &s_neweye );
                if ( s_neweye == 7 ) {
                   printf("Enter NEW cloud scene type:\n");
                   printf("     0 = UNIFORM CDO\n");
                   printf("     1 = EMBEDDED CENTER\n");
                   printf("     2 = IRREGULAR CDO\n");
                   printf("     3 = CURVED BAND\n");
                   printf("     4 = SHEAR\n");
                   scanf ( " %d", &s_newcloud );
                }
                else {
                   s_newcloud = 1;
                }
                printf("Enter OLD eye scene type:\n");
                printf("        0 = CLEAR\n");
                printf("        1 = PINHOLE\n");
                printf("        2 = LARGE CLEAR\n");
                printf("        3 = LARGE RAGGED\n");
                printf("        4 = RAGGED\n");
                printf("        5 = OBSCURED\n");
                printf("        6 = NONE\n");
                scanf ( " %d", &s_oldeye );
                if ( s_oldeye == 7 ) {
                   printf("Enter OLD cloud scene type:\n");
                   printf("     0 = UNIFORM CDO\n");
                   printf("     1 = EMBEDDED CENTER\n");
                   printf("     2 = IRREGULAR CDO\n");
                   printf("     3 = CURVED BAND\n");
                   printf("     4 = SHEAR\n");
                   scanf ( " %d", &s_oldcloud );
                }
                else {
                   s_oldcloud = 1;
                }

                iaodt=aodtv64_setscenetypes (s_neweye, s_newcloud,
                                            s_oldeye, s_oldcloud );

                printf ( "\nAODT_SetSceneTypes iaodt =   %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 22 ) {

                iaodt=aodtv64_intensity ( );

                printf ( "\nAODT_Intensity iaodt =   %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 23 ) {

                iaodt=aodtv64_bulletinoutput ( bulletin );

                printf("Output Bulletin:\n %s \n", bulletin);

                printf ( "\nAODT_BulletinOutput iaodt =  %d\n\n", iaodt );
                aodtv64_qmessage (500, 0, bulletin, retmsg);
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 30 ) {
                printf("Enter 1st Record's DATE (DDMMMYYYY)\n");
                scanf ( " %s", date1 );
                printf("Enter 1st Record's TIME \n");
                scanf ( " %s", select );
                cst_numb ( select, &time1, &ier );
                printf("Enter 2nd Record's DATE (DDMMMYYYY)\n");
                scanf ( " %s", date2 );
                printf("Enter 2nd Record's TIME \n");
                scanf ( " %s", select );
                cst_numb ( select, &time2, &ier );
                printf("Enter List/Delete FLAG:\n");
                printf("        0 = LIST   records\n");
                printf("        1 = DELETE records \n");
                scanf ( " %d", &ldelete );
 
                iaodt=aodtv64_setdatetime ( time1, time2, date1, date2, ldelete );
                printf ( "\nAODT_SetDateTime iaodt =        %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 31 ) {

                iaodt=aodtv64_historyrecordinsert ( &h_modified, &h_records );

                printf("Number of records modified:     %d \n", h_modified);
                printf("Total number of records written \n");
                printf("into history structure          %d \n", h_records);

                printf ( "\nAODT_HistoryRecordInsert iaodt =     %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, h_records-h_modified + 1, historyfile, retmsg);
                   if ( h_modified != 0 ) {
                      aodtv64_qmessage (65, h_modified, historyfile, retmsg);
                   }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 32 ) {

                iaodt=aodtv64_historywritefile ( &n_records);

                printf("Number of records written to history file: %d \n", n_records);
                printf ( "\nAODT_HistoryWriteFile iaodt =        %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, n_records, historyfile, retmsg);
                   if ( iaodt < 0 ) {
                      aodtv64_exit (retmsg);
                   }
                }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 33 ) {

                printf("Enter Record Number to Get:\n");
                printf("     0 = Get the 1st record in history data structure \n");
                printf("     1 = Get next record in history data structure\n");
                scanf ( " %d", &recnumber );

                iaodt = aodtv64_historygetnextrec ( recnumber, &historyrec);

                printf ( "\nAODT_HistoryGetNextRec iaodt =        %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, 0, NULL, retmsg);
                  if ( iaodt < 0 ) {
                     aodtv64_exit (retmsg);
                  }
                }
                aodtv64_qmessage (101, 0, NULL, retmsg);
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 34 ) {

                iaodt=aodtv64_historydeleterec ( &h_deleted, &h_modified );

                printf("Number of records deleted:      %d \n", h_deleted);
                printf("Number of records modified:     %d \n", h_modified);

                printf ( "\nAODT_HistoryDeleteRec iaodt =     %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, h_deleted, historyfile, retmsg);
                   if ( iaodt < 0 ) {
                      aodtv64_exit (retmsg);
                   }
                   if ( h_modified > 0 ) {
                      aodtv64_qmessage (65, h_modified, historyfile, retmsg);
                   }
                }
                aodtv64_qmessage (103, 0, NULL, retmsg);
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 35 ) {

                printf("Enter Record Number to List/Delete:\n");
                printf("       = 0  History File Header \n");
                printf("       > 0  Actual Records \n");
                scanf ( " %d", &recnumber );

		itype = -1;
		srcID[0] = CHNULL;
                if ( recnumber == 0 ) {
                   iaodt = aodtv64_historylistfmt ( 0, itype, srcID, listing);
                   printf("Listing:\n%s\n", listing);
                }
                else {
                 while ( historyrec != NULL ) {
                   iaodt = aodtv64_historylistfmt ( historyrec, itype, srcID, listing);
                   printf("%s", listing);
                   iaodt = aodtv64_historygetnextrec ( 1, &historyrec);
                 }
                 free (historyrec);
                }

                printf ( "\nAODT_HistoryListFmt iaodt =        %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 36 ) {

               iaodt = aodtv64_historybullfmt (historyrec, listing);
               printf("Listing:\n%s\n", listing);

               printf ( "\nAODT_HistoryBullFmt iaodt =        %d\n\n", iaodt );
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 37 ) {

		printf ("Enter comment to add: \n");
	 	scanf ( "%s", comment );
                iaodt=aodtv64_historyaddcomment( comment, &h_modified );

                printf("Number of records modified:     %d \n", h_modified);

                printf ( "\nAODT_HistoryAddcomment iaodt =     %d\n\n", iaodt );
                if ( iaodt != 0 ) {
                   aodtv64_qmessage (iaodt, h_modified, historyfile, retmsg);
                   if ( iaodt < 0 ) {
                      aodtv64_exit (retmsg);
                   } 
                }
            }
/*---------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
        }
    iaodt=aodtv64_qdiagnostics ( infomsg );
    printf ("\nDiagnostic Messages:\n%s\n", infomsg);

    return 0;
}

void aodtv64_exit (char *errormsg)
{
     char       infomsg[5000] = "\0";

     aodtv64_qdiagnostics ( infomsg );
     printf ("%s \n", infomsg);
     printf ("AODT Error: %s \n\n", errormsg);
    /*
     * free any allocated memory
     */
     aodtv64_freememory();

     return; 
}
