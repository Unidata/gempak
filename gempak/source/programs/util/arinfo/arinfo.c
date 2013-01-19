/************************************************************************
 *  ARINFO 								*
 *									*
 *  Takes a McIDAS AREA file and returns requested information about the*
 *  image from the header based on what the user requests.		*
 *									*
 **									*
 * J. Cowie/COMET	 9/94	Original				*
 * S. Jacobs/NCEP	 3/97	Added computation of min/max pix vals	*
 * S. Jacobs/NCEP	12/97	Updated variable names			*
 * D.W.Plummer/NCEP	 2/03   Added area descriptors, linked to	*
 *				imgtyp.tbl, added comments, reorg code	*
 * S. Jacobs/NCEP	 3/09	Changed all longs to ints		*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"

#define AREASIZE 	64
#define AREAFORMAT 	4
#define IMGTYPTBL 	"imgtyp.tbl"

/************************************************************************
 * arinfo.c                                                             *
 *                                                                      *
 * CONTENTS:                                                            *
 * arinfo                                                               *
 *                                                                      *
 * PRIVATE FUNCTIONS:							*
 * void            minmax ( char *filnam, unsigned int *aarr )		*
 * unsigned int   swaplong(unsigned int lend)				*
 * static void     usage ( int argc, char **argv )			*
 * unsigned int   jdtogd ( unsigned int jdate )			*
 *                                                                      *
 ***********************************************************************/

void 		minmax ( char *filnam, unsigned int *aarr );
unsigned int 	swaplong(unsigned int lend);
static void 	usage ( int argc, char **argv );
unsigned int 	jdtogd ( unsigned int jdate );

/*=====================================================================*/

int main ( int argc, char **argv )
{
int 		ch, ii, i, onum=0, ier;
unsigned int 	area[AREASIZE];
char 		nav[5];
char 		order[10], imtyp[9], satname[21];
char 		*infile;
unsigned char 	*chs;
FILE 		*ifl;

FILE	*itfile;
int	nr, nn, found, indx;
char	**it, buffer[81], *cptr;
char	cloc1[32], cloc2[32], qloc1[32], qloc2[32];

char area_desc[][32] = {"Record valid if zero        ",
			"Area format - always 4      ",
			"Sensor source number        ",
			"Collection date             ",
			"Collection time             ",
			"Image line coordinate       ",
			"Image element coordinate    ",
			"not used                    ",
			"Number of lines in area     ",
			"Number of elements per line ",
			"Number of bytes per element ",
			"Line resolution             ",
			"Element resolution          ",
			"Max number of bands per line",
			"Length of data block        ",
			"McIDAS project user number  ",
			"Creation date               ",
			"Creation time               ",
			"Channel ID                  ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Memo                        ",
			"Memo                        ",
			"Memo                        ",
			"Memo                        ",
			"Memo                        ",
			"Memo                        ",
			"Memo                        ",
			"Memo                        ",
			"Area file number            ",
			"Byte offset to DATA block   ",
			"Byte offset to NAV  block   ",
			"Validity code               ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Satellite specific info     ",
			"Length of DATA block DOC    ",
			"Length of DATA block CAL    ",
			"Length of DATA block lvl map",
			"Image source type (eg. VISR)",  
			"Calibration type (eg. RAW)  ",  
			"Internal use                ",  
			"Internal use                ",  
			"Internal use                ",  
			"Internal use                ",  
			"Internal use                ",  
			"Internal use                ",  
			"Byte offset to AUX block    ",  
			"Length of AUX block         ",  
			"not used                    ",  
			"Byte offset to CAL block    ",  
			"Number of comments in AUDIT " };

/*---------------------------------------------------------------------*/

	/* 
	 *  Get command-line arguments 
	 */
	while ((ch = getopt(argc, argv, "abdijnrstm")) != EOF)
		switch (ch) {
		
		case 'a':
			order[onum++] = 'a';
			break;
		case 'b':
			order[onum++] = 'b';
			break;
		case 'd':
			order[onum++] = 'd';
			break;
		case 'i':
			order[onum++] = 'i';
			break;
		case 'j':
			order[onum++] = 'j';
			break;
		case 'n':
			order[onum++] = 'n';
			break;
		case 'r':
			order[onum++] = 'r';
			break;
		case 's':
			order[onum++] = 's';
			break;
		case 't':
			order[onum++] = 't';
			break;
		case 'm':
			order[onum++] = 'm';
			break;
		case '?':
			usage ( argc, argv );
			break;
		}

	if ( (argc-optind) == 1 )
                infile = argv[optind];
        else {
                fprintf (stderr, "No input file specified.\n");
                usage ( argc, argv );
        }

	/* 
	 *  Read in image type table into an array for later perusal.
	 */
	itfile = (FILE *)cfl_tbop ( IMGTYPTBL, "sat", &ier );

	if ( ier != 0 )  {
	    printf("Unable to open table %s.\n", IMGTYPTBL );
	    exit (1);
	}
	cfl_tbnr ( itfile, &nr, &ier );

	if ( nr <= 0 || ier != 0 )  {
	    printf("Unable to process table %s.\n", IMGTYPTBL );
	    exit (1);
	}

	it = (char **)malloc( (size_t)nr * sizeof(char *) );	
	for ( ii = 0; ii < nr; ii++ )  {
	    it[ii] = (char *)malloc( 81 * sizeof(char) );
	}
	for ( ii = 0; ii < nr; ii++ )  {
	    cfl_trln ( itfile, sizeof(buffer), buffer, &ier );
	    strcpy ( it[ii], buffer );
	}

	/* 
	 *  Open the input file, read the area block.
	 */
	if (( ifl = fopen ( infile, "r" )) == NULL ) {
		fprintf (stderr, "Could not open image file %s.\n", infile );
		return (-1);
	}
	
	fread ( area, 4, AREASIZE, ifl);
	fread ( nav, 1, 4, ifl);
	nav[4] = 0;
	fclose (ifl);

	/* 
	 *  See if this file needs byte swapping.
	 */
	if ( (int)area[1] != AREAFORMAT ) {

	    fprintf (stderr, "Possible invalid AREA format (%d)... swapping.\n",
			    area[1] );
		/*
		 *  Byte swap the items in the area dir 
		 *  except words 25-32, 52, 53, 57
		 */

		for ( i = 0 ; i < 24; i++ ) area[i] = swaplong ( area[i] );
		for ( i = 32; i < 51; i++ ) area[i] = swaplong ( area[i] );
		for ( i = 53; i < 57; i++ ) area[i] = swaplong ( area[i] );
		for ( i = 58; i < 64; i++ ) area[i] = swaplong ( area[i] );

		imbswp = 1;

		/* 
		 *  Check on the AREA format again. 
		 */
		if ( (int)area[1] != AREAFORMAT ) {
			fprintf (stderr, "Invalid AREA format (%d)."
			    " This program works only with AREA format %d.\n",
			    area[1],AREAFORMAT);
			return (-1);
		}
		printf("(Area header info has been swapped.)\n");

	}

	imsorc = (int)area [  2 ];
	imtype = (int)area [ 18 ];

	/* 
	 *  Loop over the requested output options 
	 */
	for ( i = 0; i < onum; i++ ) {
		switch (order[i]) {
		
		case 'a':		/* Dump all information		*/

		        for ( ii = 0; ii < 24; ii++ ) 
			    printf("Word %2d ( %s ):  %d\n", 
				ii+1, area_desc[ii], area[ii]);

		        chs = (unsigned char *)&area[24];
   			printf("Words 25-32: ");
   			for ( i = 0; i < 32; i++ ) 
			    printf("%c",*chs++); printf("\n");

        		for ( ii = 32; ii < 51; ii++ ) 
			    printf("Word %d ( %s ):  %d\n", 
				ii+1, area_desc[ii], area[ii]);

		        chs = (unsigned char *)&area[51];
        		printf("Words 52-53: ");
   			for ( i = 0; i < 8; i++ ) 
			    printf("%c",*chs++); printf("\n");

        		for ( ii = 53; ii < 57; ii++ ) 
			    printf("Word %d ( %s ):  %d\n", 
				ii+1, area_desc[ii], area[ii]);

        		for ( ii = 57; ii < 64; ii++ ) 
			    printf("Word %d ( %s ):  %d\n", 
				ii+1, area_desc[ii], area[ii]);

        		printf ("Navigation type: %s\n", nav);
			minmax ( infile, area );
			break;

 		case 'b':		/* Band Number			*/

			printf ("Band Number = %d\n",area[18]);
			break;

 		case 'd':	/* Date in Gregorian YYMMDD format	*/
		case 'j':	/* Date in Julian     YYDDD format	*/
		         	/* In both cases, YY is number of	*/
		         	/* years since 1900.			*/

			printf ( "Gregorian Date - %d\n",jdtogd(area[3]));
			printf ( "Julian    Date - %d\n",area[3]);
			break;

		case 'i':		/* Image Type			*/

			strcpy ( imtyp, "unknown" );
			found = 0; nn = 0;
			sprintf(cloc1,"%d",area[18]);
			sprintf(cloc2,"%d",area[ 2]);
			while ( nn < nr && found == 0 )  {
			    /*  6-character imgtyp number		*/
			    cptr = &(it[nn][51]);
			    while ( *cptr == ' ' )  cptr++;
			    strncpy(qloc1,cptr,7);qloc1[7]='\0';
			    indx = 0;
			    while ( indx < 7 )  {
				if ( qloc1[indx] == ' ' ) qloc1[indx] = '\0';
				indx++;
			    }
			    /*  6-character satellite ID number		*/
			    cptr = &(it[nn][45]);
			    while ( *cptr == ' ' )  cptr++;
			    strncpy(qloc2,cptr,7);qloc2[7]='\0';
			    indx = 0;
			    while ( indx < 7 )  {
				if ( qloc2[indx] == ' ' ) qloc2[indx] = '\0';
				indx++;
			    }
			    if ( strcmp(cloc1,qloc1) == 0 &&
				 strcmp(cloc2,qloc2) == 0 )  {
				found = 1;
			    }
			    nn++;
			}
			if ( found == 1 )  {
			    strncpy(imtyp,&(it[nn][21]),8);
			    imtyp[8] = '\0';
			}
			printf ( "Image Type = %s\n",imtyp );
			break;

 		case 'n':		/* Navigation Type		*/

			printf ( "Navigation Type = %s\n",nav);
			break;

 		case 'r':		/* Resolution of Image		*/

			printf ( "Resolution of Image = %d\n",area[11]);
			break;

 		case 's':		/* Satellite Name		*/

 			strcpy ( satname, "unknown" );
			found = 0; nn = 0;
			sprintf(cloc2,"%d",area[ 2]);
			while ( nn < nr && found == 0 )  {
			    /*  6-character satellite ID number		*/
			    cptr = &(it[nn][45]);
			    while ( *cptr == ' ' )  cptr++;
			    strncpy(qloc2,cptr,7);qloc2[7]='\0';
			    indx = 0;
			    while ( indx < 7 )  {
				if ( qloc2[indx] == ' ' ) qloc2[indx] = '\0';
				indx++;
			    }
			    if ( strcmp(cloc2,qloc2) == 0 )  {
				found = 1;
			    }
			    nn++;
			}
			if ( found == 1 )  {
			    strncpy(satname,&(it[nn][0]),20);
			    satname[20] = '\0';
			}
			printf ( "Satellite Name is %s\n",satname);
			break;

 		case 't':		/* Time in HHMM format		*/

			printf ( "Time = %04d\n",area[4]/100);
			break;

 		case 'm':		/* Min/max pixel values		*/

			minmax ( infile, area );
			break;
 		}
 	}
 	
	printf ("\n");

	free ( imgData );

	return (0);
}

/*=====================================================================*/

void minmax ( char *filnam, unsigned int *aarr )
/************************************************************************
 * minmax                                                               *
 *                                                                      *
 * Read the image data and find the minimum and maximum pixel values.	*
 *                                                                      *
 * minmax ( filnam, aarr )						*
 *                                                                      *
 * Input parameters:                                                    *
 *  filnam	*char		Image filename				*
 *  aarr	unsigned int	Area data				*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      2/03                                           *
 * D.W.Plummer/NCEP      4/03	Add histogram computations		*
 ***********************************************************************/
{
int		ii, k, isGVARRAW, many, ier;
float		*tmpk, *fpix;
unsigned int	minpx, maxpx, pix, *ppix, minbr, maxbr;
unsigned char	*dptr;
size_t		imgsiz;
unsigned int	hgram_inc=4, hgram_size;
unsigned int	*hgram;
int	indx, total;
float	fact;
double	hgram_base;
/*---------------------------------------------------------------------*/

	minpx =  UINT_MAX;
	maxpx = (unsigned int)0;
	minbr =  UINT_MAX;
	maxbr = (unsigned int)0;

        imnlin = (int)aarr [  8 ];
        imnpix = (int)aarr [  9 ];
        imdpth = (int)aarr [ 10 ];
        rmxres = (float)aarr [ 11 ];
        rmyres = (float)aarr [ 12 ];
        imnchl = (int)aarr [ 13 ];
        imprsz = (int)aarr [ 14 ];
        imdcsz = (int)aarr [ 48 ];
        imclsz = (int)aarr [ 49 ];
        imlvsz = (int)aarr [ 50 ];
        imvald = (int)aarr [ 35 ];
	imdoff = (int)aarr [ 33 ];
	strncpy ( cmstyp, (char *)&(aarr [ 51 ]), 4);
	cmstyp[4] = '\0';
	strncpy ( cmcalb, (char *)&(aarr [ 52 ]), 4);
	cmcalb[4] = '\0';

	isGVARRAW = G_FALSE;
	if ( strstr ( cmstyp, "GVAR" ) != (char *)0  &&
	     strstr ( cmcalb,  "RAW" ) != (char *)0 )  isGVARRAW = G_TRUE;

	imldat = imnlin * ( imnpix * imdpth * imnchl + imprsz );

	imgsiz = (size_t)(imnlin * imnpix * imdpth);
	imgData = (unsigned char *) calloc ( imgsiz, sizeof(unsigned char) );
	crarea ( filnam, &ier );

	if  ( imdpth == 2 && isGVARRAW == G_TRUE )  {
	    hgram_base = pow((double)2,(double)10);
	}
	else  {
	    hgram_base = pow((double)2,(double)(imdpth*8));
	}
	hgram_size = (unsigned int)hgram_base / hgram_inc;
	hgram = (unsigned int *)malloc(hgram_size * sizeof(unsigned int));
	for ( ii = 0; (size_t)ii < hgram_size; ii++ )  hgram[ii] = 0;

	dptr = &(imgData[0]);
	if  ( imdpth == 2 && isGVARRAW == G_TRUE )  {
	    fact = (float)(pow((double)2,(double)10) / (double)(hgram_size));
	    ppix = (unsigned int *)
		 ( malloc( (size_t)(imnlin * imnpix) * sizeof(unsigned int) ) );
	    tmpk = (float *)(malloc((size_t)(imnlin * imnpix) * sizeof(float) ));
	    fpix = (float *)(malloc((size_t)(imnlin * imnpix) * sizeof(float) ));
	    for ( ii = 0; ii < imnlin*imnpix; ii++ ) {
		ppix[ii] = 0;
		for ( k = 0; k < imdpth; k++ ) {
		    ppix[ii] = ppix[ii] << 8;
		    ppix[ii] += (unsigned int)*dptr;
		    dptr++;
		}
		ppix[ii] = ppix[ii] >> 5;
	        minpx = G_MIN ( minpx, ppix[ii] );
	        maxpx = G_MAX ( maxpx, ppix[ii] );
		indx = (int)((float)ppix[ii] / fact);
		hgram[indx]++;
	    }
	    many = imnlin * imnpix;
	    im_gvtota ( &many, ppix, tmpk, &ier );
	    im_ttob ( &many, tmpk, fpix, &ier );
	    for ( ii = 0; ii < imnlin*imnpix; ii++ ) {
		if ( ERMISS(fpix[ii]) )  fpix[ii] = 0.0F;
		ppix[ii] = (unsigned int)G_NINT ( fpix[ii] );
		minbr = G_MIN ( minbr, ppix[ii] );
		maxbr = G_MAX ( maxbr, ppix[ii] );
	    }
	}
	else  {
	  fact = (float)(pow((double)2,(double)(imdpth*8)) / (double)hgram_size);
	  for ( ii = 0; ii < imnlin*imnpix; ii++ ) {
	    if  ( imdpth > 1 )  {
		pix = 0;
		for ( k = 0; k < imdpth; k++ ) {
		    pix = pix << 8;
		    pix += (unsigned int)*dptr;
		    dptr++;
		}
		minpx = G_MIN ( minpx, pix );
		maxpx = G_MAX ( maxpx, pix );
	    }
	    else {
		minpx = G_MIN ( minpx, (unsigned int)imgData[ii] );
		maxpx = G_MAX ( maxpx, (unsigned int)imgData[ii] );
		dptr++;
		indx = (int)((float)imgData[ii] / fact);
		hgram[indx]++;
	    }
	  }
	}

	printf ( "Minimum Pixel Value = %d\n", (int)minpx );
	printf ( "Maximum Pixel Value = %d\n", (int)maxpx );
	if ( isGVARRAW == G_TRUE )  {
	    printf ( "Minimum Brightness Value = %d\n", (int)minbr );
	    printf ( "Maximum Brightness Value = %d\n", (int)maxbr );
	}

	/*
	 *  Print out histogram results.
	 */
	printf("HISTOGRAM --\n");
	total = 0;
	if  ( imdpth == 2 && isGVARRAW == G_TRUE )
	  fact = (float)((double)hgram_size/pow((double)2,(double)10));
	else 
	  fact = (float)((double)hgram_size/pow((double)2,(double)(imdpth*8)));
	for ( ii = 0; ii < (int)hgram_size; ii++ )  {
	    printf("%3d - %5d-%5d - %8d\n", 
		ii, (int)((float)ii/fact), (int)(((float)ii+1.0F)/fact-1.0F), hgram[ii] );
	    total += (int)hgram[ii];
	}
	printf("Total number of pixels -- %d\n", total );

}

/*=====================================================================*/
/*
 * swaplong ()
 *
 * Convert from little endian to big endian four byte object
 * or vice-versa
 */

unsigned int swaplong( unsigned int lend )
/************************************************************************
 * swaplong                                                             *
 *                                                                      *
 * Convert from little endian to big endian four byte object or 	*
 * vice-versa.								*
 *                                                                      *
 * swaplong( unsigned int lend )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  lend	unsigned int	word					*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Function return:							*
 *  swaplong	unsigned int	Swapped word				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      2/03                                           *
 ***********************************************************************/
{
	unsigned int bend = 0;
	unsigned char *lp, *bp;

	lp = ((unsigned char *)&lend) + 3;
	bp = (unsigned char *) &bend;

	*bp++ = *lp--;
	*bp++ = *lp--;
	*bp++ = *lp--;
	*bp = *lp;

	return(bend);
}

/*=====================================================================*/

static void usage ( int argc, char **argv )
/************************************************************************
 * usage                                                                *
 *                                                                      *
 * This function provides documentation to the user and then exits.	*
 *                                                                      *
 * usage ( int argc, char **argv )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  argc	int	Number of command line arguments		*
 *  argv	**char	Array of command line arguments			*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      2/03                                           *
 ***********************************************************************/
{
int	pagflg, ier;
char	errgrp[8];
/*---------------------------------------------------------------------*/
    if ( argc != 3 )  {
        pagflg = G_FALSE;
        strcpy ( errgrp, "ARINFO" );
        ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
        exit (0);
    }

}

/*=====================================================================*/
 
unsigned int jdtogd ( unsigned int jdate )
/************************************************************************
 * jdtogd                                                               *
 *                                                                      *
 * Convert julian date (YYDDD) to gregorian (YYMMDD)			*
 *                                                                      *
 * jdtogd ( jdate )							*
 *                                                                      *
 * Input parameters:                                                    *
 *  jdate	unsigned int	Julian date				*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Function return:							*
 *  jdtogd	unsigned int	Gregorian date				*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      2/03                                           *
 ***********************************************************************/
{
unsigned int ndpm[12] = { 31, 28, 31, 30, 31, 30,
			  31, 31, 30, 31, 30, 31 };
unsigned int year, month, day, jday, ii, total=0; 
/*---------------------------------------------------------------------*/

	year = (unsigned int)(jdate / 1000);
	jday = (unsigned int)(jdate - ((unsigned int)year * 1000));

	if ( year % 4 == 0 ) ndpm[1] = 29;
	for ( ii = 0; ii < 12; ii++ )  {
		if ( total + ndpm[ii] >= jday )  {
			month = ii+1;
			day = jday - total;
			break;
		}
		total = total + ndpm[ii];
	}
	
	return ((unsigned int)(year*10000+month*100+day));

}
