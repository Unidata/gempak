#include "shapefil.h"
#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "pgprm.h"
#include <time.h>  

#define ERROR (-1)

/* 
 * Object Type codes used in main(): 
 */
#define OBJECTTYPE_NONE		0
#define OBJECTTYPE_POINT	1
#define OBJECTTYPE_LINE		2
#define OBJECTTYPE_POLYGON	3

/* 
 * Error codes for exit() routine: 
 */
#define	ERR_USAGE	1
#define ERR_TYPE	2
#define ERR_FORMAT	3
#define ERR_OBJECTTYPE	4
#define ERR_ALLOC	5
#define ERR_DBFCREATE	10
#define ERR_DBFADDFIELD	11
#define ERR_DBFOPEN	12
#define	ERR_DBFWRITEINTEGERATTRIBUTE	13
#define ERR_SHPOPEN	20

/* 
 * Maximum length for read strings,
 * if input lines with more characters appear,
 * errors are likely to occur 
 */
#define STR_BUFFER_SIZE		300

/*
 *  Global variables
 */
int	_tce = G_FALSE, _tcb = G_FALSE, _tctL = G_FALSE, _tctP = G_FALSE;

/************************************************************************
 * tpc2shp.c -- program to convert the VGF files created by gptcww      *
 *		into GIS shape files                                    *
 *			1. 3/5 cone of uncertainty                      *
 *			2. hurricane track                              *
 *			3. tropical cyclone watch warnings              *
 * 									*	
 * USAGE: tpc2shp [-uh] -o outfile -i infile.vgf	                *
 * 									*
 *	    u - detailed usage instructions				*
 *	    h - short syntax message       			        *
 *	    o - output file name					*
 *	    i - input vgf file name					*  
 *									*                      					
 *      Example: tpc2shp  -o AL122005_adv22 -i AL122005TCb.22.VGF       *				
 *									*
 * CONTENTS:                      					*
 *     Private Functions:                                               *
 *      
 * 	DateLine        - corrects the lons of the forecast track 
 * 			  if the track crosses the International Dateline
 * 	DateLineCone    - corrects the lons of the cone of uncertainty
 *    			  if the cone crosses the International Dateline
 * 	LaunchDbf	- creates and returns handle for .dbf file	*
 *	LaunchShp	- creates and returns handle for .shp file	*
 *	MakeBklAtts	- calls DBFAddField to add TCB atts to .dbf file*
 *	MakeErrAtts	- calls DBFAddField to add TCE atts to .dbf file*
 * 	MakeTrkLineAtts	- calls DBFAddField to add TCT atts to .dbf file*
 * 	MakeTrkPtsAtts	- calls DBFAddField to add TCT atts to .dbf file*
 * 	PutInfoAtts 	- calls DBFWriteStringAttribute to write TcInfo *
 *      Tcb2Shp		- writes TCB elem data to .dbf and .shp files	*
 * 	Tce2Shp		- writes TCE elem data to .dbf and .shp files	*
 *	Tct2Shp		- writes TCT elem data to .dbf and .shp files	*
 *	WrtShpLin	- calls SHPWriteObject to write out a line	*
 *	WrtShpPnt	- calls SHPWriteObject to write out a point	*
 *	WrtShpPgn	- calls SHPWriteObject to write out a polygon	*
 *	WrtShpPrj	- writes earth model to the .prj file		*
 ***********************************************************************/

DBFHandle LaunchDbf 	( const char *fname );
SHPHandle LaunchShp 	( const char *fname, int ObjectType );
typedef struct metadata METADATA;

struct metadata { 
  int vgfType;
  int numEle;
  char *date;
  char *advdate;
  float westBound;
  float eastBound;
  float northBound;
  float southBound;
  char *filename;
  char *filesize;
  char *creationDate;
  char *templateFile;
};


static void Tcb2Shp 	( VG_DBStruct *el, DBFHandle dbf, SHPHandle shp, 
			  int *shp_nmbr, METADATA *mdata, FILE *fp );
static void Tce2Shp 	( VG_DBStruct *el, DBFHandle dbf, SHPHandle shp, 
			  int *shp_nmbr, METADATA *mdata, FILE *fp );
static void Tct2Shp 	( VG_DBStruct *el, DBFHandle pdbf, SHPHandle pshp,
                          DBFHandle ldbf, SHPHandle lshp,
                          int *shp_rec_num1, int *shp_rec_num2, 
			  METADATA *mdata1,  METADATA *mdata2, FILE *fp );
static void DateLine     (  VG_DBStruct *el, int npts, double *x);	
static void DateLineCone (  VG_DBStruct *el, int npts, double *x );			  
static void PutInfoAtts	( int rec, VG_DBStruct *el, DBFHandle hDBF, FILE *fp );
static void MakeTrkLineAtts	( DBFHandle hDBF );
static void MakeTrkPtsAtts	( DBFHandle hDBF );
static void MakeErrAtts	( DBFHandle hDBF );
static void MakeBklAtts	( DBFHandle hDBF );
static void WrtShpLin	( SHPHandle hSHP, int rec, int coords, 
			  double *x, double *y);
static void WrtShpPnt	( SHPHandle hSHP, int rec, 
			  double x, double y);
static void WrtShpPgn	( SHPHandle hSHP, int rec, int coords, 
		          double *x, double *y, int nparts, int *partstarts);
static void WrtShpPrj	( char *filename);
static void getDate	( char *date);
static void WrtMetaFile	( METADATA *mdata, char *filename);
static void InitMetaObj ( METADATA *mdata, int npgn, char *date, VG_DBStruct *el);
FILE* OpenKmlFile ( char *filename, char *dataType);

/************************************************************************
 * tpc2shp                                                              *
 *                                                                      *
 * This programs reads VGF file containing TC elements and creates	*
 * output shapefile files .shp, .shx, .prj and .dbf for points  	*
 * and/or lines and/or polygons.					*
 *                                                                      *
 * Usage:                                                               *
 * 	tpc2shp [-uh] -o outfile -i infile.vgf				*
 *	    u - detailed usage instructions				*
 *	    h - short syntax message       			        *
 *	    o - output file name					*
 *	    i - input vgf file name					*
 *                                                                      *
 * main ( argc, argv )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      argc    int             number of parameters of command line    *
 *      argv    char**          parameter array of command line         *
 *                                                                      *
 * Output files:                                                   	*
 * 	.shp 			vertices of objects			*	
 *	.shx			index data to objects in .shp file	*
 * 	.dbf			attributes of objects in xBase format	*
 *	.prj			description of the earth model used	*
 *									*
 * Return parameters:                                                   *
 *      NONE                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 06/07   Created                                 *
 * m.gamazaychikov/SAIC 07/07   Clean up, split MakeTrk in two functions*
 *				for lines and points                    *
 * m.gamazaychikov/SAIC 10/07	In MakeTrkPtsAtts change LAT and LON 	*
 *				attribute field types to FTString	*
 *				In Tct2Shp print LAT and LON values 	*
 *				using 1 tenth of the degree precision 	*
 * Krautktramer/NHC	7/08	NHC modification    			*
 * m.gamazaychikov/SAIC 10/08	In MakeTrkPtsAtts added StormSrc field, *
 *                              In Tct2Shp added StormSrc to output 	*                     *
 * Krautktramer/NHC	4/09	NHC modification    	
 * Krautkramer/NHC 	4/10    Add new field - Saffir Simps	
 * Krautkramer/NHC	10/10	Corrected if () logic error in tct2shp 
 *				error caused SS# not to be calculated for 'M'
 * Krautkramer/NHC	01/11   corrected an error which occurs then the cone
 * 				of uncertainty or forecat track crosses the 
 * 				international dateline.  The changes are 
 * 				isolated to 2 new functions: DateLine and 
 * 				DateLineCone.  
 * S. Jacobs/NCEP	 3/13	Initialized kml_fptr to NULL to avoid
 * 				Seg Fault errors
 ***********************************************************************/
int main ( int argc, char **argv )

{

        /*>>>>>>>>>>>>>>>>>>>> Variable Declaration <<<<<<<<<<<<<<<<<<<<<*/
	
	int		ne, more, ier, tcto=G_FALSE;
	int		pagflg, wrtflg, curpos;
	VG_DBStruct	el;
	METADATA        mdata_ln, mdata_pt, mdata_poly, mdata_ww;
	char		asfn[32], vgfn[128], pgnfile[132];
	char		pfile[132], lfile[132], ifname[128];
        char    	wwfile[132], errgrp[8], date[40];
	char		wwkml[132], lkml[132], pgnkml[132];
	long		ifilesize;
	int		c,i,nl,np,npgn, vgfType;
	FILE		*ifptr, *kml_fptr=NULL;
	DBFHandle pDBF;		/* handle for point dBase file */
	DBFHandle lDBF;		/* handle for line dBase file */
	DBFHandle pgnDBF;	/* handle for polygon dBase file */
	SHPHandle pSHP;		/* handle for point shape files .shx and .shp*/
	SHPHandle lSHP;		/* handle for line shape files .shx and .shp */
	SHPHandle pgnSHP;	/* handle for polygon shape files .shx and .shp*/
	
	/*>>>>>>>>>>>>>>>>>>>>>>>>>> Check User Input <<<<<<<<<<<<<<<<<<<<<*/

       /*
        *  Check if number of input arguments is correct.
        */
    	if ( argc == 1 )  {
        	pagflg = G_FALSE;
       	 	strcpy ( errgrp, "TPC2SHP" );
        	ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
        	exit (0);
    	}
	
       /*
        *  Extract user input and respond appropriatly 
        */
	c=1;
	while ((i = getopt(argc, argv, "uhi:o:")) != -1) {
	   switch (i) {
             case 'u' :
	        printf("usage: tpc2shp -o outfile -i infile.vgf\n");
	        printf("reads VGF file containing TC elements \n"
		"and creates output shapefile\n"
		"files shp, shx and dbf \n"
		"for points and/or lines and/or polygons\n"
		"example:\ntpc2shp -o test -i test.vgf\n"
		"creates POINTS  files test_pts.[shp,shx,dbf] \n"
		"creates LINES   files test_lin.[shp,shx,dbf]\n"
		"creates POLYGON files test_pgn.[shp,shx,dbf]\n");
		return(0);
		break;
             case 'h' :
		printf("usage: tpc2shp [-uh] -o outfile -i infile.vgf\n");
		return(0);
		break;
             case 'o' :
		sprintf(asfn,"%s",optarg); c++;
		break;
             case 'i' :
		sprintf(vgfn,"%s",optarg); c++;
		break;
	   }
	}
	
        /*>>>>>>>>>>>>>>>>>>>>>>>>>> Open the VGF file for reading <<<<<<<<<<<<<<<<<<<<<*/
	
	/*  
	 * if user specified both input and output we are good to go
 	 */
	wrtflg = 0;
	
	/*
	 * Open the passed in vgf file as read only and get a pointer to the contents
	 * (cvg_open::libcgemlib.a function)
	 */ 
	cvg_open ( vgfn, wrtflg, &(ifptr), &ier );
	if ( ier != 0 )  {
		printf("Error opening VGF file %s\n", vgfn );
		exit (0);
	}

	/* 
	 * Create output files names 
	 */
	sprintf(pfile,"%s_pts",asfn);
	sprintf(lfile,"%s_lin",asfn);
	sprintf(pgnfile,"%s_pgn",asfn);
	sprintf(wwfile,"%s_wwlin",asfn);
	
	/* 
	 * Create output kml files names 
	 */
	sprintf(lkml,"%s_trk",asfn);
	sprintf(pgnkml,"%s_pgn",asfn);
	sprintf(wwkml,"%s_wwlin",asfn);
	
	/*
	 *  Initalize metadata filename 
	 */
	 mdata_ln.filename = lfile;
	 mdata_pt.filename = pfile;
	 mdata_poly.filename = pgnfile;
	 mdata_ww.filename = wwfile;
	 
	 /*
	 *  Initalize metadata lat/lon boundaries 
	 */
	 mdata_ww.southBound = mdata_poly.southBound = mdata_pt.southBound = mdata_ln.southBound = 90.0;
	 mdata_ww.northBound = mdata_poly.northBound =  mdata_pt.northBound = mdata_ln.northBound = -90.0;
	 mdata_ww.westBound = mdata_poly.westBound = mdata_pt.westBound = mdata_ln.westBound   = 180.0;
	 mdata_ww.eastBound = mdata_poly.eastBound = mdata_pt.eastBound = mdata_ln.eastBound   = -180.0; 
	 
	 /*
	 *  Initalize the name of the template files 
	 */
	 mdata_ln.templateFile = "tcTrackLine_template.xml";
	 mdata_pt.templateFile = "tcTrackPoint_template.xml";
	 mdata_poly.templateFile = "tcCone_template.xml";
	 mdata_ww.templateFile = "tcWatchWarn_template.xml";
	 
	 /*
	  *  Get Calendar Date
	  */ 
	 getDate(date); 
	
	/* 
	 * Determine the size of the vgf file (cfl_inqr::libgemlib.a function)
	 */
	cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );

	np = 0;
	nl = 0;
	ne = 0;
	npgn = 0;
	more = G_TRUE;
	curpos = 0;
	
	/* 
	 * Open the vgf file for reading 
	 */
	ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
	
	/*>>>>>>>>>>>> Iterate through vgf element and create dbf and empty shp files <<<<<<<<<<<<<<<<<*/
	
	while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
	
		/* 
	 	*  Read a record (VG_DBStruct - defined in vgstruct.h) specified 
		*  by curpos from the open vgf file 
	 	*/
		cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );
		if ( ier < 0 )  {
			more = G_FALSE;
		}
		
		/* 
	 	*  Check the size of the vg record is greater then 0 (specified in the vgf header)
	 	*/
		else if ( el.hdr.recsz > 0 )  {
		
			vgfType = el.hdr.vg_type;
			
			switch (el.hdr.vg_type) {
				case TCERR_ELM :
					/* 
                                         * Polygon files
					 *    1. write dbf file
					 *    2. create empty shp file
					 */
                                        _tce = G_TRUE;
        				pgnDBF = LaunchDbf(pgnfile);
        				pgnSHP = LaunchShp(pgnfile, OBJECTTYPE_POLYGON);
					break;
				case TCTRK_ELM :
					/* 
                                         * Point and line files
					 *    1. write dbf files
					 *    2. create empty shp files
					 */
                                        _tctP = G_TRUE;
        				pDBF = LaunchDbf(pfile);
                                        _tctP = G_FALSE;
                                        _tctL = G_TRUE;
        				lDBF = LaunchDbf(lfile);
                                        _tctL = G_FALSE;
        				pSHP = LaunchShp(pfile, OBJECTTYPE_POINT);
        				lSHP = LaunchShp(lfile, OBJECTTYPE_LINE);
                                        tcto = G_TRUE;
					break;
                                case TCBKL_ELM :
                                        /*
                                         * Watch/Warning Line files
					 *    1. write dbf file
					 *    2. create empty shp file
                                         */
                                        _tcb = G_TRUE;
        				lDBF = LaunchDbf(wwfile);
        				lSHP = LaunchShp(wwfile, OBJECTTYPE_LINE);
                                        break;
				default:
					break;
			}
			curpos += el.hdr.recsz;
		}
	}
	
	/* 
	 *  Free the memory associated with the vgf element and close the vgf file
	 */
        cvg_freeElPtr ( &el );
        cvg_clos ( ifptr, &ier );
	
	/*>>>>>>>>>>>> Iterate through vgf element and populate the shape file <<<<<<<<<<<<<<<<<*/
	
	/* 
	 * Read elements and call the specific functions 
         * to convert to shapefile format  
	 */
	cvg_open ( vgfn, wrtflg, &(ifptr), &ier );
	cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );

	np = 0;
	nl = 0;
	ne = 0;
	npgn = 0;
	more = G_TRUE;
	curpos = 0;
	ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
	
	while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
	
		/* 
	 	*  Read a record (VG_DBStruct - defined in vgstruct.h) specified 
		*  by curpos from the open vgf file 
	 	*/
		cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );
		if ( ier < 0 )  {
			more = G_FALSE;
		}
		
		/* 
	 	*  Check the size of the vg record is greater then 0 (specified in the vgf header)
	 	*/
		else if ( el.hdr.recsz > 0 )  {
			
			switch (el.hdr.vg_type) {
			
				case TCERR_ELM :
					/* 
					 * Polygon files
					 * copy the attribute data into the line dBase files
					 * copy the spatial data into the line shapefiles
					 */
					 if (kml_fptr == NULL) {
						kml_fptr = OpenKmlFile ( pgnkml, "Cone of Uncertainty KML");
					 }
					 Tce2Shp(&el,pgnDBF,pgnSHP,&npgn, &mdata_poly, kml_fptr );
					 InitMetaObj(&mdata_poly, npgn, date, &el);
					 break;
				case TCTRK_ELM :
					/* 
					 * Point and line files
					 * copy the attribute data into the line dBase files
					 * copy the spatial data into the line shapefiles
					 */
					  if (kml_fptr == NULL) {
					 	kml_fptr = OpenKmlFile ( lkml, "Track KML" );
					  }
					 Tct2Shp(&el,pDBF,pSHP,lDBF,lSHP,&np,&nl, &mdata_ln, &mdata_pt, kml_fptr );
					 InitMetaObj(&mdata_ln, nl, date, &el);
					 InitMetaObj(&mdata_pt, np, date, &el);
					 break;
                                case TCBKL_ELM :
                                        /* 
					 * Watch/Warning files
                                         * copy the attribute data into the line dBase files
                                         * copy the spatial data into the line shapefiles
                                         */
					  if (! kml_fptr) {
					 	kml_fptr = OpenKmlFile ( wwkml, "Watch/Warning KML" );
					  }
                                          Tcb2Shp(&el,lDBF,lSHP,&nl, &mdata_ww, kml_fptr );
					  InitMetaObj(&mdata_ww, nl, date, &el);
                                          break;
				default:
					break;
			}
			curpos += el.hdr.recsz;
		}
	}
	
	/* Close the vgf file */
        cvg_clos ( ifptr, &ier );
	
	/* 
	 * Finish by closing output shapefile files 
	 */
        if ( _tcb ) {
	    DBFClose( lDBF );
	    SHPClose( lSHP );
        }
          else if ( _tce ) {
	    DBFClose( pgnDBF );
	    SHPClose( pgnSHP );
        }
          else if ( tcto ) {
	    DBFClose( lDBF );
	    SHPClose( lSHP );
	    DBFClose( pDBF );
	    SHPClose( pSHP );
        }
	
	/*>>>>>>>>>>>> Write the projection information and the metadata <<<<<<<<<<<<<<<<<*/
	
	switch (vgfType) {
		case TCERR_ELM:
			WrtShpPrj(pgnfile);
			WrtMetaFile(&mdata_poly, pgnfile);
			break;
		case TCTRK_ELM:
			WrtShpPrj(pfile);
			WrtMetaFile(&mdata_pt, pfile);
			WrtShpPrj(lfile);
			WrtMetaFile(&mdata_ln, lfile);
			break;
		case TCBKL_ELM:	
			WrtShpPrj(wwfile);
			WrtMetaFile(&mdata_ww, wwfile);
			break;
	}
	
	fclose(kml_fptr);
	
	
	return ( 0 );
}


 
 /**************************************************************************
  * Tce2Shp -- Initalize the arrays containing the lat/lon points of the 3/5 day
  * 	       cone of uncertainty.  Calls subroutines to write attributes
  *	       into the dbf file and to write the polygon information into the
  *            shapefile.
  *
  *       Calls: PutInfoAtts - write attributes to dbf file
  *		 WrtShpPgn - write lat/lon points to shapefile
  *		 DateLineCone - determines if the feature crosses the international dateline.
  *				If a crossing has occured then correct the longitudes
  *				to be consistently + or -.
  * 	
  *	  Modification: A. Krautkramer - Add call to DateLineCone
  *		     A. Krautkramer - 9/2011 - remove longitude correction for google maps compatibility 
  *			
  **************************************************************************/	
static void    Tce2Shp ( VG_DBStruct *el, DBFHandle dbf, SHPHandle shp, int *shp_nmbr, METADATA *mdata, FILE *fp )
{
       
        int     ii, npts, nparts = 1, *pstr = NULL;
        double  xavg, yavg, *x, *y;
	
         /* Number of points that make up the cone */                                                                                
        npts = el->elem.tce.cone.npts;
	
	/*
	 *  Assign appropriate memory chunk
	 */   
        x = (double *)malloc(npts*sizeof(double));
        y = (double *)malloc(npts*sizeof(double));
        pstr = malloc( sizeof(int) * (nparts));
	
	/* Initalize variables */  
        pstr[0] = 0;
        xavg = yavg = 0.;
	
	/* Check if line crosses the international date line */
	DateLineCone(el, npts, x);
	
	 /*
	 *  Initalize arrays
	 */  
	 
	fprintf (fp, "POINTS:");
        for ( ii = 0; ii < npts; ii++ )
        {
               /* x[ii] = (double)el->elem.tce.cone.latlon[ii+npts];*/
                y[ii] = (double)el->elem.tce.cone.latlon[ii];
		
		//Check the lat/lon against the bounds
		if (y[ii] > mdata->northBound) {
			mdata->northBound = y[ii];
		}
		if (y[ii] < mdata->southBound) {
			mdata->southBound = y[ii];
		}
		if (x[ii] > mdata->eastBound) {
			mdata->eastBound = x[ii];
		}
		if (x[ii] < mdata->westBound) {
			mdata->westBound = x[ii];
		}
		
		/*fprintf (fp, "%f,%f ", x[ii], y[ii]);*/
		fprintf (fp, "%f,%f ",(double)el->elem.tce.cone.latlon[ii+npts], y[ii] );
		/*printf ("Create a cone with %.1f %.1f\n",y[ii],x[ii]);*/
		
                xavg += x[ii]; 
		yavg += y[ii];
        }
	printf ("--------------> \n");
	fprintf (fp, "\n");
	
	 /* Average x and y lat/lon */  
        xavg /= npts; 
	yavg /= npts;
	
	 /* Write attributes to dbf file */  
	fprintf (fp, "ATTRIBUTES:");
        PutInfoAtts(*shp_nmbr,el,dbf, fp);
	fprintf (fp, "\n");
	
	/* Write data to shapefile */ 
        WrtShpPgn(shp,*shp_nmbr,npts,x,y, nparts, pstr);
	
	/*Increment Shp_nmbr */
        *shp_nmbr += 1;
	
	/*
	 *  Free pointers
	 */
        free(pstr);
        free(x);
        free(y);
                                                                                         
        return;
                                                                                         
}

 /**************************************************************************
  * Tcb2Shp -- Initalize the array containing the lat/lon points of the watch
  *	       /warnings lines.  Calls subroutines to write attibutes into the
  *	       dbf file and to write the line coordinates into the shapefile
  *
  *        Calls: PutInfoAtts - write attributes to dbf file
  *		  WrtShpLin - write lat/lon points to shapefile
  *
  **************************************************************************/	                                                                                        
static void    Tcb2Shp ( VG_DBStruct *el, DBFHandle dbf, SHPHandle shp, int *shp_nmbr, METADATA *mdata, FILE *fp  )
{
                                                                                         
        int     ii, npts;
        double xavg, yavg, *x, *y;
        char tcww_code[4];
	
	/* Number of points that make up the watch/warning */                                                                                
        npts = el->elem.tcb.numBkPts;
	
	/*
	 *  Assign appropriate memory chunk
	 */   
        x = (double *)malloc(npts*sizeof(double));
        y = (double *)malloc(npts*sizeof(double));
        xavg = yavg = 0.;
	
	/*
	 *  Initalize arrays
	 */  
	 
	fprintf (fp, "POINTS:");
	
        for ( ii = 0; ii < npts; ii++ )
        {
                x[ii] = (double)el->elem.tcb.bkPntLn[ii].lon;
                y[ii] = (double)el->elem.tcb.bkPntLn[ii].lat;
		
		//Check the lat/lon against the bounds
		if (y[ii] > mdata->northBound) {
			mdata->northBound = y[ii];
		}
		if (y[ii] < mdata->southBound) {
			mdata->southBound = y[ii];
		}
		if (x[ii] > mdata->eastBound) {
			mdata->eastBound = x[ii];
		}
		if (x[ii] < mdata->westBound) {
			mdata->westBound = x[ii];
		}
		
		fprintf (fp, "%f,%f ", x[ii], y[ii]);
                xavg += x[ii]; yavg += y[ii];
        }
	fprintf (fp, "\n");
	
	/* Average x and y lat/lon */  
        xavg /= npts; 
	yavg /= npts;
	
	 /* Write attributes to dbf file */
	   
	fprintf (fp, "ATTRIBUTES:");
	
        PutInfoAtts(*shp_nmbr,el,dbf,fp);
	
	/* Convert tcww numbers to text descriptions 
	     0 - Hurricane Warning HWR
	     1 - Hurricane Watch HWA
	     2 - Tropical Storm Warning TWR
	     3 - Tropical Storm Watch TWA */
	
	if ( (int)el->elem.tcb.tcww == 0 ) {
	 	strcpy(tcww_code,"HWR");
	}
	else if ( (int)el->elem.tcb.tcww == 1 ) {	
		strcpy(tcww_code,"HWA");
	}
	else if ( (int)el->elem.tcb.tcww == 2 ) {
		strcpy(tcww_code, "TWR");
	}
	else if ( (int)el->elem.tcb.tcww == 3 ) {
		strcpy(tcww_code, "TWA");
	}
	else {
		strcpy (tcww_code, " ");
		
	}
	
	/* Write watch/warning specific attributes to dbf point file */
	/*DBFWriteStringAttribute( dbf,*shp_nmbr, 9, tcww_code );*/
	DBFWriteStringAttribute( dbf,*shp_nmbr, 7, tcww_code );
	fprintf(fp, ", tcww_code * %s \n", tcww_code);
               
	/* Write data to shapefile */                                                                                 
        WrtShpLin(shp,*shp_nmbr,npts,x,y);
	
	/*Increment Shp_nmbr */
        *shp_nmbr += 1;
	
	/*
	 *  Free pointers
	 */
        free(x);
        free(y);
                                                                                         
        return;
                                                                                         
}
                                                                                         
 /**************************************************************************
  * Tct2Shp --  Initalize the array containing the lat/lon points TC track 
  *	        lines and points.  Calls subroutines to write attibutes into the
  *	       dbf file and to write the line coordinates into the shapefile
  *
  *        Calls: PutInfoAtts - write attributes to dbf file
  *		  WrtShpLin - write lat/lon points to shapefile
  *		  DateLine - if the forecat track crosses the international date line the
  *			     longitude values are correct so the resulting shapefile is correct
  *
  *        Modified: A. Krautkramer - 1/2011 - add call to DateLine
  *		     A. Krautkramer - 9/2011 - remove longitude correction for google maps compatibility 
  *
  **************************************************************************/						 
static void    Tct2Shp ( VG_DBStruct *el, DBFHandle dbfp, SHPHandle shpp,
                                   DBFHandle dbfl, SHPHandle shpl,
                                   int *shpp_nmbr,
                                   int *shpl_nmbr, METADATA *mdata_ln, METADATA *mdata_pt, FILE *fp  )
{
                                                                                         
        int     ii, npts;
        double xavg, yavg, *x, *y;
        double xp, yp;
	char new_dtLbl[50];
	char new_dtLbl_yy[50];
	int len;
        int ssnum, maxWnd;
	
        /* Number of track points */                                                                                  
        npts = el->elem.tct.numTrackPts;
	
	/* ----------------------------------------------------------
	    TRACK
	   ----------------------------------------------------------*/ 
	   
	  /*
	 *  Assign appropriate memory chunk
	 */                                                                                  
        x = (double *)malloc(npts*sizeof(double));
        y = (double *)malloc(npts*sizeof(double));                                                                               
        xavg = yavg = 0.;
	
	/* Check if line crosses the international date line */
	DateLine(el, npts, x);
	
	fprintf (fp, "TRACK LINE\n");
	fprintf (fp, "POINTS:");
	/*
	 *  Initalize arrays for the track line
	 */  
        for ( ii = 0; ii < npts; ii++ ) {
                /*x[ii] = (double)el->elem.tct.trackPnt[ ii ].lon;*/
                y[ii] = (double)el->elem.tct.trackPnt[ ii ].lat;
		
		//Check the lat/lon against the bounds
		if (y[ii] > mdata_ln->northBound) {
			mdata_ln->northBound = y[ii];
		}
		if (y[ii] < mdata_ln->southBound) {
			mdata_ln->southBound = y[ii];
		}
		if (x[ii] > mdata_ln->eastBound) {
			mdata_ln->eastBound = x[ii];
		}
		if (x[ii] < mdata_ln->westBound) {
			mdata_ln->westBound = x[ii];
		}
		
		fprintf (fp, "%f,%f ",(double)el->elem.tct.trackPnt[ ii ].lon, y[ii]);
		//fprintf (fp, "%f,%f ", x[ii], y[ii]);
		//fprintf (fp, " %i * ( %f %f ) ", ii, x[ii], y[ii]);
		
                xavg += x[ii]; 
		yavg += y[ii];
        }
	
	fprintf (fp, "\n");
	
	/* Average x and y lat/lon */  
        xavg /= npts; 
	yavg /= npts;
	
        /* Write attributes to dbf line file */
	fprintf (fp, "ATTRIBUTES:");                                                                            
        PutInfoAtts(*shpl_nmbr,el,dbfl,fp);
	fprintf (fp, "\n");
	
	/* Write data to line shapefile */  
        WrtShpLin(shpl,*shpl_nmbr,npts,x,y);
	
	/*Increment Shp_nmbr */
        *shpl_nmbr += 1;
	
	/*
	 *  Free pointers
	 */
        free(x);
        free(y);
	
	
	/* ----------------------------------------------------------
	    POINTS
	   ----------------------------------------------------------*/ 
	
	fprintf (fp, "FORECAST POINTS\n");
             
	 /*
	 *  Initalize arrays for each track point
	 */                                                                                  
        for ( ii = 0; ii < npts; ii++ ) {
		
		/*Initalize arrays for the track point */ 
                xp = (double)el->elem.tct.trackPnt[ ii ].lon;
                yp = (double)el->elem.tct.trackPnt[ ii ].lat;
		
		//Check the lat/lon against the bounds
		if (yp > mdata_pt->northBound) {
			mdata_pt->northBound = yp;
		}
		if (yp < mdata_pt->southBound) {
			mdata_pt->southBound = yp;
		}
		if (xp > mdata_pt->eastBound) {
			mdata_pt->eastBound = xp;
		}
		if (xp < mdata_pt->westBound) {
			mdata_pt->westBound = xp;
		}
		
		//fprintf (fp, " %i - ( %f %f)\n ", ii, xp, yp);
		fprintf (fp, "POINTS:%f,%f \n", xp, yp);
		
		/* Write attributes to dbf point file */  
		
		fprintf (fp, "ATTRIBUTES:");
        	PutInfoAtts(*shpp_nmbr,el,dbfp,fp);
		
		/*START Krautkramer 7/2009*/
           
		/* Add space in date label 
		   
		   Copy the initalized array elements to the new array
		   in order to figure out the length of the string which can either
		   be 17 characters '200807061000PMSat' or 16 characters '20080704700PMSun'. */
		   
		/*printf("The original string is ...%s \n\n", el->elem.tct.trackPnt[ ii ].dtLbl);*/
		
		/*year*/ 
		strncpy(new_dtLbl_yy, el->elem.tct.trackPnt[ ii ].dtLbl, 4);
		new_dtLbl_yy[ 4 ] = '-';
		/*month*/
		strncpy(&new_dtLbl_yy[ 5 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 4 ], 2);
		new_dtLbl_yy[ 7 ] = '-';
		/*day*/
		strncpy(&new_dtLbl_yy[ 8 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 6 ], 2);
		new_dtLbl_yy[ 10] = ' ';
		/*printf("1 ... The new reformatted string is ...%s \n\n", new_dtLbl_yy);*/
		
		strcpy(new_dtLbl, el->elem.tct.trackPnt[ ii ].dtLbl);
		len = strlen(new_dtLbl);
		
		/* Reformat the strings with 15 characters ie '2008090310PMSat' */
		if (len == 17) {
			/*hour minute*/
			strncpy(&new_dtLbl_yy[ 11 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 8 ], 2);
			new_dtLbl_yy[ 13 ] = ':';
			strncpy(&new_dtLbl_yy[ 14 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 10 ], 2);
			new_dtLbl_yy[ 16 ] = ' ';
			/* am/pm*/
			strncpy(&new_dtLbl_yy[ 17 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 12 ], 2);
			new_dtLbl_yy[ 19 ] = ' ';
			/* day */
			strncpy(&new_dtLbl_yy[ 20 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 14 ], 3);
			new_dtLbl_yy[ 23 ] = ' ';
			/* timezone */
			strncpy(&new_dtLbl_yy[ 24 ], el->elem.tce.info.timezone, 3);
			new_dtLbl_yy[ 27 ] = '\0';
			/*printf("2... The new reformatted string is ...%s \n\n", new_dtLbl_yy);*/
			
			/*strncpy(new_dtLbl, el->elem.tct.trackPnt[ ii ].dtLbl, 4);
			new_dtLbl[ 4 ] = ' ';
			strncpy(&new_dtLbl[ 5 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 4 ], 3);
			new_dtLbl[ 8 ] = '\0';*/
			
			/* hour minute*/
			strncpy(new_dtLbl, &el->elem.tct.trackPnt[ ii ].dtLbl[ 8 ], 2);
			new_dtLbl[ 2 ] = ':';
			strncpy(&new_dtLbl[3], &el->elem.tct.trackPnt[ ii ].dtLbl[ 10 ], 2);
			new_dtLbl[ 5 ] = ' ';
			strncpy(&new_dtLbl[ 6 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 12 ], 2);
			new_dtLbl[ 8 ] = ' ';
			strncpy(&new_dtLbl[ 9 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 14 ], 3);
			new_dtLbl[ 12 ] = '\0';
			/*printf("2*... The new reformatted string is ...%s \n\n", new_dtLbl);*/
			
		}
		
		/* Reformat the strings with 14 characters ie '200809037AMMon' */
		else {
			/*hour minute*/
			strncpy(&new_dtLbl_yy[ 11 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 8 ], 1);
			new_dtLbl_yy[ 12 ] = ':';
			strncpy(&new_dtLbl_yy[ 13 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 9 ], 2);
			new_dtLbl_yy[ 15 ] = ' ';
			/* am/pm*/
			strncpy(&new_dtLbl_yy[ 16 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 11 ], 2);
			new_dtLbl_yy[ 18 ] = ' ';
			strncpy(&new_dtLbl_yy[ 19 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 13 ], 3);
			new_dtLbl_yy[ 22 ] = ' ';
			strncpy(&new_dtLbl_yy[ 23 ], el->elem.tce.info.timezone, 3);
			new_dtLbl_yy[ 26 ] = '\0';
			/*printf("3... The new reformatted string is ...%s \n\n", new_dtLbl_yy);*/
		
			/*strncpy(new_dtLbl, el->elem.tct.trackPnt[ ii ].dtLbl, 3);
			new_dtLbl[ 3 ] = ' ';
			strncpy(&new_dtLbl[ 4 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 3 ], 3);
			new_dtLbl[ 7 ] = '\0';*/
			
			strncpy(new_dtLbl, &el->elem.tct.trackPnt[ ii ].dtLbl[ 8 ], 1);
			new_dtLbl[ 1 ] = ':';
			strncpy(&new_dtLbl[2], &el->elem.tct.trackPnt[ ii ].dtLbl[ 9 ], 2);
			new_dtLbl[ 4 ] = ' ';
			strncpy(&new_dtLbl[ 5 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 11 ], 2);
			new_dtLbl[ 7 ] = ' ';
			strncpy(&new_dtLbl[ 8 ], &el->elem.tct.trackPnt[ ii ].dtLbl[ 13 ], 3);
			new_dtLbl[ 11 ] = '\0';
			/*printf("3*... The new reformatted string is ...%s \n\n", new_dtLbl);*/
			
		}
		
		/*END Krautkramer 7/2009*/	
		
		/* START Krautkramer*/
		/* Calculate Saffir Simpson Number */
		ssnum = 0;
		
		/* START Krautkramer 10/13/2010 - SSNUM calculation error*/
		/*if (strcmp(el->elem.tct.trackPnt[ ii ].tcDvLbl, "H") == 0) {*/
		
		if (strcmp(el->elem.tct.trackPnt[ ii ].tcDvLbl, "H") == 0 || strcmp(el->elem.tct.trackPnt[ ii ].tcDvLbl, "M") == 0) {
		
		/*END Krautkramer 10/13/2010*/
		
			maxWnd = atoi(el->elem.tct.trackPnt[ ii ].mxWnd);
			if (maxWnd >= 64 && maxWnd <= 82) 
				ssnum = 1;	
			else if (maxWnd >= 83 && maxWnd <= 95)
				ssnum = 2;
			else if (maxWnd >= 96 && maxWnd <= 113)
				ssnum = 3;
			else if (maxWnd >= 114 && maxWnd <= 135)
				ssnum = 4;	
			else if (maxWnd > 135)
				ssnum = 5;	
		}	
		/* END Krautkramer */	

		/* Write track point specific attributes to dbf point file */  
		DBFWriteDoubleAttribute(dbfp,*shpp_nmbr, 7, yp);
		DBFWriteDoubleAttribute(dbfp,*shpp_nmbr, 8, xp);
                DBFWriteStringAttribute(dbfp,*shpp_nmbr, 9, el->elem.tct.trackPnt[ ii ].advDate );
                DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 10, atoi(el->elem.tct.trackPnt[ ii ].tau) );
                DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 11, atoi(el->elem.tct.trackPnt[ ii ].mxWnd) );
                DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 12, atoi(el->elem.tct.trackPnt[ ii ].wGust) );
		DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 13, atoi(el->elem.tct.trackPnt[ ii ].mslp) );
		DBFWriteStringAttribute(dbfp,*shpp_nmbr, 14, el->elem.tct.trackPnt[ ii ].tcDv );
                DBFWriteStringAttribute(dbfp,*shpp_nmbr, 15, el->elem.tct.trackPnt[ ii ].tcDvLbl );
		
		/* START Krautkramer */
		DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 16, ssnum );
                DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 17, atoi(el->elem.tct.trackPnt[ ii ].tcDir) );
                DBFWriteIntegerAttribute(dbfp,*shpp_nmbr, 18, atoi(el->elem.tct.trackPnt[ ii ].tcSpd) );
		DBFWriteStringAttribute(dbfp,*shpp_nmbr, 19, new_dtLbl );
		DBFWriteStringAttribute(dbfp,*shpp_nmbr, 20, new_dtLbl_yy );
		DBFWriteStringAttribute(dbfp,*shpp_nmbr, 21, el->elem.tce.info.timezone );
		DBFWriteStringAttribute(dbfp,*shpp_nmbr, 22, el->elem.tct.trackPnt[ ii ].stSrc );
		/* END Krautkramer */
		
		fprintf (fp, ", lat * %.1f, lon * %.1f, ", yp, xp ); 
		fprintf (fp, "validTime * %s, tau * %i, maxWnd * %i, ", el->elem.tct.trackPnt[ ii ].advDate, atoi(el->elem.tct.trackPnt[ ii ].tau), atoi(el->elem.tct.trackPnt[ ii ].mxWnd) ); 
		fprintf (fp, "wndGust * %i, mslp * %i, ", atoi(el->elem.tct.trackPnt[ ii ].wGust), atoi(el->elem.tct.trackPnt[ ii ].mslp) );
		fprintf (fp, "tcdvlLbl * %s, tcDir * %i, tcSpd * %i, ",el->elem.tct.trackPnt[ ii ].tcDvLbl, atoi(el->elem.tct.trackPnt[ ii ].tcDir), atoi(el->elem.tct.trackPnt[ ii ].tcSpd)  );
		fprintf (fp, "dateLbl * %s, fulldateLbl * %s,", new_dtLbl, new_dtLbl_yy  );
		fprintf (fp, "stormSrc * %s, ", el->elem.tct.trackPnt[ ii ].stSrc );
		fprintf (fp, "TcDvlp * %s \n", el->elem.tct.trackPnt[ ii ].tcDv );
		/* START Krautkramer */
		fprintf (fp, "SSNUM * %i \n", ssnum );
		/* END Krautkramer */
        
		/* Write data to point shapefile */ 
		
                WrtShpPnt(shpp,*shpp_nmbr,xp,yp);
		
		/*Increment Shpp_nmbr */
                *shpp_nmbr += 1;
        }
	                                                                               
        return;
                                                                                         
}

/**************************************************************************
 *  DateLine --   Figures out if the feature crosses the International date line.
 *   	          If the track crossses the date line then correct the longitude
 *		  values so they are consistently +  or -.
 **************************************************************************/	
static void    DateLine ( VG_DBStruct *el, int npts, double *x) {

	int     ii;
 	double lon, lon2;
	int cross = 0;
	
	for ( ii = 0; ii < (npts - 1); ii++ ) {
		
		/*Initalize arrays for the track point */ 
                lon = (double)el->elem.tct.trackPnt[ ii ].lon;
		lon2 = (double)el->elem.tct.trackPnt[ ii+1 ].lon;
		x [ ii ] = lon;
		
		/* Set cross flag...
			 1 if the swath crosses the dateline from West to East.
		   	-1 if the swath crosses the dateline from East to West.
		   	0 if the swath does not cross the dateline.
		   
		   Note: This code only checks the start and end of the swath.  If
		         the wind blob crosses from W->E and back from E->W then this
			 code will not work. */	
		
		if ((lon > 0 && lon2 < 0) && (lon > 150)) {
			cross = 1;
		}
		else if ((lon < 0 && lon2 > 0) && (lon < -150)) {
			cross = -1;
		}
		
		/*printf ("Element %i\n",ii);
		printf ("The points are %.1f %.1f \n",lon, lon2);
		printf ("The cross is %i\n",cross);*/
	
	}	
	
	x [ npts - 1 ] = (double)el->elem.tct.trackPnt[ npts - 1 ].lon;
	
	/* Correct the lon value so that it is consistently + or - accross the dateline.
	   The resulting values will allow ArcGIS to correctly render the data rather 
           then wrapping the values around the globe the opposite direction.*/
	if (cross != 0) {
		
		printf ("Correct track\n");
		
		for ( ii = 0; ii < npts; ii++ ) {
		
			/*Initalize arrays for the track point */ 
                	lon = (double)el->elem.tct.trackPnt[ ii ].lon;
		
			if (cross == 1) {
				if (lon < 0) {
				
					/*el->elem.tct.trackPnt[ ii ].lon = lon + 360;*/
					x[ ii ] = lon + 360;
					
				}
			}	
			else if (cross == -1) {
				if (lon > 0) {
				
					/*el->elem.tct.trackPnt[ ii ].lon = lon - 360;*/
					x[ ii ] = lon - 360;
					
				}	
			}
			
			/*printf ("The points are %.1f\n",el->elem.tct.trackPnt[ ii ].lon);
			printf ("The corrected points is %.1f\n",x[ ii ]);*/
			
		}	
	
	}
	
}

/**************************************************************************
 *  DateLineCone --   Figures out if the feature crosses the date line.
 *		      If the feautre crosses the dateline then correct the lon
 * 		      coordinates so they are consistently + or -.
 *
 *		     A. Krautkramer - 9/2011 - uncomment correction for longitude in 
 *				order to make a valid ESRI shapefile   
 **************************************************************************/	
static void    DateLineCone ( VG_DBStruct *el, int npts, double *x) {

	int     ii;
 	double lon, lon2;
	int cross = 0;

        for ( ii = 0; ii < (npts-1); ii++ )
        {
                lon = (double)el->elem.tce.cone.latlon[ii+npts];
		lon2 = (double)el->elem.tce.cone.latlon[ii+npts+1];
		x [ ii ] = lon;
		
		/* Set cross flag...
			 1 if the swath crosses the dateline from West to East.
		   	-1 if the swath crosses the dateline from East to West.
		   	0 if the swath does not cross the dateline.
		   
		   Note: This code only checks the start and end of the swath.  If
		         the wind blob crosses from W->E and back from E->W then this
			 code will not work. */	
		
		if ((lon > 0 && lon2 < 0) && (lon > 150)) {
			cross = 1;
		}
		else if ((lon < 0 && lon2 > 0) && (lon < -150)) {
			cross = -1;
		}
		
		/*printf ("Element %i\n",ii);
		printf ("The points are %.1f %.1f \n",lon, lon2);
		printf ("The cross is %i\n",cross);*/
	
	}
	x [ npts - 1 ] = (double)el->elem.tce.cone.latlon[ npts + npts - 1 ];

	/* Correct the lon value so that it is consistently + or - accross the dateline.
	   The resulting values will allow ArcGIS to correctly render the data rather 
           then wrapping the values around the globe the opposite direction.*/
	if (cross != 0) {
	
		printf ("Correct cone");
		
		for ( ii = 0; ii < npts; ii++ ) { 
		
			/*Initalize arrays for the track point */ 
                	lon = (double)el->elem.tce.cone.latlon[ii+npts];
			
			if (cross == 1) {
				if (lon < 0) {
				
					/*x[ ii ] = lon + 360;*/
					
					x[ ii ] = lon + 360;
				}
			}	
			else if (cross == -1) {
				if (lon > 0) {
				
					/*x[ ii ] = lon - 360;*/
					
					x[ ii ] = lon - 360;
				}	
			}
			
			/*printf ("The points are %.1f \n", x[ii]);*/
			
		}
	
	}	
	
}
	
/**************************************************************************
 *  LaunchDbf --   Create an empty dbf file
 *
 *        Calls:MakeErrAtts
 *		MakeTrkLineAtts
 *		MakeTrkPtsAtts
 *		MakeBklAtts
 **************************************************************************/	
DBFHandle LaunchDbf (	const char *fname ) {

	DBFHandle	hDBF;
	char		dbffname[STR_BUFFER_SIZE];

	/* Create the name of the dbf file*/ 
	sprintf(dbffname, "%s.dbf", fname);

	/* 
	 *  Create the dbf file
	 */ 
	hDBF = DBFCreate( dbffname );
	if( hDBF == NULL ) {
		fprintf(stderr, "DBFCreate(%s) failed.\n", fname );
		exit(ERR_DBFCREATE);
	}

	/* 
	 * Initalize the attribute types in the dbf file
	 */ 
        if ( _tce ) {
	   MakeErrAtts(hDBF);
        }
        else if ( _tctL ) {
           MakeTrkLineAtts(hDBF);
        }
        else if ( _tctP ) {
           MakeTrkPtsAtts(hDBF);
        }
        else if ( _tcb ) {
           MakeBklAtts(hDBF);
        }

	/* Close the dbf file*/
	DBFClose( hDBF );

	/* 
	 *  Open the dbf file read only
	 */ 
	hDBF = DBFOpen( dbffname, "r+b" );
	if( hDBF == NULL ) {
		fprintf(stderr, "DBFOpen(%s,\"r+b\") failed.\n", dbffname );
		exit(ERR_DBFOPEN);
	}

	return hDBF;
}

/**************************************************************************
 *  LaunchShp --   Create an empty shapefile
 *
 *        Calls:SHPCreate
 **************************************************************************/
SHPHandle LaunchShp(	const char *fname,
				int ObjectType ) {
				
	SHPHandle	hSHP;
	char		shpfname[STR_BUFFER_SIZE];

	/* Create the name of the shapefile*/ 
	sprintf(shpfname, "%s.shp", fname);
	
	/* 
	 *  Create the empty shapefile
	 */ 
	switch (ObjectType) {
		case OBJECTTYPE_POINT:
			hSHP = SHPCreate( shpfname, SHPT_POINT );
			break;
		case OBJECTTYPE_LINE:
			hSHP = SHPCreate( shpfname, SHPT_ARC );
			break;
		case OBJECTTYPE_POLYGON:
			hSHP = SHPCreate( shpfname, SHPT_POLYGON );
			break;
		default:
			fprintf(stderr, "internal error: "
				"unknown ObjectType=%d\n", ObjectType);
			exit(ERR_OBJECTTYPE);
	}
	
	/* 
	 *  Check the shapefile was created correctly
	 */ 
	if( hSHP == NULL ) {
		fprintf(stderr, "SHPOpen(%s, shape_type) failed.\n", shpfname );
		exit(ERR_SHPOPEN);
	}

	return hSHP;
}


/**************************************************************************
 *  MakeTrkPtsAtts --  Track Point Attributes
 *
 **************************************************************************/
static void MakeTrkPtsAtts (DBFHandle hDBF)
{
        DBFAddField( hDBF, "STORMNAME",  FTString, 20, 0 );
        DBFAddField( hDBF, "STORMTYPE",  FTString, 20, 0 );
       /* DBFAddField( hDBF, "VALIDTIME",  FTString, 20, 0 );*/
        DBFAddField( hDBF, "ADVDATE", FTString, 20, 0 );
	DBFAddField( hDBF, "ADVISNUM",   FTString, 20, 0 );
        DBFAddField( hDBF, "STORMNUM",   FTInteger, 20, 0 );
        DBFAddField( hDBF, "FCSTPRD",    FTInteger, 20, 0 );
        DBFAddField( hDBF, "BASIN",      FTString, 20, 0 );
       /* DBFAddField( hDBF, "ISSSTATUS",  FTString, 20, 0 );*/
	DBFAddField( hDBF, "LAT",     FTDouble, 20, 1 );  /* 09 */
	DBFAddField( hDBF, "LON",     FTDouble, 20, 1 );  /* 10 */
        /* DBFAddField( hDBF, "ADVDATE", FTString, 20, 0 ); */
	DBFAddField( hDBF, "VALIDTIME",  FTString, 20, 0 );
        DBFAddField( hDBF, "TAU",     FTInteger, 20, 0 ); 
        DBFAddField( hDBF, "MAXWIND", FTInteger, 20, 0 ); 
        DBFAddField( hDBF, "GUST",   FTInteger, 20, 0 ); 
        DBFAddField( hDBF, "MSLP",    FTInteger, 20, 0 ); 
	DBFAddField( hDBF, "TCDVLP",     FTString, 20, 0 ); /* 16 */
        DBFAddField( hDBF, "DVLBL",   FTString, 20, 0 ); 
	/* START Krautkramer*/
	DBFAddField( hDBF, "SSNUM",   FTInteger, 20, 0 );
	/* END Krautkramer*/
        DBFAddField( hDBF, "TCDIR",   FTInteger, 20, 0 ); 
        DBFAddField( hDBF, "TCSPD",   FTInteger, 20, 0 ); 
        DBFAddField( hDBF, "DATELBL", FTString, 20, 0 ); /* 21 */
	/*START Krautkramer*/
	DBFAddField( hDBF, "FLDATELBL", FTString, 30, 0 ); /* 22 */
	/*END Krautkramer*/
	DBFAddField( hDBF, "TIMEZONE",   FTString, 20, 0 ); 
	DBFAddField( hDBF, "STORMSRC",   FTString, 22, 0 );/* 24 */
}

/**************************************************************************
 *  MakeTrkLineAtts --  Track Line Attributes
 *
 **************************************************************************/
static void MakeTrkLineAtts (DBFHandle hDBF)
{
        DBFAddField( hDBF, "STORMNAME",  FTString, 20, 0 );
        DBFAddField( hDBF, "STORMTYPE",  FTString, 20, 0 );
       /* DBFAddField( hDBF, "VALIDTIME",  FTString, 20, 0 );*/
        DBFAddField( hDBF, "ADVDATE", FTString, 20, 0 );
        DBFAddField( hDBF, "ADVISNUM",   FTString, 20, 0 );
        DBFAddField( hDBF, "STORMNUM",   FTInteger, 20, 0 );
        DBFAddField( hDBF, "FCSTPRD",    FTInteger, 20, 0 );
        DBFAddField( hDBF, "BASIN",      FTString, 20, 0 );
        /*DBFAddField( hDBF, "ISSSTATUS",  FTString, 20, 0 ); */
        /*DBFAddField( hDBF, "TIMEZONE",   FTString, 20, 0 );*/ 
}

/**************************************************************************
 * MakeErrAtts  --  Polygon Attributes
 *
 **************************************************************************/
static void MakeErrAtts (DBFHandle hDBF)
{
        DBFAddField( hDBF, "STORMNAME",  FTString, 20, 0 );
        DBFAddField( hDBF, "STORMTYPE",  FTString, 20, 0 );
        /*DBFAddField( hDBF, "VALIDTIME",  FTString, 20, 0 );*/
	DBFAddField( hDBF, "ADVDATE", FTString, 20, 0 );
	DBFAddField( hDBF, "ADVISNUM",   FTString, 20, 0 );
        DBFAddField( hDBF, "STORMNUM",   FTInteger, 20, 0 );
        DBFAddField( hDBF, "FCSTPRD",    FTInteger, 20, 0 );
        DBFAddField( hDBF, "BASIN",      FTString, 20, 0 );
        /*DBFAddField( hDBF, "ISSSTATUS",  FTString, 20, 0 );*/
        /*DBFAddField( hDBF, "TIMEZONE",   FTString, 20, 0 );*/
}

/**************************************************************************
 * MakeBklAtts --  Watch Warning Attributtes
 *
 **************************************************************************/
static void MakeBklAtts (DBFHandle hDBF)
{
        DBFAddField( hDBF, "STORMNAME",  FTString, 20, 0 );
        DBFAddField( hDBF, "STORMTYPE",  FTString, 20, 0 );
        /*DBFAddField( hDBF, "VALIDTIME",  FTString, 20, 0 );*/
	DBFAddField( hDBF, "ADVDATE", FTString, 20, 0 );
        DBFAddField( hDBF, "ADVISNUM",   FTString, 20, 0 );
        DBFAddField( hDBF, "STORMNUM",   FTInteger, 20, 0 );
        DBFAddField( hDBF, "FCSTPRD",    FTInteger, 20, 0 );
        DBFAddField( hDBF, "BASIN",      FTString, 20, 0 );
        /*DBFAddField( hDBF, "ISSSTATUS",  FTString, 20, 0 );*/
        /*DBFAddField( hDBF, "TIMEZONE",   FTString, 20, 0 );*/
        DBFAddField( hDBF, "TCWW",   FTString, 20, 0 );  /* 8 */
}

/**************************************************************************
  * PutInfoAtts-- Write Attribtes to the dbf file
  *
  *
  **************************************************************************/	
static void PutInfoAtts (int rec, VG_DBStruct *el, DBFHandle hDBF, FILE* fp)
{
        switch (el->hdr.vg_type) {
            case TCERR_ELM :
	DBFWriteStringAttribute(hDBF,rec, 0, el->elem.tce.info.stormName );
	DBFWriteStringAttribute(hDBF,rec, 1, el->elem.tce.info.stormType );
	DBFWriteStringAttribute(hDBF,rec, 2, el->elem.tce.info.validTime );
	DBFWriteStringAttribute(hDBF,rec, 3, el->elem.tce.info.advisoryNum ); 
	DBFWriteIntegerAttribute(hDBF,rec, 4, atoi(el->elem.tce.info.stormNum) );
	DBFWriteIntegerAttribute(hDBF,rec, 5, atoi(el->elem.tce.info.fcstpd) );
	DBFWriteStringAttribute(hDBF,rec, 6, el->elem.tce.info.basin );
	/*DBFWriteStringAttribute(hDBF,rec, 7, el->elem.tce.info.issueStatus );*/
	/*DBFWriteStringAttribute(hDBF,rec, 8, el->elem.tce.info.timezone );*/
	
	fprintf (fp, "stormName * %s, stormType * %s, advisoryDate * %s, ", el->elem.tce.info.stormName, el->elem.tce.info.stormType, el->elem.tce.info.validTime ); 
	fprintf (fp, "advisoryNum * %s, stormNum * %i, ",el->elem.tce.info.advisoryNum, atoi(el->elem.tce.info.stormNum)  );
	fprintf (fp, "fcstpd * %i, basin * %s, timezone * %s ", atoi(el->elem.tce.info.fcstpd), el->elem.tce.info.basin, el->elem.tce.info.timezone );
            break;
            case TCBKL_ELM :
	DBFWriteStringAttribute(hDBF,rec, 0, el->elem.tcb.info.stormName );
	DBFWriteStringAttribute(hDBF,rec, 1, el->elem.tcb.info.stormType );
	DBFWriteStringAttribute(hDBF,rec, 2, el->elem.tcb.info.validTime );
	DBFWriteStringAttribute(hDBF,rec, 3, el->elem.tcb.info.advisoryNum );
	DBFWriteIntegerAttribute(hDBF,rec, 4, atoi(el->elem.tcb.info.stormNum) );
	DBFWriteIntegerAttribute(hDBF,rec, 5, atoi(el->elem.tcb.info.fcstpd) );
	DBFWriteStringAttribute(hDBF,rec, 6, el->elem.tcb.info.basin );
	/*DBFWriteStringAttribute(hDBF,rec, 7, el->elem.tcb.info.issueStatus );*/
	/*DBFWriteStringAttribute(hDBF,rec, 8, el->elem.tcb.info.timezone );*/
	
	fprintf (fp, "stormName * %s, stormType * %s, advisoryDate * %s, ", el->elem.tcb.info.stormName, el->elem.tcb.info.stormType, el->elem.tcb.info.validTime ); 
	fprintf (fp, "advisoryNum * %s, stormNum * %i, ",el->elem.tcb.info.advisoryNum, atoi(el->elem.tcb.info.stormNum)  );
	fprintf (fp, "fcstpd * %i, basin * %s, timezone * %s  ", atoi(el->elem.tcb.info.fcstpd), el->elem.tcb.info.basin, el->elem.tcb.info.timezone );
            break;
            case TCTRK_ELM :
	DBFWriteStringAttribute(hDBF,rec, 0, el->elem.tct.info.stormName );
	DBFWriteStringAttribute(hDBF,rec, 1, el->elem.tct.info.stormType );
	DBFWriteStringAttribute(hDBF,rec, 2, el->elem.tct.info.validTime );
	DBFWriteStringAttribute(hDBF,rec, 3, el->elem.tct.info.advisoryNum );
	DBFWriteIntegerAttribute(hDBF,rec, 4, atoi(el->elem.tct.info.stormNum) );
	DBFWriteIntegerAttribute(hDBF,rec, 5, atoi(el->elem.tct.info.fcstpd) );
	DBFWriteStringAttribute(hDBF,rec, 6, el->elem.tct.info.basin );
	/*DBFWriteStringAttribute(hDBF,rec, 7, el->elem.tct.info.issueStatus );*/
	/*DBFWriteStringAttribute(hDBF,rec, 8, el->elem.tct.info.timezone );*/
	
	fprintf (fp, "stormName * %s, stormType * %s, advisoryDate * %s, ", el->elem.tct.info.stormName, el->elem.tct.info.stormType, el->elem.tct.info.validTime ); 
	fprintf (fp, "advisoryNum * %s, stormNum * %i, ",el->elem.tct.info.advisoryNum, atoi(el->elem.tct.info.stormNum)  );
	fprintf (fp, "fcstpd * %i, basin * %s, timezone * %s  ", atoi(el->elem.tct.info.fcstpd), el->elem.tct.info.basin, el->elem.tct.info.timezone );
            break;
            default:
            break;
        return;
        }

}

/**************************************************************************
  * WrtShpPrj -- Write projection information to a projection file
  *
  *
  **************************************************************************/	
static void WrtShpPrj (char *filename)
{
	char buffname[132];
	FILE *fp;
	sprintf(buffname,"%s.prj",filename);
	fp = fopen(buffname,"w+");
	fprintf(fp,"GEOGCS[\"GCS_Sphere\",DATUM[\"D_Sphere\",SPHEROID[\"Sphere\",6371200.0,0.0]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]]");
	fclose(fp);
}

/**************************************************************************
  * OpenKmlFile --
  *
  *
  **************************************************************************/	
FILE * OpenKmlFile (char *filename, char *dataType) {

	char buffname[132];
	FILE *fp;
	
	sprintf(buffname,"%s_google.txt",filename);
	fp = fopen(buffname,"w+");
	
	fprintf (fp, "%s\n", dataType);

	return fp;
}

/**************************************************************************
  * InitMetaObj -- 
  *
  *
  **************************************************************************/	
static void InitMetaObj (METADATA *mdata, int npgn, char *date, VG_DBStruct *el) {

	mdata->numEle = npgn;
	mdata->date = date;
	mdata->advdate = el->elem.tce.info.validTime;
	
	// The header is not initalized
	//mdata->westBound = el->hdr.range_max_lon;
	//mdata->eastBound = el->hdr.range_min_lon;
	//mdata->northBound = el->hdr.range_max_lat;
	//mdata->southBound = el->hdr.range_min_lat;

}


/**************************************************************************
  * WrtMetaFile -- Write metadata information to an xml template file.
  *
  *
  **************************************************************************/	
static void WrtMetaFile (METADATA *meta, char *filename) {

	char buffname[132];
	FILE *outFile, *inFile;
	char* pch;
        int start,end;
        int count;
	
	/* Open a new metadata file for writing */
	sprintf(buffname,"%s.shp.xml",filename);
	outFile = fopen(buffname,"w+");
	
	/* Open the template file for reading */
	//template = "tcTrackLine_template.xml";
	inFile = fopen(meta->templateFile, "r");
	
	if (inFile==NULL) {
    		printf("Error: can't open file for reading.\n");
		return;
  	}
	else if(outFile==NULL) {
    		printf("Error: can't create file for writing.\n");
		return;
  	}
	else {
	
	     char line [ 128 ];
	     char newline [ 128 ];
 	     char repl_val [ 128 ];
	     char key [ 4 ];
	     
 	     while ( fgets ( line, sizeof line, inFile ) != NULL ) /* read a line */
	     {
    		 // get start and end indices...
                 pch=strchr(line,'*');
		 
                 if (pch!=NULL)
                 {
		 
		      // Find the location of the '*'
                      start = pch-line;
                      pch=strchr(pch+1,'*');
                      end = pch-line;
		      
		      // Copy everything before first '*', concat the replacement value
    		      strncpy(newline,line,start);
    		     
		      // Parse out the key
		      strncpy(key, &line[start+1], 3);
		      key[ 3 ] = '\0';
		      
		      // NUMBER OF SHAPEFILE ENTITIES
		      if ( strcmp( key, "NUM" ) == 0 ) {
			  sprintf(repl_val, "%i",meta->numEle );
		      }
		      // PUBLICATION DATE
		      else if ( strcmp( key, "PUB" ) == 0 ) {
		      	  strcpy(repl_val, meta->date);
		      }
		      // FILE NAME
		      else if ( strcmp( key, "NAM" ) == 0 ) {
		      	  strcpy(repl_val, meta->filename);
		      }
		      // CALENDAR DATE
		      else if ( strcmp( key, "DAT" ) == 0 ) {
		          strcpy(repl_val, meta->advdate);
		      }
		      else if ( strcmp( key, "WBD" ) == 0 ) {
		       	  sprintf(repl_val, "%f",meta->westBound );
		      }
		      else if ( strcmp( key, "EBD" ) == 0 ) {
		          sprintf(repl_val, "%f",meta->eastBound );
		      }
		      else if ( strcmp( key, "NBD" ) == 0 ) {
		          sprintf(repl_val, "%f",meta->northBound );
		      }
		      else if ( strcmp( key, "SBD" ) == 0 ) {
		          sprintf(repl_val, "%f",meta->southBound );
		      }
		      else if ( strcmp( key, "SIZ" ) == 0 ) {
		          strcpy(repl_val, " ");
		      }
		      else {
		      	  strcpy(repl_val, "none");
		      }
		     
		      // Replace the key with the correct value
		      strcat(newline,repl_val);
		      
		      // concat everything after the second '*'
		     strcat(newline,&line[end + 1]);
    		    
		     // Write the line to the new file
		     fputs ( newline, outFile );
		     
                 }
		 
		 // If no '*' is found then write line directly to the new file
		 else {
		     fputs ( line, outFile );
		 }
		 
		 // Empty line and new line
		 for (count = 0; count < 128; count++) {
		     line[count] = (char)NULL;
		     newline[count] = (char)NULL;
		     repl_val[count] = (char)NULL;
		 }
		
                
             }

	     //Close the files
	     
	     fclose(outFile);
	     fclose(inFile);
	
	}
}

static void getDate( char *date ) {

       struct tm *OurT = NULL; 
       time_t Tval = 0;
       char *Month[12] = {
                     "January",   "February", "March",    "April",
                     "May",       "June",     "July",     "August",
                     "September", "October",  "November", "December"
                    };
	                 
        Tval = time(NULL);
  	OurT = localtime(&Tval);
        
	sprintf(date, "%s %d, %d",  Month[OurT->tm_mon], OurT->tm_mday, 1900 + OurT->tm_year );
}	 
				      
				    
static void WrtShpPnt(	SHPHandle hSHP,
			int rec,
			double x,
			double y ) {
	SHPObject	*psShape;

	psShape = SHPCreateObject( SHPT_POINT, rec, 0, NULL, NULL,
                               1, &x, &y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}

static void WrtShpLin(	SHPHandle hSHP,
			int rec,
			int coords,
			double * x,
			double * y ) {
	SHPObject	*psShape;

	psShape = SHPCreateObject( SHPT_ARC, rec, 0, NULL, NULL,
		coords, x, y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}

static void WrtShpPgn(	SHPHandle hSHP,
				int rec,
				int coords,
				double * x,
				double * y,
				int nparts,
				int * partstarts) {
	SHPObject	*psShape;

	psShape = SHPCreateObject( SHPT_POLYGON, rec, nparts, partstarts, NULL,
		coords, x, y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}



