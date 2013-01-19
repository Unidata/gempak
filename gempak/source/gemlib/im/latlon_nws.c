#include "geminc.h"
#include "gemprm.h"


// Define some constants for PI.

#define M_PI    3.14159265358979323846
#define M_PI_2  1.57079632679489661923
#define M_PI_4  0.78539816339744830962

// Define the Hemisphere.

#define H 1

// Define the Hawaii constants.

#define STD_LATITUDE_DEG_PSN_HAWAII 60.0
#define PROJECTION_CENTER_LATITUDE_DEG_PSN_HAWAII 90.0
#define PROJECTION_CENTER_LONGITUDE_DEG_PSN_HAWAII -157.5
#define GRID_CENTER_LATITUDE_DEG_PSN_HAWAII 20.8
#define GRID_CENTER_LONGITUDE_DEG_PSN_HAWAII -157.5
// Hawaii width 3200 km
// Hawaii height 2400 km

// Define the Alaska constants. Note that the longitude of the product center is
// not the same as the longitude of the projection center.

#define STD_LATITUDE_DEG_PSN_ALASKA 60.0
#define PROJECTION_CENTER_LATITUDE_DEG_PSN_ALASKA 60.0
#define PROJECTION_CENTER_LONGITUDE_DEG_PSN_ALASKA -155.0
#define GRID_CENTER_LATITUDE_DEG_PSN_ALASKA 62.2
#define GRID_CENTER_LONGITUDE_DEG_PSN_ALASKA -152.0
// Alaska width 3200 km
// Alaska height 2400 km

FILE * log_out;

// Define a "coords" structure to pass info in and out of the routines.

typedef struct{
     double dx;
     double dy;
     double phi_org;
     double lamda_org;
     double phi_value;
     double lamda_value;
     short x;
     short y;
     short pr_id;
     short grid_id;
} coords;

// Create instance of coords and declare a pointer to the coords structure.

coords dummy_coords;
coords *local_coords;

// Prototypes:

// Hawaii mosaic lat/lon conversion (polar stereographic north psn projection).
int psn_latlon_to_km_hawaii( coords *l_coords);

// Alaska mosaic lat/lon conversion (polar stereographic north psn projection).
int psn_latlon_to_km_alaska( coords *l_coords);

// Define variables for lat/lon of mosaic product center.

double hawaii_center_lat = GRID_CENTER_LATITUDE_DEG_PSN_HAWAII; 
double hawaii_center_lon = GRID_CENTER_LONGITUDE_DEG_PSN_HAWAII;

double alaska_center_lat = GRID_CENTER_LATITUDE_DEG_PSN_ALASKA; 
double alaska_center_lon = GRID_CENTER_LONGITUDE_DEG_PSN_ALASKA;

// Define variables to hold offset in km from the center of the
// projection to the center of the mosaic product for Alaska and Hawaii.
// The center of the projection is at the north pole.  The program
// calculates the values and writes them out.

double psn_hawaii_offset_km_x;
double psn_hawaii_offset_km_y;
double psn_alaska_offset_km_x;
double psn_alaska_offset_km_y;

void lc_latlon_to_km( double *phi_deg, double *lamda_deg, double *zx, double *zy )
{
// Lambert Conformal for National Mosaic:  converts lat/lon to km (from mosaic center).
static double deg2radians = {M_PI/180.};   /* Converts degrees to radians.  */

/*
** The following constants were calculated for the grid center, earth radius,
** and standard latitudes defined above.  DO NOT use this routine for other
** centers without recalculating the constants.
*/

static double lamda_0= {-1.710422666954443};  /* Grid origin longitude (in radians).  */

static double rho_0= {7931.823624772932817};
static double n= {0.630477697315427};
static double rf={12472.901286168398656};	/* Intermediate term =(radius*f)  */

double rho;
double theta;

double phi; 	   /* Current latitude value (radians). */
double lamda;		/* Current longitude value (radians). */

/*
** phi_deg and lamda_deg are the latitude and longitude of the object (in degrees). 
** Convert the latitude and longitude of the object from degrees to radians.
*/
	phi = (*phi_deg)*deg2radians;
	lamda = (*lamda_deg)*deg2radians;

	rho = ( rf/(pow(tan(M_PI_4 + (phi/2)),n)) );
	theta = n*(lamda - lamda_0);

/*
** Compute the x and y components of distance (nominally in kilometers).
*/
	*zx = (rho*sin(theta));
	*zy = (rho_0 - rho*cos(theta) );
}

/*======================================================================*/

void lc_km_to_latlon( double *zx, double *zy, double *phi_deg, double *lamda_deg )
{
// Lambert Conformal for National Mosaic:  converts km (from mosaic center) to lat/lon.
static double lamda_0= {-1.710422666954443};  /* Grid origin longitude (in radians). */
static double rho_0= {7931.823624772932817};
static double n= {0.630477697315427};
static double rf={12472.901286168398656};	/* Intermediate term =(radius*f)  */
static double deg2radians = {M_PI/180.}; /*Converts degrees to radians.    */

/*	----------------------Internal Variables----------------------- */
double phi; 		/* "Object" latitude (radians). 	*/
double lamda;		/* "Object" longitude (radians).	*/
double rho;
double theta;

/*
** zx / zy is the object distance (in km's)
*/
	rho = sqrt( ((*zx)*(*zx)) + ((rho_0-*zy)*(rho_0-*zy)) );

/*
** Compute the latitude in radians.
*/
	phi = ((2*atan( pow( (rf/rho) , (1/n) ))) - M_PI_2);
	theta = atan2( *zx,(rho_0 - *zy) );

/*
** Compute the longitude in radians.
*/
	lamda = ((theta/n) + lamda_0);

/*
** Convert the latitude/longitude from radians to degrees.
*/
	*phi_deg=phi/deg2radians;
	*lamda_deg=lamda/deg2radians;
}

/*===========================================================================================*/

int psn_latlon_to_km_hawaii( coords *l_coords )
{
// Polar Sereographic for Hawaii Mosaic:  converts lat/lon to km (from projection center).
    static double deg2radians = {M_PI/180.}; /* Converts degrees to radians.    */

    static double radius_of_earth_km = {6371.2213};  /*  earth radius (in kilometers).  */

    static double lonc = PROJECTION_CENTER_LONGITUDE_DEG_PSN_HAWAII;     /* Projection origin longitude (in degrees).*/

    static double std_latitude_deg = STD_LATITUDE_DEG_PSN_HAWAII;    /* Standard latitude (in degrees). */

    double phi_deg;
    double lamda_deg;

    double zx;
    double zy;

    double phi;        /* Current latitude value (radians). */
    double lamda;      /* Current longitude value (radians). */

    double hemis = 1.0;
    double lon0;
    double sigma;
    
    double sin_60;

    sin_60 = sin(std_latitude_deg*deg2radians);
    
    lon0 = (lonc +90.)*deg2radians;

/*
** Extract the latitude and longitude of the object (in degrees) from
** the l_coords structure.
*/

    phi_deg = l_coords->phi_value;
    lamda_deg = l_coords->lamda_value;
    
#if(0)
    fprintf(log_out,"Entering psn_latlon_to_km_hawaii lat=%f lon = %f\n",phi_deg,lamda_deg);
    zx=0;
    zy=0;
#endif

/*
** Convert the latitude and longitude of the object from
** degrees to radians.
*/

    phi = phi_deg*deg2radians;
    lamda = lamda_deg*deg2radians;
    
    sigma = (1.0)/(1.0 + (hemis * sin(phi)));
    
    zx = hemis * radius_of_earth_km * (1+sin_60) * sigma * cos(phi) * cos(hemis * (lamda-lon0));
    zy = hemis * radius_of_earth_km * (1+sin_60) * sigma * cos(phi) * sin(hemis * (lamda-lon0));

/*
** Assign the x,y distance components (nominally in kilometers)
** to the l_coords structure.
*/

    l_coords->dx = zx;
    l_coords->dy = zy;

#if(0)
    fprintf(log_out,"Leaving psn_latlon_to_km_hawaii zx=%f zy = %f\n\n",zx,zy);
#endif

/*
** Return a zero value.
*/

    return(0);
}

/*==========================================================================*/

void psn_km_to_latlon_hawaii( double *zx, double *zy, double *phi_deg, double *lamda_deg )
{
// Polar Sereographic for Hawaii Mosaic:  converts km (from projection center) to lat/lon.
	static double radius_of_earth_km = {6371.2213};  /*  earth radius (in kilometers).	*/
	static double deg2radians = {M_PI/180.}; /*Converts degrees to radians.    */
	static double radians2deg = {180./M_PI}; /*Converts radians to degrees.    */
	
	static double phi_0_deg = PROJECTION_CENTER_LATITUDE_DEG_PSN_HAWAII;	
	static double lonc = PROJECTION_CENTER_LONGITUDE_DEG_PSN_HAWAII;
	                  
	static double std_latitude_deg = {STD_LATITUDE_DEG_PSN_HAWAII};    /* Standard latitude (in degrees). */
	double sin_std_latitude;
	
	double phi; 		/* "Object" latitude (radians).    */
	double lamda;	   /* "Object" longitude (radians).   */
	
	double hemis = (double)H;
	
	double lon0;
	double f1;
	double f2;
	double expr;
	double acos_arg;
	double x_sqr;
	double y_sqr;
	double d0 = 1.0;	/* 381 ???????????????????????? */
	double meshr = 1.0; /* ???????????????????????? */
	
/*
** zx / zy is the object distance (in km's)
*/
	if((fabs(*zx)<2.0)&&(fabs(*zy)<2.0))
	{
	    *phi_deg = phi_0_deg;
	    *lamda_deg = lonc;
	}
	
	x_sqr = (*zx*(*zx));
	y_sqr = (*zy*(*zy));
	
	sin_std_latitude = sin(std_latitude_deg*deg2radians);
	
	lon0 = (lonc+90.)*deg2radians;
	
	f1=(radius_of_earth_km/(d0*meshr));
	f1 *= f1;
	
	f2 = ((1+sin_std_latitude)*(1+sin_std_latitude));
	
	expr = (f1*f2 - (x_sqr + y_sqr))/(f1*f2 + (x_sqr + y_sqr));
	
	phi = hemis*asin(expr);
	
	acos_arg = *zx/sqrt(x_sqr+y_sqr);
	
	if(acos_arg>1.0)
	{
	    acos_arg = 1.0; 
	}
	else if(acos_arg<-1.0)
	{
	    acos_arg = -1.0; 
	}
	
	if(*zy<=0.)
	{
	    lamda = lon0 - acos(acos_arg);
	}
	
	if(*zy>=0.)
	{
	    lamda = lon0 + acos(acos_arg);
	}
	
/*
** Convert the latitude/longitude from radians to degrees.
*/
	*phi_deg = phi*radians2deg;
	*lamda_deg = lamda*radians2deg;
	
	if(*lamda_deg>180.0)
	{
	    *lamda_deg -= 360.0;
	}
	else if(*lamda_deg<-180.0)
	{
	    *lamda_deg += 360.0;
	}
}

/*===========================================================================*/

int psn_latlon_to_km_alaska( coords *l_coords )
{
// Polar Sereographic for Alaska Mosaic:  converts lat/lon to km (from projection center).
    static double deg2radians = {M_PI/180.}; /* Converts degrees to radians.    */
	
    static double radius_of_earth_km = {6371.2213};  /*  earth radius (in kilometers).  */
	
    static double lonc = PROJECTION_CENTER_LONGITUDE_DEG_PSN_ALASKA;     /* Projection origin longitude (in degrees).*/
	
    static double std_latitude_deg = {60.0};    /* Standard latitude (in degrees). */
	
    double phi_deg;
    double lamda_deg;
	
    double zx;
    double zy;
	
    double phi;        /* Current latitude value (radians). */
    double lamda;      /* Current longitude value (radians). */
	
    double hemis = 1.0;
    double lon0;
    double sigma;
    
    double sin_60;
	
    sin_60 = sin(std_latitude_deg*deg2radians);
    
    lon0 = (lonc +90.)*deg2radians;
	
    /*
    ** Extract the latitude and longitude of the object (in degrees) from
    ** the l_coords structure.
    */
	
    phi_deg = l_coords->phi_value;
    lamda_deg = l_coords->lamda_value;
    
#if(0)
    fprintf(log_out,"Entering psn_latlon_to_km lat=%f lon = %f\n",phi_deg,lamda_deg);
#endif
	
    /*
    ** Convert the latitude and longitude of the object from
    ** degrees to radians.
    */
	
    phi = phi_deg*deg2radians;
    lamda = lamda_deg*deg2radians;
    
    sigma = (1.0)/(1.0 + (hemis * sin(phi)));
    
    zx = hemis * radius_of_earth_km * (1+sin_60) * sigma * cos(phi) * cos(hemis * (lamda-lon0));
    zy = hemis * radius_of_earth_km * (1+sin_60) * sigma * cos(phi) * sin(hemis * (lamda-lon0));
	
    /*
    ** Assign the x,y distance components (nominally in kilometers)
    ** to the l_coords structure.
    */
	
    l_coords->dx = zx;
    l_coords->dy = zy;
	
#if(0)
    fprintf(log_out,"Leaving psn_latlon_to_km_alaska zx=%f zy = %f\n\n",zx,zy);
#endif
	
    /*
    ** Return a zero value.
    */
	
    return(0);
}

/*============================================================================*/

void psn_km_to_latlon_alaska( double *zx, double *zy, double *phi_deg, double *lamda_deg )
{
// Polar Sereographic for Alaska Mosaic:  converts km (from projection center) to lat/lon.
    static double radius_of_earth_km = {6371.2213};  /*  earth radius (in kilometers).  */
    static double deg2radians = {M_PI/180.}; /*Converts degrees to radians.    */
    static double radians2deg = {180./M_PI}; /*Converts radians to degrees.    */
	
    static double phi_0_deg = PROJECTION_CENTER_LATITUDE_DEG_PSN_ALASKA;    
    static double lonc = PROJECTION_CENTER_LONGITUDE_DEG_PSN_ALASKA;
	
    static double std_latitude_deg = {60.0};    /* Standard latitude (in degrees). */
	
    double phi;         /* "Object" latitude (radians).    */
    double lamda;      /* "Object" longitude (radians).   */
	
    double hemis = 1.0;
    
    double lon0;
    double f1;
    double f2;
    double expr;
    double acos_arg;
    double x_sqr;
    double y_sqr;
    double sin_60;
    double d0 = 1.0;    /* 381 ???????????????????????? */
    double meshr = 1.0; /* ???????????????????????? */
    
/*
** zx / zy is the object distance (in km's)
*/
#if(0)
    fprintf(log_out,"Entering psn_km_to_latlon_alaska zx= %f zy= %f\n",*zx,*zy);
#endif
	
	if((fabs(*zx)<2.0)&&(fabs(*zy)<2.0))
	{
	    *phi_deg = phi_0_deg;
	    *lamda_deg = lonc;
	}
	
	x_sqr = (*zx*(*zx));
	y_sqr = (*zy*(*zy));
    
	sin_60 = sin(std_latitude_deg*deg2radians);
    
	lon0 = (lonc+90.)*deg2radians;
    
	f1 = (radius_of_earth_km/(d0*meshr));
	f1 *= f1;
    
	f2 = ((1+sin_60)*(1+sin_60));
    
	expr = (f1*f2 - (x_sqr + y_sqr))/(f1*f2 + (x_sqr + y_sqr));
    
	phi = hemis*asin(expr);
    
	acos_arg = *zx/sqrt(x_sqr+y_sqr);
    
	if(acos_arg>1.0)
	{
	    acos_arg = 1.0; 
	}
	else if(acos_arg<-1.0)
	{
	    acos_arg = -1.0; 
	}
    
	if(*zy<=0.)
	{
	    lamda = lon0 - acos(acos_arg);
	}
	
	if(*zy>=0.)
	{
	    lamda = lon0 + acos(acos_arg);
	}

/*
** Convert the latitude/longitude from radians to degrees.
*/
    *phi_deg = phi*radians2deg;
    *lamda_deg = lamda*radians2deg;
    
    if(*lamda_deg>180.0)
    {
	*lamda_deg -= 360.0;
    }
    else if(*lamda_deg<-180.0)
    {
	*lamda_deg += 360.0;
    }
	
#if(0)
    fprintf(log_out,"Leaving psn_km_to_latlon_alaska lat=%f lon = %f\n\n",*phi_deg,*lamda_deg);
#endif

}
