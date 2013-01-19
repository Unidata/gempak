/************************************************************************
 * sigbcmn.h                                                            *
 *									*
 * This include file contains BUFR descriptors used in decoding         *
 * high-level sigwx messages.                                           *
 *									*
 **									*
 *Log:									*
 * D. Kidwell/NCEP       1/02       					*
 * M. Li/SAIC		 4/04	Added D_FLIGHT_LVL & D_FLVL_SIGNIF	*
 * S. Jacobs/NCEP	10/04	Added D_EXT_DEG_TURB			*
 * M. Li/SAIC		12/04	Added D_AF_ICING			*
 ***********************************************************************/

#define D_FEATURE_NAME      1022     /* Name of feature */        
#define D_STORM_NAME        1026     /* WMO storm name */        
#define D_CENTER_ID         1031     /* Originating/generating ctr id */
#define D_YEAR              4001     /* Year */        
#define D_MINUTE            4005     /* Minute */        
#define D_LAT               5002     /* Latitude (coarse) */      
#define D_LON               6002     /* Longitude (coarse) */     
#define D_ALTITUDE          7002     /* Height or altitude (flt lvl) */
#define	D_FLIGHT_LVL        7010     /* Flight level */
#define D_VERT_SIGNIF       8001     /* Vertical sounding signif */
#define D_MET_ATTRIB        8005     /* Meteorological attribute */
#define D_DIMEN_ATTRIB      8007     /* Dimensional attribute */  
#define D_MET_FEATURE       8011     /* Meteorological feature */  
#define D_TIME_SIGNIF       8021     /* Time significance */      
#define D_STATISTIC         8023     /* First order statistics */ 
#define	D_FLVL_SIGNIF       8040     /* Flight level significance */
#define D_HEIGHT           10002     /* Height (flight level) */  
#define D_WSPD             11002     /* Wind speed */        
#define D_DEG_TURB         11031     /* Degree of turbulence */   
#define D_EXT_DEG_TURB     11030     /* Extended Degree of turbulence */
#define D_TYPE_FEATURE     19001     /* Type of synoptic feature */
#define D_DIR_FEATURE      19005     /* Direction of motion of feature */
#define D_SPD_FEATURE      19006     /* Speed of motion of feature */
#define D_CLOUD_AMT        20008     /* Cloud distribution/amount */
#define D_CLOUD_TYPE       20012     /* Cloud type */        
#define D_AF_ICING         20041     /* Airframe Icing */	
#define D_FACILITY         23002     /* Activity/facility in incident */

/*---------------------------------------------------------------------*/
#include    "proto_sigbufr.h"
