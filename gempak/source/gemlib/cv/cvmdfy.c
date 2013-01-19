#include "geminc.h"
#include "gemprm.h"


/*
 *---------------------------------------------------------------------*
 *          Private functions and macros used by CV_MDFY               *
 *---------------------------------------------------------------------*
 */
 
#define TIE_DIST      10.0
#define GHOST_POINTS  3 
#define PRECISION     1e-3 

/*
 *  Function to modify a line without further smoothing
 */
void cvm_mdfy ( int *npo, float *xo, float *yo, int *npc, float *xc, 
               float *yc, int *oline, int *maxpts, int opi, int osi, 
               int *np, float *x, float *y, int *is, int *ie, int *iret );

/*
 *  Function to check if a clicked point is on vortex or not
 */
int cvm_cptv ( int npi, float *xin, float *yin, float *xci, float *yci,  
              int line );  

/*
 *  Function to check if a clicked point is online or not
 */
int cvm_cptl ( int npi, float *xin, float *yin, float *xci, float *yci );  

/*
 *  Function to determine the angle between two line segments
 */
float cvm_angl ( float *xs, float *ys, float *xe, float *ye );  

/*
 *  Function to reverse an array
 */
void cvm_swap ( int npi, float *xin );  

/*
 * Function to modify an open line
 */
int cvm_opmd ( int npo, float *xo, float *yo, int npc, float *xc, 
              float *yc, int maxpts, int fin, int fis, int lin, 
	      int lis, float *drct, int *np, float *x, float *y,
	      int *is, int *ie );
	       
/*
 *  Function to modify a closed line
 */
int cvm_csmd ( int npo, float *xo, float *yo, int npc, float *xc, 
              float *yc, int maxpts, int fin, int fis, int lin, 
	      int lis, float *drct, int *np, float *x, float *y,
	      int *is, int *ie );

/*
 *  Function to determine the start/end index for ghost line
 */
void cvm_index ( int *np, int *is, int *ie, int line );  

/*
 *  Function to determine a given segment's direction to OL
 */
int cvm_drct ( int npo, float *xo, float *yo, int npc, float *xc,
              float *yc, int indx, int flag );  
 
/************************************************************************
 * cvmdfy.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

/*
 *---------------------------------------------------------------------*
 *                   Public interface for CV_MDFY                      *
 *---------------------------------------------------------------------*
 */

/*=====================================================================*/

void cv_mdfy ( int *npo, float *xo, float *yo, int *npc, float *xc, 
		float *yc, int *sm_lvl, int *oline, int *maxpts, 
		int *np, float *x, float *y, int *is, int *ie, int *iret )
/************************************************************************
 * cv_mdfy								*
 *									*
 * This function accepts a set of points to be graphically modified,    *
 * along with a set of points that have already been clicked, and       *
 * returns the resulting line set of points. Also returned are indices 	*
 * pointing to the beginning and end of the intended modify section     *
 *									* 
 * Notes for modifying closed lines: 					* 
 *   1.  For input, line attribute 'oline' should be set to 1 only and  *
 *       last point must have same coordinates as first point           *
 *   2.  For output, returned line will always be 'open' - first point  *
 *       will not repeat at the end of line. So before starting to      *
 *       plot ghost line, the first point should always be added to the *
 *       end of line to assure proper display                           *
 *   3.  For ghost line, it starts at '*is' and goes to '*ie' when      *
 *       *is < *ie; if *is > *ie, ghost line should starts from '*is'   *
 *       and passes the end of line (also the beginning) and then goes  *
 *       to 'ie'                                                        *
 *									*
 * void cv_mdfy ( npo, xo, yo, npc, xc, yc, sm_lvl, oline, maxpts,      *
 *                np, x, y, is, ie, iret )                              *
 * 	                                                                *
 * Input parameters:							*
 *	*npo	int	Number of points in original line 		*
 *	*xo	float	Original line x-coordinates		        *
 *	*yo	float	Original line y-coordinates		        *
 *	*npc	int	Number of clicked points		        *
 *	*xc	float	Clicked line x-coordinates		        *
 *	*yc	float	Clicked line x-coordinates		        *
 *      *sm_lvl int	Smoothing level of line                         *
 *      *oline	int	Line type of original line (0-open 1-closed )   *
 *	*maxpts	int	Maximum number of points returned		*
 * 	                                                                *
 * Output parameters:							*
 *	*np	int     Number of points returned		        *
 *	*x	float   Returned line x-coordinates		        *
 *	*y	float   Returned line y-coordinates		        *
 *	*is	int     Starting index of modification                  *
 *	*ie	int     Ending index of modification                    *
 *	*iret	int     Return value                                    *
 *	                   = 0 - normal	return				*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 * J. Wu/GSC	10/00	Added smoothing level                           *
 * J. Wu/GSC	10/00	Adjusted modification rules for open line       *
 * J. Wu/GSC	10/00	Added line type 'oline'                         *
 * J. Wu/GSC    10/00   Fixed indices of ghost line for closed line     *
 * J. Wu/GSC    11/00   Eliminated duplicates at end of closed line     *
 * J. Wu/GSC    11/00   Increased upper limit to 'LLMXPT' for smoothing *
 * J. Wu/GSC    11/00   Eliminated overlapping on ghost line            *
 * J. Wu/GSC    11/00   Refixed indexing for ghost line                 *
 * J. Wu/GSC    11/00   Made ghost line always follow cursor position   *
 * J. Wu/GSC    11/00   Refined private functions' prototypes           *
 * J. Wu/GSC    12/00   Fixed bugs for unsmoothed open lines modify     *
 * J. Wu/GSC    11/01   Eliminated duplicates for smooth/close lines    *
 ***********************************************************************/
{ 
   int    ii, jj, nn, sm_pts, ier, itr, ftr, flag;
   int    nnl, modified, op_index, os_index, max_smth, opd, osd;
   float  sm_val, crvscl, fang1, fang2, dfl;
   float  *xcp, *ycp, *sm_x, *sm_y, *xnl, *ynl;

   /*-------------------------------------------------------------------*
    * Some definitions
    * OL - Original Line         FCP - First Clicked Point
    * CL - Clicked Line          LCP - Last Clicked Point
    *
    * =v      - A point (FCP or LCP ) is within the tie distance of at 
    *           least one of OL's vortex.
    * online  - A point (FCP or LCP ) is within the tie distance of OL     
    *           but not within tie distance of any vortex of OL.
    * offline - A point (FCP/LCP ) is not within the tie distance of OL.
    *
    *------------------------------------------------------------------*/

   /* 
    * Initialize local variables 
    */
   
   max_smth = LLMXPT; /* maximum number of point for smoothed line  */
   *is = 0;
   *ie = *npo - 1;
   *iret = 0;
   itr = 1;          /* Start of segment to return on smoothed line */
   ftr = *npo;       /* End of segment to return on smoothed line   */
   sm_pts = *npo;    /* Number of points on smoothed line           */
   nnl = 0;          /* Number of points on new line when smoothed  */
   modified = -1;    /* -1 = not modified   1 - already modified    */
   op_index = -1;    /* index of FCP on OL if FCP=v                 */
   os_index = -1;    /* index of FCP on OL if FCP online            */

  /*
   * Acquire memory for copying CL, storing smoothed line and 
   * the new line after modification. If failed, return OL
   */
   
   xcp = (float *) calloc( *npc, sizeof( float ) );
   ycp = (float *) calloc( *npc, sizeof( float ) );
   sm_x = (float *) calloc( max_smth, sizeof( float ) );
   sm_y = (float *) calloc( max_smth, sizeof( float ) );
   xnl = (float *) calloc( max_smth, sizeof( float ) );
   ynl = (float *) calloc( max_smth, sizeof( float ) );

   if ( xcp == NULL  || ycp == NULL || sm_x == NULL ||
        sm_y == NULL || xnl == NULL || ynl  == NULL )   {          
       *np = *npo;
       for ( ii = 0; ii < *npo; ii++ ) {
           x [ ii ] = xo [ ii ];
           y [ ii ] = yo [ ii ];
       }
       if ( *oline == 1 ) {
	   ( *np )--;
       }
       
       free( xcp );
       free( ycp );          
       free( sm_x );
       free( sm_y );          
       free( xnl );
       free( ynl );          

       return;
   }
   else {    /* Make a copy of CL */      
       for ( ii = 0; ii < *npc; ii++ ) {
           xcp [ ii ] = xc [ ii ];
           ycp [ ii ] = yc [ ii ];
       }
   }
            
   /* 
    * Get the relative position of FCP on OL or its smoothed one. 
    */
    
    if ( *sm_lvl != 1 && *sm_lvl != 2 )  {  
        op_index = cvm_cptv ( *npo, xo, yo, &xcp[0], &ycp[0], *oline );
        if ( op_index == -1 ) {
             os_index = cvm_cptl ( *npo, xo, yo, &xcp[0], &ycp[0] );
	}
    }    
    else {
       sm_val = 0.0;     /* Set smoothing value according to sm_lvl */
       if ( *sm_lvl == 1 ) sm_val = 1.0;  
       if ( *sm_lvl == 2 ) sm_val = 5.0;

       gqcvsc( &crvscl, &ier );  /* get device curve scaling factor */
       if ( crvscl <= 0. )  crvscl = 25.0;       

       cv_prmt( npo, xo, yo, &sm_val, &max_smth, &crvscl,
                &itr, &ftr, &sm_pts, sm_x, sm_y, iret);   
       	  
       if ( *oline == 1 &&             
            G_ABS( sm_x[ sm_pts - 1 ] - sm_x[ 0 ] )  < PRECISION  &&
	    G_ABS( sm_y[ sm_pts - 1 ] - sm_y[ 0 ] )  < PRECISION ) {
           sm_x[ sm_pts ] = sm_x[ 0 ];
           sm_y[ sm_pts ] = sm_y[ 0 ];
           sm_pts++;
       }

       op_index = cvm_cptv ( sm_pts, sm_x, sm_y, &xcp[0], &ycp[0], *oline);
       if ( op_index == -1 )  
           os_index = cvm_cptl ( sm_pts, sm_x, sm_y, &xcp[0], &ycp[0] );
   }
   
   /*
    * Special case 1: if FCP offline, return CL
    */
    
   if ( op_index < 0 && os_index < 0  ) {  
       *is = 0;
       *np = *npc;
       *ie = *np - 1;       
       for ( ii = 0; ii < *npc; ii++ ) {
            x [ ii ] = xc [ ii ];
            y [ ii ] = yc [ ii ];            
       }
       
       if ( *oline == 1 && *npc > 1 && 
            G_DIST( xc[0], yc[0], xc[*npc-1], yc[*npc-1] ) < PRECISION ) {
	   ( *np )--;
       }
     
       modified = 1; 
   }
   
   /* 
    * Special case 2: return OL - (a) if FCP =v/online and number of   
    * clicked points is less than 2 or greater than 'maxpts', 
    * (b) for closed lines, if FCP and LCP hits same point and number
    * of clicked points is less than 3 or CL is a straight line.
    */
   
   if ( modified != 1 ) {  	
       fang1 = cvm_angl( &xc[0], &yc[0], &xc[1], &xc[1] );
       fang2 = cvm_angl( &xc[0], &yc[0], &xc[*npc-1], &yc[*npc-1] );
       dfl = G_DIST( xc[0], yc[0], xc[*npc-1], yc[*npc-1] );
       
       if ( ( ( op_index != -1 || os_index != -1 )   && 
              ( *npc < 2 || *npc > *maxpts ) )           || 
	    ( ( *oline == 1 && ( dfl <= TIE_DIST ) ) &&
	      ( *npc <= 3 || G_ABS( fang1 - fang2 ) < PRECISION ) ) ) {
	     
           *is = 0;
	   *ie = *npo - 1;	   	    	                          
           *np = *npo;
           for ( ii = 0; ii < *npo; ii++ ) {
               x [ ii ] = xo [ ii ];
               y [ ii ] = yo [ ii ];
           }  

           modified = 1;	   
       }       
   }   
   
   /* 
    * Special Case 3: for closed lines, if FCP =v on unsmoothed OL,  
    * ignore specified smoothing level & modify without smoothing
    */

  if ( modified != 1 && *oline == 1 ) { 
       osd = - 1;
       opd = cvm_cptv ( *npo, xo, yo, &xcp[0], &ycp[0], *oline );
       if ( opd >= 0 )  {  
           cvm_mdfy( npo, xo, yo, npc, xcp, ycp, oline, maxpts, 
                     opd, osd, np, x, y, is, ie, iret );       
       
           cvm_index( np, is, ie, *oline ); 

           modified = 1;
       }
   }
   
   /* 
    * Normal Case 1: modify without smoothing
    */
   
   if ( modified != 1 && *sm_lvl != 1 && *sm_lvl != 2 )  {  
       cvm_mdfy( npo, xo, yo, npc, xcp, ycp, oline, maxpts, 
                 op_index, os_index, np, x, y, is, ie, iret );       
       
       cvm_index( np, is, ie, *oline ); /* Indexing for ghost line */

       modified = 1;
   }
   
   /* 
    * Normal Case 2: modify with smoothing
    */
  
   if ( modified != 1 ) {       
       cvm_mdfy( &sm_pts, sm_x, sm_y, npc, xcp, ycp, oline, &max_smth, 
                 op_index, os_index, &nnl, xnl, ynl, is, ie, iret );

       /*
        * Pick desired points from returnd new line to form final
	* unsmoothed new line.
	*/
      
      nn = 0;
      for ( ii = 0; ii < nnl; ii++ ) {
	  flag = - 1;
	            
	  for ( jj = 0; jj < *npc; jj++ ) {
	      if ( flag == - 1 &&   
	          G_ABS( ( xcp[ jj ] - xnl[ ii ] ) ) < PRECISION  &&
	          G_ABS( ( ycp[ jj ] - ynl[ ii ] ) ) < PRECISION ) {                  
		  x[ nn ] = xnl[ ii ];
                  y[ nn ] = ynl[ ii ];
	      	  nn++;		  
		  flag = 1;		  
	      }
	  }
          
	  if ( flag == - 1 ) {
	      for ( jj = 0; jj < *npo; jj++ ) {
	          if ( flag == - 1 &&   
	               G_ABS( ( xo[ jj ] - xnl[ ii ] ) ) < PRECISION   &&
	               G_ABS( ( yo[ jj ] - ynl[ ii ] ) ) < PRECISION ) {
		      x[ nn ] = xnl[ ii ];
                      y[ nn ] = ynl[ ii ];
                      nn++;
		      flag = 1;
		  }
	      }
	  }	  	  	  
      }

      /* 
       *  Eliminate duplicates for close lines when LCP=v on OL.
       */
      if ( *oline == 1 ) { 
          opd = cvm_cptv ( *npo, xo, yo, &xc[*npc-1], &yc[*npc-1], *oline );
          if ( opd >= 0 )  {
	      osd = -1;
	      for ( ii = 0; ii < nn-1; ii++ ) {	  
	          if ( osd == -1 &&
		       ( ( G_ABS( x[ii] - xc[*npc-1] ) < PRECISION  &&
	                   G_ABS( y[ii] - yc[*npc-1] ) < PRECISION  &&
	                   G_ABS( x[ii+1] - xo[opd] ) < PRECISION   &&
	                   G_ABS( y[ii+1] - yo[opd] ) < PRECISION ) || 
                         ( G_ABS( x[ii] - xo[opd] ) < PRECISION  &&
	                   G_ABS( y[ii] - yo[opd] ) < PRECISION  &&
	                   G_ABS( x[ii+1] - xc[*npc-1] ) < PRECISION &&
	                   G_ABS( y[ii+1] - yc[*npc-1] ) < PRECISION ) ) ) { 
                      osd = ii;
		  }
	      }
	      
	      if ( osd >= 0 ) {  /* shift to eliminate duplicate */
		  x[ osd ] = xc[ *npc - 1 ];
                  y[ osd ] = yc[ *npc - 1 ];
	          for ( ii = osd+1; ii < nn-1; ii++ ) {
		      x[ ii ] = x[ ii + 1 ];
                      y[ ii ] = y[ ii + 1 ];		  
		  }
		  nn--;
	      }
	  }	  
      }

      *np = nn;
          
      /* 
       * get indices of FCP/LCP on new line 
       */      
      
      nn = *np;
      if ( *oline == 1)  nn = *np - 1;
      flag = - 1;
      for ( ii = 0; ii < nn; ii++ ) { 
	   if ( flag == - 1 && 
	       G_ABS( ( xcp[ 0 ] - x[ ii ] ) ) < PRECISION &&
	       G_ABS( ( ycp[ 0 ] - y[ ii ] ) ) < PRECISION ) {
               *is = ii;
		flag = 1;
	   }
      }                 

      flag = - 1;
      for ( ii = *is + 1; ii < nn; ii++ ) { 
	   if ( flag == - 1 && 
	       G_ABS( ( xcp[ *npc -1 ] - x[ ii ] ) ) < PRECISION &&
	       G_ABS( ( ycp[ *npc -1 ] - y[ ii ] ) ) < PRECISION ) {
               *ie = ii;
	       flag = 1;
	   }
      }                 
                       
      cvm_index( np, is, ie, *oline ); /* Indexing for ghost line */
   }              

   
   /*
    * chop off the last point from a closed line to return an open
    * line, the 'line_closed' attribute is still reserved via 'oline'. 
    */

   if ( *oline == 1 && ( G_MAX ( op_index, os_index ) >= 0 ) )  {
	( *np )--;		
   }

   /*
    * Clean up 
    */

   free( xcp );
   free( ycp );
   free( sm_x );
   free( sm_y );
   free( xnl );
   free( ynl );

}


/*----------------------------------------------------------------------*
 *                   Private functions for CV_MDFY                      *
 *----------------------------------------------------------------------*
 */

void cvm_mdfy ( int *npo, float *xo, float *yo, int *npc, float	*xc, 
		float *yc, int *oline, int *maxpts, int opi, int osi, 
		int *np, float *x, float *y, int *is, int *ie, int *iret ) 
/************************************************************************
 * cvm_mdfy								*
 *									*
 * This function accepts a set of points to be graphically modified,    *
 * along with a set of points that have already been clicked, and       *
 * returns the resulting line set of points. Also returned are indices 	*
 * pointing to the beginning and end of the modified section.           *
 *									*
 * void cvm_mdfy ( npo, xo, yo, npc, xc, yc, oline, maxpts, opi, osi,   *
 *                 np, x, y, is, ie, iret )                             *
 * 	                                                                *
 * Input parameters:							*
 *	*npo	int	Number of points on original line 		*
 *	*xo	float	Original line x-coordinates		        *
 *	*yo	float	Original line y-coordinates		        *
 *	*npc	int	Number of clicked points		        *
 *	*xc	float	Clicked line x-coordinates		        *
 *	*yc	float	Clicked line x-coordinates		        *
 *      *oline	int	Line type of original line ( 0-open 1-closed )  * 
 *	*maxpts	int	Maximum number of points returned		*
 * 	opi	int	index of FCP on OL if FCP=v		        *
 * 	osi	int     index of FCP on OL if FCP online		*
 * 	                                                                *
 * Output parameters:							*
 *	*np	int     Number of points returned		        *
 *	*x	float   Returned line x-coordinates		        *
 *	*y	float   Returned line y-coordinates		        *
 *	*is	int     Starting index of modification                  *
 *	*ie	int     Ending index of modification                    *
 *	*iret	int     Return value                                    *
 *	                   = 0 - normal	return				*
 **									*
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 * J. Wu/GSC	10/00	Added line type 'oline'                         *
 * J. Wu/GSC    11/00   Made ghost line always follow cursor position   *
 * J. Wu/GSC    03/01   Fixed bug on assignment to "to_stop"		*
 ***********************************************************************/
{
   int   ii, epi, esi, new_npo, to_stop, modify, st, se;  
   float org_angle, new_angle, direction;
   	
  /* 
   * Initialize local variables 
   */

   *iret = 0;
   modify = - 1;
   direction = 0;
   new_npo = *npo;
   epi = -1;  /* index of LCP on OL if LCP=v      */
   esi = -1;  /* index of LCP on OL if LCP online */ 
   
   /*
    * Eliminate possible repeating points at the end of closed lines
    */
    
    if ( *oline == 1 ) {
        to_stop = 0;
        ii = *npo - 1;
        while ( to_stop == 0 && ii >= *npo / 2 ) {
            if ( G_DIST( xo[ii], yo[ii], xo[ii-1], yo[ii-1] ) < PRECISION )
	        new_npo--;  
            else
	        to_stop = 1;
	      
            ii--;
        }
    }
 
   /* 
    * Calculate the relative position of LCP on OL. 
    */
        
    epi = cvm_cptv ( new_npo, xo, yo, &xc[*npc-1], &yc[*npc-1], *oline );
    if ( epi == -1 ) 
        esi = cvm_cptl ( new_npo, xo, yo, &xc[*npc-1], &yc[*npc-1]);
          
   /* 
    * Calculate the angle between two line segments: FCP -> SCP  
    * (second clicked point) and OL[FCP] -> OL[FCP+1] 
    */
    
    st = G_MAX( opi, osi );
    se = st + 1;
    if ( *oline == 0 && st == ( new_npo - 1 ) ) {
         se = st;
	 st = se - 1;
    }   	            

    org_angle = cvm_angl( &xo[st], &yo[st], &xo[se], &yo[se]);    
    new_angle = cvm_angl( &xc[0], &yc[0], &xc[1], &yc[1]);   	
    direction = G_ABS(new_angle - org_angle);

   /* Modify OL according to the line type, modify direction and the
    * relative positions of FCP, LCP to OL.
    */
         
    if ( *oline == 0 ) {   /* Open line */        
	modify = cvm_opmd ( new_npo, xo, yo, *npc, xc, yc, *maxpts, 
                 opi, osi, epi, esi, &direction, np, x, y, is, ie );
    }        
    else {                /* Closed line */     	
	 modify = cvm_csmd ( new_npo, xo, yo, *npc, xc, yc, *maxpts, 
                  opi, osi, epi, esi, &direction, np, x, y, is, ie );    
    }
       
   /* 
    * If errors occurred, return OL 
    */
    
   if ( modify != 0 ) {
	*is = 0;
	*ie = *npo - 1;
	*np = *npo;
        for ( ii = 0; ii < *npo; ii++ ) {
            x [ ii ] = xo [ ii ];
            y [ ii ] = yo [ ii ];
        }
    }             
} 

/*---------------------------------------------------------------------*/   

int cvm_cptv ( int npi, float *xin, float *yin, float *xci, float *yci, int line ) 
/************************************************************************
 * cvm_cptv								*
 *									*
 * This function finds if a clicked point is within the tie distance    *
 * of any vortex on a given line. If within the tie distances of more   *
 * than two vortice, choose the closest one.                            *
 *									*
 * int cvm_cptv ( npi, xin, yin, xci, yci, line )                       *
 * 	                                                                *
 * Input parameters:							*
 *	npi	int	Number of points on input line	        	*
 *	*xin	float	x-coordinates of input line		        *
 *	*yin	float	y-coordinates of input line	                *
 *	*xci	float	x-coordinate of the clicked point		*
 *	*yci	float	y-coordinate of the clicked point		*
 *      line	int	type of input line ( 0 - open; 1 - closed )     *
 *									*
 * Output parameters:							*
 * 	cvm_cptv	int	opi - index of the matched vortex	*
 *				-1 - not within tie distance of any 	*
 *								vortex  * 
 **								        *
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 ***********************************************************************/
{
   int opi, ii, check_points;
   float min_dist, ds;
    
   opi = - 1;
   min_dist = TIE_DIST;
   
   check_points = npi;
   if ( line == 1 ) check_points = npi - 1;
   
   for ( ii = 0; ii < check_points; ii++) {
       ds = G_DIST(*xci, *yci, xin [ ii ], yin [ ii ] );  
       if ( ds <= min_dist ) {
           min_dist = ds;
           opi = ii;
       }
   }
   
   return opi;
}

/*---------------------------------------------------------------------*/   

int cvm_cptl ( int npi, float *xin, float *yin, float *xci, float *yci )
/************************************************************************
 * cvm_cptl								*
 *									*
 * This function finds if a clicked point is within the tie distance    *
 * of any line segments on a given line. If the clicked point is within *
 * the tie distances of more than two segments, choose the closest one. *
 * Note: call this function only when cv_cptv returns -1.               *  
 *									*
 * int cvm_cptl ( npi, xin, yin, xci, yci )                             *
 * 	                                                                *
 * Input parameters:							*
 *	npi	int	Number of points on input line	        	*
 *	*xin	float	x-coordinates of input line		        *
 *	*yin	float	y-coordinates of input line	                *
 *	*xci	float	x-coordinate of the clicked point		*
 *	*yci	float	y-coordinate of the clicked point		*
 *									* 
 * Output parameters:							*
 *	cvm_cptl	int osi - index of the first vortex on matched  *
 *                            segment                                   *
 **								        *
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 * J. Wu/GSC	11/00	Eliminated the original projection to OL        *
 ***********************************************************************/
{
   int ii, osi;
   float min_dist, dd, d_line, cond1, cond2;
   float at, bt, a, b, c, ds, dleft, dright;
   
   osi = - 1; 
   min_dist = TIE_DIST;
   d_line = TIE_DIST * 10.;
   
   for ( ii = 0; ii < npi - 1; ii++ ) {        
       at = xin [ ii + 1 ] - xin [ ii ];
       bt = yin [ ii + 1 ] - yin [ ii ]; 
       
       if ( G_DIFFT(at, 0.0F, GDIFFD ) && G_DIFFT(bt, 0.0F, GDIFFD) )
           d_line = G_DIST( *xci, *yci, xin[ ii ], yin[ ii ] );
       else if ( G_DIFFT(at, 0.0F, GDIFFD) && G_ABS( bt ) > 0.0F )
           d_line = G_ABS( *xci - xin[ ii ] );           
       else if ( G_DIFFT(bt, 0.0F, GDIFFD) && G_ABS( at ) > 0.0F )
           d_line = G_ABS( *yci - yin[ ii ] );                 
       else {       
           a =   1.0F / at ;
           b = - 1.0F / bt ;
           c = yin [ ii ] / bt - xin [ ii ] / at; 
           
           dd = a * ( *xci ) + b * ( *yci ) + c;
           d_line = G_ABS( dd / sqrt ( a * a + b * b ) );
       }
       
       if ( d_line <= min_dist ) {           
           dleft =  G_DIST( *xci, *yci, xin[ ii ], yin[ ii ]);           
           dright = G_DIST( *xci, *yci, xin[ ii+1 ], yin[ ii+1 ]);      
           ds = G_DIST( xin[ ii+1 ], yin[ ii+1 ], xin[ ii ], yin[ ii ]);
	   cond1 = dleft * dleft + ds * ds - dright * dright;
	   cond2 = dright * dright + ds * ds - dleft * dleft;
         	   	   
	   if ( cond1 >= 0. && cond2 >= 0. ) {	      
	       min_dist = d_line;
	       osi = ii;	        	
	    }            
        }
    }
    
    return osi;
}

/*---------------------------------------------------------------------*/

float cvm_angl ( float *xs, float *ys, float *xe, float *ye ) 
/************************************************************************
 * cvm_angl								*
 *									*
 * This function calculates the direction angle of a line segment       *
 * (measured from x-axis to the segment, anticlockwise, 0 ~ 360 )       *
 *									*
 * float cvm_angl ( xs, ys, xe, ye )                                    *
 * 	                                                                *
 * Input parameters:							*
 *	*xs	float	x-coordinate of the starting point	        *
 *	*ys	float	y-coordinate of the starting point              *
 *	*xe	float	x-coordinate of the ending point	        *
 *	*ye	float	y-coordinate of the ending point                *
 * 	                                                                * 
 * Output parameters:							*
 * 	cvm_angl	float	angle - anticlockwise angle of the 	*
 *								segment * 
 **								        *
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 ***********************************************************************/
{
    float dx, dy;
    float angle;
   
    angle = 90.0F;   /* default for dx = 0 */    
    dx = *xe - *xs;
    dy = *ye - *ys; 
    if ( !G_DIFFT(dx, 0.0F, GDIFFD) ) 
        angle = atan ( G_ABS( dy / dx ) ) * RTD;
    
    if ( dx > 0.0F && dy >  0.0F )  angle = 360.0F - angle;
    if ( dx < 0.0F && dy >= 0.0F )  angle = 180.0F + angle;
    if ( dx < 0.0F && dy < 0.0F )   angle = 180.0F - angle;
    
    return angle;
}


/*---------------------------------------------------------------------*/

void cvm_swap ( int npi, float *xin ) 
/************************************************************************
 * cvm_swap								*
 *									*
 * This function reverses the sequence of an array. The reversed array  *
 * is then stored in the original array                                 *
 *									*
 * void cvm_swap ( npi, xin )                                           *
 * 	                                                                *
 * Input parameters:							*
 *	npi	int	Number of elements in array	        	*
 *	*xin	float	elements of the array		                *
 *									*
 * Output parameters:							*
 * 	none     	                                                *
 **								        *
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 ***********************************************************************/
{
   int ii;
   float element;
 
   for ( ii = 0; ii < npi/2; ii++ ) {
       element = xin [ ii ];
       xin [ ii ] = xin [ npi - ii - 1 ];
       xin [ npi - ii - 1 ] = element; 
   }               
} 

/*---------------------------------------------------------------------*/

int cvm_opmd ( int npo, float *xo, float *yo, int npc, float *xc, 
		float *yc, int maxpts, int fin, int fis, int lin, 
		int lis, float *drct, int *np, float *x, float *y, 
		int *is, int *ie )
/************************************************************************
 * cvm_opmd								*
 *									*
 * This function accepts an open line to be graphically modified,       *
 * along with a set of points that have already been clicked, and       *
 * returns the resulting set of points.                                 *
 *									*
 * int cvm_opmd ( npo, xo, yo, npc, xc, yc, maxpts,			*
 *                fin, fis, lin, lis, drct,                             * 
 *                np, x, y, is, ie )                                    *
 * 	                                                                *
 * Input parameters:							*
 *	npo	int	Number of points on OL     		        *
 *	*xo	float	x-coordinates of OL		                *
 *	*yo	float   y-coordinates of OL		                *
 *	npc	int	Number of points on CL		                *
 *	*xc	float	x-coordinates of CL		                *
 *	*yc	float	y-coordinates of CL		                *
 *	maxpts	int	Maximum number of points returned		*
 * 	fin	int	index of FCP on OL when f FCP=v		        *
 *	fis	int     index of FCP on OL when FCP online		*
 *	lin	int     index of LCP on OL when LCP=v		        *
 *	lis	int     index of LCP on OL when LCP online		*
 *	*drct	float	angle between OL & CL segments                  *
 * 	                                                                *
 * Output parameters:							*
 *	*np	int     Number of points returned		        *
 *	*x	float   x-coordinates of returned line 		        *
 *	*y	float   y-coordinates of returned line		        *
 *	*is	int     Starting index of modified section              *
 *	*ie	int     Ending index of modified section                *
 *	cvm_opmd	int	0  - successfully modified              *
 *                              -1 - failed to modify        		*
 **									*
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 * J. Wu/GSC	10/00	Adjusted the midification rules		        *
 * J. Wu/GSC    11/00   Made ghost line always follow cursor position   *
 * J. Wu/GSC    12/00   Fixed bugs for case FCP=LCP                     *
 ***********************************************************************/
{
    int FCP, LCP, ii, totalpts, isp, iep, FCP_d, F_L;
    float dlast, dtmp, tmp;
    
    FCP_d = 0;   /* direction FCP->SCP  0 - same as OL  -1 - opposite  */
    F_L = 0;     /* direction FCP->LCP  0 - same as OL  -1 - opposite  */
    FCP = G_MAX ( fin, fis );
    LCP = G_MAX ( lin, lis );
    
    if ( fin >= 0 ) {
        FCP_d = cvm_drct( npo, xo, yo, npc, xc, yc, FCP, 0 );
        F_L = cvm_drct( npo, xo, yo, npc, xc, yc, FCP, 2 );
    }

    if ( fis >= 0 ) {     
        if ( *drct > 90. && *drct < 270. )   FCP_d = -1; 
        
	dlast = cvm_angl( &xc[0], &yc[0], &xc[npc-1], &yc[npc-1] );   	
        dtmp = cvm_angl( &xo[FCP], &yo[FCP], &xo[FCP+1], &yo[FCP+1] );   	
     
        tmp = G_ABS( dlast - dtmp );
        if ( tmp > 90. && tmp < 270. ) F_L = -1; 
    }
           
    /* 
     *  FCP & LCP both on line & satisfying internal replacement.
     */
            
    if ( LCP >= 0 && 
         ( ( FCP < LCP && FCP_d == 0 ) || 
           ( FCP > LCP && FCP_d == -1 ) ||
	   ( FCP == LCP && FCP_d == 0 && F_L == 0 ) || 
	   ( FCP == LCP && FCP_d == -1 && F_L == -1 ) ) ) {
        
	isp = G_MIN ( FCP, LCP );
        iep = G_MAX ( FCP, LCP );

        if ( ( FCP < LCP  && FCP_d == 0 && fin >= 0 ) ||
	     ( FCP == LCP && FCP_d == 0 && F_L == 0 && fin >= 0 ) )  
	    isp--; 
        
	if ( ( FCP > LCP && FCP_d == -1 ) || 
	     ( FCP == LCP && fis >= 0 && FCP_d == -1 && F_L == -1 ) ) {
	    cvm_swap(npc, xc);
	    cvm_swap(npc, yc);	            
	    if ( lin >= 0 ) isp--;	    	   
        }
        	
	*is = isp;              /* Starting index for modification */	
	*ie = isp + 1 + npc;    /* Ending index for modification */	
	if ( *is < 0 ) *is = 0;
	*np = isp + 1 + npc + npo - iep - 1;
	        
	/* 
         *  If total output points exceeds 'maxpts', no modification. 
         *  otherwise, link the segments together to form NL.
	 */	 
         if ( *np > maxpts )  return  -1;
        	
	 for ( ii = 0; ii <= isp; ii++ )  {
	      x [ ii ] = xo [ ii ];
	      y [ ii ] = yo [ ii ];
	 }
         totalpts = isp + 1;
	
	 for ( ii = 0; ii < npc; ii++)  {
	      x [ totalpts + ii ] = xc [ ii ];
	      y [ totalpts + ii ] = yc [ ii ];
	 } 
         totalpts = totalpts + npc;
			
	 for ( ii = iep + 1; ii < npo; ii++)  {
	      x [ totalpts + ii - iep - 1 ] = xo [ ii ];
	      y [ totalpts + ii - iep - 1 ] = yo [ ii ];
	 } 
        
         return 0; 
    }
          
    else  {  /* External replacement */
	
	if ( FCP_d == 0 ) { 

	    isp = FCP;
	    if ( fin >= 0 ) isp--;
	    *is = isp;
	    if ( *is < 0 ) *is = 0;
	    *np = isp + 1 + npc;
	    *ie = *np - 1;	    
	     
            if ( *np > maxpts ) return -1;
	    
	    for ( ii = 0; ii <= isp; ii++)  {
	         x [ ii ] = xo [ ii ];
	         y [ ii ] = yo [ ii ];
	    }
	    
	    for ( ii = 0; ii < npc; ii++)  {
	         x [ isp + ii + 1 ] = xc [ ii ];
	         y [ isp + ii + 1 ] = yc [ ii ];
	    }

	    return 0;
	}
	
	else {	    
	    cvm_swap(npc, xc);
	    cvm_swap(npc, yc);	     	     
	    iep = FCP;	    
	    *is = 0;
	    *ie = npc; 
	    *np = npc + npo - iep - 1;
	     	     
            if ( *np > maxpts ) return -1;
	    
	    for ( ii = 0; ii < npc; ii++)  {
	         x [ ii ] = xc [ ii ];
	         y [ ii ] = yc [ ii ];
	    }
	    	    
	    for ( ii = iep + 1; ii < npo; ii++ )  {
	         x [ npc + ii - iep - 1 ] = xo [ ii ];
	         y [ npc + ii - iep - 1 ] = yo [ ii ];
	    }            
	}

        return 0;
    }   	    
}
              
/*---------------------------------------------------------------------*/

int cvm_csmd ( int npo, float *xo, float *yo, int npc, float *xc, 
		float *yc, int maxpts, int fin, int fis, int lin, 
		int lis, float *drct, int *np, float *x, float *y, 
		int *is, int *ie )
/************************************************************************
 * cvm_csmd								*
 *									*
 * This function accepts an closed line to be graphically modified,     *
 * along with a set of points that have already been clicked, and       *
 * returns the resulting line set of points.                            *
 *									*
 * int cvm_csmd ( npo, xo, yo, npc, xc, yc, maxpts,                     *
 *                fin, fis, lin, lis, drct,                             *
 *                np, x, y, is, ie)                                     *
 * 	                                                                *
 * Input parameters:							*
 *	npo	int	Number of points on OL 		                *
 *	*xo	float	x-coordinates of OL		                *
 *	*yo	float	y-coordinates of OL		       		*
 *	npc	int	Number of points on CL		                *
 *	*xc	float	x-coordinates of OL		                *
 *	*yc	float	y-coordinates of OL		                *
 *	maxpts	int	Maximum number of points returned		*
 * 	fin	int	index of FCP on OL when FCP=v 		        *
 *	fis	int     index of FCP on OL when FCP online		*
 *	lin	int     index of LCP on OL when LCP=v 		        *
 *	lis	int     index of LCP on OL when LCP online		*
 *	*drct	float	angle between OL & CL segments                  *
 * 	                                                                *
 * Output parameters:							*
 *	*np	int     Number of points returned		        *
 *	*x	float   x-coordinates of returned line		        *
 *	*y	float   y-coordinates of returned line		        *
 *	*is	int     Starting index of modified section              *
 *	*ie	int     Ending index of modified section                *
 *	cvm_csmd	int	0  - successfully modified              *
 *                              -1 - failed to modify        		*
 **									*
 * Log:									*
 * J. Wu/GSC	08/00	Created					        *
 * J. Wu/GSC    10/00   Fixed indices of ghost line                     *
 * J. Wu/GSC    11/00   Made ghost line always follow cursor position   *
 * J. Wu/SAIC	09/01	Removed duplicates of FCP/LCP when they are =v	*
 * J. Wu/SAIC	09/01	Removed duplicates when LCP hits FP on OL	*
 * J. Wu/SAIC	11/01	Reworked on duplicates problem			*
 ***********************************************************************/
{
    int FCP, LCP, ii, totalpts, isp, iep;
    int FCP_drct, LCP_drct, FCP_to_LCP;
    float ds, min_dist, dlast, dtmp, tmp;
    
    totalpts = 0;
    min_dist = 1e10;
    LCP_drct = 0;    /* OL[LCP] -> CL[LCP], when LCP off line */
    FCP_drct = 0;    /* FCP -> SCP                            */	
    FCP_to_LCP = 0;  /* CL[0] -> CL[ npc - 1 ]                */	
        
    if ( fin == npo - 1 )  fin = 0;
    FCP = G_MAX ( fin, fis );
    LCP = G_MAX ( lin, lis );

    /*
     * If FCP & LCP hits same vortex, return CL
     */
     
    if ( G_DIST( xc[0], yc[0], xc[npc-1], yc[npc-1] ) <= TIE_DIST ) {
        *is = 0;
        *np = npc;
        *ie = *np - 1;       
        for ( ii = 0; ii < npc; ii++ ) {
            x [ ii ] = xc [ ii ];
            y [ ii ] = yc [ ii ];
       }

       return 0;
    }
    
    /* 
     * If LCP is not on original line, find the closest vortex to it. 
     */
       
    if ( LCP < 0 ) {
        for ( ii = 0; ii < npo - 1; ii++) {
             ds = G_DIST( xc[npc-1], yc[npc-1], xo[ii], yo[ii] ); 
	     if ( ds <= min_dist ) {
                 min_dist = ds;
                 LCP = ii;
             } 
        }       
    }

    /* 
     * Calculate the starting modify direction (FCP_drct),
     * the ending modify direction (LCP_drct, if LCP offline)
     * and the direction from segment FCP->LCP relative to OL.
     */
          
    if ( fis >= 0 ) { /* FCP online */      
         if ( *drct > 90. && *drct < 270. )  FCP_drct = - 1; 

         dlast = cvm_angl( &xc[0], &yc[0], &xc[npc-1], &yc[npc-1] );   	
         dtmp = cvm_angl( &xo[FCP], &yo[FCP], &xo[FCP+1], &yo[FCP+1] );   	
     
         tmp = G_ABS( dlast - dtmp );
         if ( tmp > 90. && tmp < 270. ) FCP_to_LCP = - 1; 
    }
     
    if ( fin >= 0 ) { /* FCP =v */
         FCP_drct = cvm_drct( npo, xo, yo, npc, xc, yc, FCP, 0 );
         FCP_to_LCP = cvm_drct( npo, xo, yo, npc, xc, yc, FCP, 2 );
    }
                
    if ( lin == -1 && lis == -1 ) {  /* LCP offline */
        LCP_drct = cvm_drct( npo, xo, yo, npc, xc, yc, LCP, 1 );
    }    

    /* 
     * Choose a proper vortex on OL to connect with LCP if LCP offline. 
     */

    if ( lin == - 1 && lis == - 1 ) { 
        if ( FCP != LCP ) {
	    if ( fin >= 0 || ( fis >= 0 && FCP_drct >= 0 ) ) {
	        if ( FCP_drct >= 0 && LCP_drct >= 0 ) {
	            LCP++;
	            if ( FCP_to_LCP >= 0 && FCP == LCP )  LCP--;
                }	    
            
	        if ( FCP_drct < 0  &&  LCP_drct < 0 ) {
                    if ( FCP == ( npo - 2 ) && LCP == 0 )
		         LCP = FCP - 1;
		    else {
	                LCP--;	
	                if ( FCP_to_LCP < 0 && FCP == LCP ) LCP++;
		    }
                }	        	   
	    }
	    
	    if ( fis >= 0 && FCP_drct < 0 ) {
		if ( LCP_drct < 0 && FCP_to_LCP < 0 ) {
		    FCP++;
		    LCP--;
	        }
		else 
		    FCP++;
	    }
	                	    
            if ( LCP < 0 ) LCP = npo - 2; 
            if ( FCP > ( npo - 2 ) ) FCP = npo - 2; 
	    
	}
	else {
             if ( fin >= 0 ) { /* FCP =v */
	         if ( FCP_to_LCP >= 0 ) 
		     LCP++;
		 else {       /* FCP online */
		     LCP--;
		     if ( lin >= 0 ) FCP++;
		 }
                 if ( LCP < 0 ) LCP = npo - 2; 
	     }
	     else {
	         if ( FCP_drct >= 0 && FCP_to_LCP >= 0 ) LCP++;		 
		 if ( FCP_drct < 0  ) {
	             if ( LCP_drct >= 0 && FCP_to_LCP < 0 ) 
		         FCP++;
		     else if ( LCP_drct < 0 && FCP_to_LCP < 0 ) {
		         FCP++;
		         LCP--;
		    }
		 
		 }
	     }
	}	               
    } 
       
    /*
     * Case 1 : FCP & LCP fall on same segment of OL
     */
 
    if ( FCP == LCP ) {
	if ( FCP_to_LCP < 0 && FCP_drct < 0 ) {
	    cvm_swap( npc, xc );             
            cvm_swap( npc, yc );
	}                

	/* 
	 * a: CL is insert at begining of NL & closed 
	 */
	 
	isp = 0;
	iep = npc - 1;   
	*is = isp;
	*np = npc;
	
	for ( ii = 0; ii < npc; ii++ ) {
	    x [ ii ] = xc [ ii ];
	    y [ ii ] = yc [ ii ];
        }

	if ( fis >= 0 && lin == -1 && lis == -1 ) {
	    x [ npc ] = xo [ FCP ];
	    y [ npc ] = yo [ FCP ];	    

	    x [ npc + 1 ] = xc [ 0 ];
	    y [ npc + 1 ] = yc [ 0 ];	    	     	
	    
	    *np = * np + 2;
	    *ie = * np - 1;

	    return 0;
	}
	
	if ( ( fin >= 0 && lis >= 0  && FCP_drct < 0 )   ||
	     ( fin >= 0 && lin == -1 && lis == -1 )      ||
             ( fis >= 0 && lin >= 0  && FCP_drct >= 0 )  ||
             ( fis >= 0 && lis >= 0  && (
	         ( FCP_to_LCP < 0 && FCP_drct >= 0 ) ||
	         ( FCP_to_LCP >= 0 && FCP_drct < 0 ) ) )
           ) {		   	   
	   
	    x [ npc ] = xc [ 0 ];
	    y [ npc ] = yc [ 0 ];	    	     	
	    
	    *np = * np + 1;
	    *ie = *np - 1;
	    
	    return 0;
	}
	
	/* 
	 * b: CL is inserted into middle of NL 
	 */
				
        isp = FCP;
	iep = FCP + 1;
  	   
	/* 
	 * Starting segment 
	 */
	
	*is = isp;
	*np = isp + 1;
	
	for ( ii = 0; ii <= isp; ii++ ) {
	    x[ ii ] = xo [ ii ];
	    y[ ii ] = yo [ ii ];
        }
	    
	/* 
	 * Middle segment 
	 */
	 
	 if ( (fin >= 0 && lis >= 0 && FCP_drct >= 0 ) ||  
	      (fis >= 0 && lin >= 0 && FCP_drct < 0  ) ) { 
	     *np = *np + npc - 1;
	     for ( ii = 1; ii < npc; ii++ ) {
	         x[ ii + isp ] = xc [ ii ];
	         y[ ii + isp ] = yc [ ii ];
	     }
	 }	           

	 if ( fis >= 0 && lis >= 0 && (
	          ( FCP_to_LCP >= 0 &&  FCP_drct >= 0  ) ||  
	          ( FCP_to_LCP < 0 &&  FCP_drct < 0  ) ) )   { 
	     *np = *np + npc;
	     for ( ii = 0; ii < npc; ii++ ) {
	         x[ ii + isp + 1 ] = xc [ ii ];
	         y[ ii + isp + 1 ] = yc [ ii ];
	     }
	 }	           
	    
	 *ie = *np;
	 
	 /* 
	  * Ending segment 
	  */
	 
	 for ( ii = iep; ii < npo; ii++ ) {
	     x[ ii + *np - iep ] = xo [ ii ];
	     y[ ii + *np - iep ] = yo [ ii ];
	 }	           	
	
	 *np = *np + npo - iep; 	    
         if ( *ie > ( *np - 1 ) )  *ie = *np - 1;
	 
	 return 0;
    }


    /* 
     * Case 2: FCP, LCP fall on different segments of OL
     */
       
    if ( FCP_drct >= 0 ) { /* CL runs same direction of original */
        isp = FCP;
	iep = LCP;
	if ( lis >= 0 ) iep = LCP + 1;
    }        
    else  {               /* CL runs opposite direction of original */
        isp = LCP;
	iep = FCP;
	if ( fis >= 0 && ( lin >= 0 || lis >= 0 ) )  iep = FCP + 1; 
         	
	cvm_swap( npc, xc );             
        cvm_swap( npc, yc );                
    }
            
    /* 
     *  Join the line segments together to form the new line
     */
                   
    if ( ( FCP_drct >= 0 && FCP < LCP ) || 
         ( FCP_drct <  0 && FCP > LCP ) )  {
    	 
        /* If expected total number of output points is greater than
         * the allocated space ( maxpts ), return -1 
         */
	 
        totalpts = isp + 1 + npc + npo - iep;
        if ( totalpts > maxpts ) return -1;
	
        /* 
	 * Starting segment 
	 */
        	
	*is = isp;
	*np = isp + 1; 
	
	if ( lin == - 1 && lis == - 1 && isp < 0 ) isp = 0;
	     
	for ( ii = 0; ii <= isp; ii++ ) {
	     x[ ii ] = xo [ ii ];
	     y[ ii ] = yo [ ii ];
	}
	
	/* 
	 * Middle segment 
	 */

	if ( G_DIST( xc[0], yc[0], xo[isp], yo[isp] ) <= TIE_DIST ) { 
	    for ( ii = 0; ii < npc; ii++ ) {
	        x[ ii + *np - 1 ] = xc [ ii ];
	        y[ ii + *np - 1 ] = yc [ ii ];	     
	    }
	    *np = *np + npc - 1;
	}
	else {
	    for ( ii = 0; ii < npc; ii++ ) {
	         x[ ii + *np ] = xc [ ii ];
	         y[ ii + *np ] = yc [ ii ];
	    }
	    *np = *np + npc;
	}	           

	*ie = *np;
	 	           		
	/* 
	 * Ending segment 
	 */

	if ( G_DIST( xc[npc-1], yc[npc-1], xo[iep], yo[iep] ) <= TIE_DIST ) {
	    iep++; 
	}
	
	for ( ii = iep; ii < npo; ii++ ) {
	    x[ ii + *np - iep ] = xo [ ii ];
	    y[ ii + *np - iep ] = yo [ ii ];
	}
	*np = *np + npo - iep;
		
	return 0;
   }
   
   else {             	 
        totalpts = npc + isp - iep + 2;
        if ( totalpts > maxpts ) return  -1;

        /* 
	 * Starting segment 
	 */
		
	*is = 0; 
	*np = npc;
	for ( ii = 0; ii < npc; ii++ ) {
	     x[ ii ] = xc [ ii ];
	     y[ ii ] = yc [ ii ];
	}	           	
	
        *ie = *np; 
		
	/* 
	 * Ending segment 
	 */
	 	
	if ( fin >= 0 && lin >= 0 )  {
	    *np = *np + ( isp - iep - 1 );
	    for ( ii = iep + 1; ii < isp; ii++ ) {
	         x[ npc + ii - iep - 1 ] = xo [ ii ];
	         y[ npc + ii - iep - 1 ] = yo [ ii ];
	    }
	                
	    if ( G_DIST( xc[0], yc[0], xo[isp], yo[isp] ) > TIE_DIST ) { 
	        x[ *np ] = xo [ isp ];	      		
	        y[ *np ] = yo [ isp ];	      		
	        (*np)++;
 	    }
	    
	    x[ *np ] = xc [ 0 ];
	    y[ *np ] = yc [ 0 ];	      	    
	    (*np)++;
	}	           	
	 
	else if ( ( FCP_drct >= 0 && FCP < LCP && fin >= 0  && lin == -1 ) ||
                  ( FCP_drct < 0 && FCP > LCP && fin == -1 && lin >= 0 ) )  {
	    *np = *np + isp - iep + 1;
	    for ( ii = iep; ii <= isp; ii++ ) {
	         x[ npc + ii - iep ] = xo [ ii ];
	         y[ npc + ii - iep ] = yo [ ii ];
            }
	}
	
	else if ( ( FCP_drct >= 0 && FCP < LCP && fin == -1 && lin >= 0 ) ||
                  ( FCP_drct < 0 && FCP > LCP && fin >= 0  && lin == -1 ) ) {
	    *np = *np + isp - iep + 1;
	    for ( ii = iep + 1; ii <= isp; ii++ ) {
	         x [ npc + ii - iep - 1 ] = xo [ ii ];
	         y [ npc + ii - iep - 1 ] = yo [ ii ];
	    }
	    x [ *np - 1] = xc [ 0 ];
	    y [ *np - 1] = yc [ 0 ];	    
	}   
	
	else  {
	    ds = G_DIST( xc[npc-1], yc[npc-1], xo[iep], yo[iep] );
	    if ( ds <= TIE_DIST ) iep++; /* avoid duplicate */
	    *np = *np + isp - iep;
	    for ( ii = iep; ii < isp; ii++ ) {
	         x [ npc + ii - iep ] = xo [ ii ];
	         y [ npc + ii - iep ] = yo [ ii ];
	    }	           

	    if ( G_DIST( xc[0], yc[0], xo[isp], yo[isp] ) > TIE_DIST ) {
	        x[ *np ] = xo [ isp ];	      		
	        y[ *np ] = yo [ isp ];	      		
	        (*np)++;
 	    }

 	    x[ *np ] = xc [ 0 ];
	    y[ *np ] = yc [ 0 ];	      
	    (*np)++;
	}	
         	             
	return 0;
    }	
}
 
/*---------------------------------------------------------------------*/

void cvm_index ( int *np, int *is, int *ie, int line ) 
/************************************************************************
 * cvm_index								*
 *									*
 * This function determines the start and end index of the ghost line   *
 * on an input line.						        *
 *									*
 * void  cvm_index ( np, is, ie, line )                                 *
 * 	                                                                *
 * Input parameters:							*
 *	*np	int	Number of points on input line  	        *
 *	*is	int	Start index for ghost line                      *
 *	*ie	int	End index for ghost line 	                *
 *	line	int	type of inout line ( 0 - open  1- close )       *
 * 	                                                                * 
 * Output parameters:							*
 *              None							*
 **								        *
 * Log:									*
 * J. Wu/GSC	10/00	Created					        *
 * J. Wu/GSC    11/00   Eliminated overlapping when *is > *ie           *
 ***********************************************************************/
{	   
    /* 
     * Adjust starting index backward & end index forward by number of
     * 'GHOST_POINTS'.
     */
     
    *is = *is - GHOST_POINTS;
    *ie = *ie + GHOST_POINTS;
    
    /* 
     * If there are limited points on input line or the adjusted
     * starting & end indices overlap, draw whole line as ghost line.     
     * Otherwise, ghost line starts at '*is' and goes to '*ie' if 
     * *is < *ie,  or starts from '*is' and passes the end of line  
     * ( also the beginning ) and then goes to '*ie' when *is > *ie. 
     */
     
    if ( *np <= 2 * GHOST_POINTS ) {
	*is = 0;
	*ie = *np - 1;
    }
    else {         
        if ( line == 1 ) {  /* closed line */	    	    
	    if ( *is >= 0 && *ie > *np - 1 ) {
	        *ie = *ie - *np;
		if ( *ie >= *is ) { 
		    *is = 0;
		    *ie = *np - 1;
		}
	    }
	    else if ( *is < 0 && *ie <= *np - 1 ) {
	        *is = *is + *np;
		if ( *is <= *ie ) { 
		    *is = 0;
		    *ie = *np - 1;
		}		
	    }
	    else if ( *is < 0 && *ie > *np - 1 ) {
		*is = 0;
		*ie = *np - 1;	    
	    }	    
        }
        else  {   /* open line */
            if ( *is < 0 )  *is = 0;
            if ( *ie > *np - 1 )  *ie = *np - 1;       		
        }
    }        
}

/*----------------------------------------------------------------------*/
  
int cvm_drct ( int npo, float *xo, float *yo, int npc, float *xc, 
		float *yc, int indx, int flag )
/************************************************************************
 * cvm_drct								*
 *									*
 * This function determines the relative direction of an given segment  *
 * to the direction of original line.                                   *
 * 	                                                                *
 * int cvm_drct ( npo, xo, yo, npc, xc, yc, indx, flag )                *
 * 	                                                                *
 * Input parameters:							*
 *	npo	int	Number of points on OL 		                *
 *	*xo	float	x-coordinates of OL		                *
 *	*yo	float	y-coordinates of OL		       		*
 *	npc	int	Number of points on CL		                *
 *	*xc	float	x-coordinates of OL		                *
 *	*yc	float	y-coordinates of OL		                *
 *	indx	int	Starting index of the given segment 		*
 *	flag	int	0 - calculate direction for FCP->SCP 		*
 *			1 - calculate direction for xo[LCP]->LCP 	*
 *			2 - calculate direction for FCP->LCP 		*
 * 	                                                                *
 * Output parameters:							*
 *   cvm_drct	int     0 - The given segment runs same direction as OL	*
 *                     -1 - The given segment runs opposite direction   *
 *								  as OL *
 **									*
 * Log:									*
 * J. Wu/GSC	10/00	Created					        *
 ***********************************************************************/
{ 
    int direction;
    float  alpha, beta, angc, ang_from, ang_to, tmp;
    
    direction = 0;
    
    angc = cvm_angl( &xc[0], &yc[0], &xc[1], &yc[1] );
    if ( flag == 1 ) 
        angc = cvm_angl( &xo[indx], &yo[indx], &xc[npc-1], &yc[npc-1] );
    if ( flag == 2 ) 
        angc = cvm_angl( &xc[0],& yc[0], &xc[npc-1], &yc[npc-1] );

    alpha = cvm_angl( &xo[indx], &yo[indx], &xo[indx+1], &yo[indx+1]);    
    if ( indx > 0 ) 
        beta = cvm_angl( &xo[indx], &yo[indx], &xo[indx-1], &yo[indx-1]);    
    else
        beta = cvm_angl( &xo[indx], &yo[indx], &xo[npo-2], &yo[npo-2]);    
           
    tmp = G_MAX( alpha, beta );
    ang_from = tmp - G_ABS( beta - alpha ) / 2;
    ang_to = ang_from + 180.;	
    
    if ( beta >= alpha ) {    
        if ( ang_to <= 360. )  {
	    if ( angc > ang_from  && angc < ang_to )  direction = -1;
	}
        else {
	    ang_to = ang_to - 360.;
            if ( angc <= ang_to  || angc >= ang_from ) direction = -1;
	}
    }
    else {
        if ( ang_to <= 360. )  {
            if ( angc <= ang_from || angc >= ang_to )  direction = -1;
	}
        else {
	    ang_to = ang_to - 360.;
            if ( angc > ang_to && angc < ang_from ) direction = -1;
	}
    }	              

    return direction;
}

