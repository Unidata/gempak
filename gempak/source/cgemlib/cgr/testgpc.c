#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

#define	SUBJECT	0
#define	CLIP	1
#define	RESULT	2
#define	ALL	3


int main ( void )
/************************************************************************
 * TESTGPC								*
 *									*
 * This program tests the GPC contributed library public functions.	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/04						*
 * D.W.Plummer/NCEP	10/06	Added GPC_SET_EPSILON			*
 ***********************************************************************/
{
int		ii, cont, numsub, which, ier;
int		operation, readholeflag, writeholeflag, hole;
int		nverts=0;
double		e;
char		select[8];
float		xv[LLMXPT], yv[LLMXPT];
char		filnam[256], buffer[80];
int		pagflg;
char    	errgrp[8];
FILE		*fpread, *fpwrite;
gpc_polygon	subject_polygon, clip_polygon, result_polygon;
gpc_vertex_list	contour;
/*---------------------------------------------------------------------*/
    cont = G_FALSE;

    /*
     * Structure initializations.
     */
    subject_polygon.num_contours = 0;
    subject_polygon.hole         = (int*)NULL;
    subject_polygon.contour      = (gpc_vertex_list*)NULL;
    clip_polygon.num_contours    = 0;
    result_polygon.num_contours  = 0;
    contour.vertex 		 = (gpc_vertex*)NULL;
    contour.num_vertices 	 = 0;

    while ( cont == G_FALSE ) {
        printf ( "\n\n" );
        printf ( "*** ORIGINAL GPC PUBLIC FUNCTIONS ***\n");
        printf ( "   1 = GPC_READ_POLYGON    2 = GPC_WRITE_POLYGON \n");
        printf ( "   3 = GPC_ADD_CONTOUR     4 = GPC_POLYGON_CLIP \n");
        printf ( "   5 = GPC_FREE_POLYGON     \n");
        printf ( "*** GPC PUBLIC FUNCTION ADD-ONS ***\n");
        printf ( "  11 = GPC_CREATE_VERTEX_LIST   \n");
	printf ( "  12 = GPC_GET_VERTEX_LIST      \n");
	printf ( "  13 = GPC_GET_VERTEX_AREA      \n");
	printf ( "  14 = GPC_SET_EPSILON      \n");
        printf ( "*** HELP ***\n");
	printf ( "  99 = HELP on INPUT FILE FORMATS \n");
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

	    printf("Make sure your polygon file is in the proper format:\n");

	    printf("<num-contours>\n");
	    printf("<num-vertices-in-first-contour>\n");
	    printf("[<first-contour-hole-flag>]\n");
	    printf("<vertex-list>\n");
	    printf("<num-vertices-in-second-contour>\n");
	    printf("[<second-contour-hole-flag>]\n");
	    printf("<vertex-list> \n");
	    printf("etc...\n");

	    printf ( "Enter filename to read polygon from : \n");
	    scanf ( " %s", filnam );

	    printf ( "Enter whether file format contains hole flags (%d-FALSE,%d-TRUE) :\n",
		  G_FALSE, G_TRUE );
	    scanf ( " %d", &readholeflag );

	    printf ( "Enter which polygon (%d-SUBJECT,%d-CLIP) :\n",
		  SUBJECT, CLIP );
	    scanf ( " %d", &which );

	    fpread = (FILE *)cfl_ropn ( filnam, "", &ier );
	    if ( ier == G_NORMAL )  {
	      if ( which == SUBJECT )
	        gpc_read_polygon ( fpread, readholeflag, &subject_polygon );
	      else if ( which == CLIP )
	        gpc_read_polygon ( fpread, readholeflag, &clip_polygon );
	      else
		printf("Invalid polygon type\n");
	      cfl_clos ( fpread, &ier );
	    }
	    else  {
	      printf("Unable to open file %s\n", filnam );
	    }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 2 ) {

	    printf ( "Enter filename to write polygon to : \n");
	    scanf ( " %s", filnam );

	    printf ( "Enter the write hole flag (%d-FALSE,%d-TRUE):\n",
		  G_FALSE, G_TRUE );
	    scanf ( " %d", &writeholeflag );

	    printf ( "Enter which polygon (%d-SUBJECT,%d-CLIP,%d-RESULT):\n",
		  SUBJECT, CLIP, RESULT );
	    scanf ( " %d", &which );

	    fpwrite = (FILE *)cfl_wopn ( filnam, &ier );
	    if ( ier == G_NORMAL )  {
	      if ( which == SUBJECT )
	        gpc_write_polygon ( fpwrite, writeholeflag, &subject_polygon );
	      else if ( which == CLIP )
	        gpc_write_polygon ( fpwrite, writeholeflag, &clip_polygon );
	      else if ( which == RESULT )
	        gpc_write_polygon ( fpwrite, writeholeflag, &result_polygon );
	      else
		printf("Invalid polygon type\n");
	      cfl_clos ( fpwrite, &ier );
	    }
	    else  {
	      printf("Unable to open file %s\n", filnam );
	    }


        }
/*---------------------------------------------------------------------*/
	if ( numsub == 3 ) {

	    if ( nverts == 0 )  {
		printf("Must first create a vertex list (option 11)\n");
	    }
	    else  {

	        printf ( "Enter which polygon (%d-SUBJECT,%d-CLIP,%d-RESULT) to add vertex list to:\n",
		      SUBJECT, CLIP, RESULT );
	        scanf ( " %d", &which );

	        printf ( "Enter the hole flag (%d-HOLE,%d-NOT A HOLE):\n",
		      G_TRUE, G_FALSE );
	        scanf ( " %d", &hole );

	        if ( which == SUBJECT )  {
		    gpc_add_contour ( &subject_polygon, &contour, hole );
		}
		else if ( which == CLIP )  {
		    gpc_add_contour ( &clip_polygon, &contour, hole );
		}
		else {
		    printf("Invalid polygon\n");
		}
	    }

	}
/*---------------------------------------------------------------------*/
	if ( numsub == 4 ) {

	    printf ( "Enter operation (%d-GPC_DIFF,%d-GPC_INT,%d-GPC_XOR,%d-GPC_UNION):\n",
		  GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION );
	    scanf ( " %d", &operation );

	    gpc_polygon_clip ( operation, &subject_polygon,
		    &clip_polygon, &result_polygon );

	}
/*---------------------------------------------------------------------*/
        if ( numsub == 5 ) {

	    printf ( "Enter which polygon (%d-SUBJECT,%d-CLIP,%d-RESULT,%d-ALL) to free contours:\n ",
		  SUBJECT, CLIP, RESULT, ALL );
	    scanf ( " %d", &which );

	    if ( which == SUBJECT || which == ALL )  {
    	        if ( subject_polygon.num_contours != 0 )
		    gpc_free_polygon ( &subject_polygon );
	    }
	    else if ( which == CLIP || which == ALL )  {
    	        if ( clip_polygon.num_contours != 0 )
        	    gpc_free_polygon ( &clip_polygon );
	    }
	    else if ( which == RESULT || which == ALL )  {
    	        if ( result_polygon.num_contours != 0 )
        	    gpc_free_polygon ( &result_polygon );
            }


        }
/*---------------------------------------------------------------------*/
        if ( numsub == 11 ) {

	    printf ( "Enter either the number of points in polygon (to be followed by entering the points), or a filename to read points from: \n");
	    scanf ( " %s", filnam );

	    cst_numb ( filnam, &nverts, &ier );

	    if ( ier == 0 )  {
	        for ( ii = 0; ii < nverts; ii++ )
		    scanf ( "%f %f", &(xv[ii]), &(yv[ii]) );
    	        if ( contour.vertex != (gpc_vertex*)NULL )
		    free ( contour.vertex );
	        gpc_cvlist ( nverts, xv, yv, &contour, &ier );
	    }
	    else  {
	        printf ( "Note that the file format is simply a list of coordinate pairs separated by whitespace.\nThe number of points will be counted automatically. For instance, a file containing:\n0 0\n0 1\n1 1\nyields a vertex list of three points.\n\n");
		nverts = 0;
	        fpread = (FILE *)cfl_tbop ( filnam, "", &ier );
	        if ( ier == G_NORMAL )  {
		    cfl_trln ( fpread, sizeof(buffer), buffer, &ier );
		    while ( ier == 0 )  {
			sscanf ( buffer, "%f %f", 
			    &(xv[nverts]), &(yv[nverts]) );
			    nverts += 1;
			cfl_trln ( fpread, sizeof(buffer), buffer, &ier );
		    }
		    printf("EOF reached in file %s, number of vertices = %d\n", 
			    filnam, nverts );
		    cfl_clos( fpread, &ier );
    	            if ( contour.vertex != (gpc_vertex*)NULL )
		    	free ( contour.vertex );
	            gpc_cvlist ( nverts, xv, yv, &contour, &ier );
		}
	    }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 12 ) {

	    gpc_gvlist ( &contour, &nverts, xv, yv, &ier );

	    printf("gpc_gvlist, ier = %d\n", ier );

	    printf("Number of vertices = %d\n", nverts );
	    for ( ii = 0; ii < nverts; ii++ )
		printf ( "%d - %f %f\n", ii, xv[ii], yv[ii] );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 13 ) {

	    printf ( "Area of contour is %f\n", gpc_gvarea(&contour) );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 14 ) {

	    scanf ( " %lf", &e );

	    gpc_set_epsilon ( e );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 99 ) {

	    pagflg = G_FALSE;
	    strcpy ( errgrp, "TESTGPC" );
	    ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );

        }
/*---------------------------------------------------------------------*/
    }

    if ( subject_polygon.num_contours != 0 )
	gpc_free_polygon ( &subject_polygon );
    if ( clip_polygon.num_contours != 0 )
        gpc_free_polygon ( &clip_polygon );
    if ( result_polygon.num_contours != 0 )
        gpc_free_polygon ( &result_polygon );
    if ( contour.vertex != (gpc_vertex*)NULL )
	free ( contour.vertex );

    return(0);

}
