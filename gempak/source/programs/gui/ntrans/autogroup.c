#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "Nxm.h"

/************************************************************************
* autogroup.c                                                		*
*                                                                  	*
*   Automatically groups the loaded frames based on the naming     	*
*       convention.                                                	*
*                                                                  	*
*   Log:                                                           	*
*   Chien Lin/EAI      09/93  ntrans1                              	*
*   G. Krueger/EAI     09/97  Changed NxmWarning -> NxmWarn_show   	*
*   P.Bruehl/NWS       11/97  group_feature for short short titles 	*
************************************************************************/
/* Number of groups in the metafile */

void group_feature ( int nn, int type, size_t nchar );
void check_frame ( int type, size_t nchar, char *gpname, int sframe, short *status );

/*=====================================================================*/

void autogroup ( void )
{
int	nn;
char    message[200];

/*---------------------------------------------------------------------*/

	nn = meta_st.num_frames;

	GroupNo = 0;

	group_feature(nn, 0, 9);   /* group on parameter name */

        if ( GroupNo < ( MAX_NO_GROUPS - 1 ) )
	    group_feature(nn, 1, 9); /* group on valid time */
	else {
	    sprintf(message, "Warning from auto-group :\n  Number of groups exceeds the maximum allowed.\n Only %d groups were created.\n", 
			MAX_NO_GROUPS);
	    NxmWarn_show(DrawingW, message);
	}

	add_grouplist();
}

/*=====================================================================*/

void group_feature ( int nn, int type, size_t nchar )
/************************************************************************
 *   fuction group_feature. 						*
 *   auto-grouping based on the short title 				*
 *  type 0 --- grouping based on the characters after "nchar".		*
 *  type 1 --- grouping based on the first "nchar" characters.		*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	10/04	Changed MAX_PIXMAP to MAX_FRAME_GROUP	*
 ***********************************************************************/
{
int	ii, jj, outflag;
char	gpname[100];
char    message[200];
short	status[MAXFRAMES];

/*---------------------------------------------------------------------*/

	for ( ii = 0; ii < nn; ii++ ) 
		status[ii] = 0;

	for( ii = 0; ii < nn; ii++ ) { 

	    if ( GroupNo > (MAX_NO_GROUPS - 1) ) {

		sprintf(message, "Warning from auto-group :\n  Number of groups exceeds the maximum allowed.\n Only %d groups were created.\n", 
			MAX_NO_GROUPS);

		NxmWarn_show(DrawingW, message);
		break;
	    }

	    if ( status[ii] == 0 ) {

		status[ii] = 1;

		if ( type == 0 ) {

                  if (strlen((meta_st.frame[ii]).label) > nchar) 
			strcpy( gpname, &((meta_st.frame[ii]).label[nchar]));
                  else
			strcpy( gpname, (meta_st.frame[ii]).label);

		  strcpy( GroupList[GroupNo].groupname, gpname );
		}

		if ( type == 1 ) {
			strncpy( gpname, meta_st.frame[ii].label, nchar);
			gpname[nchar] = '\0';
			strcpy( GroupList[GroupNo].groupname, "Products Valid: ");
			strcat( GroupList[GroupNo].groupname, gpname );
		}

		GroupList[GroupNo].frames[0] = ii+1;
		GroupList[GroupNo].frame_num = 1;

		outflag = 0;

		if (  ( ii < nn-1 ) && (!outflag) ) {
	    	  for ( jj = ii+1; jj < nn; jj++ ) {  
	    	    if ( (GroupList[GroupNo].frame_num) >= MAX_FRAME_GROUP ) {

			sprintf(message, "Warning from auto-group :\n  Too many frames in group %s.\n This group was created with %d frames.\n", 
			GroupList[GroupNo].groupname, MAX_FRAME_GROUP);

			NxmWarn_show(DrawingW, message);
			break;
	    	    }
		    check_frame( type, nchar, gpname, jj, status );	
		  }
		}

		GroupNo ++;
	    }
	}


/*
for( ii = 0; ii < GroupNo; ii++ ) {
printf("###group[%d] name  --- %s\n", ii, GroupList[ii].groupname );
    printf("   no. of frames  --- %d\n", GroupList[ii].frame_num );
    printf("   frames --- \n");
    for ( jj = 0; jj < GroupList[ii].frame_num; jj++)
	printf(" %d ", GroupList[ii].frames[jj] );
    printf("\n");
}
*/

}

/*=====================================================================*/

void check_frame ( int type, size_t nchar, char *gpname, int sframe, short *status )
{
int	nn, icheck;

/*---------------------------------------------------------------------*/

	if ( status[sframe] == 0 ) {
	    if (type == 0)
		icheck = strcmp(&((meta_st.frame[sframe]).label[nchar]), gpname);

	    if (type == 1)
		icheck = strncmp(meta_st.frame[sframe].label, gpname, nchar);
	     
	    if ( icheck == 0 ) {
		nn = GroupList[GroupNo].frame_num;
		GroupList[GroupNo].frames[nn] = sframe + 1;
		(GroupList[GroupNo].frame_num) ++;
		status[sframe] = 1;
	    }
	}
}
