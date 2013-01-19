#include "nmscmn.h"

void nms_rtbl ( char alias[], int *ntype, NMS_types types[], 
			int *nflag, NMS_flags flags[], int *iret )
/************************************************************************
 * nms_rtbl								*
 *									*
 * This routine reads the MISC data type settings and searches for a 	*
 * match to the requested alias.					*
 *									*
 * nms_rtbl ( alias, ntype, types, nflag, flags, iret )			*
 *									*
 * Input parameters:							*
 *	alias[]		char		Alias for MISC data		*
 *									*
 * Output parameters:							*
 *	*ntype		int		Number of types			*
 *	types[]		NMS_types	Editable data settings		*
 *	*nflag		int		Number of flags			*
 *	flags[]		NMS_flags	Editable flag settings		*
 *	*iret		int		Return code			*
 *					 -3 = not found			*
 *									*
 **									*
 * S. Jacobs/NCEP	11/99	Created					*
 * S. Jacobs/NCEP	12/99	Changed table format, updated structures*
 * S. Jacobs/NCEP	 3/00	Changed table format and structure	*
 * M. Li/SAIC		 4/03	Added icolr2				*
 * F. J. Yen/NCEP	 6/04	Added arrw.ityp				*
 ***********************************************************************/
{

	int	iostat, lenn, num, ii, jclrs[2], ier;
	char	buffer[LLMXLN], clrval[13], **carr;
	float	farr[4];
	int	found, done;

	FILE	*fp;

/*---------------------------------------------------------------------*/

	*iret = -3;

/*
 *	Initialize the number of used structures.
 */
	*ntype = -1;
	*nflag = -1;

/*
 *	Open the settings table.
 */
	fp = cfl_tbop ( "miscset.tbl", "config", &ier );

/*
 *	Read until the end of the file or a match is made.
 */
	found  = G_FALSE;
	iostat = 0;
	while ( ( ! found ) && ( iostat == 0 ) )  {

	    cfl_trln ( fp, sizeof(buffer), buffer, &iostat );
	    if  ( ( iostat == 0 ) && ( buffer[0] != '!' ) )  {

		if  ( strncmp ( buffer, "ALIAS", 5 ) == 0 )  {

		    if  ( strcmp ( &buffer[6], alias ) == 0 )  {

			found = G_TRUE;
			*iret = 0;

			done  = G_FALSE;
			while ( ( ! done ) && ( iostat == 0 ) )  {

			  cfl_trln ( fp, sizeof(buffer), buffer,
				     &iostat );
			  if  ( ( iostat == 0 ) &&
				( buffer[0] != '!' ) )  {

			    if (strncmp(buffer,"ALIAS",5) == 0) {
			      done = G_TRUE;
			    }
			    else if (strncmp(buffer,"TYPE",4) == 0) {
			      (*ntype)++;
			      sscanf ( buffer, "%*s %24c %s",
				       types[*ntype].name,
				       clrval );
			      types[*ntype].name[24] = CHNULL;
			      cst_lstr ( types[*ntype].name,
					 &lenn, &ier );
			      types[*ntype].name[lenn] = CHNULL;

			      carr = (char **) malloc ( 3 * sizeof(char *) );
                              for ( ii = 0; ii < 3; ii++) {
                                  carr[ii] = (char *) malloc ( 20 );
                              }
			      cst_clst ( clrval, '/', "\0", 3, 20,
                                        carr, &num, &ier);
			      cst_lstr ( carr[0], &lenn, &ier );
			      if ( lenn <= 0 ) {
				  types[*ntype].ionoff = IMISSD;
				  types[*ntype].icolr = 1;
                                  types[*ntype].icolr2 = 1;
			      }
			      else {
			    	  cst_numb( carr[0], &(types[*ntype].ionoff), &ier);
				  cst_ilst(carr[1], ';', IMISSD, 2, jclrs, &num, &ier);
				  if ( num < 2 ) {
				     jclrs[1] = IMISSD;
				  }
				  types[*ntype].icolr  = jclrs[0];
                                  types[*ntype].icolr2 = jclrs[1]; 
			      }

			      cst_lstr ( carr[2], &lenn, &ier );
			      if ( lenn <= 0 ) {
				  types[*ntype].value = RMISSD;
			      }
			      else {
				  cst_crnm(carr[2], &(types[*ntype].value), &ier);
			      } 

			      for ( ii = 0; ii < 3; ii++) {
        		          free ( carr[ii] );
    			      }
    			      free ( carr );

			      types[*ntype].line.size = -1.0F;
			      types[*ntype].line.iwid = -1;

			      types[*ntype].symb[0].code = -1.0F;
			      types[*ntype].symb[0].size = -1.0F;
			      types[*ntype].symb[0].iwid = -1;

			      types[*ntype].symb[1].code = -1.0F;
			      types[*ntype].symb[1].size = -1.0F;
			      types[*ntype].symb[1].iwid = -1;

			      types[*ntype].arrw.size = -1.0F;
			      types[*ntype].arrw.hdsz = -1.0F;
			      types[*ntype].arrw.iwid = -1;
			      types[*ntype].arrw.ityp = -1;
			    }
			    else if (strncmp(buffer,"LINE",4) == 0) {
			      cst_rlst ( &(buffer[5]), '/', 0.0F, 2,
					 farr, &num, &ier );
			      types[*ntype].line.size = farr[0];
			      types[*ntype].line.iwid = (int) farr[1];
			    }
			    else if (strncmp(buffer,"SYM1",4) == 0) {
			      cst_rlst ( &(buffer[5]), '/', 0.0F, 3,
					 farr, &num, &ier );
			      types[*ntype].symb[0].code = farr[0];
			      types[*ntype].symb[0].size = farr[1];
			      types[*ntype].symb[0].iwid = (int) farr[2];
			    }
			    else if (strncmp(buffer,"SYM2",4) == 0) {
			      cst_rlst ( &(buffer[5]), '/', 0.0F, 3,
					 farr, &num, &ier );
			      types[*ntype].symb[1].code = farr[0];
			      types[*ntype].symb[1].size = farr[1];
			      types[*ntype].symb[1].iwid = (int) farr[2];
			    }
			    else if (strncmp(buffer,"ARRW",4) == 0) {
			      cst_rlst ( &(buffer[5]), '/', 0.0F, 4,
					 farr, &num, &ier );
			      types[*ntype].arrw.size = farr[0];
			      types[*ntype].arrw.hdsz = farr[1];
			      types[*ntype].arrw.iwid = (int) farr[2];
			      types[*ntype].arrw.ityp = (int) farr[3];
			      if ( types[*ntype].arrw.size <= 0.0F ) {
				types[*ntype].arrw.size = .2F;
			      }
			      if ( types[*ntype].arrw.iwid <= 0 ) {
				types[*ntype].arrw.iwid = 1;
			      }
			      if ( types[*ntype].arrw.ityp == 0 ||
					num != 4 ) {
				if ( types[*ntype].arrw.hdsz <= 0.0F ) {
				  types[*ntype].arrw.ityp = 2;
				}
				else {
				  types[*ntype].arrw.ityp = 1;
				}
			      }
			      else {
				if ( types[*ntype].arrw.ityp < 0 ||
					types[*ntype].arrw.ityp > 5 ) {
				  types[*ntype].arrw.ityp = 5;
				}
				if ( types[*ntype].arrw.ityp != 2 && 
					types[*ntype].arrw.hdsz <= 0.0F ) {
				  types[*ntype].arrw.hdsz = .4F;
				}
			      }
			    }
			    else if (strncmp(buffer,"FLAG",4) == 0) {
			      (*nflag)++;
			      sscanf ( buffer, "%*s %24c %d",
				       flags[*nflag].name,
				       &(flags[*nflag].iflg) );
			      flags[*nflag].name[24] = CHNULL;
			      cst_lstr ( flags[*nflag].name,
					 &lenn, &ier );
			      flags[*nflag].name[lenn] = CHNULL;
			    }
			  }
			}
		    }
		}
	    }
	}

	if  ( *ntype < 0 )  {
	    *ntype = 0;
	}
	else {
	    (*ntype)++;
	}

	if  ( *nflag < 0 )  {
	    *nflag = 0;
	}
	else {
	    (*nflag)++;
	}

/*
 *	Close the table.
 */
	cfl_clos ( fp, &ier );

}
