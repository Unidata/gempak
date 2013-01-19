#include "geminc.h"
#include "gemprm.h"

static	int	       _dlinesStateR;
static	int	       _dlinesStateL;
static  float	       _dlinesEpsilon;

void in_dlines ( char *dlines, int *iret )
/************************************************************************
 * in_dlines								*
 *									*
 * This function parses and saves infor. from the "dlines" string.	*
 * specifically the state(yes or no) and the epsilon value.		*
 *									*
 * in_dlines ( dlines, iret )						*
 *									*
 * Input parameters:							*
 *	*dlines       char	string to parse information		*
 *									*
 * Output parameters:							*
 *	*iret		int	< 0  if unsuccessful			*
 *				==0  if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/05	initial coding				*
 * M. Li/SAIC		06/06	Parse info for left of line		*
 ***********************************************************************/
{
    int		ier;
    char        *char_ptr=NULL, *char_ptr1=NULL;
/*---------------------------------------------------------------------*/

    *iret = 0;
  
    /*
     * Initialize the current info.
     */
    _dlinesStateR = G_FALSE;
    _dlinesStateL = G_FALSE;
    _dlinesEpsilon = FLT_MIN;

    if ( dlines == NULL && dlines[0] == '\0' ) return;

    char_ptr1 = strchr ( dlines, ';' );
    if ( char_ptr1 != NULL ) {
        if ( strncasecmp ( (char_ptr1+1), "YES", 3) == 0 ) {
            _dlinesStateL = G_TRUE;
        }
        else if ( strncasecmp ( (char_ptr1+1), "NO", 2) == 0 ) {
            _dlinesStateL = G_FALSE;
        }
    }

    if ( strncasecmp (dlines, "YES", 3) == 0 ) {

      _dlinesStateR = G_TRUE;
    }
    else if ( strncasecmp (dlines, "NO", 2) == 0 ) {

      _dlinesStateR = G_FALSE;
    }
    else {

      *iret = -17;
      return;
    }

    char_ptr = strchr ( dlines, '|' );
    if ( char_ptr == NULL ) return;

    cst_crnm ( (char_ptr+1), &(_dlinesEpsilon), &ier );
    if ( ier != 0 ) {

      *iret = -17; 
      return;
    }

}

void in_dlinq ( int *stateR, int *stateL,  float *epsilon, int *iret )
/************************************************************************
 * in_dlinq								*
 *									*
 * This function queries states and epsilon information.		*
 *									*
 * in_dlinq ( stateR, stateL, epsilon, iret )				*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*stateR		int	state info for right of line (yes or no)*
 *	*stateL		int	state info for left of line (yes or no)	*
 *	*epsilon	float	epsilon value				*
 *	*iret		int	< 0 if unsuccessful			*
 *				==0 if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/05	initial coding				*
 * M. Li/SAIC		0406	Added stateL				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    *stateR   = _dlinesStateR;
    *stateL   = _dlinesStateL;
    *epsilon = _dlinesEpsilon;

}
