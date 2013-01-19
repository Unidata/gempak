#include "geminc.h"
#include "gemprm.h"

typedef struct {
  float	 start_val;
  float	 end_val;
  float  value;
}DISCRETE_INFO;

static	int	       _numInfo;
static  DISCRETE_INFO  *_discreteInfo;

void in_discrete ( char *discrete, int *iret )
/************************************************************************
 * in_discrete								*
 *									*
 * This function parses and saves infor. from the "discrete" string.    *
 * "discrete" string should not have the prefix "DISCRETE=".		*
 *									*
 * in_discrete ( discrete, iret )					*
 *									*
 * Input parameters:							*
 *	*discrete       char	string to parse information		*
 *									*
 * Output parameters:							*
 *	*iret		int	< 0  if unsuccessful			*
 *				==0  if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/05	initial coding				*
 * D.W.Plummer/NCEP	07/05	Remove diagnostic/debug prints		*
 * H. Zeng/SAIC		08/05	fixed a reported bug on hpux platform	*
 ***********************************************************************/
{
    int		index, ii, ier;
    char        *info_ptr=NULL, *char_ptr=NULL, *char_ptr2=NULL;
    char	val_str[16];
/*---------------------------------------------------------------------*/

    *iret = 0;
  
    /*
     * Free current discreteInfo spaces.
     */
    _numInfo = 0;
    G_FREE ( _discreteInfo, DISCRETE_INFO );
     
    if ( discrete == NULL && discrete[0] == '\0' ) return;

    /*
     * Check the total number of range&value pairs and put it in index.
     */
    ii    = 0;
    index = 0;
    while ( discrete[ii] != '\0' ) {

       if ( discrete[ii] == '=' ) index++; 
       ii++;
    }

    if ( index == 0 ) {

       *iret = -17;
       return;
    }

    /*
     * Dynamically allocate memory for _discreteInfo.
     */
    _numInfo    = index;
    G_MALLOC ( _discreteInfo, DISCRETE_INFO, _numInfo, "MALLOC FAIL");

    index = 0;
    info_ptr = strtok (discrete, ";");
    while ( info_ptr != NULL ) {

      char_ptr = strchr ( info_ptr, '=' );
      char_ptr2= strchr ( info_ptr, '-' );

      if ( char_ptr == NULL || char_ptr2 == NULL ||
           (char_ptr - char_ptr2) <= 0               ) {

	_numInfo = 0;
        G_FREE ( _discreteInfo, DISCRETE_INFO );	
        *iret = -17; 
	return;
      }

      memcpy ( val_str, info_ptr, 
	       (char_ptr2 - info_ptr) / sizeof(char) );
      val_str[char_ptr2 - info_ptr] = '\0';

      if ( val_str[0] == '\0' ) {

	if ( index == 0 ) {

	  _discreteInfo[index].start_val = - (FLT_MAX);
        }
        else {

	  _numInfo = 0;
          G_FREE ( _discreteInfo, DISCRETE_INFO );	
          *iret = -17; 
	  return;
        }

      }
      else {

	cst_crnm ( val_str, &(_discreteInfo[index].start_val), &ier );

	if ( ier != 0 ) {

	   _numInfo = 0;
           G_FREE ( _discreteInfo, DISCRETE_INFO );	
           *iret = -17; 
	   return;
        }

      }

      memcpy ( val_str, (char_ptr2+1), 
	       (char_ptr - char_ptr2 - 1) / sizeof(char) );
      val_str[char_ptr - char_ptr2 - 1] = '\0';

      if ( val_str[0] == '\0' ) {

	if ( index == (_numInfo - 1) ) {

	  _discreteInfo[index].end_val = FLT_MAX;
        }
        else {

	  _numInfo = 0;
          G_FREE ( _discreteInfo, DISCRETE_INFO );	
          *iret = -17; 
	  return;
        }

      }
      else {

	cst_crnm ( val_str, &(_discreteInfo[index].end_val), &ier );

	if ( ier != 0 ) {

	   _numInfo = 0;
           G_FREE ( _discreteInfo, DISCRETE_INFO );	
           *iret = -17; 
	   return;
        }

      }

      memcpy ( val_str, (char_ptr+1), strlen (char_ptr+1) );
      val_str[strlen(char_ptr+1)] = '\0';

      if ( val_str[0] == '\0' ) {

	 _numInfo = 0;
         G_FREE ( _discreteInfo, DISCRETE_INFO );	
         *iret = -17; 
	 return;
      }
      else {

         cst_crnm ( val_str, &(_discreteInfo[index].value), &ier );

         if ( ier != 0 ) {

	    _numInfo = 0;
            G_FREE ( _discreteInfo, DISCRETE_INFO );	
            *iret = -17; 
	    return;
         }
      }

      index++;

      info_ptr = strtok (NULL, ";");

    } /* the end of while ( info_ptr != NULL ... */

}

void in_discmap ( float *v_line1, float *v_line2, float *v_out, int *iret )
/************************************************************************
 * in_discmap								*
 *									*
 * This function determines the discrete value v_out given the values	*
 * of two closest contour lines.					*
 *									*
 * in_discmap ( v_line1, v_line2, v_out, iret )				*
 *									*
 * Input/Output parameters:						*
 *	v_line1		float	contour line 1				*
 *	v_line2		float	contour line 2				*
 *	v_out		float	matching discrete value			*
 *									*
 * Output parameters:							*
 *	*iret		int	< 0 if unsuccessful			*
 *				==0 if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/05	initial coding				*
 * M. Li/SAIC		04/06	Use G_DIFF to get rid of compile warning*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

    for ( ii = 0; ii <_numInfo; ii++ ) {

      if ( G_DIFF (*v_line1, _discreteInfo[ii].start_val) &&
           G_DIFF (*v_line2, _discreteInfo[ii].end_val) ) {

	*v_out = _discreteInfo[ii].value;
        return;
      }

      if ( G_DIFF (-_discreteInfo[ii].start_val, FLT_MAX) &&
           *v_line1 >= _discreteInfo[ii].start_val &&
           G_DIFF (*v_line2, _discreteInfo[ii].end_val)  ) {

	*v_out = _discreteInfo[ii].value;
        return;
      }

      if ( G_DIFF (_discreteInfo[ii].end_val, (FLT_MAX)) &&
           G_DIFF (*v_line1, _discreteInfo[ii].start_val) &&
           *v_line2 <= _discreteInfo[ii].end_val      ) {

	*v_out = _discreteInfo[ii].value;
        return;
      }

    }

    *v_out = RMISSD;
    *iret  = -18;     

}

void in_discq ( int *state, int *iret )
/************************************************************************
 * in_discq								*
 *									*
 * This function returns G_TRUE or G_FALSE to indicate whether discrete *
 * levels have been set or not.						*
 *									*
 * in_discq ( state, iret )						*
 *									*
 * Input/Output parameters:						*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*state		int	status of discrete values		*
 *	*iret		int	< 0 if unsuccessful			*
 *				==0 if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/05	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( _numInfo > 0 ) { 

      *state = G_TRUE;
    }
    else {

      *state = G_FALSE;
    }

}

void in_discqleft ( float *value, float *newval, int *iret )
/************************************************************************
 * in_discq                                                             *
 *                                                                      *
 * This function returns the calue to the left of the contour line	*
 * value in the DISCRETE parameter.					*
 *                                                                      *
 * in_discq ( value, newval, iret )                                     *
 *                                                                      *
 * Input parameters:                                             	*
 *	*value		float	value to the left of contour		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *newval         int     new value to the left of contour       	*
 *      *iret           int     < 0 if unsuccessful                     *
 *                              ==0 if successful                       * 
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC         04/06   initial coding                          	*
 ***********************************************************************/
{
   int	ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

    for ( ii = 0; ii < _numInfo; ii++ ) {
	if ( G_DIFF (_discreteInfo[ii].value, *value) ) {
	    if ( ii == 0 ) {
		*newval =  RMISSD;
	    }
	    else {
		*newval = _discreteInfo[ii-1].value;
    	    }
	}
    } 
}

