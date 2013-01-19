#include "geminc.h"
#include "gemprm.h"

typedef struct {
  char   name[32];
  float  value;
}CATMAP_INFO;

static  int	     num_info;
static  CATMAP_INFO  *catmap_info;

void in_catminp ( char *catmap, int *iret )
/************************************************************************
 * in_catminp								*
 *									*
 * This function parses and saves information from the "catmap" string. *
 * "catmap" string should not have the prefix "CATMAP=".		*
 *									*
 * in_catminp ( catmap, iret )						*
 *									*
 * Input parameters:							*
 *	*catmap		char	string to parse information		*
 *									*
 * Output parameters:							*
 *	*iret		int	< 0  if unsuccessful			*
 *				==0  if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		03/05	initial coding				*
 ***********************************************************************/
{
    int		index, ii, ier;
    char        *info_ptr=NULL, *char_ptr=NULL;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Free current catmap_info spaces.
     */
    num_info = 0;
    G_FREE ( catmap_info, CATMAP_INFO );

    if ( catmap == NULL && catmap[0] == '\0' ) return;

    /*
     * Check the total number of name&value pairs and put it in index.
     */
    ii    = 0;
    index = 0;
    while ( catmap[ii] != '\0' ) {

       if ( catmap[ii] == '=' ) index++; 
       ii++;
    }

    if ( index == 0 ) {

       *iret = -17;
       return;
    }

    /*
     * Dynamically allocate memory for catmap_info.
     */
    num_info    = index;
    G_MALLOC ( catmap_info, CATMAP_INFO, num_info, "MALLOC FAIL");

    index = 0;
    info_ptr = strtok (catmap, ";");
    while ( info_ptr != NULL ) {

      char_ptr = strchr ( info_ptr, '=' );

      if ( char_ptr != NULL ) {

	memcpy ( catmap_info[index].name, info_ptr, 
		 (char_ptr - info_ptr) / sizeof(char) );
        catmap_info[index].name[char_ptr - info_ptr] = '\0';
	cst_crnm ( (char_ptr+1), &(catmap_info[index].value), &ier );

	if ( ier != 0 ) {

	   num_info = 0;
           G_FREE ( catmap_info, CATMAP_INFO );	
           *iret = -17; 
	   return;
        }

	index++;
      }

      info_ptr = strtok (NULL, ";");
    }
   
}

void in_catmmap ( char *string, float *value, int *iret )
/************************************************************************
 * in_catmmap								*
 *									*
 * This function maps either a string to a floating value or vice vesa  *
 * (depending on incoming values) using locally maintained structure.   *
 *									*
 * in_catmmap ( string, value, iret )					*
 *									*
 * Input/Output parameters:						*
 *	*string		char	character string			*
 *	*value		float	floating value				*
 *									*
 * Output parameters:							*
 *	*iret		int	< 0 if unsuccessful			*
 *				==0 if successful			* 
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		03/05	initial coding				*
 * F. J. Yen/NCEP	01/08	Added wildcard "*" after initial char	*
 ***********************************************************************/
{
    int		ii, ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( (string == NULL || string[0] == '\0') 
	 && ERMISS(*value) ) {

       *iret = -17;
       return;
    }

    if ( string != NULL && string[0] != '\0' ) {

      /*
       * String to value.
       */
      for ( ii = 0; ii < num_info; ii++ ) {

	  if ( strcasecmp(catmap_info[ii].name, string) == 0 ) {

	      *value = catmap_info[ii].value;
	      return;
          }
          else if ( strlen ( catmap_info[ii].name ) == 2 &&
			 catmap_info[ii].name[1] == '*' ) {
  	      if ( toupper (catmap_info[ii].name[0]) == toupper (string [0]) ) { 
		  /*
                   * Get value for wildcard (case insensitive)
		   */
		  *value = catmap_info[ii].value;
		  return;
	      }
	  }
      }

      /*
       * Try to convert string directly into value.
       */
      cst_crnm ( string, value, &ier );

      if ( ier != 0 ) {
	  *iret = -18;
      }

    }
    else {

      /*
       * Value to string.
       */
      for ( ii = 0; ii < num_info; ii++ ) {

	if ( G_DIFF(catmap_info[ii].value, *value) ) {
	  strcpy ( string, catmap_info[ii].name );
	  return;
        }
      }

      /*
       * Try to convert value directly into string.
       */
      sprintf ( string, "%.2f", *value );

    }
 
}
