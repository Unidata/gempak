/*
 * TableC_Find - VERSION: %I%  %E% %T%
 */
/*
 * TableC_Find - Search Table C's list of multiple descriptors for a
 * descriptor which matches the given FXY value.  Return a NULL pointer
 * if one cannot be found.
 */
 
#include <mel_bufr.h>
 
/*
..............START PROLOGUE....................................
 
  MODULE NAME:         TableC_Find
 
  DESCRIPTION:         Search Table C's list of multiple descriptors for a
			descriptor which matches the given FXY value.  
			Return a NULL pointer if one cannot be found.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int TableC_Find( FXY_t FXYval, FILE *fp, int f_num )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
   FXYval	      FXY_t	    Input	Packed FXY value
   fp		      FILE	    Input	File to print to
   f_num	      int	    Input
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  FXY_Unpack		Unpacks a FXY into its component f, x, and y
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  f		unit_t		F value of a FXY
  x		unit_t		x value of a FXY
  y		unit_t		y value of a FXY
 
  METHOD:
	Perform FXY_Unpack to get the f, x, and y value from the FXY
	If this is not a Table C entry return with error
	If the C value is out of the current bounds return with error
	Switch on the x value
	  Where the x matches a case
		Write a message
		break
	  End where
	End switch
	return
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    100997 - LAH: Removed unused variable i

..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED
 
int TableC_Find( FXY_t FXYval, FILE *fp, int f_num )
 
#else

int TableC_Find( FXYval, fp, f_num )
FXY_t FXYval;
FILE *fp;
int f_num;
 
#endif
{
    uint_t          f, x, y;

    FXY_Unpack( FXYval, &f, &x, &y );

/*  If not a Table C entry then return */
    if(f != 2) return 0;

/* If out of current bounds for Table C then return */
    if(x < 1 || x > 37)return 0;

/* Switch on the X value */

    switch(x) {
	case 1:
	  fprintf(fp," %d %01d-%02d-%03d = Change data width ", 
		f_num, f, x, y);
	  break;
	case 2:
	  fprintf(fp," %d %01d-%02d-%03d = Change scale ", f_num, f, x, y);
	  break;
	case 3:
	  fprintf(fp," %d %01d-%02d-%03d = Change reference values ", 
		f_num, f, x, y);
	  break;
	case 4:
	  fprintf(fp," %d %01d-%02d-%03d = Add associated field ", 
		f_num, f, x, y);
	  break;
	case 5:
	  fprintf(fp," %d %01d-%02d-%03d = Signifies character ", 
		f_num, f, x, y);
	  break;
	case 6:
	  fprintf(fp," %d %01d-%02d-%03d = Signify data width ", 
		f_num, f, x, y);
	  fprintf(fp,"for following local descriptor ");
	  break;
	case 21:
	  fprintf(fp," %d %01d-%02d-%03d = Data not present ", 
		f_num, f, x, y);
	  break;
	case 22:
	  fprintf(fp," %d %01d-%02d-%03d = Quality information follows ", 
		f_num, f, x, y);
	  break;
	case 23:
	  fprintf(fp," %d %01d-%02d-%03d = Substituted values operator  ", 
		f_num, f, x, y);
	  break;
	case 24:
	  fprintf(fp," %d %01d-%02d-%03d = First order statistical values ", 
		f_num, f, x, y);
	  break;
	case 25:
	  fprintf(fp," %d %01d-%02d-%03d = Difference statistical values ", 
		f_num, f, x, y);
	  break;
	case 32:
	  fprintf(fp," %d %01d-%02d-%03d = Replaced/retained values ", 
		f_num, f, x, y);
	  break;
	case 35:
	  fprintf(fp," %d %01d-%02d-%03d = Cancel backward data reference ", 
		f_num, f, x, y);
	  break;
	case 36:
	  fprintf(fp," %d %01d-%02d-%03d = Define data present bit-map ", 
		f_num, f, x, y);
	  break;
	case 37:
	  fprintf(fp," %d %01d-%02d-%03d = Use define data present bit-map ", 
		f_num, f, x, y);
	  break;
    }

    return 1;
    }
