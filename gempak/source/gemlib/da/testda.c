#include "da.h"

int getinp ( int type_in, char *str_in, int *int_in, 
				float *float_in, int size_in );

int getinp ( int type_in, char *str_in, int *int_in, 
				float *float_in, int size_in )
/************************************************************************
 * getinp								*
 * 									*
 * This function retrieves input from the keyboard.  The type of input 	*
 * to be converted to is set by "type_in"...  Before anything is done 	*
 * to the input string, the data types are checked to make sure we are 	*
 * not misbehaving.							*
 *									*
 * int getinp (type_in, str_in, int_in, float_in, size_in )		*
 * Input parameters:							*
 *	type_in		int	input type				*
 *	*str_in		char	input string				*
 *	*int_in		int	input integer				*
 *	*float_in	float	input real				*
 *	size_in		int	input size				*
 * Output parameters:							*
 *	getinp		int	status return				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/13	Copied from testcst			*
 ***********************************************************************/
{
    char temp[1000];   /* temporary input....BIG! */
    int i;

/*--------------------------------------------------------------------*/

    /* 	type -> 1 - char
	type -> 2 - integer
	type -> 3 - float */
    if ( ( (type_in == 1) && (str_in == NULL) ) ||
	 ( (type_in == 2) && (int_in == NULL) ) ||
	 ( (type_in == 3) && (float_in == NULL) )) {
	printf("Error calling \"getinp\" requesting input \n");
	return ( -99 );
    }
    
    fgets(temp, 999, stdin);

    if (temp[strlen(temp)-1] == '\n') {
	temp[strlen(temp)-1] = '\0';
    }

    if ( (int)strlen(temp) > size_in) {
	printf("Overflow on input. Expected %d characters. Got %d \n",
 		size_in, (int)strlen(temp));
	temp[size_in] = '\0';
	printf("Truncating to >%s< and continuing \n", temp);
    }

    /* now, process the input into correct type and return */
    switch (type_in)
    {
	case 1:    /* handle character input */
	  strcpy(str_in, temp);
	  break;
	case 2:	   /* handle integer input */
	  for (i=0;i<(int)strlen(temp);i++) {
	    if (! ( isdigit((unsigned long)temp[i]) ||
		  (i == 0 && temp[0] == '-') ) ) {
	      printf("Expected numeric input. Received \"%c\" ", temp[i]);
	      return ( -99 );
	    }
	  }
	  *int_in = atoi(temp);

	  break;
	case 3:	   /* handle floating point input */
	  for (i=0;i<(int)strlen(temp);i++) {
	      if (isalpha((unsigned long)temp[i])) {
		printf("Expected numeric input.  Received \"%c\" ", temp[i]);
		return ( -99 );
	      }
	  }
	  *float_in = (float)atof(temp); 
	  break;
	default:
	  printf("Can not read to this type-> %i \n", type_in);
	  return ( -99 );
    }

    return ( 0 );
}

/*=====================================================================*/

int main (void)
/************************************************************************
 * TESTDA								*
 * 									*
 * This program tests the GEMLIB DA functions.				*
 * 									*
 ** 									*
 * Log:									*
 * S. Jacobs/NCEP	 5/13	Initial coding				*
 ***********************************************************************/
{
    int		cont, ier, iret, numsub, is, ie, ii;
    char	select[5];

    char	pyfile[MXFLSZ], pymeth[MXFLSZ];

/*---------------------------------------------------------------------*/
    in_bdta(&ier);

    cont = G_TRUE;

    while ( cont ) {
	printf ( "\n\n" );
	printf ( "  1 = DA_RUNPY for CHAR output\n" );
	printf ( "  2 = DA_RUNPY for INT output\n" );
	printf ( "  3 = DA_RUNPY for FLOAT output\n" );
	printf ( "\n" );
	printf ( "Select a subroutine number or type EXIT: " );
	ier = getinp ( 1, select, NULL, NULL, 4 );
	switch ( select[0] ) {
	    case 'e':
	    case 'E':
		cont = G_FALSE;
	    default:
		numsub = atoi ( select );
		break;
	}

/*---------------------------------------------------------------------*/
	if ( numsub == 1 ) {

	    printf ( "Enter the Python file name (without .py):\n" );
	    ier = getinp ( 1, pyfile, NULL, NULL, MXFLSZ );
	    if ( ier < 0 ) {
	       	break;
	    }

	    printf ( "Enter the Python method name:\n" );
	    ier = getinp ( 1, pymeth, NULL, NULL, MXFLSZ );
	    if ( ier < 0 ) {
	       	break;
	    }

	    printf ( "Enter the number of input strings:\n" );
	    ier = getinp ( 2, NULL, &danarg, NULL, STRSIZE );
	    if ( ier < 0 ) {
	       	break;
	    }

	    daargs = (char **) malloc ( danarg * sizeof(char *) );
	    for ( ii = 0; ii < danarg; ii++ ) {
		daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
		printf ( "Enter input string %d:\n", ii );
		ier = getinp ( 1, daargs[ii], NULL, NULL, STRSIZE );
		if ( ier < 0 ) {
		    break;
		}
	    }

	    datype = DACHAR;
	    da_runpy ( pyfile, pymeth, &iret );

	    printf ( "\nDA_RUNPY: iret = %d\n\n", iret );
	    if  ( iret >= 0 )  {
		printf ( "The length of result is: %ld\n", strlen(daoutc) );
		printf ( "The result is: %s\n\n", daoutc );

		free ( daoutc );
	    }
	    free ( daargs );
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 2 ) {

	    printf ( "Enter the Python file name (without .py):\n" );
	    ier = getinp ( 1, pyfile, NULL, NULL, MXFLSZ );
	    if ( ier < 0 ) {
	       	break;
	    }

	    printf ( "Enter the Python method name:\n" );
	    ier = getinp ( 1, pymeth, NULL, NULL, MXFLSZ );
	    if ( ier < 0 ) {
	       	break;
	    }

	    printf ( "Enter the number of input strings:\n" );
	    ier = getinp ( 2, NULL, &danarg, NULL, STRSIZE );
	    if ( ier < 0 ) {
	       	break;
	    }

	    daargs = (char **) malloc ( danarg * sizeof(char *) );
	    for ( ii = 0; ii < danarg; ii++ ) {
		daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
		printf ( "Enter input string %d:\n", ii );
		ier = getinp ( 1, daargs[ii], NULL, NULL, STRSIZE );
		if ( ier < 0 ) {
		    break;
		}
	    }

	    datype = DAINT;
	    da_runpy ( pyfile, pymeth, &iret );

	    printf ( "\nDA_RUNPY: iret = %d\n\n", iret );
	    if  ( iret >= 0 )  {
		printf ( "The array size of result is: %d\n", danumi );
		printf ( "The result array is:\n" );
		is = 0;
		ie = G_MIN ( 8, danumi );
		while ( is < danumi ) {
		    for ( ii = is; ii < ie; ii++ ) {
			printf ( "%9d ", daouti[ii] );
		    }
		    printf ( "\n" );
		    is = ie;
		    ie = G_MIN ( is+8, danumi );
		}
		printf ( "\n" );

		free ( daouti );
	    }
	    free ( daargs );
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 3 ) {

	    printf ( "Enter the Python file name (without .py):\n" );
	    ier = getinp ( 1, pyfile, NULL, NULL, MXFLSZ );
	    if ( ier < 0 ) {
	       	break;
	    }

	    printf ( "Enter the Python method name:\n" );
	    ier = getinp ( 1, pymeth, NULL, NULL, MXFLSZ );
	    if ( ier < 0 ) {
	       	break;
	    }

	    printf ( "Enter the number of input strings:\n" );
	    ier = getinp ( 2, NULL, &danarg, NULL, STRSIZE );
	    if ( ier < 0 ) {
	       	break;
	    }

	    daargs = (char **) malloc ( danarg * sizeof(char *) );
	    for ( ii = 0; ii < danarg; ii++ ) {
		daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
		printf ( "Enter input string %d:\n", ii );
		ier = getinp ( 1, daargs[ii], NULL, NULL, STRSIZE );
		if ( ier < 0 ) {
		    break;
		}
	    }

	    datype = DAFLOAT;
	    da_runpy ( pyfile, pymeth, &iret );

	    printf ( "\nDA_RUNPY: iret = %d\n\n", iret );

	    if  ( iret >= 0 )  {
		printf ( "The array size of result is: %d\n", danumf );
		printf ( "The result array is:\n" );
		is = 0;
		ie = G_MIN ( 8, danumf );
		while ( is < danumf ) {
		    for ( ii = is; ii < ie; ii++ ) {
			printf ( "%9.2f ", daoutf[ii] );
		    }
		    printf ( "\n" );
		    is = ie;
		    ie = G_MIN ( is+8, danumf );
		}
		printf ( "\n" );

		free ( daoutf );
	    }
	    free ( daargs );
	}

/*---------------------------------------------------------------------*/
    }

    return 0;
}
