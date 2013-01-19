#include "geminc.h"
#include "gemprm.h"

#define	LIMIT  	10
#define STRSIZE	1024
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
 * E. Wehner/EAI	 8/96	Created					*
 * G. Krueger/EAI	10/96	Fixed fgets read length typo		*
 * G. Krueger/EAI	 9/97	Allow negative integers and fractionals	*
 * S. Jacobs/NCEP	 2/98	Changed call to CST_NOCC		*
 * T. Lee/GSC		 9/98	Added CST_TIMS				*
 * F. J. Yen		 5/99	Removed unreachable break statement	*
 * M. Li/GSC		 3/00	Modified CST_RXBL			*
 * A. Hardy/NCEP	 8/03	Added cst_sort and cst_ncat		*
 * D.W.Plummer/NCEP	 6/06	Increase STRSIZE to 1024; fmt spec to g	*
 ***********************************************************************/
{
    char temp[1000];   /* temporary input....BIG! */
    int i;

    /* 	type -> 1 - char
	type -> 2 - integer
	type -> 3 - float */
    if ( ( (type_in == 1) && (str_in == NULL) ) ||
	 ( (type_in == 2) && (int_in == NULL) ) ||
	 ( (type_in == 3) && (float_in == NULL) ))
    {
	printf("Error calling \"getinp\" requesting input \n");
	return ( -1 );
    }
    
    fgets(temp, 999, stdin);

    if (temp[strlen(temp)-1] == '\n') temp[strlen(temp)-1] = '\0';

    if ( (int)strlen(temp) > size_in)
    {
	printf("Overflow on input. Expected %i characters.  Got %i \n",
 			size_in, (int)strlen(temp));
	temp[size_in] = '\0';
	printf("Truncating to %s and continuing \n", temp);
    }

    /* now, process the input into correct type and return */
    switch (type_in)
    {
	case 1:    /* handle character input */
	  strcpy(str_in, temp);
	  break;
	case 2:	   /* handle integer input */
	  for (i=0;i<(int)strlen(temp);i++)
	  {
	      if (! ( isdigit((unsigned long)temp[i]) || (i == 0 && temp[0] == '-') ) )
	      {
		  printf("Expected numeric input.  Received \"%c\" ", temp[i]);
		  return ( -1 );
	      }
	  }
	  *int_in = atoi(temp);

	  break;
	case 3:	   /* handle floating point input */
	  for (i=0;i<(int)strlen(temp);i++)
	  {
	      if (isalpha((unsigned long)temp[i]))
	      {
		  printf("Expected numeric input.  Received \"%c\" ", temp[i]);
		  return ( -1 );
	      }
	  }
	  *float_in = (float)atof(temp); 
	  break;
	default:
	  printf("Can not read to this type-> %i \n", type_in);
	  return ( -1 );
    }

    return ( 0 );

}

/*=====================================================================*/

int main ( void )
/************************************************************************
 * testcst.c								*
 *									*
 * This programs tests the CGEMLIB "CST" functions.			*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	 5/96						*
 * L. Williams/EAI	 7/96 	added CSTCINS, CSTLCUC, CSTSRCH, 	*
 *				CSTUCLC and CSTUNPR			*
 * E. Wehner/EAi	 8/96 	Changed all scanf, gec, and gets	*
 * D.W.Plummer/NCEP	 2/97	Added CSTPTMT				*
 * G. Krueger/EAI	 9/97	Lifted some input limitations		*
 * G. Krueger/EAI	10/97	CST_xLST: Removed RSPTB; Add str limit	*
 * T. Lee/GSC		 9/98	Added CSTTIMS				*
 * S. Jacobs/NCEP	 5/99	Changed type of tmarry to dttms_t	*
 * A. Hardy/GSC          8/99   Added CST_RNAN				*
 * T. Piper/GSC         12/99   Added CST_WRAP                          *
 * D.W.Plummer/NCEP	 2/00	Changed call to cst_wrap; added cst_ctrl*
 * D.W.Plummer/NCEP	 3/00	Added cst_rpst				*
 * D.W.Plummer/NCEP	 5/01	Added cst_gtag				*
 * R. Tian/SAIC          1/02   Added cst_ctod				*
 * E. Safford/SAIC	10/04	added cst_rmtag				*
 * R. Tian/SAIC		10/04	added cst_rspc				*
 * R. Tian/SAIC		 1/06	added cst_opcl				*
 * T. Piper/SAIC	 1/06	Updated for cst_wrap CSC		*
 * J. Wu/SAIC	 	04/06	added newLineStr in cst_wrap 		*
 * S. Jacobs/NCEP	12/06	Added cst_zpad				*
 ***********************************************************************/
{
char		choice[STRSIZE], string[STRSIZE], sabbr[STRSIZE];
char		substr[STRSIZE], repstr[STRSIZE], outstr[STRSIZE];
char		tag[STRSIZE], tagval[STRSIZE];
char		pattern[STRSIZE], strout[STRSIZE];
char		defstr[STRSIZE], schar[STRSIZE], str[STRSIZE];
char		alpha[STRSIZE], omega[STRSIZE], incr[4];
char		sep[STRSIZE], sub[STRSIZE], eol[STRSIZE]; 
char		newLineStr[STRSIZE];
dttms_t		tmarry[100];
char		**ary_ptr, ch;
int		status, iret, match;
int		subnum, fl, typ;
int		index, i, ndp, sens;
int		*int_ptr, pstr;
int		len, strlim, idef, num, leneol;
int		ntimes;
int		instat;		/* input status */
float		val, *flt_ptr;
char		value[32], **cnam_arr;
int		ii, itype, nout;
char		*locopn, *loccls;

/* ------------------------------------------------------------------- */

	status = 0;

	while( status == 0 ) {
	   fl=0;
	   iret=0;
	   num=0;
	   val=0.0F;
	   pstr=0;
	   index=0;
	   subnum=0;
	   printf( "\n\n" );
	   printf( " 1 = CSTABBR\t 2 = CSTALNM\t 3 = CSTCLST\t 4 = CSTCRNM\n");
	   printf( " 5 = CSTCTOI\t 6 = CSTFIND\t 7 = CSTFLST\t 8 = CSTILST\n");
	   printf( " 9 = CSTINCH\t10 = CSTITOC\t11 = CSTITOS\t12 = CSTLDSP\n");
	   printf( "13 = CSTNCPY\t14 = CSTNOCC\t15 = CSTNUMB\t16 = CSTRANG\n");
	   printf( "17 = CSTRLCH\t18 = CSTRLST\t19 = CSTRMBL\t20 = CSTRMST\n");
	   printf( "21 = CSTRXBL\t22 = CSTSTOI\t23 = CSTLSTR\t24 = CSTCINS\n");
	   printf( "25 = CSTLCUC\t26 = CSTSRCH\t27 = CSTUCLC\t28 = CSTUNPR\n");
	   printf( "29 = CSTPTMT\t30 = CSTTIMS\t31 = CSTRNAN\t32 = CSTWRAP\n");
	   printf( "33 = CSTRPST\t34 = CSTGTAG\t35 = CSTCTOD\t36 = CSTSTAG\n");
	   printf( "37 = CSTNCAT\t38 = CSTSORT\t39 = CSTRMTAG\t40 = CSTRSPC\n");
	   printf( "41 = CSTOPCL\t42 = CSTZPAD\n");
	   printf( "Select a subroutine number or type Exit: " );
	   instat = getinp(1, choice, NULL, NULL, 4);
	   switch( choice[0] ) {
		case 'e':
		case 'E':
		   status = 1;
		   break;
		default:
		   subnum = atoi( choice );
		   break;
	   }

/*-----------------------------------------------------------------------*/

	   if( subnum == 1 ) {

		printf( "Enter a string:\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) break;
		printf( "Enter abbreviation:\n" );
		instat = getinp(1, sabbr, NULL, NULL, STRSIZE);
		if (instat < 0) continue;
		cst_abbr( string, sabbr, &fl, &iret );

		printf( "\nflag = %d : iret = %d\n", fl, iret );
	   }

/*-----------------------------------------------------------------------*/

	   if( subnum == 2 ) {

		typ=0;
		printf( "Enter a character\n" );
		instat = getinp(1,&ch, NULL, NULL, 1);
		if (instat < 0) continue;
		
		cst_alnm( ch, &typ, &iret );
		printf( "\ntype = %d : iret = %d\n", typ, iret );
	   }

/*-----------------------------------------------------------------------*/

	   if( subnum == 3 ) {

		printf( "Enter string:\n" );	
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;


		printf( "Enter separator:\n" );
		instat  = getinp(1, &ch, NULL, NULL, 1);
		if (instat < 0) continue;


		printf( "Enter default string:\n" );
		instat = getinp( 1, defstr, NULL, NULL, STRSIZE);
		if (instat < 0) continue;


		printf( "Enter number of expected elements:\n" );
		instat = getinp(2, NULL, &index, NULL, 5);
		if (instat < 0) continue;

		ary_ptr = (char **)malloc((size_t)index * sizeof(char *));

		for( i=0; i < index; i++ )
		   ary_ptr[i] = (char *)malloc(STRSIZE * sizeof(char));

		cst_clst( string, ch, defstr, index, STRSIZE, ary_ptr, &num,
			  &iret );

		printf( "\nNumber of strings = %d : iret = %d\n", num, iret );

		for( i=0; i < index; i++ ) {
		    printf( "%s\n", ary_ptr[i] );
		    free( ary_ptr[i] );
		}

		if( iret == 1 ) {
		   printf( "\nMore than the expected number of values");
		   printf( " entered.\n");
		}

		if( ary_ptr )
		  free( (char **)ary_ptr );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 4 ) {

		printf( "Enter a string:\n" );

		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_crnm( string, &val, &iret );

		printf( "\nValue = %g : iret = %d\n", val, iret );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 5 ) {

		printf( "Enter number of strings (max %d) :\n", LIMIT );
		instat = getinp(2, NULL, &index, NULL, 2);
		if (instat < 0) continue;


		if ( index <= LIMIT ) {
		   int_ptr = (int *)malloc((size_t)index * sizeof(int));
		   ary_ptr = (char **)malloc((size_t)index * sizeof(char *));

		   for( i=0; i < index; i++ ) {
		      printf( "Enter string #%d: ", i );
		      ary_ptr[i] = (char *)malloc(STRSIZE * sizeof(char));
		      instat = getinp(1, ary_ptr[i], NULL, NULL, STRSIZE);
		      if (instat < 0) continue;

		   }

		   cst_ctoi( ary_ptr, index, int_ptr, &iret );

		   printf( "\niret = %d\n", iret );
		   for( i=0; i < index; i++) {
		      printf( "%d\n", int_ptr[i] );
		      free( ary_ptr[i] );
		   }

		   if( int_ptr )
		      free( int_ptr );

		   if ( ary_ptr )
		      free( (char **) ary_ptr );
		}
		else
		   printf( "\nInvalid number of integers\n");
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 6 ) {

		printf( "Enter number of elements in list:\n");
		instat = getinp(2, NULL, &index, NULL, 3);
		if (instat < 0) continue;


		ary_ptr = (char **)malloc((size_t)index * sizeof(char *));

		for( i=0; i < index; i++ ) {
		   printf( "Enter string #%d: ", i+1 );
		   instat = getinp(1,str, NULL, NULL, STRSIZE);
		   if (instat < 0) continue;
		   len = (int)strlen( str );
		   ary_ptr[i] = (char *)malloc((size_t)(len+1) * sizeof(char));
		   strcpy( ary_ptr[i], str );
		}

		printf( "Enter search str:\n");
		instat = getinp(1, schar, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_find( schar, (const char **)ary_ptr, index, &pstr, &iret );

		printf( "\niret = %d : Position = %d\n", iret, pstr );

		for( i=0; i < index; i++ )
		    free( ary_ptr[i] );

		if( ary_ptr )
		   free( (char **) ary_ptr);
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 7 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, string, NULL, NULL, sizeof(string));
		if (instat < 0) continue;

		printf( "Enter Seperator:\n" );
		instat = getinp(1, &ch, NULL, NULL, 1);
		if (instat < 0) continue;

		printf( "Enter default string:\n");
		instat = getinp(1, defstr, NULL, NULL, sizeof(defstr));
		if (instat < 0) continue;

		printf( "Enter number of expected elements:\n");
		instat = getinp(2, NULL, &index, NULL, 3);
		if (instat < 0) continue;

		ary_ptr = (char **)malloc((size_t)index * sizeof(char *));
		for( i=0; i < index; i++ )
		   ary_ptr[i] = (char *)malloc(STRSIZE * sizeof(char));

		cst_flst( string, ch, defstr, index, STRSIZE, ary_ptr, &num,
			  &iret );

		printf( "\nNumber of names = %d : iret = %d\n", num, iret );
		for( i=0; i < index; i++ ) {
		   printf( "%s\n", ary_ptr[i] );
		   free( ary_ptr[i] );
		}

		if( iret == 2 ) {
		   printf( "\nMore than the expected number of values");
		   printf( " entered.\n");
		}

		if( ary_ptr )
		   free( (char **)ary_ptr );


	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 8 ) {

		printf( "Enter string of integers:\n" );
		instat = getinp(1, string, NULL, NULL, sizeof(string));
		if (instat < 0) continue;

		printf( "Enter separator:\n" );
		instat = getinp(1, &ch, NULL, NULL, 1);
		if (instat < 0) continue;

		printf( "Enter default value:\n" );
		instat = getinp(2, NULL, &idef, NULL, 9);
		if (instat < 0) continue;

		printf( "Enter number of expected elements:\n" );
		instat = getinp(2, NULL, &index, NULL, 4);
		if (instat < 0) continue;

		int_ptr = (int *)malloc((size_t)index * sizeof(int));

		cst_ilst( string, ch, idef, index, int_ptr, &num, &iret );

		printf( "\nNumber of integers = %d : iret = %d\n", num, iret );
		for( i=0; i < index; i++ )
		   printf( "%d\n", int_ptr[i] );

		if( iret == 1 ) {
		   printf( "\nMore than the expected number of values");
		   printf( " entered.\n");
		}

		if( int_ptr )
		  free( int_ptr );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 9 ) {

		printf( "Enter an integer:\n" );
		instat =  getinp(2, NULL, &index, NULL, 11);
		if (instat < 0) continue;

		cst_inch( index, string, &iret );
		printf( "\nstring = %s : iret = %d\n", string, iret );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 10 ) {

		printf( "Enter number of integers (max %d):\n", LIMIT );
		instat = getinp(2, NULL, &index, NULL, 2);
		if (instat < 0) continue;

		if( index <= LIMIT ) {

		   int_ptr = (int *)malloc((size_t)index * sizeof(int));
		   ary_ptr = (char **)malloc((size_t)index * sizeof(char *));

		   for( i=0; i < index; i++ ) {
		      printf( "Enter integer #%d: ", i+1);
		      ary_ptr[i] = (char *)malloc(STRSIZE * sizeof(char));
		      instat = getinp(2, NULL, &int_ptr[i], NULL, 11);
		      if (instat < 0) continue;

		   }

		   cst_itoc( int_ptr, index, ary_ptr, &iret );

		   printf( "\niret = %d\n", iret );
		   for( i=0; i < index; i++ ) {
		      printf( "%s\n", ary_ptr[i] );
		      free( ary_ptr[i] );
		   }

		   if( int_ptr )
		      free( int_ptr );

		   if( ary_ptr )
		      free( (char **)ary_ptr );
		}
		else
		   printf( "\nInvalid number of integers\n");
	   }

/*----------------------------------------------------------------*/

	   if ( subnum == 11 ) {

		printf( "Enter number of integers (max %d):\n", LIMIT );
		instat = getinp(2, NULL, &index, NULL, 2);
		if (instat < 0) continue;

		if( index <= LIMIT ) {
		   int_ptr = (int *)malloc((size_t)index * sizeof(int));

		   for( i=0; i < index; i++ ) {
		      printf( "Enter integer #%d: ", i+1);
		      instat = getinp(2, NULL, &int_ptr[i], NULL, 11);
		      if (instat < 0) continue;
		   }

		   cst_itos( int_ptr, index, &num, string, &iret );

		   printf( "\niret = %d : num of chars = %d\n", iret, num ); 
		   printf( "string = %s\n", string );

		   if( int_ptr )
		      free( int_ptr );
		}
		else
		   printf( "\nInvalid number of integers\n");
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 12 ) {

		printf( "Enter string:\n" );
	    	instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_ldsp( string, string, &num, &iret );
		
		printf( "\nnc = %d\nstring = %s", num , string ); 
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 13 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, str, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter the number of chars to copy:\n" );
		instat = getinp(2,NULL, &index, NULL, 3);
		if (instat < 0) continue;

		cst_ncpy( string, str, index, &iret );

		printf( "\niret = %d\n", iret );
		printf( "str1 = %s\nstr2 = %s\n", string, str );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 14 ) {

		sens=0;
		printf( "Enter string:\n" );
		instat = getinp( 1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;


		printf( "Enter search char:\n" );
		instat = getinp( 1, &ch, NULL, NULL, 1);
		if (instat < 0) continue;


		printf( "Enter number of occurrence:\n" );
		instat = getinp( 2, NULL, &index, NULL, 3);
		if (instat < 0) continue;

		printf( "Enter sensitivity state\n" );
		printf( "default=case sensitive   0=non sensitive:\n" );
		instat = getinp(2, NULL, &sens, NULL, 1);
		if (instat < 0) continue;


		cst_nocc( string, ch, index, sens, &pstr, &iret );

		printf( "\niret = %d : string = %s\n", iret, string );
		if ( iret == 0 ) {
		   printf( "position = %d\n", pstr );
		}

	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 15 ) {

		printf( "Enter a string:\n");
		instat = getinp( 1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;


		cst_numb( string, &num, &iret );

		printf( "computed number = %d : iret = %d\n", num, iret );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 16 ) {

		printf( "Enter a string range (use -):\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rang( string, alpha, omega, incr, &typ, &iret );

		printf( "\niret = %d\n", iret );
		printf( "first = %s : last = %s : inc = %s : type = %d\n",
				alpha, omega, incr, typ );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 17 ) {

		printf( "Enter floating point number:\n" );
		instat = getinp(3, NULL,  NULL, &val, 11);
		if (instat < 0) continue;


		printf( "Enter number of decimal places:\n" );
		instat = getinp(2, NULL, &ndp, NULL, 5);
		if (instat < 0) continue;

		cst_rlch( val, ndp, string, &iret );

		printf( "\nstring = %s : iret = %d\n", string, iret );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 18 ) {

		printf("Enter string of real values:\n");
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf("Enter Seperator:\n");

		instat = getinp(1, &ch, NULL, NULL, 1);
		if (instat < 0) continue;

		printf("Enter default value:\n");
		instat = getinp(3,NULL, NULL, &val, 11);
		if (instat < 0) continue;

		printf( "Enter number of expected elements (max %d):\n",
			LIMIT );
		instat = getinp(2, NULL, &num, NULL, 3);
		if (instat < 0) continue;

		if ( num <= LIMIT ) {
		    flt_ptr = (float *)malloc((size_t)num * sizeof(float) );

		    cst_rlst( string, ch, val, num, flt_ptr, &index, &iret );

		    printf( "\nNumber of values = %d : iret = %d\n", index, iret );
		    for( i=0; i < num; i++ )
		       printf( "%f\n", flt_ptr[i] );

		    if( iret == 1 ) {
		       printf( "\nMore than the expected number of values");
		       printf( " entered.\n");
		    }

		    if( flt_ptr )
		       free( flt_ptr );
		} else {
		    printf ( "\nToo many expected elements.\n" );
		}
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 19 ) {

		printf( "Enter string\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rmbl( string, str, &len, &iret );

		printf( "\niret = %d\n", iret );
		printf( "return string = %s\nlength = %d\n", str, len );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 20 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter substring:\n" );
		instat = getinp(1,sub, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rmst( string, sub, &pstr, str, &iret );

		printf( "\niret = %d\n", iret );
		printf( "outstr = %s\npos = %d\n", str, pstr );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 21 ) {

		printf( "Enter string:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rxbl( string, string, &num, &iret );

		printf( "\niret = %d\n", iret );
		printf( "outstr = %s\nlength = %d\n", string, num );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 22 ) {

		printf("Enter string:\n");
		instat = getinp( 1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		strlim = (int)strlen(string) + 1;
		if ( LIMIT * 4 < strlim ) strlim = LIMIT * 4;
		printf( "Enter number of characters to use (max %d):\n",
			strlim);
		instat = getinp( 2, NULL, &len, NULL, 3);
		if (instat < 0) continue;

		if ( len <= strlim ) {
		    int_ptr = (int *)malloc((size_t)len * sizeof(int));

		    cst_stoi( string, len, &num, int_ptr, &iret );

		    printf( "\nnval = %d : iret = %d\n", num, iret );
		    for( i=0; i < num; i++ )
			printf( "%d\n", int_ptr[i] );

		    if( int_ptr )
		       free( int_ptr );
		} else {
		    printf( "\nToo many characters.\n" );
		}
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 23 ) {

		printf( "Enter string:\n" );
		instat  = getinp( 1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_lstr( string, &num, &iret );
		
		printf( "\nnc = %d\nstring = %s", num , string ); 
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 24 ) {

		printf( "Enter string:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter character to insert:\n" );
		instat = getinp(1, &ch, NULL, NULL, 1);
		if (instat < 0) continue;


		printf( "Enter position of insertion\n" );
		instat = getinp(2, NULL, &index, NULL, 3);
		if (instat < 0) continue;
		
		cst_cins( &string[index], ch, &iret );

		printf( "\nstring = %s\n", string );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 25 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_lcuc( string, string, &iret );

		printf( "\nstring = %s\n", string );
		
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 26 ) {

		printf( "Enter beginning position\n" );
		instat = getinp(2, NULL, &index, NULL, 3);
		if (instat < 0) continue;

		printf( "Enter end position\n" );
		instat = getinp(2, NULL, &num, NULL, 3);
		if (instat < 0) continue;


		printf( "Enter search string:\n" );
		instat = getinp(1, schar, NULL, NULL, STRSIZE);
		if (instat < 0) continue;


		printf( "Enter text to search\n" );
		instat = getinp(1, str, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_srch( index, num, schar, str, &pstr, &iret );
		
		printf( "\niret = %d : position = %d\n", iret, pstr );
		printf( "text = %s\n", str ); 
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 27 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_uclc( string, string, &iret );

		printf( "\nstring = %s\n", string );

	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 28 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_unpr( string, string, &iret );
		
		printf( "\nstring = %s\n", string );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 29 ) {

		printf( "Enter string:\n" );
		instat = getinp( 1, string, NULL, NULL, STRSIZE );
		if ( instat < 0 ) continue;

		printf( "Enter pattern:\n" );
		instat = getinp( 1, pattern, NULL, NULL, STRSIZE );
		if ( instat < 0 ) continue;

		cst_ptmt( string, pattern, &match, &iret );
		
		if (   match )  printf("\nMATCH.\n" );
		if ( ! match )  printf("\nNO MATCH.\n" );
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 30 ) {

		printf( "Enter number of times in the string:\n" );
		instat = getinp( 2, NULL, &ntimes, NULL, 3 );
		if ( instat < 0 ) continue;

		printf( "Enter time string:\n" );
		instat = getinp( 1, string, NULL, NULL, STRSIZE );
		if ( instat < 0 ) continue;

		cst_tims( ntimes, string, tmarry, &iret );

		printf( "\niret = %d\n", iret );

		for( i=0; i < ntimes; i++ ) {
		    printf( "%s\n", tmarry[i] );
		}
	   }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 31 ) {

		printf( "Enter string:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rnan( string, str, &iret );

		printf( "\niret = %d\n", iret );
		printf( "outstr = %s\n\n", str );
	   }

/*-----------------------------------------------------------------------*/

	    if ( subnum == 32 ) {

		printf( "Enter string:\n" );
		instat = getinp(1, string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter the separator character(s):\n");
		instat = getinp(1, sep, NULL, NULL, STRSIZE);
		if (instat < 0) continue;
		
		printf( "Enter maximum length per line\n" );
		instat = getinp( 2, NULL, &len, NULL, 3 );
		if ( instat < 0 ) continue;

		printf( "Enter end-of-line substitution string\n" );
		instat = getinp( 1, eol, NULL, NULL, STRSIZE );
		if ( instat < 0 ) continue;
		
		cst_ctrl( eol, eol, &leneol, &iret );

		printf( "Enter indentation string for lines after first line\n" );
		instat = getinp( 1, newLineStr, NULL, NULL, STRSIZE );
		if ( instat < 0 ) continue;

		cst_wrap( string, sep, &len, eol, newLineStr, str, &iret );
		printf( "\niret = %d\n", iret );
		printf( "\nMaximum line length = %d\n", len);
		printf( "\noutstr (length %d, bracketed >< ) = \n>%s<\n\n",
			(int)strlen(str), str);
            }

/*-----------------------------------------------------------------------*/

	    if ( subnum == 33 ) {

		printf( "Enter string:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter substring:\n" );
		instat = getinp(1,substr, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter replacement string:\n" );
		instat = getinp(1,repstr, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rpst( string, substr, repstr, outstr, &iret );

		printf( "\niret = %d\n", iret );
		printf( "\noutstr (length %d, bracketed >< ) = \n>%s<\n\n", 
			(int)strlen(outstr), outstr);
	    }

/*-----------------------------------------------------------------------*/

 	    if ( subnum == 34 ) {

		printf( "Enter tag:\n" );
		instat = getinp(1,tag, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter string (\"NULL\" for blank):\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;
		if ( strcmp(string,"NULL") == 0 )  string[0] = '\0';

		printf( "Enter default string:\n" );
		instat = getinp(1,defstr, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_gtag( tag, string, defstr, tagval, &iret );

		printf( "\niret = %d\n", iret );
		printf( "\nval (length %d, bracketed >< ) = \n>%s<\n\n", 
			(int)strlen(tagval), tagval);
	    }

/*-----------------------------------------------------------------------*/

 	    if ( subnum == 35 ) {
		double degree = 0.0;

		printf( "Enter latitude or longitude:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_ctod( string, &degree, &iret );

		printf( "\niret = %d\n", iret );
		printf( "\ndegree =  %f\n", degree);
	    }

/*-----------------------------------------------------------------------*/

	   if ( subnum == 36 ) {

		printf( "Enter tag:\n" );
		instat = getinp(1,tag, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter tag value:\n" );
		instat = getinp(1,value, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter string (\"NULL\" for blank):\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;
		if ( strcmp(string,"NULL") == 0 )  string[0] = '\0';

		len = sizeof(string);
		cst_stag( tag, value, &len, string, &iret );

		printf( "\niret = %d\n", iret );
		printf( "\nstring (length %d, bracketed >< ) = \n>%s<\n\n", 
			(int)strlen(string), string);
	   }

/*-----------------------------------------------------------------------*/
	   if ( subnum == 37 ) {

		printf( "Enter target string:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		printf( "Enter string to be concatenated:\n" );
		instat = getinp(1, str, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_ncat( string, str, &index, &iret );

		printf( "\niret = %d\n", iret );
		printf( "\nstr1 = %s\n", string );
	   }
/*-----------------------------------------------------------------------*/
	   if ( subnum == 38 ) {

	        printf ( "Enter type of sort to be done.\n");
	        printf ( "1 = Forward,  -1 = Backward \n");
	        printf ( "2 = Forward unique only,  -2 = Backward  unique only\n");
		scanf ( " %d", &itype);

		printf ( "Enter number of items to be sorted.\n");
		scanf ( " %d", &num);

		cnam_arr = (char **)malloc(num * sizeof(char *));

		for ( ii = 0; ii < num; ii++ ) {
		    printf ( "Enter county name #%d :\n", ii+1 );
		    scanf ( " %s", str);
		    len = strlen( str );
		    cnam_arr[ii] = (char *)malloc((len+1) * sizeof(char));
		    strcpy( cnam_arr[ii], str );
		}
		cst_sort ( itype, &num, cnam_arr, &nout, cnam_arr, &iret);

		printf( "\niret = %d\n", iret );

		for( ii = 0; ii < nout; ii++ ) {
		    printf( "%s\n", cnam_arr[ii]);
		}

		for( ii = 0; ii < nout; ii++ ) {
		    free( cnam_arr[ii] );
		}

		if ( cnam_arr ) {
		   free( (char **) cnam_arr );
		}

	   }

/*-----------------------------------------------------------------------*/
	   if ( subnum == 39 ) {

		printf( "Enter string (\"NULL\" for blank):\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;
		if ( strcmp(string,"NULL") == 0 )  string[0] = '\0';

		printf( "Enter tag:\n" );
		instat = getinp(1,tag, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rmtag( tag, string, &iret );

		printf( "\niret = %d\n", iret );
		printf( "\nval (length %d, bracketed >< ) = \n>%s<\n\n", 
			(int)strlen(string), string);

           }
/*-----------------------------------------------------------------------*/
	   if ( subnum == 40 ) {

		printf( "Enter string:\n" );
		instat = getinp(1,string, NULL, NULL, STRSIZE);
		if (instat < 0) continue;

		cst_rspc( string, &iret );

		printf( "\niret = %d\n", iret );
		printf( "%s\n", string );

	   }
/*-----------------------------------------------------------------------*/
	   if ( subnum == 41 ) {

                printf( "Enter string:\n" );
                instat = getinp(1, string, NULL, NULL, STRSIZE);
                if (instat < 0) continue;

		printf ( "Open Parenthesis Index:\n" );
		instat = getinp(2, NULL, &num, NULL, 5);
                if (instat < 0) continue;

		locopn = &string[num];
                cst_opcl( string, locopn, &loccls, &iret );

		printf ( "iret = %d Close Parenthesis Index = %d\n",
		    iret, (int)(loccls-string) );

           }
/*-----------------------------------------------------------------------*/
	   if ( subnum == 42 ) {

                printf( "Enter number as a string:\n" );
                instat = getinp(1, string, NULL, NULL, STRSIZE);
                if (instat < 0) continue;

		printf ( "Total number of characters for result:\n" );
		instat = getinp(2, NULL, &num, NULL, 5);
                if (instat < 0) continue;

                cst_zpad ( string, &num, strout, &iret );

		printf ( "iret = %d\nZero padded string = >%s<\n",
		    iret, strout );

           }
/*-----------------------------------------------------------------------*/
	} /* while status loop */
    return (0);
}
