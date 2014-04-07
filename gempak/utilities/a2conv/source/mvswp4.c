#include <stdio.h>

int mv_swp4 ( int *n, void *valin, void *valout) 
				 	
/************************************************************************
* MV_SWP4								*
*									*
* This function converts the byte order of each word from 1-2-3-4 to	*
* 4-3-2-1.								*
*									*
* INTEGER MV_SWP4  ( N, VALIN, VALOUT )					*
*									*
* Input parameters:							*
*	N		INTEGER		Number of values to convert	*
*	VALIN (N)	BYTE		Input data			*
*									*
* Output parameters:							*
*	VALOUT (N)	BYTE		Converted data			*
*	MV_SWP4		INTEGER		Return code			*
*					 0 = normal return		*
*					>0 = # of invalid inputs	*
*									*
* Log:									*
* Q. Zhou/Chug	        1/10		Re-write the fortran function   *
*                                       of the same name        	*
************************************************************************/
{
        unsigned char  *vali, *valo, *p;
        unsigned char  temp;
	int i=0;
	

/*      Loop through the words swapping the byte order from 1-2-3-4 to
        4-3-2-1. */
	vali = (unsigned char*) valin;
	valo = (unsigned char* )valout;
	p=	valout;
	while (i < *n) {
		temp = *vali;
		*valo = *(vali+3);
		*(valo +3) = temp;
		temp = *(vali+1);
		*(valo+1) = *(vali+2);
		*(valo+2) = temp;

		vali += 4;
		valo += 4;		
    		i++;
    	}

	valout = (void* )p;
	
        return 0;  
}       

/*int main ( int argc, char **argv )
{
int n=1;
int t=100;
int x,y;
mv_swp4 ( &n, &t, &x);
mv_swp4 ( &n, &x, &y);
    
printf ( "Enter81: %d\n", t );
printf ( "Enter81: %d\n", x );
printf ( "Enter81: %d\n", y );

    return ( 0 );

}*/
