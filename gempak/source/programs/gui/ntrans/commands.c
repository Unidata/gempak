#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "Nxm.h"


#define min(a,b) ((int)(a)<(int)(b)?(int)(a):(int)(b))

extern int  	meta_id;
extern int	WordCount;
extern int	doflip;
extern short	*FrameBuffer;
extern int	_colrMode;


void do_line_sub ( int number );

/************************************************************************
 *									*
 * commands.c								*
 *									*
 * This module takes care of executing meta file commands		*
 * It calls GEMPAK gplt functions.					*
 * 									*
 * C. Lin/EAI								*
 * S. Wang/GSC		01/97   replace all Xt functions with		*
 *				GPLT functions.				*
 * S. Jacobs/NCEP	 9/97	Changed call to gstext			*
 * S. Jacobs/NCEP	10/97	Changed gtextc to gtext			*
 * S. Jacobs/NCEP	 1/98	Added check for color 101		*
 * S. Jacobs/NCEP	 3/98	Added do_fill_patrn; call gsfill	*
 * I. Durham/GSC	 5/98   Changed call for underscore		*
 * S. Jacobs/NCEP	 7/98	Changed gtext to gtextc			*
 * A. Hardy/GSC          3/99   Changed kk to static int        	*
 * A. Hardy/GSC         11/00   renamed coord. sys declaration  	*
 ***********************************************************************/

/*=====================================================================*/

void do_color_tab ( void )
{
int 	indx, red, green, blue, iret;
float   fred, fgreen, fblue;
unsigned short temp;

	temp = FrameBuffer[WordCount];
        if(doflip == 1) byte_flip(&temp);

	indx = (int)( temp >>8 );
	red   = (int)((temp & 0x00ff)*257);
	WordCount ++;

        temp  = FrameBuffer[WordCount];
        if(doflip == 1) byte_flip(&temp);

	green = (int)((temp >> 8)*257);
	blue  = (int)((temp & 0x00ff)*257);
	WordCount ++;

	fred   = (float)red/65535.0F;
        fgreen = (float)green/65535.0F;
        fblue  = (float)blue/65535.0F;
	NxmSetColorInTable( indx, fred, fgreen, fblue );

	gscrgb(&indx, &red, &green, &blue, &iret);

	if ( iret != 0 ) {
		printf(" error calling gscrgb, iret = %d\n",
			iret);
		return;
	}
}

/*=====================================================================*/

void do_line ( int len )
{
int  	i, np, ntimes, nleft;
/*---------------------------------------------------------------------*/
	np = len/4;

	ntimes = np/100;
	nleft  = np - ntimes*100;

	if (ntimes == 0)
		do_line_sub(nleft);
	else {

		for (i = 0;i < ntimes;i++) {
			if (i == 0)
		   		do_line_sub(100);
			else 
		   		do_line_sub(101);

               		WordCount -= 2;
		};

		if (nleft) {
			nleft ++;
			do_line_sub(nleft);
		}
		else
               		WordCount += 2;
	}
	
}

/*====================================================================*/

void do_line_sub ( int number )
{
unsigned short  temp1,temp2;
int 		i;
float		fx[101], fy[101];
int             iret;

/*--------------------------------------------------------------------*/
        for (i = 0; i < number; i++) {

                temp1 = FrameBuffer[WordCount];
                WordCount ++;
                temp2 = FrameBuffer[WordCount];
                WordCount ++;
                if(doflip == 1)
                   {
                   byte_flip(&temp1);
                   byte_flip(&temp2);
                   }

		fx[i]  = (float)temp1;
		fy[i]  = (float)temp2;

	}
        gscolr( &(GraphicsData.line_colr), &iret );
	if ( iret != 0 ) {
		printf(" error calling gscolr\n");
		return;
	}

        gline(sys_M, &number, fx, fy, &iret, strlen(sys_M) );
	if ( iret != 0 ) {
		printf(" error calling gline, iret = %d\n", iret);
		return;
	}
}

/*=====================================================================*/

void do_polyfill ( int len ) 
{
int 	 i, np, iret, reset_patn;
float	 *fx, *fy, szfill;
unsigned short temp;
/*--------------------------------------------------------------------*/

        np = len/4;

        fx = (float *)malloc( np * sizeof(float) );
        fy = (float *)malloc( np * sizeof(float) );

        for (i = 0;i < np;i++) {
                temp = FrameBuffer[WordCount];
                if(doflip == 1) byte_flip(&temp);
                fx[i] = (float)temp;
		WordCount ++;

                temp = FrameBuffer[WordCount];
                if(doflip == 1) byte_flip(&temp);
                fy[i] = (float)temp;
		WordCount ++;
        }

	gscolr( &(GraphicsData.fill_colr), &iret );

	if ( iret != 0 ) {
		printf(" error calling gscolr, iret =%d\n", iret);
		return;
	}

	szfill = 1.0F;
	reset_patn = 1;
	gsfill ( &szfill, &(GraphicsData.fill_patn), &iret );

	if ( iret != 0 ) {
		printf(" error calling gsfill, iret = %d\n", iret);
		exit(0);
	}

	gfill( sys_M, &np, fx, fy, &iret, strlen(sys_M) );

	if ( iret != 0 ) {
		printf(" error calling gfill, iret = %d\n", iret);
		exit(0);
	}

	gsfill ( &szfill, &reset_patn, &iret );

	if ( iret != 0 ) {
		printf(" error calling gsfill, iret = %d\n", iret);
		exit(0);
	}
	
        free( fx );
        free( fy );
}

/*=====================================================================*/

void do_lnwdth ( int lnwdth )
{
int	ltype, t_flag, w_flag;
int 	iret;
/*--------------------------------------------------------------------*/
	ltype  = 0;
	t_flag = 0;
	w_flag = 0;

	gsline( &ltype, &t_flag, &lnwdth, &w_flag, &iret);

	if ( iret != 0 )
		printf(" error calling gsline, iret = %d\n", iret );
}

/*=====================================================================*/

void do_circle ( void )
{
float   fx[2], fy[2];
int     iret;
unsigned short temp;
int	number;
/*--------------------------------------------------------------------*/
	number = 2;

        temp = FrameBuffer[WordCount];
        if(doflip == 1) byte_flip(&temp);
        fx[0] = (float)temp;
        WordCount ++;

        temp = FrameBuffer[WordCount];
        if(doflip == 1) byte_flip(&temp);
        fy[0] = (float)temp;
        WordCount ++;

        temp = FrameBuffer[WordCount];
        if(doflip == 1) byte_flip(&temp);
	WordCount ++;

	fx[1] = fx[0];
	fy[1] = fy[0];

	/* use line color */
        gscolr( &(GraphicsData.line_colr), &iret );

	gline(sys_M, &number, fx, fy, &iret, strlen(sys_M) );
        if ( iret != 0 ) {
                printf(" error calling gline, iret = %d\n", iret);
                return;
        }
}

/*=====================================================================*/

void do_line_color ( int indx )
{
    GraphicsData.line_colr = indx;
    if ( NxmPrt_isPrtFlgSet()==0  )
	NxmDisplayCbColorcell( indx );
}

/*=====================================================================*/

void do_fill_color ( int indx )
{
    GraphicsData.fill_colr = indx;
    if ( NxmPrt_isPrtFlgSet()==0 && GraphicsData.fill_colr != 101 )
	NxmDisplayCbColorcell( indx );
}

/*=====================================================================*/

void do_fill_patrn ( int indx )
{
    GraphicsData.fill_patn = indx;
}

/*=====================================================================*/

void do_text ( int len )
{
long int        nc;
float		fx, fy;
int		ixoff, iyoff;
int             iret;
float		rotat;
char		textstr[400];
unsigned short	temp1, temp2;

/*--------------------------------------------------------------------*/

        nc = min( (len - 4), 399 );

	ixoff = 1;
	iyoff = 1;
	rotat = 0.0F;

        temp1 = FrameBuffer[WordCount];
        WordCount ++;
        temp2 = FrameBuffer[WordCount];
        WordCount ++;
        if(doflip == 1)
        {
           byte_flip(&temp1);
           byte_flip(&temp2);
        }

        fx = (float)temp1;
        fy = (float)temp2;

	memcpy(textstr, (char *)(&FrameBuffer[WordCount]), (size_t)nc);
	textstr[nc] = '\0';

        gscolr( &GraphicsData.line_colr, &iret );

	if ( iret != 0 ) {
		printf(" error calling gscolr, iret = %d\n", iret);
		return;
	}

	if  ( GraphicsData.textalgn % 10 == 2 )
	    gtextc ( sys_M, &fx, &fy, textstr, &rotat, &ixoff, &iyoff,
		     &iret, strlen(sys_M), strlen(textstr) );
	else
	    gtext  ( sys_M, &fx, &fy, textstr, &rotat, &ixoff, &iyoff,
		     &iret, strlen(sys_M), strlen(textstr) );

	if ( iret != 0 ) {
	    printf(" error calling gtext/gtextc, iret = %d\n", iret);
	    return;
	}	

        WordCount += (int)(nc/2 + nc%2);
}

/*=====================================================================*/

void do_text_set ( void )
{
int 	flag, width, ibrdr, irrotn, iret;
/*--------------------------------------------------------------------*/
	flag = 2;
	width = 0;
	ibrdr = 111;
	irrotn = 1;

	if ( GraphicsData.textfont != GraphicsData.oldtextfont ) { 
		gstext(&(GraphicsData.textfont), &flag, 
			&(GraphicsData.textsize), &width,
			&ibrdr, &irrotn, &(GraphicsData.textalgn),
			&iret);
		GraphicsData.oldtextfont = GraphicsData.textfont;
	}

	if ( !G_DIFF(GraphicsData.textsize, GraphicsData.oldtextsize) ) {
		gstext(&(GraphicsData.textfont), &flag, 
			&(GraphicsData.textsize), &width,
			&ibrdr, &irrotn, &(GraphicsData.textalgn),
			&iret);
		GraphicsData.oldtextsize = GraphicsData.textsize;
	}

	if ( GraphicsData.textalgn != GraphicsData.oldtextalgn ) {
		gstext(&(GraphicsData.textfont), &flag, 
			&(GraphicsData.textsize), &width,
			&ibrdr, &irrotn, &(GraphicsData.textalgn),
			&iret);
		GraphicsData.oldtextalgn = GraphicsData.textalgn;
	}
}
