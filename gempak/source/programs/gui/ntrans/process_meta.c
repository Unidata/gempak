#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "Nxm.h"

#define NMC_HEADER	36080
#define FLABELSIZE 	64	
#define HEADERSTRING	"NTRANS METAFILE VERSION1.0\0\0\0\0\0\0"

int  doflip;
int  meta_id;
int  WordCount;
int  prtFlg;

extern short	*FrameBuffer;
extern char	*PRINTCAP;


/*
 * Private functions
 */
int	check_header ( void );	
void 	do_command ( unsigned short indx, int wlen ); 
int	get_long_data ( void );
void	scan_hmeta ( void );
void    Scan_Meta ( void );


/************************************************************************
*       PROCESS_META.C                                             	*
*                                                                  	*
*   Module to take care of translating metafile command.           	*
*                                                                  	*
*   Log:                                                           	*
*   Chien Lin/EAI      10/92                                       	*
*   Chien Lin/EAI       9/93 taking out frame allocation           	*
*   S. Chiswell/Unidata 11/96 Added byte swapping when necessary   	*
*   S. Wang/GSC		01/97 combine scan_meta.c int to it and    	*
*				replace all Xlib funcitons with	   	*
*				gplt functions.			   	*
*   G. Krueger/EAI	09/97 Changed NxmWarning -> NxmWarn_show   	*
*   S. Jacobs/NCEP	11/97 Changed calc of justification	   	*
*   S. Jacobs/NCEP	12/97 Added clip flag to justification     	*
*   S. Jacobs/NCEP	 3/98 Added processing of FILL_STYLE	   	*
*   I. Durham/GSC	 5/98 Changed call for underscore	   	*
*   S. Jacobs/NCEP	12/97 Removed clip flag from justification 	*
*   S. Jacobs/NCEP	 1/99 Reordered header files		   	*
*   R. Tian/SAIC	 4/02 Added retrival of file header		*
*   M. Li/SAIC		 7/02 Added bytes flip of the file head values	*
************************************************************************/

int ScanFile ( char *meta_file )
{
int		status;
char		*def_dir, filename[256];

/*---------------------------------------------------------------------*/

 	def_dir = (char *)getenv("NMC_META");

        if (!meta_file) {
                printf("No metafile. \n");
		return(0);
        }

        if ((meta_id = open(meta_file,O_RDONLY)) < 0) {

		strcpy(filename, def_dir);
		strcat(filename,"/");
		strcat(filename, meta_file);

        	if ((meta_id = open(filename,O_RDONLY)) < 0) {
                	printf("Can not open metafile. \n");
			return(0);
		}
		else 
			strcpy (MetaFile , filename);
        }

        meta_st.frame_size = MAXFRAMES;
        meta_st.num_frames = 0;

	status = check_header();
	if ( status  == 1 ) {
		Scan_Meta();
	}
	else {
		scan_hmeta();
	}

	return(1);

}

/*====================================================================*/

void Scan_Meta ( void )
{
short	 	buffer;
unsigned  	indx;
int		data_len;
int             frame_count = 0;
int		byte_count = 0;
unsigned char   dummy[32767];

/*-------------------------------------------------------------------*/
	byte_count = 0;

        while (( read(meta_id, &buffer, 2)) > 0) {
		byte_count += 2;
		indx    = (unsigned)buffer & (~(LONG_FORM));
		data_len = buffer & (LONG_FORM);
		if (data_len == (LONG_FORM)) {
			data_len = 0;
			data_len = get_long_data();
			if ( data_len == -1 ) 
				return;
			byte_count += 2;
		}
		data_len += (data_len%2);
		byte_count += data_len;
		read(meta_id,dummy,(size_t)data_len);

                if (indx ==  BEG_PICBODY) {
      
                        frame_count++;

                        printf("picture body...have to doflip?\n");
                     	(meta_st.frame[meta_st.num_frames]).off_byte = 
				byte_count; 
 
                     	strcpy((meta_st.frame[meta_st.num_frames]).label," ");
                        (meta_st.num_frames)++;
		}

                if (indx ==  END_PIC) 
                    (meta_st.frame[meta_st.num_frames]).end_byte = 
				byte_count; 
        }

        lseek(meta_id,(off_t)0,0);
}

/*====================================================================*/

void Close_Meta ( void )
{
    close(meta_id);
}

/*====================================================================*/

void Trans_Frame ( int frame_no )
{
int   		start_byte, end_byte, byte_len;
int 		i, iret;
off_t 		offset;
int		status;
unsigned short  indx;
int		data_len;
int		ltype, t_flag, w_flag;

/*---------------------------------------------------------------------*/

	i = 1;
	ltype = 0;
	t_flag = 0;
	w_flag = 0;

	gsline(&ltype, &t_flag, &i, &w_flag, &iret);   
	gscolr(&i, &iret);  

        start_byte = (meta_st.frame[frame_no]).off_byte;
        end_byte = (meta_st.frame[frame_no]).end_byte;
        byte_len = end_byte - start_byte;
	WordCount = 0;

	offset = (off_t)(start_byte);
	lseek(meta_id,offset,SEEK_SET);

	status = read(meta_id, FrameBuffer, (size_t)byte_len);

	if (status < 0 ) {
		printf(" error calling read, %d returned\n", status );
		return;
	}

	prtFlg = NxmPrt_isPrtFlgSet();

	while(WordCount <= (byte_len/2)) {

		indx = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1) byte_flip(&indx);
		WordCount ++;

		data_len = indx & (LONG_FORM);

		if (data_len != 0) { 
			if (data_len == (LONG_FORM)){
			    data_len = 0;
			    data_len = get_long_data();
			};
		};

		indx &= ~(LONG_FORM);

		if (indx == END_PIC)	{
		break;
		};

		do_command(indx, data_len);
	}

	geplot(&iret);
	if ( iret != 0 ) {
		printf(" error calling geplot, iret = %d\n", iret);
		return;
	}
}

/*=======================================================================*/

int get_long_data ( void )
{
unsigned short 	ext; 
int 	llen = 0 ;

/*----------------------------------------------------------------------*/

	ext = (unsigned short)FrameBuffer[WordCount];
        if(doflip == 1) byte_flip(&ext);

        if( ext == 0) {
		return(-1); 
        }

	if ((ext & 0x8000) == 1)
		printf("more data to come...\n");

	WordCount ++;

        llen = abs((int)ext) ;

        return( llen ) ;
}

/*=======================================================================*/

void do_command ( unsigned short indx, int wlen )
{
unsigned short	temp;
int 		len, temp_pam;

/*-------------------------------------------------------------------------*/
	
	temp = 0;
	len = wlen + (wlen%2);

	switch ( indx ) {
	
	case	NOOP :
		if(len)
		WordCount += len/2;
		break;
	case	BEG_PICBODY :
		if(len)
		WordCount += len/2;
		break;
	case		LINE :
		do_line(len);
		break;
	case		CGM_CLIP :
		if(len)
		WordCount += len/2;
		break;
	case		CLIP_RECT :
		if(len)
		WordCount += len/2;
		break;
	case		LINE_COLOR :
		temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1)
                   temp_pam = temp & 0x00ff;
                else
		   temp_pam = (temp >> 8) & 0x00ff;
		do_line_color(temp_pam);
		if (len)
		WordCount += len/2;
		break;
	case		COLOR_TAB :
		do_color_tab();
		break;
	case		POLYGON_SET :
		if (len)
		WordCount += len/2;
		break;
	case		POLYGON :
		do_polyfill(len);
		break;
	case		FILL_STYLE :
                temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1)
                   temp_pam = temp >> 8;
                else
                   temp_pam = temp & 0x00ff;
		do_fill_patrn(temp_pam);
		if (len)
		WordCount += len/2;
		break;
	case		FILL_COLR :
                temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1)
                   temp_pam = temp & 0x00ff;
                else
                   temp_pam = temp >> 8;
		do_fill_color(temp_pam);
		if (len)
		WordCount += len/2;
		break;
	case		PATTN_SIZE :
		if (len)
		WordCount += len/2;
		break;
	case		LINE_WIDTH :
                temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1)
                   temp_pam = (temp >> 8) & 0x00ff;
                else
                   temp_pam = temp & 0x00ff;
		do_lnwdth(temp_pam);
		if (len)
		WordCount += len/2;
		break;
	case		BEG_PIC :
		if (len)
		WordCount += len/2;
		break;
	case		MESSAGE :
		if (len)
		WordCount += len/2;
		break;
	case		CIRCLE :
		do_circle();
		break;
	case		TEXT :
		do_text(wlen);
		break;
	case		TEXT_FONT_INX :
                temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1) byte_flip(&temp);
                temp_pam = (int)temp;

		if (temp_pam%3 == 0)
		   GraphicsData.textfont = (temp_pam/3 - 1)*10 + 3;
		else
		   GraphicsData.textfont = (temp_pam/3)*10 + temp_pam%3;

		do_text_set();
		WordCount ++;
		break;
	case		CHAR_HEIGHT :
                temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1) byte_flip(&temp);
                temp_pam = (int)temp;
                GraphicsData.textsize = (float)((double)temp_pam/14.0);

		if ( MpanelMode == 1 && prtFlg )  {
		    if ( Mpanel.columns==2 && Mpanel.rows == 1  )
			GraphicsData.textsize = 0.8F;
		    else
			GraphicsData.textsize = 0.5F;
		}

		do_text_set();
		WordCount ++;
		break;

	case		TEXT_ALIGN:
                temp = (unsigned short)FrameBuffer[WordCount];
                if(doflip == 1) byte_flip(&temp);
                temp_pam = (int)temp;
		GraphicsData.textalgn = temp_pam;

		do_text_set();
		WordCount += len/2;
		break;
	case	BEG_DEFAULTS:
		break;
	default:
		printf("unknown command : indx ---- %04x %d\n",indx,len);

		if (len)
		WordCount += len/2;
		break;
	}
}

/*=========================================================================*/

void scan_hmeta ( void )
{
short   char1;
int     frame_count = 0;
int   	intbuf, nword; 
int	byte_in[2],byte_out[2];
char	title[FLABELSIZE], message[200];

/*-------------------------------------------------------------------------*/

        lseek( meta_id, 36, SEEK_SET);
        read(meta_id, &char1, 2);

        if(char1 > 255) {
           doflip = 1;
        }
        else {
           doflip = 0;
        } 

	lseek( meta_id, 0, SEEK_SET);
        read(meta_id, &meta_head, sizeof(nc_file_header));

/*
 * Flip the bytes of the file header values
 */
        if ( doflip ) {
            byte_flip ( &(meta_head.maxframe) );
            byte_flip ( &(meta_head.machtype) );
            byte_flip ( &(meta_head.version) );
            byte_flip ( &(meta_head.fxsize) );
            byte_flip ( &(meta_head.fysize) );
	}
 
        while (( read(meta_id, &title[0], FLABELSIZE)) > 0) {

		if ( frame_count == MAXFRAMES ) {
                sprintf(message, "The number of frames in metafile exceeds the limit of %d\n. Only %d frames are loaded.\n", MAXFRAMES, MAXFRAMES);
                NxmWarn_show ( DrawingW, message);
                break;
		}

        	read(meta_id, &intbuf, 4);
                if(doflip == 1)
                   {
                   byte_in[0] = intbuf;
                   nword = 1;
                   mv_swp4(&nword,byte_in,byte_out);
                   intbuf = byte_out[0];
                   }
		if (intbuf != 0) {
                        frame_count++;

                        (meta_st.frame[meta_st.num_frames]).off_byte =
                                intbuf;
                        strcpy((meta_st.frame[meta_st.num_frames]).label,
                                title);

        		read(meta_id, &intbuf, 4);
                        if(doflip == 1)
                           {
                           byte_in[0] = intbuf;
                           nword = 1;
                           mv_swp4(&nword,byte_in,byte_out);
                           intbuf = byte_out[0];
                           }
                        (meta_st.frame[meta_st.num_frames]).end_byte =
                                intbuf;
                        (meta_st.num_frames)++;
                }
		else	break;
        }
        lseek( meta_id, 0, SEEK_SET);
}

/*===========================================================================*/

int check_header ( void )
{
int	IsHeader = 0;
char	dummy[NMC_HEADER];

/*-----------------------------------------------------------------------------*/

        read(meta_id, &dummy[0], 32);
	strcat(dummy,"\0");

	if (strncmp(HEADERSTRING, dummy,FLABELSIZE) != 0)
		IsHeader = 1;
	else
		IsHeader = 0;

        lseek( meta_id, 0, 0);

	return (IsHeader);

}
