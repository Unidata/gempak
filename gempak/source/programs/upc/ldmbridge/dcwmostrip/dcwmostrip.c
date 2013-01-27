#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"
#include "mkdirs_open.h"

int main ( int argc, char **argv )
/************************************************************************
 * wmostrip								*
 *									*
 * This program strips the wmo header from a GIF, PNG, or FAX/G3	*
 * graphics file. The header is considered to be anything before the 	*
 * magic words "GIF", "\211PNG", and "DFAX". The program takes two	*
 * command line entries:						*
 *									*
 * 	wmostrip input_file output_file					*
 *									*
 * Where input_file is the file with the WMO header before the graphic	*
 * data, and output_file is the file to be created without the header.	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/02	Created					*
 * S. Chiswell/Unidata	 7/02	Create based on wmostrip.c		*
 ***********************************************************************/
{

#define NUMEXP  1
#define BUFSZ	8192

int     nexp    = NUMEXP;
char    *prgnam = "DCWMOSTRIP";

long	flen = BUFSZ, rlen;
int	i, ihstrt, nread, nbytes, iret, graphstart=0;
int	in_file, out_file;
unsigned char	*buffer;

mode_t omode=S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH;

/*
**      Do not change these variables. These variables are used by all
**      decoders for getting the command line parameters.
*/
char    parms[NUMEXP][DCMXLN];
int     num;

char    gemfil[DCMXLN];
char	exttyp[5];

/*---------------------------------------------------------------------*/

/*
**      Initialize the output logs, set the time out and
**      parse the command line parameters.
*/
        dc_init ( prgnam, argc, argv, nexp, parms, &num, &iret );
/*
**      Check for an initialization error.
**      On an error, exit gracefully.
*/
        if  ( iret < 0 )  {
            dc_exit ( &iret );
        }
/*
**      The output file name must be present.
**
**      Change this section for the specific decoder.
*/
        strcpy ( gemfil, parms[0] );

    /*
     * Check the user input.
     *
    if  ( argc < 3 )  {
    	printf ( "\nUsage: %s input_file output_file\n\n", argv[0] );

	printf ( "This program removes the WMO header from a GIF, PNG, or FAX/G3\n" );
	printf ( "graphic data file. The graphic data starts with either\n" );
	printf ( "\"GIF\", \"\\211PNG\", or \"DFAX\", respectively. This program will remove\n" );
	printf ( "everything before these keywords and write the remaining\n" );
	printf ( "data to the output file. The graphic data can then be\n" );
	printf ( "displayed using a standard plotting tool such as \"display\",\n" );
	printf ( "which is part of the ImageMagick package.\n\n" );

	printf ( "Examples:\n" );
	printf ( "wmostrip PGEE07_KKCI_20020420_0000 pgee07.fax\n" );
	printf ( "wmostrip QBBE00_PAWU_20020514_2105 qbbe00.gif\n\n" );

	return (1); 
    }*/

    memset(exttyp,0,sizeof(exttyp));
    
    out_file = -1;
    in_file = STDIN_FILENO;

    /*
     * Read from input stream into the buffer.
     */
    buffer = (unsigned char *) malloc ( (size_t)flen * sizeof(unsigned char) );

    while((rlen = read(in_file,buffer,flen)) > 0)
       {
       ihstrt = 0;
       if(graphstart == 0)
          {
          /*
           * Determine where the actual data starts.
           * Find either the "GIF", "\211PNG", or "DFAX" header and cut everything
           * before that.
           */
          for ( i = 0; i < (int)rlen; i++ ) 
              {
              if  ( ( buffer[i]   == 'G' &&
                      buffer[i+1] == 'I' &&
                      buffer[i+2] == 'F' )
		      ||
		    ( (char)(buffer[i]) == '\211' &&
                      buffer[i+1] == 'P' &&
                      buffer[i+2] == 'N' &&
                      buffer[i+3] == 'G' )
                      ||
                    ( buffer[i]   == 'D' &&
                      buffer[i+1] == 'F' &&
                      buffer[i+2] == 'A' &&
                      buffer[i+3] == 'X' ) )  
                  {

                  if(memcmp(buffer+i,"GIF",3) == 0)
                     sprintf(exttyp,".gif\0");
                  else if (memcmp(buffer+i,"DFAX",4) == 0)
                     sprintf(exttyp,".fax\0");
                  else if (memcmp(buffer+i,"\211\120\116\107",4) == 0)
                     sprintf(exttyp,".png\0");
                     
                  graphstart++;
                  ihstrt = i;
                  break;
                  }
              }
          nbytes = (int)rlen - ihstrt;
          if  ( nbytes <= 0 ) {
              printf ( "\n%s: The output file size is incorrect\n\n", argv[0] );
              close ( out_file );
              return (1);
          }

          /*
           * Write the output data to the output file.
           */
          write (out_file, buffer+ihstrt, nbytes);
          }
       else
          {
          ihstrt = 0;
          nbytes = rlen;
          }

       /*
        * Open the output file, including leading directories.
        */
       if(out_file < 0) 
          {
          strcat(gemfil,exttyp);
          out_file = mkdirs_open(gemfil,O_WRONLY|O_CREAT|O_TRUNC,omode);
          if(out_file < 0)
             printf("cound not open out_file %s\n",gemfil);
          }

       if(out_file >= 0)
	  {
	  if ( memcmp(buffer+ihstrt+nbytes-4,"\015\015\012\003",4) == 0 ) nbytes -=4;
          if ( nbytes > 0 ) write (out_file, buffer+ihstrt, nbytes);
          }
       }

    free(buffer);
    if(out_file >= 0)
       close(out_file);


    return (0); 
}
