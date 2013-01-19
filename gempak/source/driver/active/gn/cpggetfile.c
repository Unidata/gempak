#include "geminc.h"
#include "cpgcmn.h"

#define PLOT_NODE "ipplo1"
#define PLOT_USERNAME "plot"
#define PLOT_DIR "/usr2/mgs/import/data"
#define PLOT_INDEXCMD "showit.sh"


int pg_getfile ( char *fname )
/************************************************************************
 * pg_getfile								*
 *									*
 * This function retrieves a plot file from a plot node based upon the 	*
 * plot filename.							*
 *									*
 * int pg_getfile ( fname )						*
 *									*
 * Input parameters:							*
 * 	*fname		char	Name of file to be retrieved		*
 *									*
 * Output parameters:							*
 *	pg_getfile  	int	Returned value:				*
 *				-1	Failed to retrieve file		*
 *	 			 0	Success				*
 **									*
 * Log:									*
 *	E. Wehner/EAi	 6/96	Created				 	*
 *	T. Piper/GSC	10/98	Prolog update				*
 ***********************************************************************/
{

    FILE *pfp;
    char cmd_in[120];
    char ftp_file[120];

    sprintf(ftp_file, "ftpfile.sh %s %s", PLOT_NODE, fname);

    if ( (pfp = popen(ftp_file, "r")) != 0)
    {
        fgets(cmd_in, sizeof(cmd_in), pfp);

        pclose(pfp);
        return 0;
    }
    else
        return -1;

    
}
