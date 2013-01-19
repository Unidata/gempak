#include "geminc.h"
#include "cpgcmn.h"

#define PLOT_NODE "ipplo1"
#define PLOT_USERNAME "plot"
#define PLOT_DIR "/usr2/mgs/import/data"
#define PLOT_INDEXCMD "showit.sh"

int pg_getfname ( char *subset, char *fname, char *descr )
/************************************************************************
 * pg_getfname								*
 *									*
 * This function retrieves the name of a plot file from the plot node	*
 * based upon the product index table.					*
 *									*
 * int pg_getfname  ( subset, fname, descr )				*
 *									*
 * Input parameters:							*
 *	*subset		char 	Subset to retrieve			*
 *									*
 * Output parameters:							*
 *	*fname		char 	Name of file for this subset		*
 *	*descr		char	ASCII descriptor of that subset		*
 *	pg_getfname	int	Return value:				*
 *				-1	Failure				*
 *				 0	Success				*
 **									*
 * Log:									*
 * E. Wehner/EAi	 5/96	Created				 	*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{

    FILE *pfp;
    char cmd[120];
    char cmd_in[120];
    char file_name[20];
    char *s;
    int i;

    sprintf(cmd, "rsh %s@%s %s %s | cut -c7-", 
				PLOT_USERNAME, PLOT_NODE, PLOT_INDEXCMD,
				subset);

    if ( (pfp = popen(cmd, "r")) != 0)
    {
        fgets(cmd_in, sizeof(cmd_in), pfp);
 
        pclose (pfp);

        /* get the name of the file by looking at the first 14 bytes */
        strncpy(file_name, cmd_in, 16);
        file_name[16] = '\0';

        s = &file_name[0];
        /* remove the left padding from the filenam */
        while ( (*s <= ' ') && (s < &file_name[19]) ) s++;

        if ( ( s[0] < ' ') || (s[0] > 'z') )
        {
            return -1;
        }
        sprintf(fname, "%s/%s", getenv("FAX_TEMP"), s);

        /* remove cr if it is there */
        for (i=0;i<(int)strlen(fname); i++)
            if (fname[i] <= ' ') fname[i] = '\0';


        strncpy(descr, &cmd_in[15], 100);

    }
    else
        return -1;

    return 0;
  
}
