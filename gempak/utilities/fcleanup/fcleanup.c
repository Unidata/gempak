#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define TAB     9

int main ( int argc, char *argv[])
{
int ii, jj, ilen=0, qcnt, qflg=0, lastq, informat=0;
char line[512], cline[512], oline[512];
FILE *fp;

    if ( (fp = fopen(argv[1],"r") ) != NULL ) {
       while ( fgets(line,511,fp) != NULL) {
          /*
           * Check for tabs in continuation lines only
           */
	  lastq = 0;
          if ( ( line[0] == ' ' ) && ( strchr(line, TAB) != NULL ) ) {
             jj = 0;
             for ( ii=0; ii < strlen(line); ii++) {
                if ( line[ii] != TAB ) {
                   cline[jj] = line[ii];
                   jj++;
                }
                else {
                   cline[jj] = ' '; jj++;
                   while ( ( jj % 8 ) != 0 ) {
                      cline[jj] = ' '; jj++;
                   }
                }
             }
             cline[jj] = '\0';

             if ( ( cline[5] != ' ' ) && 
		( ( informat ) || ( qflg ) ) ) {
                printf("%s",cline);
	        if ( ( jj > 72 ) && ( cline[72] != '\n' ) ) {
		   ilen++;
		   printf("C Above line greater than 72 characters [%d]\n",jj);
	        }
		/*else
		   printf("C look qflg [%d] informat [%d]\n",qflg,informat);*/
             }
             else
                printf("%s",line);

             if ( cline[5] != ' ' ) {
		lastq = 1;
	     }
	     else {
		informat = 0;
	     }
          }
          else {
             printf("%s",line);
             if ( ( line[0] != ' ' ) || ( line[5] == ' ' ) ) 
	        informat = 0;
	  }

	  qcnt = 0;
          for ( ii=0; ii < strlen(line); ii++) {
             oline[ii] = toupper(line[ii]);
             if ( oline[ii] == '\'' ) qcnt++;
	  }
          oline[strlen(line)] = '\0';
          if ( ( qcnt % 2 != 0 ) || ( ( lastq ) && ( qflg ) ) )
	     qflg = 1;
	  else
             qflg = 0;

	  if ( strstr(oline,"FORMAT") != NULL ) informat = 1;
       }
    }
    if ( ilen )
       exit(1);
    else
       exit(0);
}

