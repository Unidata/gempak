#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAB     9

int main ( int argc, char *argv[])
{
int ii, jj, maxlen=72;
char line[512], cline[512];
FILE *fp;

    if ( (fp = fopen(argv[1],"r") ) != NULL ) {
       while ( fgets(line,511,fp) != NULL) {
          if ( ( line[0] == ' ' ) || ( line[0] == TAB ) ) {
             ii = strlen(line) - 1;
             while ( ( ii >= 0 ) && 
		( ( line[ii] == '\n') || ( line[ii] == ' ' ) ) ) {
                line[ii] = '\0';
                ii--;
             }
             if ( strlen(line) > maxlen) maxlen = strlen(line);
          }
       }
       if ( maxlen > 72 ) {
	  printf("check file %s %d\n",argv[1],maxlen);
          exit(1);
       }
    }
exit(0);
}

