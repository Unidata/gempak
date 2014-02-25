#include <geminc.h>
#include <gemprm.h>

#include "dccmn.h"
#include "dcuspln.h"


void dcuspln_input (nldn_file ltgf, int *retval)
{
char	line[512], line2[512];
int 	linep = 0, l2p=0, bufb = 0;
int	len;
int     iret;
int	DONE=0;

char errgrp[]={"dcuspln"},errstr[LLMXLN];
int loglev, numerr;

while ( ! DONE )
   {

   if ( linep > sizeof(line) ) {
      sprintf(errstr, "linep exceeded %d", sizeof(line));
      loglev = 0;
      numerr = -1;
      dc_wclg(loglev, errgrp, numerr, errstr, &iret);
   }
   if ( linep >= sizeof(line) ) linep = 0;

   if ( bufb == 0 )
      {
      if((DONE = bufread(STDIN_FILENO,line + linep,sizeof(line) - linep,&len)) != 0)
         {
         if(DONE < 0)
            continue;
         }

      bufb = len;
      }

   while ( ( bufb > 0 ) && ( line[linep] != '\n' ) )
      {
      line2[ l2p ] = line[linep];
      l2p++; linep++; bufb--;
      }

   if ( bufb == 0 )
      continue;

   line2[l2p] = '\0'; linep++; bufb--;
   l2p = 0;

   decode_strike(line2, ltgf);

   }


*retval = -1;

}
