#include "context.h"
#include "direct.h"
#include "coords.h"
#include "util.h"
#include "arasubs.h"
#include "mcnav.h"

#define mxcdsz 640

int ARTOEA(int iar,int iarlin,int iarele,int *inbuf,int *ilin,int *iele,
            double *xlat,double *xlon)  
/* transform area coordinates to earth coordinates 
   Inputs  : iar    - area number
             iarlin - area line coordinate 
             iarele - area element coordinate
             inbuf  - area directory array (64 words)
   Outputs : ilin   - satellite coordinate (line)
             iele   - satellite coordinate (element)
             xlat   - earth coordinate (latitude)
             xlon   - earth coordinate (longitude)
*/
{
  double dum1,dum2, dlin, dele;
  int iok=0;
  int navsiz;
  int iarr[mxcdsz];
  float uplin, upele, reslin, resele;

  dum1 = 0.0;
  dum2 = 0.0;
  uplin = (float) inbuf[5];
  upele = (float) inbuf[6];
  reslin = (float) inbuf[11];
  resele = (float) inbuf[12];
  dlin = (double) (iarlin*reslin + uplin);
  dele = (double) (iarele*resele + upele);
  *ilin = dblnint(dlin);
  *iele = dblnint(dele);

  /* get info for nvprep and nvini calls */
  navsiz=inbuf[62]-inbuf[34];
  if(inbuf[62]==0) navsiz=inbuf[33]-inbuf[34];
  iok = araget(iar,inbuf[34],navsiz, iarr);
  if(nvprep(1,iarr)!=0) return(-2);
  iok=nv1sae(dlin, dele,dum1, xlat, xlon, &dum2); 
/* Should return iok instead of 0?? */
  return(iok);
} 

int EATOAR(int iar,double xlat,double xlon,int *inbuf,
            int *iarlin,int *iarele) 
/* transform earth coordinates to area coordinates 
   Inputs  : iar    - file descriptor for area
             xlat   - earth coordinate (latitude)
             xlon   - earth coordinate (longitude)
             inbuf  - area directory array (64 words)
   Outputs : iarlin - area line coordinate 
             iarele - area element coordinate
*/
{
  double xxras,xxpix,dum1, dum2;
  int iok=0;
  int navsiz;
  int iarr[mxcdsz];
  float uplin, upele, reslin, resele;

  dum1 = 0.0;
  dum2 = 0.0;

  /* get info for nvprep and nvini calls */
  navsiz=inbuf[62]-inbuf[34];
  if(inbuf[62]==0) navsiz=inbuf[33]-inbuf[34];
  iok = araget(iar,inbuf[34],navsiz,iarr);
  if(nvprep(1,iarr)!=0) return(-2);
  uplin = (float) inbuf[5];
  upele = (float) inbuf[6];
  reslin = (float) inbuf[11];
  resele = (float) inbuf[12];
  iok=nv1eas(xlat,xlon,dum1,&xxras,&xxpix,&dum2);
  *iarlin= dblnint((xxras-(double)uplin)/(double)reslin);
  *iarele= dblnint((xxpix-(double)upele)/(double)resele);
  return(0);
} 

int IMTOAR(int ilin,int iele,int *inbuf,
            int *iarlin,int *iarele)
/* transform image coordinates to area coordinates 
   Inputs  : ilin   - satellite coordinate (line)
             iele   - satellite coordinate (element)
             inbuf  - area directory array (64 words)
   Outputs : iarlin - area line coordinate
             iarele - area element coordinate
*/
{ 
  double xlin, xele, uplin, upele, reslin, resele;

  xlin= (double) ilin;
  xele= (double) iele;
  uplin= (double) inbuf[5];
  upele= (double) inbuf[6];
  reslin= (double) inbuf[11];
  resele= (double) inbuf[12];

  *iarlin= dblnint( (xlin-uplin)/reslin );
  *iarele= dblnint( (xele-upele)/resele );
  return(0);
}

int ARTOIM(int *ilin,int *iele,int *inbuf,int iarlin,int iarele)
/* transform area coordinates to image coordinates 
   Inputs :  iarlin - area line coordinate
             iarele - area element coordinate
             inbuf  - area directory array (64 words)
   Outputs : ilin   - satellite coordinate (line)
             iele   - satellite coordinate (element)
*/
{
  double xarlin,xarele,uplin,upele,reslin,resele;

  xarlin=(double)iarlin;
  xarele=(double)iarele;
  uplin=(double)inbuf[5];
  upele=(double)inbuf[6];
  reslin=(double)inbuf[11];
  resele=(double)inbuf[12];
  *ilin = dblnint( (iarlin*reslin)+uplin );
  *iele = dblnint( (iarele*resele)+upele );
  return(0);
}

