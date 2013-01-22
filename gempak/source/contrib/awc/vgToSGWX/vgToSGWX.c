#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>

#define  STMSYM      25.0F
#define  VOLSYM      201.0F
#define  RADSYM      41.0F
#define  STMSYM_N    25.0F
#define  STMSYM_S    27.0F

/* Software: vgToSGWX.c
   Purpose: This software serves as a bridge to transfer the VGF objects 
      created by the SIGWX BUFR software into SGWX object VGF files to be
      used for placement in gpmap/gpmap_gif software.
    
   Input VGFs (from sigavgf):  
      This software acts on the following VGF files for input
      CAT - Create SGWX Clear Air Turbulence Objects
      CLD - Create SGWX Cloud Objects
      VTS - Creates SGWX Special Symbol Objects for Volcano/Tropical Cyclone/
            Radiation Symbols
      JET - Create Jet Objects with Labels properly rotated so they can
            be registered with placement objects to be placed around the jet.
   
   Usage Statement: Usage: vgToSGWX -i <filenamein> -o <filenameout> 
   
Log:
   L. Hinson/AWC     06/12    Created     
******************************************************************************/

static void genSGWXTurb(VG_DBStruct *el_line, VG_DBStruct *el_txt, 
                        char *vgFileOut);
static void genSGWXObject(VG_DBStruct *el_line, VG_DBStruct *el_txt, 
                        char *vgFileOut, int subtype);
static void genSGWXSpSymObject(VG_DBStruct *el_sym, VG_DBStruct *el_txt,
                                char *vgFileOut,int subtype);
static void genJetObject(VG_DBStruct *el, char *vgFileOut);

static void cvg_crthdrcolor ( VG_DBStruct *el, int np, float *lat, float *lon,
                              int major, int minor, int grouptype, int groupnumber, int *iret );

int main (int argc, char *argv[]) {
  static char usageString[] = "Usage: vgToSGWX -i <filenamein> -o <filenameout>";
  char vgFileIn [FILE_FULLSZ];
  char vgFileOut [FILE_FULLSZ];
  VG_DBStruct *el;
  FILE *fptr;
  char vgfname[128];
  long fileSize;
  int i, curPos, nextEl, nEl, one, ier;
  int infileset = 0;
  int outfileset = 0;
  int *flag;
  int iret;
  char command[256];
  long fsize;
  char newfil[256];
  
  for (i=0; i < argc; i++) {
    if (strcmp(argv[i], "-h") == 0) {
      printf("%s", usageString);
    }
    if (strcmp(argv[i], "-i") == 0) {
      strcpy(vgFileIn, argv[i+1]);
      infileset = -1;
    }
    if (strcmp(argv[i], "-o") == 0) {
      strcpy(vgFileOut, argv[i+1]);
      outfileset = -1;
    }
  }
  if (! infileset) {
    printf("%s",usageString);
    exit(1);
  }
  /* Load up Settings Table */
  ces_rtbl(&iret);
  one = 1;
  /* Read Input VGF File... */
  cvg_open(vgFileIn, 0, &fptr, &ier);
  if (ier != 0) {
    ier = -14;
    er_wmsg("vgToSGWX", &ier, vgFileIn, &iret, strlen("vgToSGWX"), strlen(vgFileIn) );
    iret = -1;
    return -1;
  }
  cfl_inqr(vgFileIn, NULL, &fileSize, vgfname, &ier );
  curPos = 0;
  nextEl = 0;
  nEl =    0;
  ier =    0;
  el =   NULL;
  flag = NULL;
  /*
  *  Read el from the input vg file.
  */
  while (nextEl < MAX_EDITABLE_ELEMS) {
    G_REALLOC(el, VG_DBStruct, nEl + 1, "vgToSGWX" );
    cvg_rdrecnoc ( vgfname, fptr, curPos, &el[ nEl ], &ier );
    if ( ier != 0 ) break;

    if ( el[ nEl ].hdr.recsz > 0 )  {

      curPos += el[ nEl ].hdr.recsz;

      if ( !el[ nEl ].hdr.delete )  {

        nEl++;

      }
    }

    nextEl++;
  }
  cvg_clos (fptr, &ier);
  printf("Read/Closed VGF File... Process VGF File...\n");
  /* Create Output VGF File... */
  cfl_inqr(vgFileOut, NULL, &fsize, newfil, &ier);
  if (ier < 0) {
    cvg_crvgf(vgFileOut, &ier);
  } else {
    sprintf(command,"rm -f %s",vgFileOut);
    system(command); 
    cvg_crvgf(vgFileOut, &ier);
  }
  for ( i = 0; i < nEl; i++) {
    /* Process Implied CAT Objects */
    if (el[i].hdr.vg_type == LINE_ELM ) {
      if (el[i+1].hdr.vg_type == SPTX_ELM ) {
        /* We will create SGWX object from these two types of data... */
        /* If Line Type is 5... call this a SGWX Turb Object...*/
        if (el[i].elem.lin.info.lintyp == 5) {
          genSGWXTurb(&el[i],&el[i+1],vgFileOut);
        }
      }
    }
    /* Process Implied Cloud/CB Objects */
    if (el[i].hdr.vg_type == SPLN_ELM ) {
      if (el[i+1].hdr.vg_type == SPTX_ELM) {
        /* If Special line type of 3 (Scallop)...  */
        if (el[i].elem.spl.info.spltyp == 3) {
          /* If the Special Text Type is 4, call this a Cloud/CB Object */
          if (el[i+1].elem.spt.info.sptxtyp == 4) {
            genSGWXObject(&el[i],&el[i+1],vgFileOut, SGWX_CONV);
          } else {
            /* If the special Text Type is 15, call this a ICETurb Object */
            if (el[i+1].elem.spt.info.sptxtyp == 15) {
              genSGWXObject(&el[i],&el[i+1],vgFileOut, SGWX_ICETURB);
            }
          }
        }
      }
    }
    if (el[i].hdr.vg_type == SPSYM_ELM  || el[i].hdr.vg_type == WXSYM_ELM ) {
      /* Is this a VOLSYM, STMSYM, or RADSYM? */
      if (fabs(el[i].elem.sym.data.code[0] - VOLSYM) < .001 ||
          fabs(el[i].elem.sym.data.code[0] - STMSYM) < .001 ||
          fabs(el[i].elem.sym.data.code[0] - RADSYM) < .001) {
        if (el[i+1].hdr.vg_type == SPTX_ELM) {
          genSGWXSpSymObject(&el[i],&el[i+1],vgFileOut, SGWX_SPSYM);
        }
      }
    }
    if (el[i].hdr.vg_type == JET_ELM ) {
      genJetObject(&el[i],vgFileOut);
    }      
  }
  return 0;
}

static void genSGWXTurb(VG_DBStruct *el_line, VG_DBStruct *el_txt, 
                        char *vgFileOut)
{
  VG_DBStruct el;
  int grpnum, ii, np, start, loc, ier;
  float sumlat, sumlon;
  float lat[256], lon[256];
  el.hdr.vg_class = CLASS_MET;
  el.hdr.vg_type = SGWX_ELM;
  el.elem.sgwx.info.subtype = SGWX_TURB;
  ces_get (SGWX_TURB, &el, &ier);
  el.hdr.grptyp = GRPTYP_SGWX;
  crg_ggnxt(GRPTYP_SGWX, &grpnum, &ier);
  el.hdr.grpnum = grpnum;
  /* Fill out rest of el structure */
  np = el_line->elem.lin.info.numpts;
  el.hdr.closed = el_line->hdr.closed;
  el.hdr.filled = 0;
  el.elem.sgwx.info.textlat = el_txt->elem.spt.info.lat;
  el.elem.sgwx.info.textlon = el_txt->elem.spt.info.lon;
  /* Compute a centroid */
  sumlat = sumlon = 0.0;
  for (ii = 0; ii < (np -1); ii++) {
    sumlat += el_line->elem.lin.latlon[ii];
    sumlon += el_line->elem.lin.latlon[ii+np];
  }
  el.elem.sgwx.info.arrowlat = sumlat/(np-1);
  el.elem.sgwx.info.arrowlon = sumlon/(np-1);
  /* Transfer the Turbulence Symbol */
  el.elem.sgwx.spt.info.turbsym = el_txt->elem.spt.info.turbsym;
  /* Transfer the Text Label Contents */
  strcpy(el.elem.sgwx.spt.text, el_txt->elem.spt.text);
  el.elem.sgwx.spt.info.sptxtyp = el_txt->elem.spt.info.sptxtyp;
    
  /* Write the SGWX Polygon Object */
  start = -1;
  el.elem.sgwx.info.npts = np;
  el.hdr.recsz = (sizeof (VG_HdrStruct) + sizeof (SGWXType));
  for ( ii = 0; ii < np; ii++ ) {
    lat[ii] = el.elem.sgwx.latlon[ii] = el_line->elem.lin.latlon[ii];
    lon[ii] = el.elem.sgwx.latlon[ii+np] = el_line->elem.lin.latlon[ii+np];
  }
  cvg_crthdrcolor ( &el, np, lat, lon, el.hdr.maj_col, el.hdr.min_col, 8,
                     grpnum, &ier);
  cvg_writefD( &el, start, el.hdr.recsz, vgFileOut, &loc, &ier);
}

static void genSGWXObject(VG_DBStruct *el_line, VG_DBStruct *el_txt, 
                        char *vgFileOut, int subtype)
{
   VG_DBStruct el;
  int grpnum, ii, np, start, loc, ier;
  float sumlat, sumlon;
  float lat[256], lon[256];
  el.hdr.vg_class = CLASS_MET;
  el.hdr.vg_type = SGWX_ELM;
  
  el.elem.sgwx.info.subtype = subtype;
  ces_get (subtype, &el, &ier);
  el.hdr.grptyp = GRPTYP_SGWX;
  crg_ggnxt(GRPTYP_SGWX, &grpnum, &ier);
  el.hdr.grpnum = grpnum;
  /* Fill out rest of el structure */
  np = el_line->elem.lin.info.numpts;
  /* el.hdr.closed = 1;*/
  el.hdr.closed = el_line->hdr.closed;
  el.hdr.filled = 0;
  el.elem.sgwx.info.textlat = el_txt->elem.spt.info.lat;
  el.elem.sgwx.info.textlon = el_txt->elem.spt.info.lon;
  /* Compute a centroid */
  sumlat = sumlon = 0.0;
  if (subtype == SGWX_TURB) {
    for (ii = 0; ii < (np -1); ii++) {
      sumlat += el_line->elem.lin.latlon[ii];
      sumlon += el_line->elem.lin.latlon[ii+np];
    }
  } else if (subtype == SGWX_CONV || subtype == SGWX_ICETURB) {
    for (ii = 0; ii < (np -1); ii++) {
      sumlat += el_line->elem.spl.latlon[ii];
      sumlon += el_line->elem.spl.latlon[ii+np];
    }
  }
  
  el.elem.sgwx.info.arrowlat = sumlat/(np-1);
  el.elem.sgwx.info.arrowlon = sumlon/(np-1);
  /* Transfer the Turbulence Symbol */
  el.elem.sgwx.spt.info.turbsym = el_txt->elem.spt.info.turbsym;
  /* Transfer the Text Label Contents */
  strcpy(el.elem.sgwx.spt.text, el_txt->elem.spt.text);
  el.elem.sgwx.spt.info.sptxtyp = el_txt->elem.spt.info.sptxtyp;
  el.elem.sgwx.spt.info.filcol = 31;
  el.elem.sgwx.spt.info.lincol = 32;
    
  /* Write the SGWX Polygon Object */
  start = -1;
  el.elem.sgwx.info.npts = np;
  el.hdr.recsz = (sizeof (VG_HdrStruct) + sizeof (SGWXType));
  if (subtype == SGWX_TURB) {
    for ( ii = 0; ii < np; ii++ ) {
      lat[ii] = el.elem.sgwx.latlon[ii] = el_line->elem.lin.latlon[ii];
      lon[ii] = el.elem.sgwx.latlon[ii+np] = el_line->elem.lin.latlon[ii+np];
    }
  } else if (subtype == SGWX_CONV || subtype == SGWX_ICETURB) {
    for ( ii = 0; ii < np; ii++ ) {
      lat[ii] = el.elem.sgwx.latlon[ii] = el_line->elem.spl.latlon[ii];
      lon[ii] = el.elem.sgwx.latlon[ii+np] = el_line->elem.spl.latlon[ii+np];
    }
  }
  cvg_crthdrcolor ( &el, np, lat, lon, el.hdr.maj_col, el.hdr.min_col, 8,
                     grpnum, &ier);
  cvg_writefD( &el, start, el.hdr.recsz, vgFileOut, &loc, &ier);
}


static void genSGWXSpSymObject(VG_DBStruct *el_sym, VG_DBStruct *el_txt,
                                char *vgFileOut,int subtype)
{
  VG_DBStruct el;
  int grpnum;
  int loc, start, ier;
  el.hdr.vg_class = CLASS_MET;
  el.hdr.vg_type = SGWX_ELM;
  el.hdr.closed = 0;
  el.elem.sgwx.info.subtype = subtype;
  ces_get( subtype, &el, &ier);
  el.hdr.grptyp = GRPTYP_SGWX;
  crg_ggnxt(GRPTYP_SGWX, &grpnum, &ier);
  el.hdr.grpnum = grpnum;
  el.elem.sgwx.info.npts = 1;
  el.elem.sgwx.info.splsym = 0;
  el.elem.sgwx.info.wxsym = 0;
  el.elem.sgwx.latlon[0] = el_sym->elem.sym.data.latlon[0];
  el.elem.sgwx.latlon[1] = el_sym->elem.sym.data.latlon[1];
  el.elem.sgwx.info.textlat = el_txt->elem.spt.info.lat;
  el.elem.sgwx.info.textlon = el_txt->elem.spt.info.lon;
  
  strcpy(el.elem.sgwx.spt.text,"");
  if (el_sym->hdr.vg_type == SPSYM_ELM || el_sym->hdr.vg_type == WXSYM_ELM) {
    /* Is this a STMSYM? */
    if (fabs(el_sym->elem.sym.data.code[0] - STMSYM) < .001) {
      if (el_sym->elem.sym.data.latlon[0] >= 0.0) {
        el.elem.sgwx.info.splsym = STMSYM_N;
      } else {
        el.elem.sgwx.info.splsym = STMSYM_S;
      }
      if (strstr(el_txt->elem.spt.text,"TCNN")) {
        strcpy(el.elem.sgwx.spt.text, "TC NN");
      } else {
        strcpy(el.elem.sgwx.spt.text, el_txt->elem.spt.text);
      }
    }
    /* Is this a VOLSYM? */
    if (fabs(el_sym->elem.sym.data.code[0] - VOLSYM) < .001) {
      el.elem.sgwx.info.wxsym = VOLSYM;      
      el.elem.sgwx.spt.info.turbsym = 0;
      strcpy(el.elem.sgwx.spt.text, el_txt->elem.spt.text);      
    }
    /* Is this a RADSYM? */
    if (fabs(el_sym->elem.sym.data.code[0] - RADSYM) < .001) {
      el.elem.sgwx.info.splsym = RADSYM;
      strcpy(el.elem.sgwx.spt.text, el_txt->elem.spt.text);
    }
    el.elem.sgwx.spt.info.sptxtyp = 4;
    el.elem.sgwx.spt.info.turbsym = 4;
    el.elem.sgwx.spt.info.txtcol = el_txt->elem.spt.info.txtcol;
    el.elem.sgwx.spt.info.filcol = 31;
    el.elem.sgwx.spt.info.lincol = 32;
    
  }
  /* Write the SGWX Polygon Object */
  start = -1;
  el.hdr.recsz = (sizeof (VG_HdrStruct) + sizeof(SGWXType));
  cvg_writefD ( &el, start, el.hdr.recsz, vgFileOut, &loc, &ier);
}

static void genJetObject(VG_DBStruct *el, char *vgFileOut)
{
  int np,i,start,ier,loc,yoffset;
  float lwfactor, angle;
  float lat[256], lon[256];
  /* Fix el jet structure */
  el->elem.jet.line.spl.info.spldir = 0;
  
  start = -1;
  np = el->elem.jet.line.spl.info.numpts;
  lwfactor = (int)(el->elem.jet.line.spl.info.splwid / 14.0);
  
  for (i=0; i < el->elem.jet.nbarb; i++) {
    el->elem.jet.barb[i].spt.info.sztext=0.7;
    el->elem.jet.barb[i].spt.info.itxfn=2;
    el->elem.jet.barb[i].spt.info.lat = el->elem.jet.barb[i].wnd.data.latlon[0];
    el->elem.jet.barb[i].spt.info.lon = el->elem.jet.barb[i].wnd.data.latlon[1];
    angle = el->elem.jet.barb[i].wnd.data.spddir[1] - 360.0;
    el->elem.jet.barb[i].spt.info.rotn = 270.0 - angle;
    if (el->elem.jet.barb[i].spt.info.rotn < 0.0) {
      el->elem.jet.barb[i].spt.info.rotn+=360.0;
    }
    el->elem.jet.barb[i].spt.info.rotn += 1000.0F;
    if (strchr (el->elem.jet.barb[i].spt.text,'\n') == NULL ) {
      yoffset = 2;  /* no deltas in FL text, 1 line */
    } else {
      yoffset = 3;  /* there are 2 lines of FL text */
    } 
    if ( el->elem.jet.barb[i].spt.info.lat >= 0.0) {
      /* We are in the northern hemisphere */      
      el->elem.jet.barb[i].spt.info.offset_y = -yoffset - lwfactor;
    } else {
      /* We are in the Southern Hemisphere, so change the label locations...*/
      el->elem.jet.barb[i].spt.info.offset_y = yoffset + lwfactor;
    }
  }
  cvg_crthdrcolor(el, np, lat, lon,
                  el->hdr.maj_col, el->hdr.min_col, 0, 0, &ier);
  
  cvg_writefD( el, start, el->hdr.recsz, vgFileOut, &loc, &ier);
}
    
static void cvg_crthdrcolor ( VG_DBStruct *el, int np, float *lat, float *lon,
                              int major, int minor, int grouptype, int groupnumber, int *iret )
/************************************************************************
    * cvg_crthdr                                                           *
    *                                                                      *
    * This function builds common header attributes for an VGF element     *
    *                                                                      *
    * cvg_crthdr ( el, np, lat, lon, iret )                                *
    *                                                                      *
    * Input parameters:                                                    *
    *      *el     VG_DBStruct     pointer to an element structure         *
    *       np     int             number of points                        *
    *      *lat    float           latitude to initialize max/min range    *
    *      *lon    float           longitude to initialize max/min range   *
    *                                                                      *
    * Output parameters:                                                   *
    *      *iret   int             Return code                             *
    *                                                                      *
    **                                                                     *
    * Log:                                                                 *
    * J. Wu/GSC    01/01   Created                                         *
 ***********************************************************************/
{
  int         ii;
  /*---------------------------------------------------------------------*/

  *iret = G_NORMAL;

  cvg_initelm( el );

    /*
  *   Get the major/minor color and group type/number.
    */
  el->hdr.maj_col=major;
  el->hdr.min_col=minor;
  el->hdr.grptyp = grouptype;
  el->hdr.grpnum = groupnumber;

    /*
  *  Find the maximum and minimum range.
    */
  el->hdr.range_min_lon = lon[0];
  el->hdr.range_min_lat = lat[0];
  el->hdr.range_max_lon = lon[0];
  el->hdr.range_max_lat = lat[0];

  for ( ii = 0; ii < np; ii++ ) {
    if ( el->hdr.range_min_lon > lon[ii] )
      el->hdr.range_min_lon = lon[ii];
    if ( el->hdr.range_min_lat > lat[ii] )
      el->hdr.range_min_lat = lat[ii];
    if ( el->hdr.range_max_lon < lon[ii] )
      el->hdr.range_max_lon = lon[ii];
    if ( el->hdr.range_max_lat < lat[ii] )
      el->hdr.range_max_lat = lat[ii];
  }
}
