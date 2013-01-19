#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "fromToStateARTCC.h"

int main (int argc, char *argv[])
/******************************************************************************
 fromToStateARTCC

 This program was developed to take a FROM Line with VOR references as argument
 input and identify the states affected.  It was initially developed for
 Non-Convective SIGMET use.  Portions of Code adapted from Convective SIGMET
 From Line Generation Logic in GEMPAK nmap module: nmap_pgsigw.c, and modified
 for non-convective sigmets.

 Usage:
 fromToStateARTCC -from "<fromline>"  where the Fromline is enclosed in quotes.

 Example:
 fromToStateARTCC -from "FROM ONL TO 60WSW MKC TO 10NNW ICT TO 30WNW LBF TO ONL"

 Lists the 2-letter IDs of the states affected:  KS NE
 Lists the 3-letter ARTCC regions affected on a separate line: ZMP ZKC ZDV

 If Cstl Waters are affected only,  the states followed by CSTL WTRS is listed:
   Example:  TX CSTL WTRS

 If the states and cstl wtrs are affected:
   Example: TX LA AND CSTL WTRS

 Contents:
 main
 
 loadVorTable
 splitpoints
 trim_spaces
 reorderpoints
 xlateVorToLatLon
 getindex
 computenewlatlon
 obtainStates
 obtainARTCCs
 
**
 Log:
 L. Hinson/AWC          5/09         Created
                        1/10         Updated for GEMPAK Release
                        2/11         Fixed to add Great Lakes
*******************************************************************************/
{
  static char usageString[] = "Usage:fromToStateARTCC -from <fromline>\n";
  float lats[100];
  float lons[100];
  float rlats[100];
  float rlons[100];
  vor_record vortable[1000];
  char vorpointrefs[100][15];
  char states[512];
  char artccs[512];
  char fromline[512];
  int i;

  int vor_count = 0;
  int vorpoint_count;
  int fromlineset = 0;
  fromline[0]='\0';
  for (i = 0; i < argc; i++) {
    if (strcmp(argv[i],"-h")==0) {
      printf(usageString);
      exit(1);
    }
    if (strcmp(argv[i],"-from")==0) {
      strcpy(fromline,argv[i+1]);
      fromlineset = -1;
    }
  }
  if (!fromlineset) {
    printf(usageString);
    exit(1);
  }
  if (strlen(fromline)<9) {
    printf("Fromline is too short.  Example AAA TO 20WSW BBB TO 50E CCC\n");
    exit(1);
  }

  /*
   0. Load the Vor Table.
   1. Convert this from line to latlon points... in arrays lats and lons
   2. From the Lat/Lon Arrays identify the states affected...
  */
  vor_count = loadVorTable(vortable);
  vorpoint_count = splitpoints(fromline+5,"TO",vorpointrefs);
  xlateVorToLatLon(vorpointrefs,vorpoint_count,vortable,vor_count,lats,lons);
  reorderpoints(vorpoint_count, lats, lons, rlats, rlons);
  obtainStates(rlats, rlons, &vorpoint_count, states);
  printf("%s",states);
  if(strlen(states)==0) {
    printf("No states found.\n");
    exit(1);
  }
  obtainARTCCs(rlats, rlons, &vorpoint_count, artccs);
  printf("%s",artccs);
  return(0);
}

static int loadVorTable(vor_record vortable[]) {
  FILE *fil1;
  char rec[80];
  char id[5],name[25],st[3],co[3];
  int lat,lon,stnm;
  int vor_count = 0;
  int ier = 0;
  fil1 = (FILE *) cfl_tbop( "vors.tbl", "stns", &ier );
  if (fil1 !=NULL && ier == 0) {
    while (! feof(fil1)) {
      fgets(rec,80,fil1);
      if (strstr(rec,"!")==NULL) {
        sscanf(rec,"%3s %d %s %s %s %d %d",id,&stnm,name,st,co,&lat,&lon);
        strcpy(vortable[vor_count].vorid,id);
        vortable[vor_count].lat = lat/100.0;
        vortable[vor_count].lon = lon/100.0;
        vor_count++;
      }
    }
    fclose(fil1);
  }
  return vor_count;
}

static int splitpoints(char *str, char *delimiter, char vorpoints[][15]) {
  char *ptr;
  int vorrefcount = 0;
  unsigned int i;
  /* Replace 'TO' with '- ' */
  for (i=0;i <= strlen(str); i++) {
    if (strncmp(str+i," TO ",4)==0) {
      str[i+1] = '-';
      str[i+2] = ' ';
      i++;
    }
  }
  ptr = strtok(str, "-");
  while (ptr != NULL) {
    strcpy(vorpoints[vorrefcount],trim_spaces(ptr));
    vorrefcount++;
    ptr = strtok(NULL, "-");
  }
  return vorrefcount;
}

static char *trim_spaces(char *str) {
  int len, i;
  char *out = str;
  len = strlen(str);
  for (i=0; i < len && isspace(str[i]); i++) out++;
  for (i=len-1; i>=0 && isspace(str[i]); i--) str[i]=0;
  return out;
}

static void reorderpoints(int npoints, float lat[], float lon[], float rlat[], float rlon[]) {
  int i;

  int count = 0;
  for (i=npoints-1; i>=0; i--) {
    rlat[count] = lat[i];
    rlon[count] = lon[i];
    count++;
  }
}

static void xlateVorToLatLon(char vorpointrefs[][15], int count,
     vor_record vortable[], int vor_count, float lats[], float lons[]) {
  int i;
  int distance;
  char direction[4];
  char vorid[4];
  int vorindex=0;
  float newlat, newlon;
  for (i=0; i< count; i++) {
    if (strstr(vorpointrefs[i]," ") != NULL) {
      if ((sscanf(vorpointrefs[i],"%d%s %s",&distance,direction,vorid)) != 3) {
        printf("Problem with vor point %s",vorpointrefs[i]);
      }
    } else {
      distance = 0;
      strcpy(direction,"N");
      strcpy(vorid,vorpointrefs[i]);
    }
    /* Get the VOR ID Index from vortable... */
    vorindex = getindex(vorid,vortable,vor_count);
    if (vorindex != -1) {
      /* coordpoint = computenewlatlon(&vortable[vorindex].lat, &vortable[vorindex].lon,
                       &distance, direction); */
      computenewlatlon(&vortable[vorindex].lat, &vortable[vorindex].lon, &distance, direction,
                       &newlat, &newlon);
      lats[i] = newlat;
      lons[i] = newlon;
    } else {
      lats[i] = 99999.0;
      lons[i] = 99999.0;
      printf("Problem in finding VOR Point %s.\n",vorid);
      exit(1);
    }
  }
}

static int getindex(char *vorid, vor_record vortable[], int vor_count) {
  int i, index;
  index = -1;
  for (i=0;i < vor_count; i++) {
    if (strcmp(vorid,vortable[i].vorid)==0) {
       index=i;
       break;
    }
  }
  return index;
}

static void computenewlatlon(float *lat, float *lon, int *radius, char *compassdir, 
                      float *newlat, float *newlon)
{
  /* const float PI = 3.14159; */
  char  *directions[] = {"N",  "NNE", "NE", "ENE", "E",  "ESE", "SE",  "SSE", "S",   "SSW", "SW",  "WSW", "W",   "WNW", "NW",  "NNW"};
  float dirdegrees[] = {360.0, 22.5, 45.0, 67.5,  90.0, 112.5, 135.0, 157.5, 180.0, 202.5, 225.0, 247.5, 270.0, 292.5, 315.0, 337.5};
  float dirdeg;
  float radc,rlat,rdis,rdir,qlat,tlat,temp,qlng,tlng;
  int i;
  radc = PI/180.0;
  for (i=0; i < 16; i++) {
    if (strcmp(directions[i], compassdir) == 0) {
      dirdeg = dirdegrees[i];
      break;
    }
  }
  rlat = (*lat) * radc;
  rdis = (*radius) / 69.0 * radc;
  rdir = dirdeg*radc;
  qlat = rlat+rdis*cos(rdir);
  tlat = qlat/radc;
  temp = rlat+qlat;
  qlng = -(*lon) * radc;
  tlng = (-(qlng-((rdis*sin(rdir))/cos((temp)/2)))/radc);
  *newlat = tlat;
  *newlon = tlng;
}

static void obtainStates(float lat[], float lon[], int *npoints, char *state_list) {
/**
  This function computes the state list.  It is modelled after nmap_pgsigw.c::pgsigw_getState.
  
  obtainStates (lat, lon, npoints, state_list)
  
  Input parameters:
    lat[]       float
    lon[]       float
    *npoints    int
  Output parameters:
    *state_list char
    
**
  Log:
  L. Hinson/AWC    1/2010   GEMPAK Release
                   2/2011   Fixed to add Great Lakes    
**/
  
  int ier, all;
  int iret=0;
  int npts = *npoints;
  char lakes_list[128], cstl_list[128];
  int ii;
  char info[128],stpo[4],qst[4];
  char *cptr;
  int mode, istat;
  char device[13],dfilnam[73];
  char proj[80];
  int iunit, itype;
  float xsize,ysize;
  float lllat, lllon, urlat, urlon;
  char  pro[80], *ptr;
  float prjang1, prjang2, prjang3;
  in_bdta(&ier);
  mode = 1;
  ginitp(&mode,&istat,&ier);
  strcpy(device,"GN");
  strcpy(dfilnam,"TESTSTATE");
  iunit = 1;
  itype = 1;
  xsize = 500.0;
  ysize = 500.0;
  gsdeva(device,&iunit,dfilnam,&itype,&xsize,&ysize,&iret,12,72);
  lllat = 10.0F;
  lllon = -120.0F;
  urlat = 50.0F;
  urlon = -50.0F;
  strcpy ( proj, "str/90;-105;0" );
  strcpy ( pro, strtok ( proj, "/" ) );
  prjang1 = 0.0F;  prjang2 = 0.0F;  prjang3 = 0.0F;
  ptr = strtok(NULL,";");
  if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
  ptr = strtok(NULL,";");
  if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
  ptr = strtok(NULL,";");
  if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );
  gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                         &lllat, &lllon, &urlat, &urlon, &iret, strlen(pro));

  clo_init( &iret);
  clo_binpoly( "GREAT_LAKES", npts, lat, lon, &iret);

  lakes_list[0] = '\0';
  for ( ii = 0; ii < clo_qnhot(); ii++ )  {

      clo_bginfo( "GREAT_LAKES", ii, info, &ier );
      cst_gtag( "ID", info, "?", stpo, &ier );
      strcat(lakes_list, stpo);
      strcat(lakes_list, " ");

  }

  clo_binpoly( "ADJ_CSTL", npts, lat, lon, &iret);

  cstl_list[0] = '\0';
  for ( ii = 0; ii < clo_qnhot(); ii++ )  {

      clo_bginfo( "ADJ_CSTL", ii, info, &ier );
      cst_gtag( "ID", info, "?", stpo, &ier );
      strcat(cstl_list, stpo);
      strcat(cstl_list, " ");

  }

  clo_binpoly( "STATE_BNDS", npts, lat, lon, &iret);

  state_list[0] = '\0';
  for ( ii = 0; ii < clo_qnhot(); ii++ )  {

      clo_bginfo( "STATE_BNDS", ii, info, &ier );
      cst_gtag( "STATE", info, "?", stpo, &ier );
      strcat(state_list, stpo);
      strcat(state_list, " ");

  }

  /*
   *  Check each state to see if it's coastal waters are included.
   *  If all states' coastal waters are included, then simply add
   *  "AND CSTL WTRS" to the states/lakes string.
   *  If any state does not include it's coastal waters, then
   *  simply add "AND" plus the coastal string plus "CSTL WTRS".
   */
  all = G_TRUE;
  cptr = cst_split ( state_list, ' ', sizeof(qst)/sizeof(qst[0]),
                     qst, &ier );
  while ( cptr != (char *)NULL )  {
      if ( strstr ( cstl_list, qst ) == (char *)NULL )  {
          all = G_FALSE;
          break;
      }
      else  {
          cptr = cst_split ( cptr, ' ',
                             sizeof(qst)/sizeof(qst[0]), qst, &ier );
      }
  }

  strcat ( state_list, lakes_list );
  if ( all && strlen(state_list) > (size_t)0  &&
          strlen(state_list) == strlen(cstl_list) )  {
      strcat ( state_list, "AND CSTL WTRS" );
  }
  else  {
      if ( strlen ( cstl_list ) != (size_t)0 )  {
          if ( strlen(state_list) > (size_t)0 )  strcat(state_list, "AND ");
          if (strlen(state_list) == (size_t)0 )  strcat ( state_list, cstl_list );
          strcat ( state_list, "CSTL WTRS" );
      }
  }
  strcat(state_list, "\n");
}

static void obtainARTCCs(float lat[], float lon[], int *npoints, char *artcc_list) {
  int ier;
  int iret;
  int npts = *npoints;
  int ii;
  char info[128],stpo[4];
  int mode, istat;
  char device[13],dfilnam[73];
  char proj[80];
  int iunit, itype;
  float xsize,ysize;
  float lllat, lllon, urlat, urlon;
  char  pro[80], *ptr;
  float prjang1, prjang2, prjang3;
  in_bdta(&ier);
  mode = 1;
  ginitp(&mode,&istat,&ier);
  strcpy(device,"GN");
  strcpy(dfilnam,"TESTARTCC");
  iunit = 1;
  itype = 1;
  xsize = 500.0;
  ysize = 500.0;
  gsdeva(device,&iunit,dfilnam,&itype,&xsize,&ysize,&iret,12,72);
  lllat = 10.0F;
  lllon = -120.0F;
  urlat = 50.0F;
  urlon = -50.0F;
  strcpy ( proj, "str/90;-105;0" );
  strcpy ( pro, strtok ( proj, "/" ) );
  prjang1 = 0.0F;  prjang2 = 0.0F;  prjang3 = 0.0F;
  ptr = strtok(NULL,";");
  if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
  ptr = strtok(NULL,";");
  if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
  ptr = strtok(NULL,";");
  if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );
  gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                         &lllat, &lllon, &urlat, &urlon, &iret, strlen(pro));
  clo_init( &iret);
  clo_binpoly( "ARTCC_BOUNDS", npts, lat, lon, &iret);
  artcc_list[0] = '\0';
  for ( ii=0; ii < clo_qnhot(); ii++ ) {
    clo_bginfo( "ARTCC_BOUNDS", ii, info, &ier );
    cst_gtag ( "REGION", info, "?", stpo, &ier);
    strcat(artcc_list, stpo);
    strcat(artcc_list, " ");
  }
  strcat(artcc_list, "\n");
}


