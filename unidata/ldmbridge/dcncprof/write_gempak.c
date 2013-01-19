#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

#include "ulog.h"
#include "profiler.h"
#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"

#ifdef UNDERSCORE
#define	sn_atim	sn_atim_
#define sn_astn	sn_astn_
#define	sn_rdat	sn_rdat_
#define	sn_wdat	sn_wdat_
#define sn_gtim	sn_gtim_
#define sn_dtim	sn_dtim_
#define dc_fcyl	dc_fcyl_
#define dc_fint	dc_fint_
#endif

#define NUMVARS	11
#define HGHT	0
#define	SPED	1
#define	DRCT	2
#define	SIGS	3
#define	SIGW	4
#define	WWND	5
#define LMOD    6
#define CNUM    7
#define PPOW    8
#define TVRK    9
#define SPCW    10

char dcdlog[DCMXLN];
char cprgnm[DCMXLN];
int ipid;
int ivrblv = 2;
int itmout = 600;
int irltim = G_TRUE;
int logflg = G_FALSE;

int
rasscmp (rass_data ** p1, rass_data ** p2, rass_data ** head,
	 rass_data * last)
{
  if ((*p1)->level > (*p2)->level)
    {
      if (*p1 == *head)
	{
	  *head = *p2;
	  (*p1)->nextlev = (*p2)->nextlev;
	  (*p2)->nextlev = *p1;
	}
      else
	{
	  (*p1)->nextlev = (*p2)->nextlev;
	  (*p2)->nextlev = *p1;
	  last->nextlev = *p1;
	}
      return (1);
    }
  else
    return (0);
}

int
profcmp (prof_data ** p1, prof_data ** p2, prof_data ** head,
	 prof_data * last)
{
  if ((*p1)->level > (*p2)->level)
    {
      if (*p1 == *head)
	{
	  *head = *p2;
	  (*p1)->nextlev = (*p2)->nextlev;
	  (*p2)->nextlev = *p1;
	}
      else
	{
	  (*p1)->nextlev = (*p2)->nextlev;
	  (*p2)->nextlev = *p1;
	  last->nextlev = *p1;
	}
#ifdef DEBUG
      {
	prof_data *ptest = *head;
	while (ptest != NULL)
	  {
	    printf ("check lev %f\n", ptest->level);
	    ptest = ptest->nextlev;
	  }
      }
#endif
      return (1);
    }
  else
    return (0);
}

void *
sortlev (void *head, int dtype)
{
  void *newhead, *old, *cur, *next;
  int nochange, ival;

  if ((dtype < 0) || (dtype > 1))
    {
      uerror ("Unknown sort type");
      return (head);
    }

  newhead = head;

  nochange = 1;
  while (nochange == 1)
    {
      old = NULL;
      cur = newhead;
      nochange = 0;
      while (cur != NULL)
	{
	  if (dtype == 0)
	    next = (void *) (((prof_data *) cur)->nextlev);
	  else
	    next = (void *) (((rass_data *) cur)->nextlev);

	  if (next != NULL)
	    {
	      if (dtype == 0)
		ival =
		  profcmp ((prof_data **) (&cur), (prof_data **) (&next),
			   (prof_data **) (&newhead), old);
	      else
		ival =
		  rasscmp ((rass_data **) (&cur), (rass_data **) (&next),
			   (rass_data **) (&newhead), old);
	      if (ival != 0)
		nochange = 1;
	    }
	  if (old == NULL)
	    old = newhead;
	  else
	    {
	      if (dtype == 0)
		old = (void *) (((prof_data *) old)->nextlev);
	      else
		old = (void *) (((rass_data *) old)->nextlev);
	    }

	  if (dtype == 0)
	    cur = (void *) (((prof_data *) old)->nextlev);
	  else
	    cur = (void *) (((rass_data *) old)->nextlev);
	}
    }

  return (newhead);
}



void
write_gempak (char *ofil, sta_struct * head, char *logfname, int *iret)
{
  sta_struct *p;
  prof_data *plev;
  rass_data *rlev;
  int i, j, ier;
  char dattim[12], oldtim[12], gemfil[256];

  int maxstat = 200, maxtim = LLMXTM, iflno;
  int maxfiles = 2, iflsrc = MFRAOB;
  int nparms, ntime;
  char packfl[] = "profiler_fsl.pack";
  char statfl[] = "profiler_fsl.stn";
  char prmlist[MMPARM][4];
  char timlst[LLMXTM * 11 + 1];

  char stn[9], stid[9], state[3], coun[3];
  float slat[1], slon[1], selv[1];
  int stnm[1], numlevs, itim, nadd;
  float dataray[LLMXLV * MMPARM], olddata[LLMXLV * MMPARM];
  int pos[NUMVARS];
  float drct, zlev, z1, z2, z3;

  dcdlog[0] = '\0';
  if (logfname != NULL)
    strcpy (dcdlog, logfname);
  else
    strncat (dcdlog, "-", 1);
  cprgnm[0] = '\0';
  sprintf (cprgnm, "dcncprof\0");
  ipid = (int) getpid ();

  udebug ("write_gempak %s logs %s\0", ofil, logfname);
/* make sure we can get to directory for output */
  if (diraccess (ofil, (R_OK | W_OK), !0) == -1)
    {
      serror ("Couldn't access directories leading to %s", ofil);
      *iret = -1;
      return;
    }

  memset (prmlist, '\0', 4 * MMPARM);

  in_bdta (&ier);

  dc_fint (&maxfiles, &iflsrc, packfl, &ier, strlen (packfl));

  p = head;
  while (p != NULL)
    {
      dattim[0] = '\0';
      sprintf (dattim, "%02d%02d%02d/%02d%02d\0",
	       p->year % 100, p->month, p->day, p->hour, p->minute);
      memset (gemfil, '\0', 256);
      cfl_mnam (dattim, ofil, gemfil, &ier);
      i = 0;
      dc_fcyl (gemfil, &iflsrc, statfl, &maxstat, &maxtim, &iflno, &i,
	       prmlist, &ier, strlen (gemfil), strlen (statfl),
	       sizeof (prmlist[0]));
      if (ier != 0)
	{
	  uerror ("Could not open gempak file %s\0", gemfil);
	  *iret = -1;
	  return;
	}
      if (i > MMPARM)		/* should probably exit here */
	uerror ("Returned more parameters than allowed: %d\0", MMPARM);
      if (i > 0)
	nparms = i;
      for (i = 0; i < NUMVARS; i++)
	pos[i] = -1;
      for (i = 0; i < nparms; i++)
	{
	  if (strncmp (prmlist[i], "HGHT", 4) == 0)
	    pos[HGHT] = i;
	  if (strncmp (prmlist[i], "SPED", 4) == 0)
	    pos[SPED] = i;
	  if (strncmp (prmlist[i], "DRCT", 4) == 0)
	    pos[DRCT] = i;
	  if (strncmp (prmlist[i], "SIGS", 4) == 0)
	    pos[SIGS] = i;
	  if (strncmp (prmlist[i], "SIGW", 4) == 0)
	    pos[SIGW] = i;
	  if (strncmp (prmlist[i], "WWND", 4) == 0)
	    pos[WWND] = i;
	  if (strncmp (prmlist[i], "LMOD", 4) == 0)
	    pos[LMOD] = i;
	  if (strncmp (prmlist[i], "CNUM", 4) == 0)
	    pos[CNUM] = i;
	  if (strncmp (prmlist[i], "PPOW", 4) == 0)
	    pos[PPOW] = i;
	  if (strncmp (prmlist[i], "TVRK", 4) == 0)
	    pos[TVRK] = i;
	  if (strncmp (prmlist[i], "SPCW", 4) == 0)
	    pos[SPCW] = i;
	}
      uinfo ("Station data for %d %s: %s %s\0", p->wmoStaNum, p->staName,
	     dattim, gemfil);
      uinfo ("   Lat: %7.2f Lon: %7.2f Elev: %7.2f\0", p->staLat, p->staLon,
	     p->staElev);
      uinfo ("   number of levels: %d\0", p->numlevs);
      uinfo ("   time interval %d\0", p->time_interval);
      uinfo ("   temp %6.2f dwpc %6.2f relh %6.2f\0",
	     p->sfc_temp, p->sfc_dwpc, p->sfc_relh);
      plev = (void *) sortlev (p->pdata, 0);
      rlev = (void *) sortlev (p->rdata, 1);

      for (i = 0; i < (MMPARM * LLMXLV); i++)
	dataray[i] = RMISSD;


      /* set time in output file, add if necessary */
      sn_stim (&iflno, dattim, &ier, strlen (dattim));
      if (ier == -12)
	{
	  sn_atim (&iflno, dattim, &ier, strlen (dattim));
	  if (ier == -5)
	    {
	      i = maxtim;
	      ntime = 0;
	      memset (timlst, '\0', LLMXTM * 11 + 1);
	      sn_gtim (&iflno, &i, &ntime, timlst, &ier, 11);
	      oldtim[0] = '\0';
	      strncat (oldtim, timlst, 11);
	      uerror ("Maximum times exceeded -- deleting %s to make room\0",
		      oldtim);
	      sn_dtim (&iflno, oldtim, &ier, strlen (oldtim));
	      sn_atim (&iflno, dattim, &ier, strlen (dattim));
	    }
	  if (ier != 0)
	    uerror ("could not at time %s [SN %d]\0", dattim, ier);
	  else
	    sn_stim (&iflno, dattim, &ier, strlen (dattim));
	}
      if (ier != 0)
	{
	  uerror ("could not complete station %s\0", p->staName);
	  *iret = -1;
	  return;
	}


      /* set station, add if necessary */
      stn[0] = '\0';
      strncpy (stn, p->staName, 8);
      memset (stid, '\0', 9);
      sn_sstn (&iflno, stn, stid, stnm, slat, slon, selv, &ier,
	       strlen (stn), sizeof (stid));
      if (ier == -11)
	{
	  i = 1;
	  stid[0] = '\0';
	  strncpy (stid, p->staName, 8);
	  stnm[0] = p->wmoStaNum;
	  slat[0] = p->staLat;
	  slon[0] = p->staLon;
	  selv[0] = p->staElev;
	  strcpy (state, "--\0");
	  strcpy (coun, "--\0");
	  sn_astn (&iflno, &i, stid, stnm, slat, slon, selv, state, coun,
		   &nadd, &ier, strlen (stid), strlen (state), strlen (coun));
	  memset (stid, '\0', 9);
	  sn_sstn (&iflno, stn, stid, stnm, slat, slon, selv, &ier,
		   strlen (stn), sizeof (stid));
	  uinfo ("Station added (not in table) %s %d at %s %s\0", stn,
		 stnm[0], dattim, stid);
	}
      else if (ier != 0)
	udebug ("ier not nill %d\0", ier);

      /* read in existing data */
      sn_rdat (&iflno, &nadd, olddata, &itim, &ier);
      udebug ("read data %d nlev %d itim %d", ier, nadd, itim);
      if (ier != 0)
	nadd = 0;

      /* copy over the surface data first if available */
      if (nadd > 0)
	memcpy (dataray, olddata, nparms * sizeof (float));

      numlevs = 0;
      /* store new surface, dataray was initialized to RMISSD above */
      if (pos[HGHT] >= 0)
	dataray[numlevs * nparms + pos[HGHT]] = p->staElev;
      if (pos[SPED] >= 0)
	dataray[numlevs * nparms + pos[SPED]] = p->sfc_sped;
      if (pos[DRCT] >= 0)
	dataray[numlevs * nparms + pos[DRCT]] = p->sfc_drct;

      /*if (pos[SIGS] >= 0)
         dataray[numlevs * nparms + pos[SIGS]] = RMISSD;
         if (pos[SIGW] >= 0)
         dataray[numlevs * nparms + pos[SIGW]] = RMISSD;
         if (pos[WWND] >= 0)
         dataray[numlevs * nparms + pos[WWND]] = RMISSD;
         if (pos[LMOD] >= 0)
         dataray[numlevs * nparms + pos[LMOD]] = RMISSD;
         if (pos[CNUM] >= 0)
         dataray[numlevs * nparms + pos[CNUM]] = RMISSD;
         if (pos[PPOW] >= 0)
         dataray[numlevs * nparms + pos[PPOW]] = RMISSD;
         if (pos[TVRK] >= 0)
         dataray[numlevs * nparms + pos[TVRK]] = RMISSD;
         if (pos[SPCW] >= 0)
         dataray[numlevs * nparms + pos[SPCW]] = RMISSD; */

      numlevs++;

      i = 1;			/* start merge at first level of old data */
      while ((rlev != NULL) || (plev != NULL) || (i < nadd))
	{
	  if ((plev != NULL) && (plev->level == RMISSD))
	    {
	      plev = plev->nextlev;
	      continue;
	    }
	  if ((rlev != NULL) && (rlev->level == RMISSD))
	    {
	      rlev = rlev->nextlev;
	      continue;
	    }
	  if ((i < nadd) && (pos[HGHT] >= 0))
	    {
	      z1 = olddata[i * nparms + pos[HGHT]];
	      if (z1 == RMISSD)
		{
		  i++;
		  continue;
		}
	    }
	  else
	    z1 = RMISSD;

	  if (rlev != NULL)
	    z2 = rlev->level + p->staElev;
	  else
	    z2 = RMISSD;

	  if (plev != NULL)
	    z3 = plev->level + p->staElev;
	  else
	    z3 = RMISSD;

	  zlev = z1;
	  if ((z2 != RMISSD) && ((z2 < zlev) || (zlev == RMISSD)))
	    zlev = z2;
	  if ((z3 != RMISSD) && ((z3 < zlev) || (zlev == RMISSD)))
	    zlev = z3;

	  if (zlev != RMISSD)
	    {
	      if (zlev == z1)
		{
		  udebug ("memcpy z1 %f", z1);
		  memcpy (dataray + (size_t) (numlevs * nparms),
			  olddata + (size_t) (i * nparms),
			  nparms * sizeof (float));
		  i++;
		}
	      if (zlev == z2)
		{
		  udebug
		    ("      Z %5.0f TVRK = %7.4f PPOW = %7.4f CNUM = %7.4f SPCW %7.4f\0",
		     rlev->level, rlev->tmpv, rlev->power, rlev->consensus,
		     rlev->specw);
		  if (pos[HGHT] >= 0)
		    dataray[numlevs * nparms + pos[HGHT]] =
		      rlev->level + p->staElev;
		  if (pos[CNUM] >= 0)
		    dataray[numlevs * nparms + pos[CNUM]] = rlev->consensus;
		  if (pos[PPOW] >= 0)
		    dataray[numlevs * nparms + pos[PPOW]] = rlev->power;
		  if (pos[TVRK] >= 0)
		    dataray[numlevs * nparms + pos[TVRK]] = rlev->tmpv;
		  if (pos[SPCW] >= 0)
		    dataray[numlevs * nparms + pos[SPCW]] = rlev->specw;
		  rlev = rlev->nextlev;
		}
	      if (zlev == z3)
		{
		  udebug
		    ("      Z %5.0f u = %7.4f v = %7.4f w = %7.4f mode %d\0",
		     plev->level, plev->u, plev->v, plev->w, plev->levmode);
		  if ((plev->u != RMISSD) && (plev->v != RMISSD)
		      && (plev->w != RMISSD))
		    {
		      if (pos[HGHT] >= 0)
			dataray[numlevs * nparms + pos[HGHT]] =
			  plev->level + p->staElev;
		      if (pos[SPED] >= 0)
			{
			  if ((plev->u != RMISSD) && (plev->v != RMISSD))
			    dataray[numlevs * nparms + pos[SPED]] =
			      sqrt (plev->u * plev->u + plev->v * plev->v);
			  else
			    dataray[numlevs * nparms + pos[SPED]] = RMISSD;
			}
		      if (pos[DRCT] >= 0)
			{
			  if ((plev->u != RMISSD) && (plev->v != RMISSD))
			    {
			      drct = atan2 (plev->v, -plev->u) * 180. / M_PI;
			      if (drct < 0)
				drct = drct + 360;
			      drct = drct - 270;
			      if (drct < 0)
				drct = drct + 360;
			      dataray[numlevs * nparms + pos[DRCT]] = drct;
			    }
			  else
			    dataray[numlevs * nparms + pos[DRCT]] = RMISSD;
			}
		      if (pos[SIGS] >= 0)
			dataray[numlevs * nparms + pos[SIGS]] =
			  plev->sigma_uv;
		      if (pos[SIGW] >= 0)
			dataray[numlevs * nparms + pos[SIGW]] = plev->sigma_w;
		      if (pos[WWND] >= 0)
			dataray[numlevs * nparms + pos[WWND]] = plev->w;
		      if (pos[LMOD] >= 0)
			dataray[numlevs * nparms + pos[LMOD]] = plev->levmode;
		    }
		  plev = plev->nextlev;
		}

	      numlevs++;
	    }
	  else
	    {
	      uerror ("zlev == RMISSD, shouldn't get here");
	      i = nadd;
	      rlev = NULL;
	      plev = NULL;
	    }
	}


      itim = p->hour * 100 + p->minute;
      sn_wdat (&iflno, &itim, &numlevs, dataray, &ier);
      if (ier != 0)
	uerror ("ier time set / stn %d", ier);
      p = p->next;
    }

  if (iflno != -1)
    sn_clos (&iflno, &ier);
  *iret = 0;
}
