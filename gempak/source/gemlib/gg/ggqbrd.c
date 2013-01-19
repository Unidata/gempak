#include "mel_bufr.h"

#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define gg_qbrd	gg_qbrd_
#endif

void QSWind_Plot (char *stime, char *etime, char *dtime,
		  float fwninc[], int icolrs[], int icolrs2[],
		  int *numclr, int iflags[], int *iwtyp, int *ierr);


/* iflags */
#define HIGH    0
#define LOW     1
#define RAIN    2
#define AVAIL   3
#define RAINCLR 4


static Boolean
_QCcheck (unsigned int iqual, int bnum)
{
  int i;
  static int ibits[17];
  static unsigned int isav = 0;
  int mask = 1;

  if (iqual == 0)
    return (FALSE);

  if (iqual != isav)
    {
      isav = iqual;

      /*
       * Get the bit values for the data quality flag.
       */
      for (i = 0; i < 17; i++)
	{
	  ibits[17 - i - 1] = (((iqual & mask) == 0) ? 0 : 1);
	  iqual >>= 1;
	}

    }

/*if ( ibits[9] == 1 )
   printf("Ice on %d\n",isav);
if ( ibits[8] == 1 )
   printf("Land on %d\n",isav);*/

  if (ibits[bnum] == 1)
    return (TRUE);
  else
    return (FALSE);

}

typedef struct qsct_wind
{
  int idtarr[5];
  float latd, lond, sped, sknt, drct, popr, qval;
} QSWind_t;

QSWind_t qswind;

void
QSWind_init ()
{
  qswind.latd = RMISSD;
  qswind.lond = RMISSD;
  qswind.sped = RMISSD;
  qswind.sknt = RMISSD;
  qswind.drct = RMISSD;
  qswind.popr = RMISSD;
  qswind.qval = RMISSD;
  qswind.idtarr[0] = IMISSD;
  qswind.idtarr[1] = IMISSD;
  qswind.idtarr[2] = IMISSD;
  qswind.idtarr[3] = IMISSD;
  qswind.idtarr[4] = IMISSD;
}

void
QSWind_val (BUFR_Val_t bv)
{
  unsigned int bv_FXY, k;

  bv_FXY = (unsigned int) Get_Val_FXY (bv);

  if ((bv_FXY >= 1025) && (bv_FXY <= 1029))
    {
      k = bv_FXY - 1025;
      qswind.idtarr[k] = Get_Val_Int (bv);
    }
  else
    {
      switch (bv.FXY_Val)
	{
	case 1282:
	  qswind.latd = (float) Get_Val_Double (bv);
	  break;
	case 1538:
	  qswind.lond = (float) Get_Val_Double (bv);
	  break;
	case 2828:
	  qswind.sped = (float) Get_Val_Double (bv);
	  qswind.sknt = pr_mskn (&qswind.sped);
	  break;
	case 2827:
	  qswind.drct = (float) Get_Val_Int (bv);
	  break;
	case 5498:
	  qswind.popr = (float) Get_Val_Double (bv);
	  break;
	case 5485:
	  qswind.qval = (float) Get_Val_Int (bv);
	  break;
	}
    }
}

int bufr_get_nextds ( BUFR_DataSet_t* DataSet, int IgnoreAFs, int *status )
{
BUFR_Val_t  bv;
int   vals_assigned = 0;

      while ( 1 ) {
	 if( ( *status = (int)BUFR_Get_Value( &bv, IgnoreAFs )) != BUFR_OK && *status != BUFR_EOD )
        {
            if(  *status == BUFR_ERROR )
            {
                BUFR_Err_Log( "BUFR_Get_Dataset Flag:  BUFR_ERROR" );
		return vals_assigned;
            }
            else    /* Must be EOM */
            {
		return vals_assigned;
            }
        }
        BUFR_DataSetEntryPut(DataSet, bv, IgnoreAFs);
        vals_assigned++;
        if ( *status == BUFR_EOD ) {
	    return vals_assigned;
	}
      }
}

void
gg_qbrd (char *tfile, char *stime, char *etime, char *dtime,
	 float fwninc[], int icolrs[], int icolrs2[],
	 int *numclr, int iflags[], int *iwtyp, int *ierr)
{
  int ier;
  int n, j, k, ndsets;

  static BUFR_Info_t BInfo;
  extern BUFR_Cntl_t BUFR_Cntl;
  extern BUFR_Msg_t BUFR_Msg;
  static BUFR_DataSet_t data_set;
  static BUFR_Val_t *ds_pr;
  int DONE = 0, IgnoreAFs, status=0;
  char newfil[4096];
  long flen;
  FILE *fp;

  static int icls_stderr=0;

  cfl_inqr (tfile, NULL, &flen, newfil, &ier);

  if (BUFR_Info_Init (&BInfo))
    {
      printf (" >>>> Could not initilize BUFR info structure >>>>>\n");
      BUFR_perror ("gg_qbrd");
      return;
    }

  /* Initialize Data Set structure */
  BUFR_DataSet_Init (&data_set);

  /* Read the bufr tables on the first pass only */
  BUFR_Msg.LocalTable_Flag = 0;

#ifndef DEBUG
  /* close standard error and redirect output to /dev/null */
  if ( ! icls_stderr ) {
    close(STDERR_FILENO);
    fcntl(open("/dev/null", O_WRONLY), F_DUPFD, stderr);
    icls_stderr=!0;
  }
#endif
fp = stderr;

  while (!DONE)
    {

      Set_Flag (BUFR_LOG_TO_stdout); 
      if (BUFR_Init (&BInfo, newfil, DECODING))
        {
	BUFR_perror ("gg_qbrd");
	DONE = 1;
	break;
	}        

      /* redirect Destory messages to stderr */
      BUFR_Cntl.bufr_log = fp;
      fprintf(BUFR_Cntl.bufr_log, "call BUFR_Init newfil %s\n",newfil);

      /* Keep the local copy of tables, including table B additions.
         This is done to prevent the rereading of all tables every time.
       */
      BUFR_Msg.LocalTable_Flag = 1;

      ndsets = BUFR_NumDatasets ();

      QSWind_init ();

      IgnoreAFs = 1;
      k = 0;
      while ( 1 ) {
          n = bufr_get_nextds ( &data_set, IgnoreAFs, &status );
	  k++;
	 
	  if ( n > 0 )
	    {
	      fprintf (BUFR_Cntl.bufr_log, " >>> DATA SET #%d  of %d<<<\n", k, ndsets);
	      ds_pr = data_set.head->next;
	      for (j = 0; j < n; j++)
		{
		  BUFR_Val_Print (*ds_pr, 1, 0, BUFR_Cntl.bufr_log);
		  QSWind_val (*ds_pr);
		  ds_pr = ds_pr->next;
		}
	      /*fprintf (BUFR_Cntl.bufr_log, "**** END OF DATASET ****\n\n");*/
	      QSWind_Plot (stime, etime, dtime,
			   fwninc, icolrs, icolrs2, numclr, iflags, iwtyp,
			   &ier);
	      QSWind_init ();
	      BUFR_DataSet_Empty (&data_set);
	      /*printf("look subset_index %d\n",BUFR_Msg.subset_index);*/
	    }
	 else 
	    {
	      break;
	    }
      }


      DONE = 1;
      if (status == BUFR_EOM)
	{
	  /* There's another message to read. */

	  status = BUFR_Find_End ();
	  if ( status != 0 )
             fprintf (BUFR_Cntl.bufr_log, "*** END OF FILE *** %s\n",newfil);
          else
	     {
	     DONE = 0;
	     /*printf ("*** END OF MESSAGE *** Find end\n");*/
             }


	}
      else if (status == BUFR_EOF)
	{
	  fprintf (BUFR_Cntl.bufr_log, "*** END OF FILE ***\n");
	}
      else
	{
	n = BUFR_Find_End ();
	printf ("status == %d [n = %d]\n", status,n);
        if ( n != 1 ) DONE = 0;
	}

     /*BUFR_Destroy (0);*/

    }

  BUFR_Destroy (1); 
  BUFR_Close ();
}

void
QSWind_Plot (char *stime, char *etime, char *dtime,
	     float fwninc[], int icolrs[], int icolrs2[],
	     int *numclr, int iflags[], int *iwtyp, int *ierr)
{
  int ier, ilen;
  int nmin1, nmin2, icolr;
  int kk, ival;
  dattm_t ctime;
  static char Sys_M[] = "M";

      /** time check **/
  ti_itoc (qswind.idtarr, ctime, &ier, sizeof (ctime));
  st_null (ctime, ctime, &ilen, &ier, sizeof (ctime), sizeof (ctime));

  /*
   * Compute the differences between the times to determine
   * if the data falls in the time range.
   */
  ti_diff (ctime, stime, &nmin1, &ier, strlen (ctime), strlen (stime));
  ti_diff (etime, ctime, &nmin2, &ier, strlen (etime), strlen (ctime));

  if (nmin1 * nmin2 < 0)
    return;

  kk = *numclr - 1;
  while ((kk > 0) && (qswind.sknt < fwninc[kk]))
    kk--;

  icolr = icolrs[kk];

      /** This is the 17 bit flag check table,
 021109 SEAWINDS WIND VECTOR CELL QUALITY (17-BIT FLAG TABLE)
1  NOT ENOUGH GOOD SIGMA-0 AVAILABLE FOR WIND RETRIEVAL
2  POOR AZIMUTH DIVERSITY AMONG SIGMA-0 FOR WIND RETRIEVAL
8  SOME PORTION OF WIND VECTOR CELL OVER LAND
9  SOME PORTION OF WIND VECTOR CELL OVER ICE
10 WIND RETRIEVAL NOT PERFORMED FOR WIND VECTOR CELL
11 REPORTED WIND SPEED IS GREATER THAN 30M/S
12 REPORTED WIND SPEED IS LESS THAN OR EQUAL TO 3M/S
13 MULTI-PARAMETER RAIN PROBABILITY RAIN FLAG IS NOT USEABLE
14 MULTI-PARAMETER RAIN PROBABILITY DETECTS RAIN
15 NORMALIZED OBJECTIVE FUNCTION RAIN FLAG IS NOT USEABLE
16 NORMALIZED OBJECTIVE FUNCTION DETECTS RAIN **

      ** However, We find the following in data from NOAAPORT
      0-7 not encountered
      8  SOME PORTION OF WIND VECTOR CELL OVER LAND
      9  SOME PORTION OF WIND VECTOR CELL OVER ICE
      10 not encouuntered
      11 REPORTED WIND SPEED IS GREATER THAN 30M/S
      12 REPORTED WIND SPEED IS LESS THAN OR EQUAL TO 3M/S
      13 MULTI-PARAMETER RAIN PROBABILITY RAIN FLAG IS NOT USEABLE
      14 MULTI-PARAMETER RAIN PROBABILITY DETECTS RAIN
      15 Some data unavailable
      16 Always set! **/


  /* first check to see if land or ice contaminated, then check user choices** */
  if ((_QCcheck ((unsigned int) qswind.qval, 8)) ||
      (_QCcheck ((unsigned int) qswind.qval, 9)))
    return;

  if (iflags[AVAIL] == 0)
    if (_QCcheck ((unsigned int) qswind.qval, 15))	/* Some data not available */
      return;

  if (iflags[HIGH] == 0)
    if (_QCcheck ((unsigned int) qswind.qval, 11))	/* High speed */
      return;

  if (iflags[LOW] == 0)
    if (_QCcheck ((unsigned int) qswind.qval, 12))	/* Low speed */
      return;

  if (iflags[RAIN] == 0)
    {
      if ((!_QCcheck ((unsigned int) qswind.qval, 13))	/* rain flag not unusable */
	  && (_QCcheck ((unsigned int) qswind.qval, 14)))
	return;
    }
  else if (iflags[RAINCLR] == 1)
    {
      if ((!_QCcheck ((unsigned int) qswind.qval, 13))	/* rain flag not unusable * */
	  && (_QCcheck ((unsigned int) qswind.qval, 14)))
	icolr = icolrs2[kk];
    }

  if (_QCcheck ((unsigned int) qswind.qval, 10))
    printf ("look bit 10 is on %f\n", qswind.qval);

  if (qswind.qval >= 512)
    printf ("check qval %f\n", qswind.qval);

  gscolr (&icolr, &ier);
  if (*iwtyp == 2 || *iwtyp == 5)
    {
      ival = 1;
      gbarb (Sys_M, &ival, &qswind.latd, &qswind.lond, &qswind.sknt,
	     &qswind.drct, &ier, strlen (Sys_M));
    }
  else if (*iwtyp == 4)
    {
      ival = 1;
      garrw (Sys_M, &ival, &qswind.latd, &qswind.lond, &qswind.sknt,
	     &qswind.drct, &ier, strlen (Sys_M));
    }
  else
    {
      ival = 1;
      gdarr (Sys_M, &ival, &qswind.latd, &qswind.lond, &qswind.drct, &ier,
	     strlen (Sys_M));
    }
}
