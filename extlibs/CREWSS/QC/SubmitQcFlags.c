#include "gui.h"


int ObsQCed (void);
void MakeQcRec( char *outrec);

int ReadOnly;

/*ARGSUSED*/
void SubmitQCFlagsCb( Widget widget, int tag, XtPointer cb_data)
/************************************************************************
 * SubmitQCFlagsCb                                                      *
 *                                                                      *
 * Create the Flags file and execute the send_flags script.             *
 * Write out all obs in the current synoptic period that have been      *
 * flagged or edited.                                                   *
 *                                                                      *
 ***********************************************************************/
{
  if (ReadOnly) {
     PopupErrorMsg("Executing in READONLY mode--cannot send flag file.");
     return;
  }
  XtVaSetValues( widget, XmNforeground, Red.pixel, NULL); /* change button color to red */
  XmUpdateDisplay( widget );

  Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */

  if (SaveObsChanges()) {
     WriteQCFlags();

  }

  XtVaSetValues( widget, XmNforeground, Black.pixel, NULL); /* change button color to black */
  XmUpdateDisplay( widget );

}

/*=====================================================================*/

void WriteQCFlags(void)
{
   FILE *Flag_File;
   int i,j, status;
   char outrec[124];
   char command[100];
   DIR  *qcdir;
   struct dirent *this_qc_file;

/*---------------------------------------------------------------------*/

   Flag_File =  fopen(Flag_Filename, "w");

   if ( Flag_File  == NULL) {
      fprintf(stderr,"Could not open flag file: %s\n", Flag_Filename);
      exit(1);
   }

   i = 0;
   qcdir = opendir(Dated_QC_Directory);
   if (qcdir == NULL) {
      printf( "Error opening directory %s\n", Dated_QC_Directory);
      exit(1);
   } else {
      while ((this_qc_file = readdir( qcdir )) != NULL) {
         if ((strcmp(this_qc_file->d_name, ".") != 0) &&
            (strcmp(this_qc_file->d_name, "..") != 0) &&
            (strcmp(this_qc_file->d_name, "nobs") != 0) &&
            (strcmp(this_qc_file->d_name, "Flags") != 0)) {
            strcpy(Db_Filename, Dated_QC_Directory);
            strcat(Db_Filename, "/");
            strcat(Db_Filename, this_qc_file->d_name);
            if (!  IsDir(Db_Filename)) {
               j = 0;
               Db_File = NULL;
               while (ReadWriteObs( &Db_File, Db_Filename, j, 1,
                   obsdate, obstime, callsign, &lat, &lon,
                   &slp,  &airtemp, &windspd, &winddir, &sst,
                   &dewp, &presschg3hr, &cloud, &vis,  &reptype, &wx, 
                   &ice, &waveper, &model_slp,
                   &model_airtemp, &model_windspd, &model_winddir,
                   &model_sst, edit_callsign, Edited_Value, QC_Flags,
                   &duplicate, &suspect_slp,  &suspect_airtemp, 
		   &suspect_windspd, &suspect_winddir, &suspect_sst, 
		   &suspect_dup, &suspect_loc, &suspect) != -1) {
  
                  j++; 
                  if (ObsQCed()) {
                     /*printf("--> obs %s %s was qc'ed\n", callsign, obstime);*/
                     i++;
                     MakeQcRec(outrec);
                     fprintf(Flag_File, "%s\n", outrec);
                  }
               }
               fclose(Db_File);
            }
       
         }
      }

      printf("Wrote %d obs\n", i);
      fclose(Flag_File);
      sprintf(command, "%s/scripts/send_flags %s > %s/send_flags.log", 
				QC_Directory, Flag_Filename, QC_Directory);
      printf("Command to send off changes: %s\n", command);
      status = system(command);
      status = status/256;
      printf("status return: %d\n", status);
      if (status == 0)
         PopupErrorMsg("Flag file FAILED to upload to primary or backup.");
      else if (status == 1)
         PopupErrorMsg("Flags uploaded successfully.");
      else
         PopupErrorMsg("Flags uploaded successfully to backup."); 
         
   }

}

/*=====================================================================*/

int ObsQCed(void)
/* return true if any flag was set or value edited. */
{

    int i;
/*---------------------------------------------------------------------*/
    for (i=0; i<8; i++) {
       /* printf("QC_Flags[%i]=%i\n",i, QC_Flags[i]) */;
       if (QC_Flags[i]>0) return 1;
    }

    for (i=1; i<8; i++) {
       /* printf("Edited_Value[%i]=%f\n",i,  Edited_Value[i]); */
       if (Edited_Value[i]!= -99.) return 1;
    }

    if (strcmp(edit_callsign,"-99") != 0) return 1;

    if (duplicate) return 1;

    return 0;

}

/*=====================================================================*/

void MakeQcRec( char *outrec)
{
   int i, flag;
   char dupchar, slpchar, windchar, tempchar, sstchar;
   char corrections[70];
   int  outlon, outlat, outslp, outsst, outairtemp, outwinddir, outwindspd;
   int  editlon, editlat, editslp, editsst, editairtemp, editwinddir, 
					editwindspd;
   int  nchars, pos;

/*---------------------------------------------------------------------*/

   for (i=0; i<124; i++) outrec[i]=' ';
   outlat = (int) (lat*100);                        /* hundredths of degrees */
   outlon = (int) (((360.-lon)+.005)*100);          /* hundredths of degrees westbound */ 
   outslp = (int) (slp*10);                         /* tenths of millibars */
   outsst = (int) (sst*10);                         /* tenths of degrees */
   outairtemp = (int) (airtemp*10);                 /* tenths of degrees */
   outwinddir = (int) winddir;                      /* degrees from true north */
   outwindspd = (int) windspd;                      /* whole knots */

   editlat = -99; editlon = -99; editslp = -99; editslp = -99;
   editairtemp = -99; editwinddir = -99; editwindspd = -99; editsst = -99;

   editlat = (int) (Edited_Value[1]*100);           /* hundredths of degrees */
   editlon = (int) ((360-Edited_Value[2]+.005)*100);/* hundredths of degrees westbound */
   editslp = (int) (Edited_Value[3]*10);            /* tenths of millibars */
   editairtemp = (int) (Edited_Value[4]*10);        /* tenths of degrees */
   editwindspd = (int) Edited_Value[5];             /* whole knots */
   editwinddir = (int) Edited_Value[6];             /* degrees from true north */
   editsst = (int) (Edited_Value[7]*10);            /* tenths of degrees */

   if (strcmp(edit_callsign, "-99") == 0) strcpy(edit_callsign, "        ");

   strcpy(corrections,"");

   dupchar = ' '; slpchar = ' '; tempchar = ' ';
   sstchar = ' '; windchar = ' ';

   if (duplicate) dupchar = 'L';

   sprintf(outrec, "%-8s % 4.4d %5.4d %8s %4s  %3.3d", callsign, 
			outlat, outlon, obsdate, obstime, reptype); 

   for (i=0; i<39; i++) if (outrec[i] == '\0') outrec[i] = ' ';

   for (i=39; i<100; i++) outrec[i]=' ';
   outrec[100] = '\0';

   pos=0;

   if (QC_Flags[1] != 0) {           /* qc decision for lat */
       switch (QC_Flags[1]) {
         case 1: break;
         case 2: break;
         case 3: 
            nchars = sprintf(&corrections[pos], "QL%5.4d", editlat);
            if (corrections[pos+2]==' ') corrections[pos+2]='0';
            pos = pos + nchars;
            break;
       }
   }

   if (QC_Flags[2] != 0) {           /* qc decision for lon */
       switch (QC_Flags[2]) {
         case 1: break;
         case 2: break;
         case 3: 
            nchars = sprintf(&corrections[pos], "QG%5.4d", editlon);
            if (corrections[pos+2]==' ') corrections[pos+2]='0';
            pos = pos + nchars;
            break;
      }
   }
 
   if (QC_Flags[3] != 0) {           /* qc decision for slp */
      switch (QC_Flags[3]) {
        case 1: 
           slpchar='H';
           break;
        case 2: 
           slpchar='P';
           break;
        case 3: 
           nchars = sprintf(&corrections[pos], "OP%5.4dQP%5.4d", 
							outslp, editslp);
           if (corrections[pos+2]==' ') corrections[pos+2]='0';
           if (corrections[pos+9]==' ') corrections[pos+9]='0';
           pos = pos + nchars;
           break;
      }
   }

   if (QC_Flags[4] != 0) {           /* qc decision for air temp */
      switch (QC_Flags[4]) {
        case 1: 
           tempchar='H';
           break;
        case 2: 
           tempchar='P';
           break;
        case 3: 
           nchars = sprintf(&corrections[pos], "OT%5.4dQT%5.4d", outairtemp, 
								editairtemp);
           if (corrections[pos+2]==' ') corrections[pos+2]='0';
           if (corrections[pos+9]==' ') corrections[pos+9]='0';
           pos = pos + nchars;
           break;
      }
   }

/* qc decision for wind spd and dir*/
   if ((QC_Flags[5] != 0) || (QC_Flags[6] != 0)) {
      if (QC_Flags[5] != 0) flag = QC_Flags[5]; else flag = QC_Flags[6];
      switch (flag) {
        case 1: 
           windchar='H';
           break;
        case 2: 
           windchar='P';
           break;
        case 3: 
           if (editwinddir==-99.) editwinddir=winddir;
           if (editwindspd==-99.) editwindspd=windspd;
           nchars = sprintf(&corrections[pos], "0W%3.2d%2.1dQW%3.2d%2.1d", 
				outwinddir, outwindspd, editwinddir, 
				editwindspd);
           if (corrections[pos+2]==' ') corrections[pos+2]='0';
           if (corrections[pos+5]==' ') corrections[pos+5]='0';
           if (corrections[pos+9]==' ') corrections[pos+9]='0';
           if (corrections[pos+12]==' ') corrections[pos+12]='0';
           pos = pos + nchars;
           break;
      }
   }

   if (QC_Flags[7] != 0) {           /* qc decision for sst */
      switch (QC_Flags[7]) {
        case 1: 
           sstchar='H';
           break;
        case 2: 
           sstchar='P';
           break;
        case 3: nchars = sprintf(&corrections[pos], "OS%5.4dQS%5.4d", 
						outsst, editsst);
           if (corrections[pos+2]==' ') corrections[pos+2]='0';
           if (corrections[pos+9]==' ') corrections[pos+9]='0';
           pos = pos + nchars;
           break;
      }
   }


   corrections[pos] = '\0';

   sprintf(&outrec[39], "%c%c%c%c%c %-8s  %s", dupchar, slpchar, windchar, 
			tempchar, sstchar, edit_callsign, corrections); 

}
