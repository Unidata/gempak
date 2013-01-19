C************************************************************************
C                                                                       *
 	INTEGER FUNCTION DECODE_SM ( BULTIN, LENB, OFILE, FILTYP, PFILE, 
     &                               SFILE, MAXSTN, MAXTIM, YYYYMM )
C                                                                       *
C************************************************************************
C									*
C*									*
C* This function decodes SYNOPTIC, SHIP, BUOY and CMAN observations 	*
C* in real time. It was designed to be called by the UNIDATA LDM	*
C* and in turn calls SM subroutines which do the actual decoding. The	*
C* SM routines came from John Neilsen at MIT. 			 	*
C* Output data is written to GEMPAK format surface data files.          *
C*	  								*
C*  Input:                                                              *
C*          BULTIN      A complete bulletin		 		*
C*            LENB      Length of said bulletin				*
C*           OFILE      Output data file to be used			*
C*	    FILTYP	Type of file to create (SHIP or NORMAL)		*
C*           PFILE      Parameter packing file				*
C*	     SFILE	Station data base file				*
C*	    MAXSTN	Max number of stations to add			*
C*	    MAXTIM	Max numbers of times in file			*
C**									*
C* Log:									*
C*							  		*
C* Jim Cowie (NPS)  4/89  - Original code				*
C* Jim Cowie (NPS) 10/89  - Modified to be used by LDM2			*
C* Jim Cowie (NPS)  6/90  - Added FILTYP argument for determining type	*
C*			    of output file to create. Consolidated the  *
C*			    PREPxx routines into one.			*
C* Fabrice Cuq (UCLA) 8/90 Modified to work on Sun                      *
C* John Nielsen (TAMU) 2/92 Appended gemtime.f to make self-contained	*
C*			    Made compatible with new RS library		*
C************************************************************************
C
      INCLUDE		'GEMINC:GEMPRM.PRM'
C*
      CHARACTER*(*)  bultin, ofile, pfile, sfile, filtyp
      CHARACTER  dattim*15, dtype*4, stn*7
      CHARACTER  parms(MMPARM)*4
      INTEGER  data(200), idd, ihh, iwscod, lenr
      INTEGER    dd, hh, early, late, yyyymm
      REAL       gdata(MMPARM)
      CHARACTER*512 report
      DATA maxrpt / 512 /
      LOGICAL    more, decoded, first_stn, ship, stmflg
C
      DATA early / 35 /, late / -1440 /
C
C------------------------------------------------------------------------
C
      more = .TRUE.
      first_stn = .TRUE.
	open(69, file='/gempak/logs/buoy.log',status='unknown',
     &       access='append')
      CALL ST_LCUC ( filtyp, filtyp, ier)
      IF ( filtyp .EQ. 'SHIP' ) THEN
         ship = .TRUE.
      ELSE
         ship = .FALSE.
      END IF
C
C*   Decode bulletin header and next line
C
      CALL RS_DHDR ( bultin, lenb, IMISSD, ibindex, idd, ihh, 
     &              dtype, iret )
      CALL RS_DLN2 ( bultin, lenb, IMISSD, ibindex, idd, ihh,
     &              dtype, iwscod, iret )
      IF ( iret .LT. 0) then
	write(69,*)' BAD bulletin: ',dtype,'  ',bultin(12:ibindex-1)
	close(69)
	RETURN
      endif
CC	if (dtype .eq. 'SMNA' .or. dtype .eq. 'XXXX')dtype='SMUS'
C
C*   Generate the GEMPAK time for this bulletin
C
      dd = idd
      hh = ihh
      CALL GEMTIME ( dd, hh, early, late, dattim, iret )
      IF (iret .NE. 0) THEN
         WRITE(69,*)'   DECODE_SM: Bulletin header: ',
     &             bultin(12:ibindex-1)
	 close(69)
         RETURN
      END IF
C
C*    Get the GEMPAK data file unit number and check for error
C
      stmflg = .true.
      CALL GETSMF ( ofile, ship, sfile, pfile, maxstn, maxtim,
     &             stmflg, iflno, parms, nparm, iret)
      IF ( iret .NE. 0 ) then
	close(69)
	RETURN
      endif
C
C*   Loop through reports.
C
      lenr = 0
      DO WHILE  ( more )
C
C*    Extract and decode the next report
C
	 CALL RS_GRPT ( bultin, lenb, maxrpt, ibindex, report,
     &			lenr, iret )
	 IF ( iret .EQ. 0 ) THEN
C
	    CALL RS_DECO ( report, lenr, dtype, idd, ihh, iwscod,
     &			   IMISSD, stn, data, iret )
            IF ( iret .EQ. 0 ) THEN
C
C*       Set or add the time in the file for the first station.
C*       (not for ship file, though)
C
               IF ( first_stn .AND. .NOT. ship ) THEN
                  CALL SETTIM ( iflno, dattim, ofile, ier)
                  IF ( ier .NE. 0 ) then
		     close(69)
		     RETURN
		  endif
                  first_stn = .FALSE.
               END IF
C
C*      Prepare the output data array, and write data.
C        
               CALL PREPDAT ( data, parms, nparm, IMISSD, RMISSD, gdata)
               CALL WRITE_SM ( iflno, ship, dtype, stn, ihhmm, dattim, 
     &                         data(5), data(6), gdata)
            END IF
         ELSE
            more = .FALSE.
         END IF
C
      END DO
C
C*	Close surface file.
C
      CALL SF_CLOS  ( iflno, ier )
C
      close(69)
      RETURN
      END		
C
C************************************************************************
C
      SUBROUTINE GETSMF ( file, ship, stnfile, prmfile, maxstn, maxtim,
     &                    stmflg, iflno, parms, nparm, iret)
C
C  This routine opens the GEMPAK data file requested. The file will be
C  created if it doesn't exist. This routine will create a SHIP-type
C  surface file if the SHIP flag is set.
C
C  Input Parameters:
C        FILE     CHAR*     Name of the data file
C        SHIP     LOGICAL   Ship file flag
C        STNFILE  CHAR*     File name of station table
C        PRMFILE  CHAR*     File name of parameter file
C        MAXSTN   INTEGER   Maximum number of stations allowed
C        MAXTIM   INTEGER   Maximum number of times allowed
C        STMFLG   LOGICAL   Station time flag
C
C  Output Parameters:
C        IFLNO    INTEGER   Unit number of opened file
C        PARMS    CHAR(*)   Parameter names
C        NPARM    INTEGER   Number of parameters
C        IRET     INTEGER   Return code
C                           =  0  normal return
C                           = -1  file could not be opened/created
C
C
C  Jim Cowie (NPS) 4/89
C
C************************************************************************
C
      INCLUDE		'GEMINC:GEMPRM.PRM'
C
      CHARACTER*(*) prmfile, stnfile, file
      CHARACTER parms(MMPARM)*4
      LOGICAL   pkflg, stmflg, ship
C
C----------------------------------------------------------------------
C
      iret = 0
C
C*  Open the GEMPAK data file
C
      CALL SF_OPNR ( file, iflno, isorc, nparm, parms, ier)
      IF ( ier .EQ. -2) THEN
C
C*     File couldn't be opened, try to open again to find out why.
C*     (SF_OPNR should really give us better information, but doesn't)
C
         CALL FL_GLUN ( lun, iret )
         OPEN ( UNIT = lun, FILE = file, STATUS = 'OLD', 
     &          ACCESS = 'DIRECT', IOSTAT = iostat,
     &          RECL = MBLKSZ * MMRECL, err = 10 )
C
C*     Exit with an error for all cases except when file doesn't exist
C  SUN error code: 118 can't find 'old' file
  10     IF ( iostat .NE. 118 .and. iostat.ne. 0) THEN
		write(69,*)' iostat',iostat
            iret = -1
         ELSE
C
C*       Create a new file, ship or normal
C
            IF ( ship ) THEN
               CALL SF_CSDP ( file, prmfile, MFSHIP, maxtim, stmflg,
     &                        iflno, nparm, parms, pkflg, iret) 
               IF ( iret .LT. 0) THEN
                  CALL SF_CLOS ( iflno, iret )
                  iret = -1
                ELSE
                   CALL ST_LSTR ( file, len, ier)
                   WRITE(69,*)'File ',file(1:len),' created.'
               END IF
C
            ELSE 
               CALL SF_CRFP ( file, prmfile, MFAIR, maxstn, maxtim,
     &                        stmflg, iflno, nparm, parms, pkflg, ier1) 
               CALL SF_STNF ( iflno, stnfile, ier2)
               IF ( ier1 .LT. 0 .OR. ier2 .LT. 0) THEN
                   WRITE(69,*)'DECODE_SM: Error creating file',
     &                       '/adding stns', ier1, ier2
                   CALL SF_CLOS ( iflno, iret )
                   iret = -1
                ELSE
                   CALL ST_LSTR ( file, len, ier)
                   WRITE(69,*)'File ',file(1:len),' created.'
                END IF
            END IF
         END IF
cc       CLOSE ( lun )
cc       CALL FL_FLUN ( lun, ier )
         CALL FL_CLOS ( lun, ier )
      END IF
C
      IF ( nparm .GT. MMPARM) THEN
         WRITE(69,*)'Number of parms too large:',nparm,' from file: ',
     &              file
         nparm = MMPARM
      END IF
      RETURN
      END
C
C************************************************************************
C*
      SUBROUTINE SETTIM ( iflno, dattim, ofile, ier )
C*
C*
C*  This routine sets the time in the file
C*
C*  Input Parameters:
C*
C*        IFLNO    INTEGER    File unit number
C*        DATTIM   CHAR*      Time to set /add
C*        OFILE    CHAR*      Output file name
C*
C*  Output Parameters:
C*
C*        IER      INTEGER    Return code
C*                            0 = OK
C*                           -1 = time not added or set
C*
C*
C************************************************************************
C
      CHARACTER*(*) dattim, ofile
C
C-----------------------------------------------------------------------
C
      ier = 0
C
C*  Set the time, add it if it's not there
C
      CALL SF_FTIM ( iflno, dattim, iret )
C
      IF ( iret .EQ. -11) THEN
         CALL SF_ATIM ( iflno, dattim, iret)
         IF ( iret .LT. 0) THEN
            WRITE(69,*)'DECODE_SM -- Error',iret,' adding ',
     &                    dattim(1:11),' to ',ofile
            CALL SF_CLOS ( iflno, iret)
            ier = -1
            RETURN
         ELSE
            WRITE(69,*)'DECODE_SM: Adding time ',dattim(1:11),
     &                 ' to ',ofile
         END IF
         CALL SF_FTIM ( iflno, dattim, iret )
      END IF
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE WRITE_SM ( IFLNO, SHIP, DTYPE, STN, IHHMM, DATTIM,
     &                      LAT, LON, GDATA)
C
C  This routine sets the station (if needed) and outputs the data to the
C  GEMPAK data file.
C
C  Input Parameters:
C        IFLNO    INTEGER   Data file logical unit number
C        SHIP     LOGICAL   Ship file flag
C        DTYPE    CHAR*     The type of data this is
C        STN      CHAR*7    The station ID or number
C        IHHMM    INTEGER   Hour and minute time fields
C        DATTIM   CHAR*15   Date/time string from bulletin
C        LAT      INTEGER   Latitude of station from report
C        LON      INTEGER   Longitude of station from report
C        GDATA    REAL(*)   Output data array
C
C
C  Jim Cowie (NPS) 4/89
C
C**************************************************************************
C
      INCLUDE		'GEMINC:GEMPRM.PRM'
C
      CHARACTER*(*) stn, dattim, dtype
      CHARACTER*2 st, cn
      INTEGER lat, lon
      REAL gdata(*)
      LOGICAL ship
C
C--------------------------------------------------------------------------
C
C*  We will write data for two types of files; SHIPS which we simply write
C*  out, and everything else, for which we set the station, then write the
C*  data. 
C
C*  Do SHIPS first:
C        
      IF ( ship ) THEN
C
C*    Determine if this is a id name or number
C
         CALL ST_NUMB ( stn, isnum, ier)
         IF ( ier .EQ. 0) stn = '    '
         rlat = lat/10.
         rlon = lon/10.
         elev = 0.
         CALL SF_WSDD ( iflno, dattim, stn, isnum, rlat, rlon,
     &                  elev, st, cn, ihhmm, gdata, iret)
         IF ( iret .NE. 0) 
     &      WRITE(69,*)'Error writing SHIP stn: ',stn,isnum,iret
C
C*  Now, all the rest:
C
      ELSE 
C
         CALL SF_FSTN ( iflno, stn, iret)
         IF ( iret .LT. 0) THEN
            OPEN (UNIT=91,FILE='/gempak/logs/UNK'//dtype//'.STNS',
     &           access='append', STATUS='UNKNOWN')
            WRITE(91,101)stn,' not set:',iret,
     &           ' Lat/lon:',lat/10.,lon/10.,' time: ',dattim(1:11)
            CLOSE (91)
         ELSE
            CALL SF_WDAT ( iflno, ihhmm, gdata, iret)
            IF ( iret .NE. 0) 
     &         WRITE(69,*)'Error writing ',TYPE,' stn: ',stn,iret
         END IF
      END IF
C
 101  FORMAT(1X,A,A,1X,I3,A,F8.2,F8.2,A,A)
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE PREPDAT ( DATA, PARMS, NPARM, IMISS, RMISS, GDATA )
C
C  This subroutine extracts certain elements of the decoded data array 
C  and puts these elements in an output array. The values are changed
C  to the proper units (as needed) and error checked.
C
C  Input Parameters:
C        DATA    INTEGER     Full array of decoded report elements
C	 PARMS   CHAR(*)     Parameter names needed
C 	 NPARM	 INTEGER     Number of parameters
C        IMISS   INTEGER     "Missing" value for input integer array
C        RMISS   REAL        "Missing" value for output real array
C
C  Output Parameters:
C        GDATA   REAL        Output array, with the elements arranged
C                            in order of PARMS
C
C
C   Jim Cowie (NPS)  6/90 - Consolidated version of PREPxx
C   Fabrice Cuq (UCLA) 9/90 Modified to get WNUM
C
C************************************************************************
C
	character*6 pt_wtmo
	real pt_wnum
      character*6 wtmo

      INCLUDE		'GEMINC:GEMPRM.PRM'

      INCLUDE 'parmdefs.inc'
C
      INTEGER data(*), imiss
      REAL gdata(*), rmiss
      CHARACTER parms(MMPARM)*4
	wtmo='      '
C
C------------------------------------------------------------------------
C
C*  Loop over the requested parameters
C
      DO 10 ip = 1, nparm
         DO 20 ipn = 1, MAXPNAM
            IF ( parms(ip) .EQ. pname(ipn)) GO TO 30
 20      CONTINUE
C        WRITE(69,*)'Invalid parameter name: ',parms(ip), ip, ipn
         gdata(ip) = RMISS
         GO TO 10
C
 30      CONTINUE
C
C*    Got the parameter name, so convert units as needeed and error-check
C
         IF ( data(ipn) .EQ. IMISS) THEN
            gdata(ip) = RMISS
         ELSE
C
C*      Take care of special cases (always something to spoil the fun)
C
            IF ( parms(ip) .EQ. 'SPED' ) THEN
               IF ( data(ipn) .LT. 500 ) THEN
                  gdata(ip) = data(ipn)
               ELSE
                  gdata(ip) = (data(ipn) - 500) * factor(ipn)
               END IF
            ELSE IF ( parms(ip) .EQ. 'P06I' ) THEN
               IF ( data(8) .NE. 6 ) THEN
                  gdata(ip) = RMISS
               ELSE IF ( data(ipn) .EQ. 9999) THEN
                  gdata(ip) = 1.0
               ELSE
                  gdata(ip) = data(ipn) * factor(ipn)
               END IF
            ELSE IF ( parms(ip) .EQ. 'GUST' ) THEN
               IF ( data(ipn) .LT. 500 ) THEN
                  gdata(ip) = data(ipn) * factor(ipn)
               ELSE
                  gdata(ip) = data(ipn) - 500
               END IF
	    else if (parms(ip) .eq. 'WWMO') then
c keep the value of index of WWMO for WNUM
		  ipos1 = ip
		  gdata(ip) = data(ipn) * factor(ipn)
	    else if (parms(ip) .eq. 'WNUM') then
C keep the position of WNUM
		  ipos = ip
            ELSE
               gdata(ip) = data(ipn) * factor(ipn)
            END IF
            CALL ERRCHEK ( gdata(ip), min(ipn), max(ipn), RMISS)
         END IF

c         print*,parms(ip), data(ipn), gdata(ip)

C
 10   CONTINUE
C
C get WNUM
	if( gdata(ipos1) .eq. RMISS) then
	  gdata(ipos) = 0.
	  else
C first convert WWMO into character code
	  wtmo = pt_wtmo(gdata(ipos1))
C then convert to Gempak numerical code
	  gdata(ipos)= pt_wnum(wtmo)
	endif
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE ERRCHEK ( VAL, MIN, MAX, MISS)
C
C  This subroutine verifies that the input VAL lies between MIN and MAX 
C  values. If it does not, MISS is returned for VAL.
C
C  Input Parameters:
C        VAL      REAL     The input value to be checked
C        MIN      REAL     Minimum value VAL can have
C        MAX      REAL     Maximum value VAL can have
C        MISS     REAL     Missing value inserted if VAL is out of range
C
C   Jim Cowie (NPS) 4/89 - Original code. All rights refused.
C
C************************************************************************
C
      REAL val, min, max, miss
C
C------------------------------------------------------------------------
C
C*  EASY!!
C
      IF ( val .GT. max .OR. val .LT. min ) val = miss
C
      RETURN
      END
C
C**********************************************************************
C*
      SUBROUTINE GEMTIME ( dd, hh, early, late, dattim, iret)
C*
C*
C*  This routine determines the date/time which can be used for this
C*  bulletin. The day and hour from the bulletin, and the month and year
C*  from the system are used.
C*
C*  Input Parameters:
C*
C*        DD	   INTEGER   Day from bulletin header
C*        HH	   INTEGER   Hour from bulletin header
C*        EARLY    INTEGER   Earliest an early bulletin can be
C*        LATE     INTEGER   Latest a late bulletin can be
C*
C*  Output Parameters:
C*
C*        DATTIM   CHAR*15   Date/time for bulletin
C*        IRET     INTEGER   return code
C*                           = 0 all OK
C*                           = -1 error, time out of range or invalid
C* 
C*  Jim Cowie (NPS) 7/90
C*
C**********************************************************************
C*
      INTEGER stime(5), btime(5), dpm(0:12)
      INTEGER  dd, hh, early, late, tdiff
      CHARACTER dattim*15, systim*15
      DATA dpm / 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C
C----------------------------------------------------------------------
C
      iret = 0
C
C*  First, check that the day and hour was extracted properly
C
      IF (dd.GT.31 .OR. dd.LT.1 .OR. hh.GT.23 .OR. hh.LT.0) THEN
         WRITE(69,199)'GEMTIME: Bad dd/hh group:',dd,hh
         iret = -1
         RETURN
      END IF
C
C*  Get current time; convert to integer; convert to Greenwich.
C 
      CALL TI_GTIM  ( systim, ier1 )
      CALL TI_CTOI  ( systim, stime, ier2 )
      CALL TI_GREN  ( stime, stime, ier3 )
      CALL TI_ITOC  ( stime, systim, ier )
C
C*  Use the year and month from the system time, day and hour from
C*  the bulletin header, assume minutes=00
C
      btime(1) = stime(1)
      btime(2) = stime(2)
      btime(3) = dd
      btime(4) = hh
      btime(5) = 0
C
C*  If the day from the bulletin isn't the same as that from the system -
C
      IF ( stime(3) .NE. btime(3) ) THEN
         IF ( MOD(stime(1),4) .EQ. 0) dpm(2) = 29
C
C*    Check if bulletin says first day of the month and system says last
C*    day of current month. If so, add a month to the time.
C
         IF ( btime(3) .EQ. 1 .AND. stime(3) .EQ. dpm(stime(2))) THEN
            btime(2) = btime(2) + 1
C
C*       Add a year if necessary
C
            IF ( btime(2) .GT. 12 ) THEN
               btime(2) = 1
               btime(1) = btime(1) + 1
            END IF
C
C*    Check if system says first day of the month and bulletin has last
C*    day of previous month. If so, subtract a month.
C
         ELSE IF (( stime(3) .EQ. 1)
     &          .AND. (btime(3) .EQ. dpm(btime(2)-1))) THEN
            btime(2) = btime(2) - 1
C
C*       Add a year if necessary
C
            IF ( btime(2) .LT. 1 ) THEN
               btime(2) = 12
               btime(1) = btime(1) - 1
            END IF
C
C*    The only other allowed oddity would be a normal day change, so
C*    write out an error message if not that.
C
         END IF
      END IF
C
C*  Make sure the time is reasonable. Compare to allowable window.
C
      CALL TI_MDIF ( btime, stime, tdiff, ier )
      IF ( ier .NE. 0) THEN
         WRITE(69,200)'GEMTIME: Error computing time diff, dd/hh td:',
     &                dd,hh, tdiff,' SYSTIM: ',systim(1:11)
         iret = -1
      ELSE IF ( tdiff .GT. early ) THEN
         WRITE(69,200)'GEMTIME: Early bulletin, dd/hh td:',dd,hh,tdiff,
     &               ' SYSTIM: ',systim(1:11)
         iret = -1
      ELSE IF ( tdiff .LT. late ) THEN
         WRITE(69,200)'GEMTIME: Late  bulletin, dd/hh td:',dd,hh,tdiff,
     &               ' SYSTIM: ',systim(1:11)
         iret = -1
C
C*  Convert the bulletin time array to character
C
      ELSE
         CALL TI_ITOC ( btime, dattim, ier)
         IF ( ier .NE. 0) iret = -1
      END IF
C
 200  FORMAT (1X,A,1X,I2.2,'/',I2.2,'/',I6,A,A)
 199  FORMAT (1X,A,1X,I2.2,'/',I2.2)
      RETURN
      END


