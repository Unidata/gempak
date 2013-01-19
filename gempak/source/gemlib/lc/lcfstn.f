	SUBROUTINE LC_FSTN  ( stntbl, stn, rlat, rlon, iret )
C************************************************************************
C* LC_FSTN								*
C*									*
C* This subroutine searches the station table file for a particular 	*
C* station and returns the latitude and longitude of the station.	*
C*									*
C* The input parameter STN must be in upper case letters.		*
C*									*
C* LC_FSTN  ( STNTBL, STN, RLAT, RLON, IRET )				*
C*									*
C* Input parameters:							*
C*	STNTBL		CHAR*		Station table name		*
C*	STN		CHAR*8		Station identifier		*
C*									*
C* Output parameters:							*
C*	RLAT		REAL		Station latitude		*
C*	RLON		REAL		Station longitude		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C*					  -3 = station table not opened	*
C*					  -4 = station not in table	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/84						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 4/90	Fix error msg if table can't be opened	*
C* K. Brill/NMC		 8/93	Added ISPRI to TB_RSTN call		*
C* S. Jacobs/NMC	 7/94	Added STNTBL to calling sequence	*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* L. Sager/NCEP         6/96   Changed calling sequence for TB_RSTN    *
C*                              to add character string parameter       *
C* T. Piper/GSC		11/98	Updated prolog				*
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	stntbl, stn
C*
	CHARACTER 	stnid*8, stnnam*20, state*6, coun*6, tbchrs*20 
	LOGICAL 	eof
C------------------------------------------------------------------------
	iret = -4
	rlat = 0.0
	rlon = 0.0
C
C*	Check for a station number in place of a the ID.
C
	CALL ST_NUMB  ( stn, istnm, inumer )
C
C*	Open the table file.
C
	CALL FL_TBOP  ( stntbl, 'stns', lun, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ierr, stntbl, ier )
	    iret = -3
	    RETURN
	END IF
C
C*	Loop through the file looking for the station.
C
	eof = .false.
	DO WHILE  ( .not. eof )
	    CALL TB_RSTN  ( lun, stnid, stnnam, isnum, state, coun,
     +			    tlat, tlon, hght, ispri, tbchrs, ier )
	    IF  ( ier .ne. 0 )  THEN
		eof = .true.
	      ELSE
		IF  ( ( ( stn .eq. stnid ) .and.
     +			( inumer .ne. 0  ) ) .or.
     +		      ( ( istnm .eq. isnum ) .and.
     +			( inumer .eq. 0 ) ) )  THEN
		    eof  = .true.
		    iret = 0
		    rlat = tlat
		    rlon = tlon
		END IF
	    END IF
	END DO
C
C*	Close the table file.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
