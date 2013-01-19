	SUBROUTINE SFESTN  ( isffln, stn, tim, curtim, data, ihhmm, 
     +			     iret )
C************************************************************************
C* SFESTN								*
C*									*
C* This subroutine sets the time and station and returns the current	*
C* data at that station.						*
C*									*
C* SFESTN  ( ISFFLN, STN, TIM, CURTIM, DATA, IHHMM, IRET )		*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	STN		CHAR*		Station				*
C*	TIM		CHAR*		Date/time			*
C*									*
C* Input and output parameters:						*
C*	CURTIM		CHAR*		Current date/time		*
C*									*
C* Output parameters:							*
C*	DATA (*)	REAL		Data at station			*
C*	IHHMM		INTEGER		Station time			*
C*	IRET		INTEGER		Return code			*
C*					 +7 = time cannot be added	*
C*					 +2 = station cannot be added	*
C*					  0 = normal return		*
C** Log:								*
C* M. desJardins/GSFC	 6/88						*
C* M. desJardins/GSFC	11/89	Changed station time			*
C* M. desJardins/GSFC	12/89	Fixed calling sequence for SF_SSTN	*
C* K. Brill/NMC		01/92	Return +7 when time cannot be added	*
C* K. Brill/NMC		 8/93	Added ISPRI to SF_ASTN call		*
C* A. Hardy/GSC		 3/99	Added priority parameter to SF_SSTN     *
C* A. Hardy/GSC		 3/99	Removed ispri = 0			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stn, tim, curtim
	REAL		data (*)
C*
	CHARACTER	stid*8, stat*8, coun*8
	LOGICAL		new
C------------------------------------------------------------------------
	iret = 0
	new  = .false.
C
C*	Check to see if this is the same time as last.
C
	IF  ( tim .ne. curtim )  THEN
C
C*	    Set the time in the file.
C
	    CALL SF_STIM  ( isffln, tim, ier )
C
C*	    If time is not found, add to file.
C
	    IF  ( ier .ne. 0 )  THEN
		new = .true.
		CALL SF_ATIM  ( isffln, tim, ier )
		IF  ( ier .ne. 0 )  THEN
		    iret = +7
		    CALL ER_WMSG ( 'SFEDIT', -7, tim, ierr )
		    RETURN
		END IF
		CALL SF_STIM  ( isffln, tim, ier )
	    END IF
	END IF
C
C*	Set the station.  If not found, add station.
C
	CALL SF_SSTN ( isffln, stn, stid, istnm, slat, slon, selv, 
     +		       ispri, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ST_NUMB  ( stn, istnm, ierr )
	    IF  ( ierr .eq. 0 )  THEN
		stid  = ' '
	      ELSE
		stid  = stn
		istnm = IMISSD
	    END IF
	    slat = RMISSD
	    slon = RMISSD
	    selv = RMISSD
	    stat = ' '
	    coun = ' '
C
C*	    Now, add the station.
C
	    new = .true.
	    CALL SF_ASTN  ( isffln, 1, stid, istnm, slat, slon, selv,
     +			    stat, coun, ispri, n, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = +2
		CALL ER_WMSG  ( 'SFEDIT', iret, stn, ier )
		RETURN
	      ELSE
		CALL SF_SSTN ( isffln, stn, stid, istnm, slat, slon,
     +			       selv, ispri, ier )
	    END IF
	END IF
C
C*	Now, read the data from the file.
C
	IF  ( .not. new )  THEN
	    CALL SF_RDAT  ( isffln, data, ihhmm, ier )
	    IF  ( ier .ne. 0 )  THEN
		new = .true.
	    END IF
	END IF
C
C*	Set missing data.
C
	IF  ( new )  THEN
	    ihhmm = IMISSD
	    DO  i = 1, MMPARM
		data (i) = RMISSD
	    END DO
	END IF
C*
	RETURN
	END
