	SUBROUTINE SNEWRT  ( isnfln, time, istime, stid, istnm, slat, 
     +			     slon, selv, data, nlev, iret )
C************************************************************************
C* SNEWRT								*
C*									*
C* This subroutine writes data to the sounding file.			*
C*									*
C* SNEWRT  ( ISNFLN, TIME, ISTIME, STID, ISTNM, SLAT, SLON, SELV, 	*
C*           DATA, NLEV, IRET )							*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	TIME		CHAR*		Time				*
C* 	ISTIME		INTEGER		Station time HHMM format	*
C*	STID		CHAR*		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	DATA		REAL		Station data			*
C*	 (NPARMS,NLEV)							*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-14 = error writing to file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* S. Schotz/GSC	12/89  	Setting of time/station now in SNESET   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, time
	REAL		data (*)
C*
	CHARACTER 	output*32, stname*8
C------------------------------------------------------------------------
	iret   = 0
C
C*	Set time and station if necessary
C
	CALL SNESET ( isnfln, time, stid, istnm, slat, slon, selv, 
     +                stname, ier )
        IF ( ier .eq. 0 ) THEN
C
C*	    Write station data.
C
	    CALL SN_WDAT  ( isnfln, istime, nlev, data, iret )
	    IF  ( iret .ne. 0 )  THEN
	        CALL ER_WMSG  ( 'SN', iret, ' ', ier )
	        iret = -16
	        CALL ER_WMSG  ( 'SNEDIT', iret, ' ', ier )
	    ELSE
	        CALL ST_LSTR  ( stname, len, ier )
	        output = stname ( : len ) // ' at ' // time
	        CALL ER_WMSG  ( 'SNEDIT', +1, output, ier )
	    END IF
        END IF
C*
	RETURN
	END
