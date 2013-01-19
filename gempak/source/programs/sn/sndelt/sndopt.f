	SUBROUTINE SNDOPT  ( time, snfile, area, iret )
C************************************************************************
C* SNDOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SNDOPT  ( TIME, SNFILE, AREA, IRET )					*
C*									*
C* Input parameters:							*
C*	TIME		CHAR*		Date/time			*
C*	SNFILE		CHAR*		Data file name			*
C*	AREA		CHAR*		Area				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C************************************************************************
	CHARACTER*(*)	snfile, time, area
	LOGICAL		respnd
C*
     	CHARACTER	csnfil*60
C------------------------------------------------------------------------------
	iret = 0
C*
	WRITE  ( 6, 1000 ) time
1000	FORMAT ( / ' SNDELT PARAMETERS: ' / 
     +             ' Deleting data for time : ', A )
C*
	csnfil = snfile
	WRITE  ( 6, 1001 ) csnfil, area
1001	FORMAT ( ' Sounding file:  ', A, / , 
     +           ' Area:           ', A, / 
     +           ' WARNING:  If area is DSET or ALL, all data for',
     +           ' this time will be deleted.' / )
C*
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
