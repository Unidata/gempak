	SUBROUTINE SFXDSP  ( sffile, trace, ntrace, stns, npts, trange, 
     +			     iret )
C************************************************************************
C* SFXDSP								*
C*									*
C* This subroutine allows the user to exit from SFGRAM.			*
C*									*
C* SFXDSP  ( SFFILE, TRACE, NTRACE, STNS, NPTS, TRANGE, IRET )		*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Surface file name		*
C*	TRACE (5) 	CHAR*		Traces				*
C*	NTRACE		INTEGER		Number of traces		*
C*	STNS(*)		CHAR*		Stations			*
C*	NPTS		INTEGER		Number of points		*
C*	TRANGE		CHAR*		Time range			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  1 = user entered EXIT		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 4/90	GEMPAK 5 version			*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C* S. Jacobs/NMC	 9/94	Changed format of output for stations	*
C*				and traces				*
C************************************************************************
	CHARACTER*(*)	sffile, trace (*), stns (*), trange
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret = 0
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' SFGRAM PARAMETERS' / )
	CALL ST_LSTR  ( sffile,  lenf, ier )
	WRITE  ( 6, 1005, IOSTAT = iostat )  sffile ( :lenf ), npts,
     +					     trange
1005	FORMAT ( ' Surface file:       ', A  /
     +           ' Number of times:    ', I4 /
     +           ' Time range:         ', A / 
     +           ' Traces: ' )
C
C*	Write out trace information.
C
	DO  i = 1, ntrace
	    CALL ST_LSTR ( trace (i), lent, ier )
	    WRITE  (6,*)  '    ', stns (i), '  ', trace (i) ( :lent )
	END DO
	WRITE  (6,*) '  '
C
C*	Allow user to exit if respond is set.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = 1
	END IF
C*
	RETURN
	END
