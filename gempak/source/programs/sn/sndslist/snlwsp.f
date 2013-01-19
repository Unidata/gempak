	SUBROUTINE SNLWSP  ( stnprm, nstnp, rdata, luns, nlun, iret )
C************************************************************************
C* SNLWSP								*
C*									*
C* This subroutine writes the station data to the output devices.	*
C*									*
C* SNLWSP  ( STNPRM, NSTNP, RDATA, LUNS, NLUN, IRET )			*
C*									*
C* Input parameters:							*
C*	STNPRM (NSTNP)	CHAR*		Station parameters		*
C*	NSTNP		INTEGER		Number of station parameters	*
C*	RDATA  (NSTNP)	REAL		Station data values		*
C*	LUNS   (NLUN)	INTEGER		Output LUNS			*
C*	NLUN		INTEGER		Number of output units		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stnprm (*)
	REAL		rdata  (*)
	INTEGER		luns   (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Write blank line.
C
	DO  i = 1, nlun
	    WRITE  ( luns (i), 1000 )
1000	    FORMAT ( 1X )
	END DO
C
C*	Write out station parameter data.
C
	DO  i = 1, nlun
	    WRITE  ( luns (i), 1010, IOSTAT = iostat )  
     +				( stnprm (j), rdata (j), j = 1, nstnp )
1010	    FORMAT ( 4 ( 1X, A4, ' = ', F8.2, 3X ) )
	END DO
C*
	RETURN
	END
