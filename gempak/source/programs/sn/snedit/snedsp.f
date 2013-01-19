	SUBROUTINE SNEDSP  ( snfile, merge, newfil, parms, nparms, 
     +			     iret )
C************************************************************************
C* SNEDSP								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SNEDSP  ( SNFILE, MERGE, NEWFIL, PARMS, NPARMS, IRET )		*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Sounding file name		*
C*      MERGE		LOGICAL		Sounding merge flag		*
C*	NEWFIL		LOGICAL		New file was created		*
C*	PARMS (NPARMS)	CHAR*		Parameter names			*
C*	NPARMS		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* S. Schotz/GSC	12/89 	Updated for unmerged option		*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C************************************************************************
	CHARACTER*(*)	snfile, parms (*)
	LOGICAL		newfil, respnd, merge
C------------------------------------------------------------------------------
	iret = 0
	WRITE  ( 6, 1000 )
1000	FORMAT ( / ' SNEDIT PARAMETERS: ' / )
	CALL ST_LSTR  ( snfile, ll, ier )
	WRITE  ( 6, 1001 ) snfile ( : ll )
1001	FORMAT ( ' Output sounding file: ', A )
C*
	IF  ( newfil )  THEN
	    WRITE  ( 6, 1010 )
1010	    FORMAT ( ' This file has been created.' )
	  ELSE
	    WRITE  ( 6, 1011 )
1011	    FORMAT ( ' This is an existing file.' )
	END IF
C*
	IF ( merge ) THEN
	    WRITE  ( 6, 1020 )  ( parms (i), i = 1, nparms )
1020	    FORMAT ( / ' The following parameters are in the file: ' /
     +             12 ( 1X, A4 ) )
        END IF
	WRITE  ( 6, 1021 )
1021	FORMAT ( 1X )
C
C*	Get user response.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
