	SUBROUTINE SFFOPT  ( sffile, prmfil, stnfil, nstns, iadstn,
     +			     nst, maxtim, shipfl, iret )
C************************************************************************
C* SFFOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SFFOPT  ( SFFILE, PRMFIL, STNFIL, NSTNS, IADSTN, NST, MAXTIM,	*
C*           SHIPFL, IRET )						*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Data file name			*
C*	PRMFIL		CHAR*		Parameter file name		*
C*	STNFIL		CHAR*		Station file			*
C*	NSTNS		INTEGER		Number of stations in file	*
C*	IADSTN		INTEGER		Number of additional stns	*
C*	NST		INTEGER		Total number of stations	*
C*	MAXTIM		INTEGER		Maximum number of times		*
C*	SHIPFL		LOGICAL		Ship file flag			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* S. Schotz/GSC	 5/90	Get respond flag from IP_RESP		*
C************************************************************************
	CHARACTER*(*)	sffile, prmfil, stnfil
	LOGICAL		respnd, shipfl
C*
     	CHARACTER	csffil*60, cprmfl*60, cstnfl*60
C------------------------------------------------------------------------------
	iret = 0
	WRITE  ( 6, 1000 )
1000	FORMAT ( / ' SFCFIL PARAMETERS: ' / )
	csffil = sffile
	cprmfl = prmfil
	cstnfl = stnfil
	WRITE  ( 6, 1001 ) csffil, cprmfl, cstnfl
1001	FORMAT ( ' New surface file:      ', A, / , 
     +           ' Parameter file:        ', A, / ,
     +           ' Station file:          ', A )
C*
	WRITE  ( 6, 1002 ) nstns, iadstn, nst, maxtim
1002	FORMAT ( ' Number of stations in STNFIL: ', I7 , / ,
     +           ' Number of additional stations:', I7, / ,
     +           ' Total number of stations:     ', I7, / ,
     +           ' Total number of times:        ', I7, / )
C*
	IF  ( shipfl )  THEN
	    WRITE  ( 6, 1003 )
	  ELSE
	    WRITE  ( 6, 1004 )
	END IF
1003	FORMAT ( ' This file will be a ship file.', / )
1004	FORMAT ( ' This file will be a standard surface file.', / )
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
