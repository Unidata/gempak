	SUBROUTINE SNFOPT  ( snfile, prmfil, stnfil, nstns, iadstn,
     +			     nst, maxtim, mrgtyp, imtype, iret )
C************************************************************************
C* SNFOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SNFOPT  ( SNFILE, PRMFIL, STNFIL, NSTNS, IADSTN, NST, MAXTIM,	*
C*           MRGTYP, IMTYPE, IRET )					*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Data file name			*
C*	PRMFIL		CHAR*		Parameter file name		*
C*	STNFIL		CHAR*		Station file			*
C*	NSTNS		INTEGER		Number of stations in file	*
C*	IADSTN		INTEGER		Number of additional stns	*
C*	NST		INTEGER		Total number of stations	*
C*	MAXTIM		INTEGER		Maximum number of times		*
C*	MRGTYP		LOGICAL		Merged file flag		*
C*	IMTYPE		INTEGER		Unmerged file type		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C************************************************************************
	CHARACTER*(*)	snfile, prmfil, stnfil
	LOGICAL		respnd, mrgtyp
C*
     	CHARACTER	csffil*60, cprmfl*60, cstnfl*60
C------------------------------------------------------------------------------
	iret = 0
	WRITE  ( 6, 1000 )
1000	FORMAT ( / ' SNCFIL PARAMETERS: ' / )
	csffil = snfile
	cprmfl = prmfil
	cstnfl = stnfil
	IF  ( mrgtyp )  THEN
	    WRITE  ( 6, 1001 ) csffil, cprmfl, cstnfl
1001	    FORMAT ( ' New sounding file:     ', A, / , 
     +               ' Parameter file:        ', A, / ,
     +               ' Station file:          ', A )
	 ELSE
	    WRITE  ( 6, 2001 ) csffil, cstnfl
2001	    FORMAT ( ' New sounding file:     ', A, / , 
     +               ' Station file:          ', A )
	END IF
C*
	WRITE  ( 6, 1002 ) nstns, iadstn, nst, maxtim
1002	FORMAT ( ' Number of stations in STNFIL: ', I7 , / ,
     +           ' Number of additional stations:', I7, / ,
     +           ' Total number of stations:     ', I7, / ,
     +           ' Total number of times:        ', I7, / )
C*
	IF  ( mrgtyp )  THEN
	    WRITE  ( 6, 1003 )
	  ELSE IF  ( imtype .eq. 1 )  THEN
	    WRITE  ( 6, 1004 )
	  ELSE IF  ( imtype .eq. 2 )  THEN
	    WRITE  ( 6, 1005 )
	  ELSE IF  ( imtype .eq. 3 )  THEN
	    WRITE  ( 6, 1006 )
	END IF
1003	FORMAT ( ' This file will be a merged sounding file.', / )
1004	FORMAT ( ' This file will be an unmerged sounding file',
     +           ' containing ' / ' mandatory data below 100 mb.' / )
1005	FORMAT ( ' This file will be an unmerged sounding file',
     +           ' containing ' / ' mandatory and significant data',
     +           ' below 100 mb.' / )
1006	FORMAT ( ' This file will be an unmerged sounding file',
     +           ' containing ' / ' mandatory and significant data',
     +           ' below and above 100 mb.' / )
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
