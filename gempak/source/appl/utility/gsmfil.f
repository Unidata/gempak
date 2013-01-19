	SUBROUTINE GSMFIL  ( mapfil, iret )
C************************************************************************
C* GSMFIL								*
C*									*
C* This subroutine sets the name of the map file used to draw maps.	*
C*									*
C* GSMFIL  ( MAPFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	MAPFIL 		CHAR*   	Map file name 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Vilardo/RDS	12/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	 4/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 3/89	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	CHARACTER*(*) 	mapfil
C
	CHARACTER	cbuff*80	
	INTEGER 	isend (23)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	cbuff = mapfil	                
	CALL ST_LSTR ( cbuff, nchar, iret )      
	nwrd = (nchar - 1) / 4  + 1            
C
	isend (1) = nwrd + 3
	isend (2) = FSMFIL
	isend (3) = nchar
	CALL ST_STOI  ( cbuff, nchar, nv, isend (4), iret )
C
	CALL GPUT ( isend, nwrd + 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
