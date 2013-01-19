	SUBROUTINE GDRGRD ( dellat, dellon, lblfrq, blat, blon, 
     +                      ifrmat, iret )
C************************************************************************
C* GDRGRD								*
C* 									*
C* This subroutine draws a uniform latitude/longitude grid.  The map 	*
C* projection must be defined before GDRGRD is called.  The current	*
C* color, line and text attributes are used.                            *
C* 									*
C* GDRGRD  ( DELLAT, DELLON, LBLFRQ, BLAT, BLON, IFRMAT, IRET )		*
C* 									*
C* Input parameters:							*
C* 	DELLAT		REAL  		Latitude interval in degrees 	*
C*	DELLON		REAL		Longitude interval in degrees	*
C*	LBLFRQ (2)	INTEGER		Label frequency			*
C*				  	  0 = no grid labels		*
C*				   	  1 = every grid line		*
C*				   	  2 = every other grid line	*
C*				   	  n = every n-th line		*
C*	BLAT		REAL		Longitude label locations	*
C*	BLON		REAL		Latitiude label locations	*
C*	IFRMAT		INTEGER		Lat/lon format			*
C*					  1 = +/- value			*
C*					  2 = N,S,E,W addded, no '-'    * 
C*					  Defaults to 1 if not set to 2 *
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* R. Shah/RDS		8/81						*
C* J. M. Vilardo/RDS	1/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	6/88	Clean up 				*
C* M. desJardins/NMC	8/91	Separate lat/lon label frequencies	*
C* L. Williams/EAi      3/94    Removed blank comments from header      *
C* A. Hardy/GSC         6/98    Cleaned up prolog                       *
C* A. Hardy/GSC		12/00   Added lat/lon label locations, format	*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER		lblfrq (2) 
C*
	INTEGER 	isend (2)
	REAL 		rsend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 9
	isend (2) = FDRGRD
	rsend (1) = dellat
	rsend (2) = dellon
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( rsend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
	CALL GPUT ( lblfrq, 2, iret )
C
	CALL GPUTR ( blat, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
	CALL GPUTR ( blon, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
	CALL GPUT ( ifrmat, 1, iret )
C
C*	If successful write, get output parameters.
C
	IF( iret .ne. NORMAL ) RETURN
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
