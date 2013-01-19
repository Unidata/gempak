	SUBROUTINE GSGMAP  ( proj, kx, ky, dlatll, dlonll, dlatur,
     +			     dlonur, iret )
C************************************************************************
C* GSGMAP								*
C* 									*
C* This subroutine defines the coordinate system for a grid which is	*
C* evenly spaced in a simplified map projection.  It is valid for 	*
C* the following map projection types:					*
C*									*
C*        CED       Cylindrical equidistant				*
C*        MCD       Modified cylindrical equidistant			*
C*        MER       Mercator						*
C*        NPS       North polar stereographic				*
C*        SPS       South polar stereographic				*
C*        LCC       Lambert conic conformal ( Northern hemisphere )	*
C*        SCC       Lambert conic conformal ( Southern hemisphere )	*
C*        NOR       North polar orthographic 				*
C*        SOR       South polar orthographic 				*
C*									*
C* GSGMAP  ( PROJ, KX, KY, DLATLL, DLONLL, DLATUR, DLONUR, IRET )	*
C* 									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C* 									*
C* Output parameters:							*
C* 	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* T. Piper/GSC		 5/98	Changed type for PROJ in prolog         *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	CHARACTER*(*) 	proj
C*
	CHARACTER	cprj*4
	INTEGER 	isend (5)
	REAL 		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C*	Convert the character variable to an integer.
C
	cprj = proj
	CALL ST_STOI  ( cprj, 4, nv, iprj, ier )
	isend (1) = 9
	isend (2) = FSGMAP
	isend (3) = iprj
	isend (4) = kx
	isend (5) = ky
C
	rsend (1) = dlatll
	rsend (2) = dlonll
	rsend (3) = dlatur
	rsend (4) = dlonur
C
	CALL GPUT  ( isend, 5, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
