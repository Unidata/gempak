	SUBROUTINE GSMMAP  ( proj, dlatll, dlonll, dlatur, dlonur, 
     +			     iret )
C************************************************************************
C* GSMMAP								*
C*									*
C* This subroutine provides a simplified call for defining the map      *
C* projection and bounds used for plotting in map coordinates.  The  	*
C* angles necessary for defining these projections are based on the	*
C* specified bounds.  The following projections are valid:		*
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
C*        UTM       Universal transverse Mercator		        *
C*									*
C* If the projection name is DEF, the program checks to see that a	*
C* map coordinate system, which may be a map projection or satellite    *
C* overlay, has been defined previously.  The bounds specified in this	*
C* case are not used.   						*
C*									*
C* GSMMAP  ( PROJ, DLATLL, DLONLL, DLATUR, DLONUR, IRET )		*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection			*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/84	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	proj
C*
	CHARACTER	cprj*4
	INTEGER		isend (3)
	REAL		rsend (4)
C------------------------------------------------------------------------
	cprj = proj
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 7
	isend (2) = FSMMAP
	CALL ST_STOI  ( cprj, 4, nv, isend (3), ier )
	rsend (1)   = dlatll
	rsend (2)   = dlonll
	rsend (3)   = dlatur
	rsend (4)   = dlonur
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( rsend, 4, iret )
C
C*	Check status of write.  Get output parameters.
C
	IF  ( iret .eq. NORMAL )  THEN
	    CALL GGET  ( iret, 1, ier )
	    IF  ( ier .ne. NORMAL )  iret = ier
	END IF
C*
	RETURN
	END
