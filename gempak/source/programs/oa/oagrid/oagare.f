	SUBROUTINE OAGARE  ( gridar, dataar, gltln, arenam, dltln,
     +			     datflg, iret )
C************************************************************************
C* OAGARE								*
C*									*
C* This subroutine translates the area parameters into latitude		*
C* and longitude ranges.						*
C*									*
C* OAGARE  ( GRIDAR, DATAAR, GLTLN, ARENAM, DLTLN, DATFLG, IRET )	*
C*									*
C* Input parameters:							*
C*	GRIDAR		CHAR*		Grid area name			*
C*	DATAAR		CHAR*		Data area name			*
C*									*
C* Input parameters:							*
C*	GLTLN (4)	REAL		Grid area lat/lon bounds	*
C*	ARENAM		CHAR*		Grid area name			*
C*	DLTLN (4)	REAL		Data area lat/lon bounds	*
C*	DATFLG		LOGICAL		Flag set if data area defined	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid area		*
C*					-14 = Old format geog. table	*
C*					-15 = Bad geog. table entry	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* G. Krueger/EAI	 6/96	Add default projection			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gridar, dataar, arenam
	LOGICAL		datflg
	REAL		gltln (*), dltln (*)
C*
	REAL		centrd (2)
	CHARACTER	cdproj*30
C------------------------------------------------------------------------
	iret   = 0
	datflg = .false.
C
C*	Translate the grid area.
C
	CALL LC_GARE  ( gridar, gltln, cdproj, centrd, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -3
	    CALL ER_WMSG  ( 'OAGRID', -3, gridar, ier )
	    RETURN
	  ELSE
	    ip = INDEX ( gridar, ';' )
	    IF  ( ip .eq. 0 )  THEN
		arenam = gridar
	      ELSE
		arenam = ' '
	    END IF
	END IF
C
C*	Translate the data area.
C
	IF  ( dataar .eq. ' ' )  RETURN
	CALL LC_GARE  ( dataar, dltln, cdproj, centrd, ier )
	IF  ( ier .ne. 0 )  THEN
	    ier = -4
	    CALL ER_WMSG  ( 'OAGRID', ier, dataar, ierr )
	    datflg = .false.
	ELSE
	    datflg = .true.
	END IF
C*
	RETURN
	END
                              
