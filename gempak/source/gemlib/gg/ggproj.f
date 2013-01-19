	SUBROUTINE GG_PROJ  ( proj, cprj, angle, zmarg, angflg, iret )
C************************************************************************
C* GG_PROJ								*
C*									*
C* This subroutine decodes the user input for the parameter PROJ.	*
C* The input may contain parts separated with slashes.  The first	*
C* part must be the projection name.  Other parts may include:		*
C*									*
C*     NM           -  margins will be set to 0				*
C*     3 numbers    -  angles for a full map projection			*
C*     4 numbers    -  margins						*
C*									*
C* If angles are input, ANGFLG will be set to indicate that a full map	*
C* projection was specified.  If margins are not input and NM is also	*
C* not included in the string, default margins will be set.  The	*
C* default for map projections is (0,3,0,0) and for graphs is		*
C* (6,4,4,1).  A complete description of projections and margins can	*
C* be found in the GEMPLT Programmer's Guide.				*
C*									*
C* GG_PROJ  ( PROJ, CPRJ, ANGLE, ZMARG, ANGFLG, IRET )			*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Input projection string		*
C*									*
C* Output parameters:							*
C*	CPRJ		CHAR*		Projection name			*
C*	ANGLE (3)	REAL		Projection angles		*
C*	ZMARG (4)	REAL		Margins				*
C*	ANGFLG		LOGICAL		Angle flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/88						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* S. Jacobs/NMC	 2/95	Increased the size of ccc 24->72 chars	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	proj, cprj
	REAL		zmarg (*), angle (*)
	LOGICAL		angflg
C*
	CHARACTER 	charr (4)*20, ccc*72
	REAL 		rbuff (4)
C-------------------------------------------------------------------
C*	Break into strings and extract projection name.
C
	iret   = 0
	angflg = .false.
	CALL ST_LCUC  ( proj, ccc, ier )
	CALL ST_CLST  ( ccc, '/', ' ', 4, charr, num, ier )
	cprj   = charr (1)
C
C*	Set default margins for graph and map mode.
C
	IF  ( ( cprj .eq. 'LIN' ) .or. ( cprj .eq. 'LOG' ) .or.
     +	      ( cprj .eq. 'KAP' ) .or. ( cprj .eq. 'POL' ) )  THEN
	    zmarg (1) = 6.
	    zmarg (2) = 4.
	    zmarg (3) = 4.
	    zmarg (4) = 1.
	  ELSE
	    zmarg (1) = 0.
	    zmarg (2) = 3.
	    zmarg (3) = 0.
	    zmarg (4) = 0.
	END IF
C
C*	Parse remainder.
C
	DO  i = 2, num
C
C*	    Do nothing for blank.
C
	    IF  ( charr (i) .eq. ' ' )  THEN
C
C*		Set margins to zero for 'NM'.
C
	      ELSE IF  ( charr (i) .eq. 'NM' )  THEN
		zmarg (1) = 0.
		zmarg (2) = 0.
		zmarg (3) = 0.
		zmarg (4) = 0.
C
C*		Otherwise, see if string contains numbers.
C
	      ELSE 
		CALL ST_RLST  ( charr (i), ';', RMISSD, 4, rbuff, nnn,
     +				ier )
		IF  ( ier .ne. 0 )  nnn = 0
C
C*		Check for three numbers -- angles.
C
		IF  ( nnn .eq. 3 )  THEN
		    DO  j = 1, 3
			angle (j) = rbuff (j)
		    END DO
		    angflg = .true.
C
C*		    Check for four numbers -- - margins.
C
	          ELSE IF  ( nnn .eq. 4 )  THEN
		    DO  j = 1, 4
			zmarg (j) = rbuff (j)
		    END DO
		END IF
	    END IF
	END DO
C*
	RETURN
	END
