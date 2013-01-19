	SUBROUTINE GSMFIL  ( mapfil, iret )
C************************************************************************
C* GSMFIL								*
C*									*
C* This subroutine sets the name of the map file used to draw maps.	*
C* If the map file cannot be found, the subroutine will check for	*
C* the file in GEMMAPS.  If the input file name includes directory	*
C* information and cannot be found, the subroutine will check for	*
C* the file name in the standard GEMPAK map file directory.  If         *
C* the file still cannot be found, the default map file will be used.	*
C*									*
C* The map files in GEMMAPS are named by concatenating the resolution,	*
C* map boundaries and area with the three letter source file type.	*
C* For example, the medium political world map from GSFC is called	*
C* MEPOWO.GSF.								*
C* 									*
C* 	RESOLUTION	BOUNDARIES	AREA		SOURCE		*
C*									*
C*	HIgh		POlitical	WOrld 		GSFc		*
C*	MEdium		COastline	NW quadrant	WISconsin	*
C*	LOw		REgional	NE quadrant			*
C*					SE quadrant			*
C*					SW quadrant			*
C*					US				*
C*					WE hemisphere			*
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
C* I. Graffman/RDS	 3/85	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 7/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 2/88	Added INQUIRE 				*
C* M. desJardins/GSFC	 3/89	Documentation				*
C* S. Schotz/GSC	 1/90	Account for older version of GP$MAPS	*
C*                              in MAPFIL				*
C* S. Schotz/GSC	 4/90	Call FL_INQR instead of INQUIRE		*
C* M. desJardins/GSFC	 4/90	Use default if name is bad		*
C* M. desJardins/GSFC	 7/90	Use SS_PATH to check for map in GEMMAPS	*
C* M. desJardins/NMC	 1/92	Replace SS_PATH with logical name	*
C* S. Jacobs/NMC         2/94   Added file name return to FL_INQR       *
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* S. Jacobs/NCEP	 8/96	Modified to save the user input for	*
C*				mapfil, w/o $GEMMAPS, to common		*
C* S. Maxwell/GSC	 1/97	Added check for no change in mapfil	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*) 	mapfil
C*
	CHARACTER	mpbuf*72, newfil*132
	LOGICAL		exist
C------------------------------------------------------------------------
	iret  =  NORMAL
C
C*	Check for no change in the map file name.
C
	IF ( mapfil .eq. mpfil ) RETURN
C
C*	Determine if map file exists. Look at file name in local
C*	and $GEMMAPS.
C
	mpbuf = mapfil
	CALL FL_INQR  ( mpbuf, exist, newfil, ier )
	IF  ( .not. exist )  THEN
C
C*          Look for file in $GEMMAPS.
C
	    mpbuf = '$GEMMAPS/' // mapfil
	    CALL FL_INQR  ( mpbuf, exist, newfil, ier )
	END IF
C
C*	If the map file is invalid, return to default.
C*	Save the new map file name in common XYDEF.CMN .
C
	IF  ( .not. exist )  THEN
	    iret  = NOMFIL
	    mpfil = cmpfil
	ELSE
	    mpfil = mapfil
	END IF
C*
	RETURN
	END
