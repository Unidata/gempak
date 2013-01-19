	SUBROUTINE GDXGTSF ( time, ivcord, rgx, rgy, nhxs, grid,
     +			     vclsfc, havsfc, parm, iret )
C************************************************************************
C* GDXGTSF								*
C*									*
C* This subroutine gets the surface data for a cross section.		*
C*									*
C* GDXGTSF ( TIME, IVCORD, RGX, RGY, NHXS, GRID, VCLSFC, HAVSFC, 	*
C*           PARM, IRET )						*
C*									*
C* Input parameters:							*
C*	TIME  (2)	  CHAR*		Time to search for levels	*
C*	IVCORD		  INTEGER	Vertical coordinate for search	*
C*	RGX  (NHXS)	  REAL		X grid coordinates		*
C*	RGY  (NHXS)	  REAL		Y grid coordinates		*
C*	NHXS		  INTEGER	Number of xsect pts in horiz.	*
C*									*
C* Work parameters:							*
C*      GRID (*) 	  REAL 		Work array for reading in grid  *
C*									*
C* Output parameters:							*
C*      VCLSFC (NHXS)     REAL 		Vert coord location of sfc	*
C*      HAVSFC            LOGICAL       Flag for existence of sfc	*
C*	PARM		  CHAR*		Parameter name			*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					 -6 = GVCORD is invalid		*
C*					 +2 = no sfc value found	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89   					*
C* K. Brill/GSC         11/89   Append +# to parm in DG_GRID		*
C* S. Schotz/GSC	 6/90	Removed respnd flag			*
C* K. Brill/NMC         10/90   Change -12 to +2 error			*
C* K. Brill/NMC		01/91	Look for sfc when vc = NONE		*
C*				Remove GVCORD from call			*
C* K. Brill/NMC		03/92	Set HAVSFC=.false. first		*
C* S. Jacobs/NMC	10/94	Rewrote to simplify the process		*
C* S. Jacobs/NCEP	11/01	Changed DGCMN.CMN to GEMPRM.PRM		*
C* K. Brill/HPC		 5/02	Allow cross section path to cross grid	*
C*				discontinuity.				*
C* R. Tian/SAIC         10/02   Remove COMMON/GDXS, GR_RARG, GSGPRJ     *
C* K. Brill/HPC		 2/04	Changes for new DG			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	time ( 2 ), parm
        REAL		rgx ( * ), rgy ( * ), vclsfc ( * )
	LOGICAL		havsfc
C*
	CHARACTER	gdtm*36, glev*4, gvcd*4, pfunc*72, dattim(2)*20
	CHARACTER	prm*12
	REAL		grid ( * )
	INTEGER		level ( 2 )
C------------------------------------------------------------------------
	iret = 0
	havsfc = .false.
C
C*	Set the level and parameter for which to search.
C
	CALL LV_CCRD  ( ivcord, parm, ier )
C
C*  	Make character string grid identifiers for DG_GRID.
C
	glev='0'
	CALL TG_DUAL ( time, gdtm, ier )
C
C*	Try to read the surface data on IVCORD.
C
	CALL DG_GRIDN ( gdtm, glev, parm, parm, pfunc, grid, igx, igy,
     +		       dattim, level, jvcord, prm, ier )
	IF  ( ier .eq. 0 )  THEN
	    havsfc = .true.
	ELSE
C
C*	    Try to read the surface data on JVCORD = 0.
C
	    gvcd = 'NONE'
	    CALL DG_GRIDN ( gdtm, glev, gvcd, parm, pfunc, grid,
     +			   igx, igy, dattim, level, jvcord, prm, ier )
	    IF  ( ier .eq. 0 )  THEN
		havsfc = .true.
	    END IF
	END IF
C
C*      Interpolate the grid to the cross section line.
C
	IF  ( havsfc )  THEN
C
C*	    Set interpolation type.
C
	    inttyp = 1
C
C*	    Iterpolate the data.
C
	    CALL GR_INTP  ( inttyp, rgx, rgy, nhxs, igx, igy,
     +			    grid, vclsfc, ierr )
C
C*	    If there is an error, set the surface values to missing.
C
	    IF  ( ierr .ne. 0 )  THEN
		DO ii = 1, nhxs
		    vclsfc ( ii ) = RMISSD
		END DO
		havsfc = .false.
	    END IF
	END IF
C*
	RETURN
	END
