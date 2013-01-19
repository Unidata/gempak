	SUBROUTINE GDDDEL  ( iflinp, ngrid, gdfile, gdattim, glevel,
     +                       gvcord, gfunc, iret )
C************************************************************************
C* GDDDEL								*
C*									*
C* This subroutine deletes the grids in a grid file for the GDDELT      *
C* program.								*
C*									*
C* GDDDEL  ( IFLINP, NGRID, GDFILE, GDATTIM, GLEVEL, GVCORD, GFUNC,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	IFLINP		INTEGER		Input grid file number		*
C*	NGRID		INTEGER		Number of grids in file		*
C*	GDFILE		CHAR*		Grid file			*
C*	GDATTIM		CHAR*           Grid date/time                  *
C*      GLEVEL          CHAR*           Grid levels                     *
C*      GVCORD          CHAR*           Grid vertical coordinate        *
C*      GFUNC           CHAR*           Grid parameter name             *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = error reading input grid  *
C*					 -8 = error writing input grid	*
C*					-12 = cannot parse user input   *
C*					-13 = cannot match user input   *
C**									*
C* Log:									*
C* S. Maxwell/GSC	10/96	Based on GDOADD				*
C* S. Jacobs/NCEP	11/96	Added a warning if no grids were found	*
C* S. Jacobs/NCEP	12/96	Changed GDMOD to GDDELT in error msg	*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* R. Tian/SAIC		 3/05	Change calling seq, removed GDU_GINP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	glevel, gvcord, gfunc, gdattim, gdfile
C*
	CHARACTER	parm*12, gdattm (2)*20, vparm*12, trange*36,
     +			prmlst ( MMPARM )*12, timfnd (LLMXGT)*36
	REAL		rlevel ( LLMXLV, 2 )
	INTEGER		level (2)
        LOGICAL         match
C------------------------------------------------------------------------
	iret = 0
C
C*      Parse user input for levels vertical coordinate, parameters,
C*	and times. 
C
	CALL GDU_GLEV ( glevel, LLMXLV, gvcord, nlev, rlevel,
     +                  levtyp, vparm, icord, ier)
        CALL ST_LCUC ( gfunc, gfunc, ier )
        CALL ST_CLST ( gfunc, ';', ' ', MMPARM , prmlst, nparm, ier )
	CALL ST_LCUC ( gdattim, gdattim, ier )
	IF ( gdattim .eq. 'ALL' ) THEN
	    ntime = 1
            timfnd (1) = gdattim
          ELSE
            CALL DG_NFIL ( gdfile, gdfile, ier )
            CALL DG_NDTM ( gdattim, ier )
            CALL DG_QTMS ( LLMXGT, .true., timfnd, ntime, trange, ier )
	END IF
C
C*	Loop through all grids to find matches.
C
	knt = 0
        DO  nbeg = ngrid, 1, -1 
C
	    CALL GD_GIDN  ( iflinp, nbeg, gdattm, level, ivcord, parm,
     +	 		    ier )
C
	    CALL GDU_MTCH ( LLMXLV, gdattm, level, ivcord, parm, levtyp,
     +                      nlev, rlevel, icord, nparm, prmlst, ntime,
     +	  	            timfnd, match, iret )
C
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'GDU', iret, ' ', ier )
		iret = -13
		RETURN
	    END IF 
C
	    IF  ( match )  THEN
		knt = knt + 1
C
C*          	Write information to the screen. 
C
		WRITE ( 6, 1000 )
1000            FORMAT ( / '      Deleting grid: ' )
		CALL GR_WTRM  ( 6, .false., nbeg, gdattm, level, ivcord, 
     +	                        parm, ier )
C
C*	        Read grid from input file. 
C
		CALL GD_DGRD  ( iflinp, gdattm, level, ivcord, parm, 
     +                          ier )
		IF  ( ier .ne. 0 )  THEN
                    iret = -7
                    CALL ER_WMSG  ( 'GDDELT', iret, ' ', ier )
		END IF
	    END IF
        END DO
C
C*	If no grids were found, write a warning message.
C
	IF  ( knt .eq. 0 )  THEN
	    CALL ER_WMSG ( 'GDU', 2, ' ', ierr )
	END IF
C*
	RETURN
	END
