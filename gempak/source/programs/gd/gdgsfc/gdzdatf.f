	SUBROUTINE GDZDATF ( iproc, ipos, timfnd, gdfile, glevel,
     +			    gvcord, gfunc, scale, sffile, sfp,
     +			    grid, isffln, iret ) 
C************************************************************************
C* GDZDATF                                                              * 
C*                                                                      *
C* This subroutine reads the grid data, interpolates to stations and    *
C* writes to output surface file.                                       *
C*                                                                      *
C* GDZDATF ( IPROC, IPOS, TIMFND, GDFILE, GLEVEL, GVCORD, GFUNC, SCALE, * 
C*           SFFILE, SFP, GRID, ISFFLN, IRET )                          *
C*                                                                      *
C* Input parameters:                                                    *
C*	IPROC		INTEGER		Process flag			*
C*	IPOS		INTEGER		Position of string in list      *
C*	TIMFND		CHAR*		Range of grid times		*
C*      GDFILE          CHAR*           Grid file                       *
C*	GLEVEL		CHAR*		Grid level			*
C*	GVCORD		CHAR*		Vertical coordinate		*
C*      GFUNC           CHAR*           Grid function                   *
C*	SCALE		CHAR*		Scaling factor			*
C*      SFFILE          CHAR*           Surface file                    *
C*      SFP             CHAR*           Surface parameter               *
C*	ISFFLN		INTEGER		Surface file number		*
C*                                                                      *
C* Work parameters:                                                     *
C*      GRID(*)         REAL            Work array for reading in grid  *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC		11/07						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	timfnd, gdfile, glevel, gvcord, gfunc, scale, 
     +			sffile, sfp
    	REAL		grid (*)
C*
	REAL		slatlon(2), rdata(MMPARM)
	INTEGER         level (2)
	CHARACTER	time (2)*20, parm*12, sstring*36,
     +			stid*8, tstr*48,
     +			pfunc*72, sdattim*20
	LOGICAL		proces, contin
C-----------------------------------------------------------------------
		iret = 0
		IF ( iproc .eq. 0 ) THEN
		    proces = .false.
		  ELSE
		    proces = .true.
		END IF
C
C*		Compute the grid data.
C
		IF  ( proces )  THEN
		    CALL DG_GRIDN  ( timfnd, glevel, gvcord, gfunc,
     +			            pfunc, grid, igx, igy, time,
     +				    level, ivcord, parm, iperr )
		    IF  ( iperr .ne. 0 )  THEN
		        CALL ER_WMSG  ( 'DG', iperr, pfunc, ier )
			proces = .false.
		    END IF
		END IF
C
C*		Define scaling.
C
		IF  ( proces )  THEN
		    CALL IN_SCAL  ( scale, iscals, iscalv, ier )
		    IF  ( ABS ( iscals ) .gt. 20 .or.
     +		          iscals .eq. IMISSD )  iscals = 0
		    sscale = 10. ** iscals
		END IF
C
C*		Convert the grid time to a valid time for use in 
C*		the surface file.
C
		IF  ( proces )  THEN
		    CALL TG_VALD  ( time(1), sstring, ier )
		    sdattim = sstring(1:11)
		    CALL SF_FTIM  ( isffln, sdattim, iperr )
C
C*		    If the time does not exist in the file, add it.
C
		    IF  ( iperr .ne. 0 )  THEN
		        CALL SF_ATIM  ( isffln, sdattim, iperr )
		        IF  ( iperr .ne. 0 )  THEN
		            CALL ER_WMSG  ( 'SF', iperr, sdattim, ier )
			    proces = .false.
		        END IF
		    END IF
C
C*		    Set the time for further surface data processing.
C
		    CALL SF_STIM ( isffln, sdattim, iperr )
		    IF  ( iperr .ne. 0 )  THEN
		        CALL ER_WMSG  ( 'SF', iperr, ' ', ier )
		        proces = .false.
		    END IF
		END IF
C
C*		Give user a chance to exit.
C
		IF  ( proces )  THEN
		    CALL GDZDSP ( gdfile, time, parm, ivcord,
     +                            level, iscals, sffile, sfp,
     +                            sdattim, iperr )
		    IF  ( iperr .ne. 0 )  proces = .false.
		END IF
C
C*		Loop over all stations.
C
		IF  ( proces )  THEN
		    iostat = 0
		    contin = .true.
		    DO WHILE ( iostat .eq. 0 .and. contin )
		        CALL SF_SNXT  ( isffln, stid, istnm, slat,
     +				        slon, selv, ispri, iostat )
			IF  ( iostat .eq. 0 .and. contin )  THEN
C
C*			    Read any current surface data.
C
			    CALL SF_RDAT  ( isffln, rdata, ihhmm, iret )
C
C*			    Interpolate the grid data to this station.
C
			    slatlon(1) = slat
			    slatlon(2) = slon
			    CALL ST_LSTF  ( slatlon, 2, ';', 2,
     +			                    tstr, ier )
			    CALL GR_PLOC  ( tstr, gx, gy,
     +				            rlat, rlon, ier )
			    IF  ( ier .eq. 0 )  
     +			        CALL GR_INTP  ( 1, gx, gy, 1,
     +						igx, igy, grid,
     +						sdint, ier )
C
C*			    Update the data array and write the data
C*			    to the surface file.
C
			    rdata(ipos) = sdint * sscale
			    CALL SF_WDAT  ( isffln, ihhmm, rdata, iret )
			    IF  ( iret .ne. 0 )  THEN
			        CALL ER_WMSG  ( 'SF', iret,
     +						' ', ier )
				contin = .false.
			    END IF
			END IF
		    END DO
		END IF
C*
		RETURN
		END
