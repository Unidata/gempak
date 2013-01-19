	SUBROUTINE GDBDSP ( gdfile, timfnd, ltime, glevel, level,
     +			    gvcord, ivcord, gfunc, gvect, lovrly,
     +			    lfunct, lvectr, garea,	iret )
C************************************************************************
C* GDBDSP								*
C*									*
C* This subroutine allows the user to accept parameters for the GDPLOT	*
C* program.								*
C*									*
C* GDBDSP ( GDFILE, TIMFND, LTIME, GLEVEL, LEVEL, GVCORD, IVCORD,	*
C*	    GFUNC, GVECT, LOVRLY, LFUNCT, LVECTR, GAREA, IRET )		*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	TIMFND (200)	CHAR*		List of times to display	*
C*	LTIME		INTEGER		Last time in TIMFND		*
C*	GLEVEL		CHAR*		Grid levels string		*
C*	LEVEL (2)	INTEGER		Grid level			*
C*	GVCORD		CHAR*		Grid vertical coords string	*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	GFUNC		CHAR*		Grid parameters string		*
C*	GVECT		CHAR*		Vector names string		*
C*	LOVRLY		INTEGER		The number of plot overlays	*
C*	LFUNCT		INTEGER		The number of contour functions	*
C*	LVECTR		INTEGER		The number of vector functions	*
C*	GAREA		CHAR*		Graphics area			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* I. Graffman/RDS	 3/86	Respond flag fix, added min,max values	*
C* I. Graffman/RDS	 6/86	Added line width			*
C* I. Graffman/RDS	 3/88	Standard respond flag handling		*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* M. desJardins/NMC	12/91	Added contour fill			*
C* K. Brill/NMC		01/92	Increment I1 & I2 in fill loop		*
C* G. Krueger/EAI	05/93	Modified for multiple overlays - GDPLOT	*
C* G. Krueger/EAI	08/93	Removed some output & IP_RESP.		*
C* G. Krueger/EAI	10/93	Modified u-case & correct # of functs	*
C* P. Bruehl/Unidata	 8/94	Added "1" in front of X in formats	*
C************************************************************************
	INTEGER		level (2)
	CHARACTER*(*)	gdfile, timfnd (*), gfunc, gvect, glevel,
     +			gvcord, garea
	CHARACTER*30	eglev, egvcrd, egfunc, egvect
	CHARACTER	vcord*4
C-----------------------------------------------------------------------
	iret = 0
	ier  = 1
	WRITE ( 6, 1001 )
1001	FORMAT ( / ' PARAMETERS FOR GDPLOT: ' )
C
C*	Write out the grid file name.
C
	WRITE  ( 6, 1000 ) gdfile
1000	FORMAT ( / ' Grid file: ', A )
C
	WRITE ( *, 1020 ) timfnd (1)
1020	FORMAT ( 1X, 'TIMES:', 4X, A )
	DO  itime = 2, ltime
	    WRITE ( *, 1030 ) timfnd (itime)
1030	    FORMAT ( 11X, A )
	END DO
C
C*	Write out the grid identifier.
C
	WRITE ( 6, 2000 )
2000    FORMAT ( / ' GRID IDENTIFIER: ' )
C
C*	Translate vertical coordinate.
C
	CALL LV_CCRD ( ivcord, vcord, ier )
	IF ( lovrly .eq. 1 ) THEN
	    WRITE ( *, 2010 ) 'LEVEL', 'VCORD', 'PARM', 'WPARM'
	    CALL ST_PELT ( gfunc, 1, ' ', nscal, egfunc, iret )
	    CALL ST_LCUC ( egfunc, egfunc, iret )
	    CALL ST_PELT ( gvect, 1, ' ', nscal, egvect, iret )
	    CALL ST_LCUC ( egvect, egvect, iret )
	    WRITE ( *, 2030 ) level (1), vcord, egfunc, egvect
	ELSE
	    WRITE ( *, 2020 ) 'PLOT', 'LEVEL', 'VCORD', 'PARM', 'WPARM'
	    DO ifunct = 1, lovrly
		CALL ST_PELT ( glevel, ifunct, ' ', nscal, eglev,
     +			       iret )
		CALL ST_LCUC ( eglev, eglev, iret )
		CALL ST_PELT ( gvcord, ifunct, ' ', nscal, egvcrd,
     +			       iret )
		CALL ST_LCUC ( egvcrd, egvcrd, iret )
		IF ( ifunct .le. lfunct ) THEN
		    CALL ST_PELT ( gfunc, ifunct, ' ', nscal, egfunc,
     +				   iret )
		    CALL ST_LCUC ( egfunc, egfunc, iret )
		ELSE
		    egfunc = ' '
		ENDIF
		IF ( ifunct .le. lvectr ) THEN
		    CALL ST_PELT ( gvect, ifunct, ' ', nscal, egvect,
     +				   iret )
		    CALL ST_LCUC ( egvect, egvect, iret )
		ELSE
		    egvect = ' '
		ENDIF
		WRITE ( *, 2040 ) ifunct, eglev, egvcrd, egfunc, egvect
	    END DO
	ENDIF
2010	FORMAT ( 4X, A, 1X, A, 1X, A, T49, A )
2020	FORMAT ( 4X, A, 1X, A, 1X, A, 1X, A, T49, A )
2030	FORMAT ( 1X, I8, 2X, A4, 1X, A27, T49, A27 )
2040	FORMAT ( 1X, I7, 1X, A5, 1X, A5, 1X, A27, T49, A27 )
C
C*	Write out the graphics area and scaling factor.
C
	WRITE  ( 6, 3000 ) 'GAREA:  ', garea
3000	FORMAT ( / 1X, A, A )
C*
	RETURN
	END
