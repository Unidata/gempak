	SUBROUTINE SFXSKY  ( iloc, parms, icolor, data, ntime, 
     +			     xval, iret )
C************************************************************************
C* SFXSKY								*
C*									*
C* This subroutine plots combined sky coverage and wind barbs.		*
C*									*
C* SFXSKY  ( ILOC, PARMS, ICOLOR, DATA, NTIME, XVAL, IRET )		*
C*									*
C* Input parameters:							*
C*	ILOC		INTEGER		Location on axis (1,2,3)	*
C*	PARMS		CHAR*		Parm*condition			*
C*	ICOLOR		INTEGER		Color				*
C*	DATA (NTIME)	REAL		Sky & wind			*
C*	NTIME		INTEGER		Number of times			*
C*	XVAL (NTIME)	REAL		Points on x axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		12/91 						*
C* K. Brlll/NMC		04/92	Corrected plot after missing data	*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms
	REAL		xval (*), data ( * )
C*
	CHARACTER	condtn*8
	REAL		y (LLMXTM),    ddd (LLMXTM),
     +			sym (LLMXTM),  sss (LLMXTM)
C*
	INTEGER		ixoff ( LLMXTM ), iyoff ( LLMXTM )
C*
	INCLUDE		'ERMISS.FNC'
        DATA            ixoff / LLMXTM * 0 /, iyoff / LLMXTM * 0 /
C*
C------------------------------------------------------------------------
	iret = 0
	iwtyp = 0
C
C*	Set color.
C
	CALL GSCOLR  ( icolor, ier )
C
C*	Get value on y axis.
C
	CALL SFXLOC  ( iloc, 'W', yval, ier )
C
C*	Set barb size, width, type parameters.
C
	CALL IN_PWND ( parms, 'B', .true., condtn, ier )
C
C*	Loop through points plotting symbols and barbs.
C
	npt = 0
	ist = 0
	DO  i = 1, ntime
		IF  ( ERMISS ( data (i) ) ) THEN
		    IF  ( npt .gt. 0 )  THEN
		        CALL GBARB  ( 'M', npt, xval (ist), y,
     +			              sss, ddd, ier )
                        CALL GSKY  ( 'M', npt, sym, xval (ist),
     +                                 y, ixoff, iyoff, ier )
			npt = 0
		    END IF
		  ELSE
		    npt = npt + 1
		    y (npt) = yval
		    sss (npt) = data (i) / 10000.
		    ddd (npt) = MOD ( NINT ( data (i) ), 10000 ) / 10
		    sym (npt) = MOD ( NINT ( data (i) ), 10 )
		    IF ( npt .eq. 1 ) ist = i
		END IF
	END DO
C
C*	Plot last points.
C
	IF  ( npt .gt. 0 )  THEN
	    CALL GBARB  ( 'M', npt, xval (ist), y, sss, ddd, ier )
	    CALL GSKY   ( 'M', npt, sym, xval (ist), y,
     +                   ixoff, iyoff, ier )
	END IF
C*
	RETURN
	END
