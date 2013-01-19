	SUBROUTINE SFXSYM  ( iloc, parms, icolor, data, ntime, xval,
     +			     iret )
C************************************************************************
C* SFXSYM								*
C*									*
C* This subroutine plots symbols for SFGRAM.				*
C*									*
C* SFXSYM  ( ILOC, PARMS, ICOLOR, DATA, NTIME, XVAL, IRET )		*
C*									*
C* Input parameters:							*
C*	ILOC		INTEGER		Location on axis (1,2,3)	*
C*	PARMS		CHAR*		Parm*condition			*
C*	ICOLOR		INTEGER		Color				*
C*	DATA (NTIME)	REAL		Data				*
C*	NTIME		INTEGER		Number of times			*
C*	XVAL (NTIME)	REAL		Points on x axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/90						*
C* S. Jacobs/SSAI	 9/91	Added sky cover and cloud types		*
C* K. Brill/NMC		12/91	Use IN_ routines to set sizes & widths  *
C* S. Jacobs/EAI	 6/92	Fixed call to IN_WSYM to get condition	*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* D. Kidwell/NCEP	 9/02	Added TWSY and TSKC                     *
C* D. Kidwell/NCEP	 5/03	Added TPWS, AWSY, VWSY and WSKC         *
C* D. Kidwell/NCEP	10/04	Added TCSL                              *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms
	REAL		xval (*), data (*)
C*
	CHARACTER	condtn*8, ppp*4
	INTEGER		ixoff (LLMXTM), iyoff (LLMXTM)
	REAL		y (LLMXTM)
C*
	INCLUDE		'ERMISS.FNC'
	DATA		ixoff / LLMXTM * 0 /, iyoff / LLMXTM * 0 /
C*
C------------------------------------------------------------------------
	iret = 0
	npt  = 0
C
C*	Store the parameter name.
C
	ppp    = parms
C
C*	Set color.
C
	CALL GSCOLR  ( icolor, ier )
C
C*	Get value on y axis.
C
	CALL SFXLOC  ( iloc, 'C', yval, ier )
C
C*	Plot the weather symbols.
C
	IF  ( ppp .eq. 'WSYM' .or.
     +	      ppp .eq. 'TWSY' .or.
     +	      ppp .eq. 'TPWS' .or.
     +	      ppp .eq. 'AWSY' .or.
     +	      ppp .eq. 'VWSY' )  THEN
C
C*	    Set the symbol size and width.
C
C--	    CALL IN_WSYM ( ppp, condtn, ier )
	    CALL IN_WSYM ( parms, condtn, ier )
C
C*	    Loop through points plotting symbols.
C
	    DO  i = 1, ntime
		IF  ( ERMISS ( data (i) )  )  THEN
		    IF  ( npt .gt. 0 )  THEN
			CALL GWTHR  ( 'M', npt, data (ist), xval (ist),
     +				       y, ixoff, iyoff, ier )
			npt = 0
		    END IF
		  ELSE 
		    IF  ( npt .eq. 0 )  ist = i
		    npt = npt + 1
		    y (npt) = yval
		END IF
	    END DO
C
C*	    Flush the last points from the buffer.
C
	    IF  ( npt .ne. 0 )  THEN
	        CALL GWTHR  ( 'M', npt, data (ist), xval (ist), y, 
     +			      ixoff, iyoff, ier )
	    END IF
	END IF
C
C*	Plot sky cover
C
	IF  ( ppp .eq. 'SKYC' .or.
     +	      ppp .eq. 'TSKC' .or.
     +	      ppp .eq. 'WSKC' )  THEN
C
C*	    Set the symbol size and width.
C
	    CALL IN_SKYC ( parms, condtn, ier )
C
C*	    Loop through points plotting symbols.
C
	    DO  i = 1, ntime
		IF  ( ERMISS ( data (i) )  )  THEN
		    IF  ( npt .gt. 0 )  THEN
			CALL GSKY  ( 'M', npt, data (ist), xval (ist),
     +				       y, ixoff, iyoff, ier )
			npt = 0
		    END IF
		  ELSE 
		    IF  ( npt .eq. 0 )  ist = i
		    npt = npt + 1
		    y (npt) = yval
		END IF
	    END DO
C
C*	    Flush the last points from the buffer.
C
	    IF  ( npt .ne. 0 )  THEN
	        CALL GSKY  ( 'M', npt, data (ist), xval (ist), y, 
     +			      ixoff, iyoff, ier )
	    END IF
	END IF
C
C*	Plot cloud types
C
	IF  ( ( ppp .eq. 'CSYL' ) .or. ( ppp .eq. 'CSYM' ) .or.
     +	      ( ppp .eq. 'CSYH' ) .or. ( ppp .eq. 'CSYT' ) .or.
     +	      ( ppp .eq. 'TCSL' ) )  THEN
C
C*	    Set the symbol size and width.
C
	    CALL IN_CLDT ( parms, condtn, ier )
C
C*	    Loop through points plotting symbols.
C
	    DO  i = 1, ntime
		IF  ( ERMISS ( data (i) )  )  THEN
		    IF  ( npt .gt. 0 )  THEN
			CALL GCTYP  ( 'M', npt, data (ist), xval (ist),
     +				       y, ixoff, iyoff, ier )
			npt = 0
		    END IF
		  ELSE 
		    IF  ( npt .eq. 0 )  ist = i
		    npt = npt + 1
		    y (npt) = yval
		END IF
	    END DO
C
C*	    Flush the last points from the buffer.
C
	    IF  ( npt .ne. 0 )  THEN
	        CALL GCTYP  ( 'M', npt, data (ist), xval (ist), y, 
     +			      ixoff, iyoff, ier )
	    END IF
	END IF
C*
        IF  ( ppp .eq. 'PTND' .or. ppp .eq. 'PTSY' ) THEN
C
C*          Set the pressure tendency symbol size and width.
C
	    CALL IN_PTND ( parms, condtn, ier )
C
C*          Loop through points plotting symbols.
C
            DO  i = 1, ntime
                IF  ( ERMISS ( data (i) )  )  THEN
                    IF  ( npt .gt. 0 )  THEN
                        CALL GPTND  ( 'M', npt, data (ist), xval (ist),
     +                                 y, ixoff, iyoff, ier )
                        npt = 0
                    END IF
                  ELSE
                    IF  ( npt .eq. 0 )  ist = i
                    npt = npt + 1
                    y (npt) = yval
                END IF
            END DO
C
C*          Flush the last points from the buffer.
C
            IF  ( npt .ne. 0 )  THEN
                CALL GPTND  ( 'M', npt, data (ist), xval (ist), y,
     +                        ixoff, iyoff, ier )
            END IF
        END IF
C*
        IF  ( ppp .eq. 'PWTH' ) THEN
C
C*          Set the past weather symbol size and width.
C
	    CALL IN_PWTH ( parms, condtn, ier )
C
C*          Loop through points plotting symbols.
C
            DO  i = 1, ntime
                IF  ( ERMISS ( data (i) )  )  THEN
                    IF  ( npt .gt. 0 )  THEN
                        CALL GPWTH  ( 'M', npt, data (ist), xval (ist),
     +                                 y, ixoff, iyoff, ier )
                        npt = 0
                    END IF
                  ELSE
                    IF  ( npt .eq. 0 )  ist = i
                    npt = npt + 1
                    y (npt) = yval
                END IF
            END DO
C
C*          Flush the last points from the buffer.
C
            IF  ( npt .ne. 0 )  THEN
                CALL GPWTH  ( 'M', npt, data (ist), xval (ist), y,
     +                        ixoff, iyoff, ier )
            END IF
        END IF
C*
	RETURN
	END
