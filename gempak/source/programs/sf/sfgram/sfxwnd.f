	SUBROUTINE SFXWND  ( iloc, parms, icolor, data, ntime, 
     +			     xval, iret )
C************************************************************************
C* SFXWND								*
C*									*
C* This subroutine plots wind symbols for SFGRAM.			*
C*									*
C* SFXWND  ( ILOC, PARMS, ICOLOR, DATA, NTIME, XVAL, IRET )		*
C*									*
C* Input parameters:							*
C*	ILOC		INTEGER		Location on axis (1,2,3)	*
C*	PARMS		CHAR*		Parm:size:width:type:hdsize	*
C*	ICOLOR		INTEGER		Color				*
C*	DATA (NTIME)	REAL		Wind				*
C*	NTIME		INTEGER		Number of times			*
C*	XVAL (NTIME)	REAL		Points on x axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/90						*
C* S. Schotz/GSC	 8/90	Added arrow head size to GSARRW,GQARRW	*
C* S. Jacobs/SUNYA	 5/91	Added choices for wind barbs		*
C*				(BRBM=m/s or BRBK=knts)			*
C* K. Brill/NMC		12/91	Added ARRM and ARRK; set arrow and barb *
C*				type to 122				*
C* K. Brill/NMC		12/91   Receive wind as combined spd & dir;	*
C*				Use IN_PWND				*
C* D. Kidwell/NCEP	 5/98   Added DAWV and call to GDARR            *
C* D. Kidwell/NCEP	 3/02   Added DASH                              *
C* D. Kidwell/NCEP	 9/02   Added BRGK                              *
C* D. Kidwell/NCEP	 4/05   Added BRPK and BRBS                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms
	REAL		xval (*), data ( * )
C*
	CHARACTER	condtn*8, ppp*4, wwtyp*1
	REAL		y (LLMXTM), ddd (LLMXTM), sss (LLMXTM)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	iwtyp = 0
C
C*	Get the parameter and size, width, type and head size for it.
C
	ppp    = parms
	wwtyp  = parms (1:1)
C*
	CALL IN_PWND ( parms, wwtyp, .false., condtn, ier )
C
C*      Set color.
C
        CALL GSCOLR  ( icolor, ier )
C
C*	Get value on y axis.
C
	CALL SFXLOC  ( iloc, 'W', yval, ier )
C
C*	Check for different types of wind symbols.
C
	IF  ( ( ppp .eq. 'BARB' ) .or.
     +	      ( ppp .eq. 'BRBK' ) .or.
     +	      ( ppp .eq. 'BRBM' ) .or.
     +	      ( ppp .eq. 'BRGK' ) .or.
     +	      ( ppp .eq. 'BRBS' ) .or.
     +	      ( ppp .eq. 'BRPK' ) ) THEN
            iwtyp = 1
	  ELSE IF ( ( ppp .eq. 'DARR' ) .or.
     +	            ( ppp .eq. 'DAWV' ) .or.
     +	            ( ppp .eq. 'DASH' ) ) THEN
	    iwtyp = 2
	END IF
C
C*	Loop through points plotting barbs or arrows.
C
	npt = 0
	ist = 0
	DO  i = 1, ntime
	    IF  ( ERMISS ( data (i) ) ) THEN
		IF  ( npt .gt. 0 )  THEN
		    IF ( iwtyp .eq. 1 ) THEN		
		        CALL GBARB  ( 'M', npt, xval (ist), y,
     +		          	      sss, ddd, ier )
		      ELSE IF ( iwtyp .eq. 0 ) THEN
		        CALL GARRW  ( 'M', npt, xval (ist), y,
     +				      sss, ddd, ier )
		      ELSE 
		        CALL GDARR  ( 'M', npt, xval (ist), y,
     +				      ddd, ier )
		    END IF
		    npt = 0
		END IF
	      ELSE
		npt = npt + 1
		y (npt) = yval
		sss (npt) = data (i) / 1000.
		ddd (npt) = MOD ( NINT ( data (i) ), 1000 )
	        IF ( npt .eq. 1 ) ist = i
	    END IF
	END DO
C
C*	Plot last points.
C
	IF  ( npt .gt. 0 )  THEN
	    IF ( iwtyp .eq. 1 ) THEN
	        CALL GBARB  ( 'M', npt, xval (ist), y, sss, ddd, ier )
	      ELSE IF ( iwtyp .eq. 0 ) THEN
	        CALL GARRW  ( 'M', npt, xval (ist), y, sss, ddd, ier )
	      ELSE
	        CALL GDARR  ( 'M', npt, xval (ist), y, ddd, ier )
	    END IF
	END IF
C*
	RETURN
	END
