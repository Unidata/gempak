	SUBROUTINE SNMPLT  ( icolr, parm, sx, sy, slat, slon,
     +			     chflg, ncprm, outd, chd, iret )
C************************************************************************
C* SNMPLT								*
C*									*
C* This subroutine plots data for one station.				*
C*									*
C* SNMPLT  ( ICOLR, PARM, SX, SY, SLAT, SLON, CHFLG, NCPRM,		*
C*	     OUTD, CHD, IRET )						*
C*									*
C* Input parameters:							*
C*	ICOLR (*)	INTEGER		Color array			*
C*	PARM (*)	CHAR*		Parameter name array		*
C*	SX		REAL		Station x coordinate		*
C*	SY		REAL		Station y coordinate		*
C*	SLAT		REAL		Station x coordinate (map coord)*
C*	SLON		REAL		Station y coordinate (map coord)*
C*	CHFLG (*)	LOGICAL		Character data flag		*
C*	NCPRM		INTEGER		Number of parameters		*
C*	OUTD (*)	REAL		Real station data		*
C*	CHD (*)		CHAR*		Character station data		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* I. Graffman/RDS	 6/85   GEMPLT Version 3.1 			*
C* M. desJardins/GSFC	 1/86	Changed x offsets to plot closer	*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* I. Graffman/RDS	12/86	Added positions 9 & 10			*
C* M. desJardins/GSFC	 6/88	Eliminated in-line encoding		*
C* S. Schotz/GSC	 8/90	Changed for plotting data at station,	*
C*				also removed scale			*
C* J. Whistler/SSAI	 7/91	Moved parm filter up into SNMAP		*
C* M. desJardins/NMC	10/91	Pass in P rather than M coords; cleanup	*
C* K. Brill/NMC		12/91	Changes for wind parms: removed IWNCLR  *
C*			        and WINTYP; add PARM input array; use	*
C*				IPOS not I in checking position		*
C* S. Jacobs/EAI         6/92   Fixed plotting of wind to be done in    *
C*                              map coordinates; added check for        *
C*                              missing winds                           *
C* S. Maxwell/GSC        3/97   Added call to GMARK                     *
C* S. Maxwell/GSC        3/97   Removed imark                           *
C* D. Kidwell/NCEP       5/98   Added call to GDARR, cleaned up         *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parm (*), chd (*)
	INTEGER		icolr (*)
	LOGICAL		chflg (*)
	REAL		outd  (*)
C*
	CHARACTER	chbuf*8
	INTEGER		ixof (10), iyof (10)
	INCLUDE		'ERMISS.FNC'
C*
	DATA		ixof  / 0, 0, 2, 2,  0,  2, 0,  0, 0,  0 /
	DATA		iyof  / 2, 0, 2, 0, -2, -2, 4, -4, 2, -2 /
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Plot each parameter.
C
	DO  i = 1, ncprm
C
C*	    Station position is one less than the element number.
C
	    ipos = i - 1
	    IF  ( icolr (i) .gt. 0 )  THEN
		CALL GSCOLR  ( icolr (i), ier )
		IF  ( .not. chflg (i) )  THEN
		    IF ( .not. ERMISS (outd (i) ) )  THEN
		        IF ( parm (i) (1:3) .eq. 'BRB' .or.
     +			     parm (i)       .eq. 'DARR' .or.
     +			     parm (i) (1:3) .eq. 'ARR' ) THEN
			     isiz = 1
			     chbuf = ' '
			  ELSE
			    intg = NINT ( outd (i) )
			    CALL ST_INLN  ( intg, chbuf, isiz, ier )
			END IF
		      ELSE IF ( parm (i) .eq. 'MARK' ) THEN
			chbuf = ' '
			isiz = 1
		      ELSE
			chbuf = ' '
			isiz  = 0
		    END IF
	          ELSE
		    chbuf = chd (i)
		    CALL ST_LDSP  ( chbuf, chbuf, isiz, ier )
	        END IF
C
C*		Set offsets for station positions and plot data 
C*              if string is not blank.
C
		IF  ( isiz .gt. 0 .and. chbuf .ne. ' ' )  THEN
C*
		    IF  ( ipos .eq. 0 )  THEN 
                        ix = -isiz + 1
                        iy = 0
C*
		      ELSE IF  ( ( ipos .eq. 1 ) .or. ( ipos .eq. 2 ) 
     +				 .or. ( ipos .eq. 5 ) )  THEN
	                ix = ixof (ipos) - (isiz * 2) 
                        iy = iyof (ipos)
C*
		      ELSE IF  ( ( ipos .eq. 3 ) .or. ( ipos .eq. 4 ) 
     +				 .or. ( ipos .eq. 6 ) )  THEN
                        ix = ixof ( ipos )
                        iy = iyof ( ipos )
C*
		      ELSE IF  ( ( ipos .eq. 7 ) .or. ( ipos .eq. 8 )
     +						 .or. 
     +				 ( ipos .eq. 9 ) .or. ( ipos .eq. 10 ) )
     +			   THEN
                        ix = -isiz + 1
                        iy = iyof ( ipos )
C*
	            END IF
C*
 	            CALL GTEXT  ( 'P', sx, sy, chbuf (1:isiz), 
     +				   0., ix, iy, iret )
C*
		  ELSE IF ( parm (i) (1:3) .eq. 'BRB' ) THEN
		    IF  ( .not. ERMISS ( outd (i) ) )  THEN
                    	spd = FLOAT ( INT ( outd (i) / 1000. ) )
                    	dir = FLOAT ( MOD ( INT ( outd (i) ), 1000 ))
                        CALL GBARB ( 'M', 1, slat, slon, spd, dir, 
     +                               iret )
		    END IF
		  ELSE IF ( parm (i) (1:3) .eq. 'ARR' ) THEN
		    IF  ( .not. ERMISS ( outd (i) ) )  THEN
                    	spd = FLOAT ( INT ( outd (i) / 1000. ) )
                    	dir = FLOAT ( MOD ( INT ( outd (i) ), 1000 ))
                        CALL GARRW ( 'M', 1, slat, slon, spd, dir, 
     +                               iret )
		    END IF
		  ELSE IF ( parm (i) .eq. 'DARR' ) THEN
		    IF  ( .not. ERMISS ( outd (i) ) )  THEN
                        CALL GDARR ( 'M', 1, slat, slon, outd (i), 
     +                               iret )
		    END IF
		  ELSE IF ( parm (i) .eq. 'MARK' ) THEN
		    CALL GMARK ( 'P', 1, sx, sy, iret )
	        END IF
	    END IF
	END DO
C*
	RETURN
	END
