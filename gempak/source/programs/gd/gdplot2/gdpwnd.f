	SUBROUTINE GDPWND  ( wind, wintyp, iwnclr, iret )
C************************************************************************
C* GDPWND								*
C*									*
C* This subroutine decodes the input for WIND.  The variable has	*
C* several parts separated by a slashes.  The first part contains the	*
C* wind color number.  							*
C* The second part contains the size, width, type of the arrow or barb,	*
C* and the arrowhead size separated by slashes.  The arrow/barb size	*
C* is a multiple of the base size.  Type 1 plots a circle or an		*
C* arrowhead for calm winds.  Type 2 does not plot anything for calm	*
C* winds.  The arrowhead size is a multiple of the base arrowhead size. *
C*									*
C* An example of the wind string is: 6/1.0/5/2				*
C*									*
C* GDPWND  ( WIND, WINTYP, IWNCLR, IRET )				*
C*									*
C* Input parameters:							*
C*	WIND		CHAR*		Wind input			*
C*	WINTYP		CHAR*1		Wind type			*
C*					  B = wind barb 		*
C*					  A = wind arrow		*
C* Output parameters:							*
C*	IWNCLR		INTEGER		Wind color 			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84	Original source IP_WIND			*
C* M. desJardins/GSFC	 4/85	Changed default to MKS; changed code.	*
C* I. Graffman/RDS	 5/86	Renamed IN_WIND				*
C* M. desJardins/GSFC	 6/88	Added size				*
C* S. Schotz/GSC	 1/90	Added width				*
C* M. desJardins/GSFC	 5/90	Fixed problem with width but no size	*
C* S. Schotz/GSC	 5/90	Added type				*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* K. Brill/NMC		01/93	Return N as output in WINUNI		*
C* D.W.Plummer/NCEP	11/96	From IN_WIND, wintyp input, not output	*
C* D.W.Plummer/NCEP	 7/97	Bug fix stripping color from string	*
C* D.W.Plummer/NCEP	 5/98	Added call to set directional arrows	*
C* D.W.Plummer/NCEP	 2/99	Bug fix when input wind vrbl is blank	*
C************************************************************************
       INCLUDE         'GEMPRM.PRM'
C
	CHARACTER* (*)	wind, wintyp
C*
	CHARACTER	win*24, win2*24, w*1
	REAL		rwind (4)
C------------------------------------------------------------------------
	iret  = 0
C
C*	Check for size.
C
	ibreak = INDEX  ( wind, '/' )
	IF  ( ibreak .eq. 0 )  THEN
	    CALL ST_LSTR ( wind, lens, iret )
	    i = 1
	    w = wind (i:i)
	    DO WHILE ( i .le. lens .and.
     +			.not. (( w .ge. '0' ) .and. ( w .le. '9' )) )
		i = i + 1
	        w = wind (i:i)
	    END DO
	    win = wind (i:)
	    CALL ST_NUMB ( win, iwnclr, ier )
	    IF ( iwnclr .eq. IMISSD )  iwnclr = 1
	    size   = 0.
	    iwidth = 0
   	    itype = 0
            sizehd = 0.
	  ELSE
	    win2 = wind ( : ibreak-1 )
	    CALL ST_LSTR ( win2, lens, iret )
	    i = 1
	    w = win2 (i:i)
	    DO WHILE ( i .le. lens .and.
     +			.not. (( w .ge. '0' ) .and. ( w .le. '9' )) )
		i = i + 1
	        w = win2 (i:i)
	    END DO
	    win = win2 (i:)
	    CALL ST_NUMB ( win, iwnclr, ier )
	    IF ( iwnclr .eq. IMISSD )  iwnclr = 1
	    win2 = wind ( ibreak+1: )
	    CALL ST_RLST  ( win2, '/', 0., 4, rwind, n, ier )
	    size   = rwind (1)
	    iwidth = NINT ( rwind (2) )
	    itype  = NINT ( rwind (3) )
            sizehd = rwind (4)
	END IF
C
C*	Now, set size, width, type, and/or arrow head size
C
	IF  ( ( size .gt. 0. ) .or. ( iwidth .ne. 0 ) .or. 
     +        ( itype .ne. 0 ) .or. ( sizehd. gt. 0 ) )  THEN
C	    IF  ( wintyp .eq. 'B' )  THEN
		CALL GSBARB  ( size, iwidth, itype, ier )
C	      ELSE IF  ( wintyp .eq. 'A' )  THEN
		CALL GSARRW  ( size, sizehd, iwidth, itype, ier )
C	      ELSE IF  ( wintyp .eq. 'D' )  THEN
		CALL GSDARR  ( size, sizehd, iwidth, itype, ier )
C	    END IF
	END IF
C*
	RETURN
	END
