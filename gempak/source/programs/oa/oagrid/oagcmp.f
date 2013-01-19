	SUBROUTINE OAGCMP  ( gltln, snfile, sffile, source, 
     +			     sfparm, snparm, dattim, levels, deltan, 
     +			     deltax, deltay, iret )
C************************************************************************
C* OAGCMP								*
C*									*
C* This area computes deltan, deltax, deltay from the station spacing	*
C* within the data set.							*
C*									*
C*  OAGCMP ( GLTLN, SNFILE, SFFILE, SOURCE, SFPARM, SNPARM, DATTIM,	*
C*	     LEVELS, DELTAN, DELTAX, DELTAY, IRET )			*
C*									*
C* Input parameters:       						*	
C*	GLTLN (4)	REAL		Grid area bounds		*
C*	SNFILE		CHAR*		Sounding file			*
C*	SFFILE		CHAR*		Surface file			*
C*	SOURCE		CHAR*		Data source [ SN or SF ]	*
C*	SFPARM		CHAR*		Surface parameter		*
C*	SNPARM		CHAR*		Sounding parameter		*
C*	DATTIM		CHAR*		Datetime			*
C*	LEVELS		CHAR*		Levels				*
C*									*
C* Output parameters:							*
C*	DELTAN		REAL		Deltan				*
C*	DELTAX		REAL		Deltax				*
C*	DELTAY		REAL		Deltay				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = invalid parameter		*
C*					 -7 = parm cannot be calculated	*
C*                                      -13 = invalid level		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Invalid level check; rlevel = 0 for SF	*\
C* S. Schotz/GSC	 5/90	Removed respnd flag			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		gltln (4)
	CHARACTER*(*)	snfile, sffile, source, sfparm, snparm, dattim, 
     +			levels
C*
	CHARACTER	filnam*72, sss*12, time*20, parm*4
	LOGICAL		chrflg, cmpflg
	REAL		slat (LLSTFL), slon (LLSTFL)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check which file is to be used.
C
	CALL ST_LCUC  ( source, sss, ier )
	IF  ( sss .eq. 'SN' )  THEN
	    filnam = snfile
	  ELSE IF  ( sss .eq. 'SF' )  THEN
	    filnam = sffile
	  ELSE
	    iret = -10
	    CALL ER_WMSG  ( 'OAGRID', iret, source, ier )
	    RETURN
	END IF
C
C*	If file name is not specified, return an error.
C
	IF  ( filnam .eq. ' ' )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'OAGRID', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Open the file.
C
	CALL OAGOPN  ( filnam, sss, dattim, iflno, time, ivert, 
     +		       iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Find the parameter to process.
C
	IF  ( sss .eq. 'SN' )  THEN
	    CALL IN_PARM  ( 1, snparm, parm, n, ier )
	  ELSE
	    CALL IN_PARM  ( 1, sfparm, parm, n, ier )
	END IF
C
C*	Write error message.
C
	IF  ( n .lt. 1 )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'OAGRID', iret, ' ', ier )
	    IF  ( sss .eq. 'SN' )  THEN
		CALL SN_CLOS  ( iflno, ier )
	      ELSE
		CALL SF_CLOS  ( iflno, ier )
	    END IF
	    RETURN
	END IF
C
C*	Set the computation of the requested parameter.
C
	CALL PC_DFLV  ( 1, parm, chrflg, cmpflg, n, iret )
	IF  ( ( iret .ne. 0 ) .or. ( .not. cmpflg ) )  THEN
	    iret = -7
	    CALL ER_WMSG ( 'OAGRID', iret, parm, ier )
	    IF  ( sss .eq. 'SN' )  THEN
		CALL SN_CLOS  ( iflno, ier )
	      ELSE
		CALL SF_CLOS  ( iflno, ier )
	    END IF
	    RETURN
	END IF
C
C*	Find the first level input and use it.  
C
	IF  ( sss .eq. 'SN' )  THEN
	    CALL ST_RLST  ( levels, ';', 0., 1, rlevel, n, ier )
	    IF ( ier .ne. 0 ) THEN
	      iret = -13
	      RETURN
	    END IF
	ELSE
	    rlevel = 0.0
	END IF
C
C*	Get the locations of the stations within bounds.
C
	CALL LC_SBND  ( gltln, ier )
	CALL OAGSTN   ( iflno, sss, rlevel, ivert, slat, slon, number,
     +			iret )
C
C*	Close the file before checking the return code.
C
	IF  ( sss .eq. 'SN' )  THEN
	    CALL SN_CLOS  ( iflno, ier )
	  ELSE
	    CALL SF_CLOS  ( iflno, ier )
	END IF
C
C*	Now check return code from OAGSTN.
C
	IF  ( iret .ne. 0 )  RETURN
C
C*	Compute the station spacings.
C
	CALL OAGSPC  ( gltln, slat, slon, number, dscomp, dsunif,
     +		       iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Update the common area with new values.
C
	CALL OAGWCM  ( filnam, sss, gltln, parm, rlevl, time,
     +		       dscomp, dsunif, number, deltan, deltax, deltay,
     +		       iret )
C*
	RETURN
	END
