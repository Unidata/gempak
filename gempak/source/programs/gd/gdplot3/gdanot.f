        SUBROUTINE GDANOT ( shape, info, loci, line, type, iret )
C************************************************************************
C* GDANOT                                                               *
C*                                                                      *
C* This subroutine acts as a driver which allows the user to place an 	*
C* object at any location on graphics device.				*
C*                                                                      *
C* Input parameters:							*
C*	SHAPE		CHAR*		Name of the object		*
C*	INFO		CHAR*		Information defines the object	*
C*	LOCI		CHAR*		Points to place the object	*
C*	LINE		CHAR*		Line attributes			*
C*	TYPE		CHAR*		Object fill or no-fill		*
C*                                                                      *
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +3 = invalid coordinate	*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* S. Jacobs/SSAI       12/91                                           *
C* T. Lee/SAIC		01/06	Rewritten for GDPLOT3			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	CHARACTER*(*)	shape, info, loci, line, type
C*
        LOGICAL         lflag, sflag, nflag, bflag
        LOGICAL         fflag
C------------------------------------------------------------------------
C
C*	Return if grid coordinate.
C
	IF  ( loci (1:1) .eq. '@' )  THEN
	    iret = +3
	    CALL ER_WMSG ( 'gdplot3', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Query current settings and set defaults.
C
	CALL GQFILL ( szfil, jftyp, ier )
	CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GQTEXT ( jtxfn, jtxhw, sztext, jtxwid, jbrdr, jrrotn, 
     +		      jjust, ier )
	CALL GSTEXT ( 1, 1, 1., 1, 111, 1, 1, ier )
C
C*	Choose a section based on SHAPE.
C
	CALL ST_LCUC ( shape, shape, ier )
C
C*	Set annotation fill type.
C
	CALL IN_CTYP  ( type, nflag, lflag, sflag, bflag,  fflag, ier )
        IF ( ( shape .eq. 'ARC' ) .or.  ( shape .eq. 'SPARC' ) ) THEN
	    ictyp = 3
	    IF ( fflag ) THEN
		ictyp = 2
	    END IF
	    IF ( bflag ) THEN
		ictyp = 1
	    END IF
	  ELSE
	    IF ( fflag ) THEN
		ictyp = 2
	      ELSE
		ictyp = 1
	    END IF
	END IF
	IF  ( shape .eq. 'TEXT' )  THEN
	    CALL GDNTXT ( ictyp, line, loci, info, 1, ier )
	  ELSE IF  ( shape .eq. 'TXSY' )  THEN
	    CALL GDNTXT ( ictyp, line, loci, info, 2, ier )
	  ELSE IF  ( shape .eq. 'POLYGON' )  THEN
	    CALL GDNPLY ( ictyp, line, loci, info, ier )
	  ELSE IF  ( shape .eq. 'REGPOLY' )  THEN
	    CALL GDNREG ( ictyp, line, loci, info, ier )
	  ELSE IF  ( shape .eq. 'ARC' )  THEN
	    CALL GDNARC ( ictyp, line, loci, info, 1, ier )
	  ELSE IF  ( shape .eq. 'SPARC' )  THEN
	    CALL GDNARC ( ictyp, line, loci, info, 2, ier )
	  ELSE IF  ( shape .eq. 'CURVE' )  THEN
	    CALL GDNCRV ( ictyp, line, loci, info, ier )
	  ELSE IF  ( shape .eq. 'SPLN' )  THEN
	    CALL GDNLIN ( ictyp, line, loci, info, 2, ier )
	  ELSE IF  ( shape .eq. 'LINE' )  THEN
	    CALL GDNLIN ( ictyp, line, loci, info, 1, ier )
	  ELSE IF  ( shape .eq. 'ARROW' )  THEN
	    CALL GDNARW ( ictyp, line, loci, info, ier )
	  ELSE IF  ( shape .eq. 'BARB' )  THEN
	    CALL GDNBRB ( ictyp, line, loci, info, ier )
	  ELSE IF  ( shape .eq. 'COLDFRONT' )  THEN
	    CALL GDNFNT ( ictyp, line, loci, info, 1, ier )
	  ELSE IF  ( shape .eq. 'WARMFRONT' )  THEN
	    CALL GDNFNT ( ictyp, line, loci, info, 2, ier )
	  ELSE IF  ( shape .eq. 'OCCLFRONT' )  THEN
	    CALL GDNFNT ( ictyp, line, loci, info, 4, ier )
	  ELSE IF  ( shape .eq. 'STATFRONT' )  THEN
	    CALL GDNFNT ( ictyp, line, loci, info, 3, ier )
	  ELSE IF  ( shape .eq. 'WEATHER' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 1, ier )
	  ELSE IF  ( shape .eq. 'CLOUD' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 2, ier )
	  ELSE IF  ( shape .eq. 'SKY' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 3, ier )
	  ELSE IF  ( shape .eq. 'MARKER' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 4, ier )
	  ELSE IF  ( shape .eq. 'PRESTEND' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 5, ier )
	  ELSE IF  ( shape .eq. 'PASTWTHR' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 6, ier )
	  ELSE IF  ( shape .eq. 'TURB' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 7, ier )
	  ELSE IF  ( shape .eq. 'ICNG' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 8, ier )
	  ELSE IF  ( shape .eq. 'SPCL' )  THEN
	    CALL GDNSYM ( ictyp, line, loci, info, 9, ier )
	END IF
C
C*	Reset line, fill and text attributes.
C
	CALL GSFILL ( szfil, jftyp, ier )
	CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GSTEXT ( jtxfn, jtxhw, sztext, jtxwid, jbrdr, jrrotn, 
     +		      jjust, ier )
	
C*
        END
