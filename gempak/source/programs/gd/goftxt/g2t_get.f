	SUBROUTINE G2T_GET ( ktype, nt, d1, d2, de1, de2, mx, mn, 
     +			     ex, id, iret )
C************************************************************************
C* G2T_GET								*
C*									*
C* This subroutine gets wind/wave data for trending.			*
C*									*
C* G2T_GET ( ktype, nt, d1, d2, de1, de2, mx, mn, ex, id, iret )	*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER			Data type		*
C*					 	 1 = Wave		*
C*					 	 2 = Wind		*
C*	NT		INTEGER			Nth time step		*
C*									*
C* Output parameters:							*
C*	D1(3)		CHAR*			Wind direction		*
C*	D2(3)		CHAR*			Wind direction		*
C*	DE1(3)		CHAR*			Direction for EXCEPTION	*
C*	DE2(3)		CHAR*			Direction for EXCEPTION	*
C*	MX(3)		INTEGER			Max wind		*
C*	MN(3)		INTEGER			Min wind		*
C*	EX(3)		INTEGER			Exception max wind	*
C*	ID(3)		CHAR*			Subarea ID		*
C*	IRET		INTEGER		  	Return code		*
C*					   	  0 = normal return	*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C* T. Lee/SAC		04/07	Get fsuba_d				*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	id(*), d1(*), d2(*), de1(*), de2(*)
	INTEGER		mx (*), mn (*), ex (*)
C-----------------------------------------------------------------------
	iret = 0
	nth = nt
C
	DO kk = 1, 3
	    IF ( ktype .eq. 2 )  THEN
		d1 ( kk ) = wdrmod ( 1, 1, nth )
		d2 ( kk ) = wdrmod ( 2, 1, nth )
	      ELSE
		d1 ( kk ) = ' '
		d2 ( kk ) = ' '
	    END IF
	    mx ( kk ) = mxval ( ktype, 1, nth )
	    mn ( kk ) = mnval ( ktype, 1, nth )
	    ex ( kk ) = IMISSD
	    id ( kk ) = ' '
	    IF ( fsuba ( ktype, nth ) )  THEN
	        ex ( kk ) = mxval_s ( ktype, nbnd ( ktype, nth ) , nth )
		id ( kk ) = subid ( ktype, nth )	
		IF ( ktype .eq. 2 )  THEN
		    de1 ( kk ) = wdrmod ( 1, nbnd ( ktype, nth ), nth )
		    de2 ( kk ) = wdrmod ( 2, nbnd ( ktype, nth ), nth )
		  ELSE
		    de1 ( kk ) = ' '
		    de2 ( kk ) = ' '
		END IF
	     ELSE
		ex ( kk ) = IMISSD
		id ( kk ) = ' '
		de1 ( kk ) = ' '
		de2 ( kk ) = ' '
	   END IF
	   nth = nth + 1
	END DO
C*
	RETURN
	END
