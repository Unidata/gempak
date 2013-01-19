	SUBROUTINE AF_PLYR  ( cdtp, islyr, ielyr, nlyr, iret )
C************************************************************************
C* AF_PLYR								*
C*									*
C* This subroutine separates the turbulence, sky cover, or icing data	*
C* from a PIREP report into layers that can be individually decoded.	*
C* A layer of data is separated from the other layers by a "like-type"	*
C* group containing a "/" character.  The data must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output consists of arrays containing the indicees of the "like-type"	*
C* groups which contain the start and the end of the data for each of	*
C* the layers.								*
C*									*
C* AF_PLYR  ( CDTP, ISLYR, IELYR, NLYR, IRET )				*
C*									*
C* Input parameters:							*
C*	CDTP		CHAR*		Tag describing type of data	*
C*									*
C* Output parameters:							*
C*	ISLYR (NLYR)	INTEGER		Indicees of "like-type" groups	*
C*					marking start of each layer	*
C*	IELYR (NLYR)	INTEGER		Indicees of "like-type" groups	*
C*					marking end of each layer	*
C*	NLYR		INTEGER		Number of layers 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	INTEGER		islyr (*), ielyr (*)
	CHARACTER*(*)	cdtp
C*
	LOGICAL		slash
C-----------------------------------------------------------------------
	iret = 0
	nlyr = 1
	islyr ( nlyr ) = 1
C
	IF  ( nflds .gt. 2 )  THEN
	    DO ii = 2, ( nflds - 1 )
		IF  ( itypsf (ii) .eq. NALNMR )  THEN
C
C*		    Does this non-alphanumeric "like-type"  group
C*		    contain a "/" character?
C
		    slash = .false.
		    DO jj = 1, lensf (ii)
			IF  ( fields (ii) (jj:jj)  .eq. '/' )  THEN
			  slash = .true.
			END IF
		    END DO
		    IF  ( slash )  THEN
C
C*			This "like-type" group contains a "/" character,
C*			so it separates two layers of the data.
C
			ielyr ( nlyr ) = ii - 1
			IF  ( nlyr .lt. MXNLYR )  THEN
			  nlyr = nlyr + 1
			  islyr ( nlyr ) = ii + 1
			ELSE
			  WRITE ( UNIT = logmsg, FMT = '( I2, 3A )' )
     +			    MXNLYR, ' ', cdtp, ' layers'
			  CALL DC_WLOG  ( 2, 'AF', 3, logmsg, ierwlg )
			  RETURN
			END IF
		    END IF
		END IF
	    END DO
	END IF
C
	ielyr ( nlyr ) = nflds
C*
	RETURN
	END
