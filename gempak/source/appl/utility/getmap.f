	SUBROUTINE GETMAP ( maptyp, mxelts, mxpts,  nelts, npts, ielts,
     +			    xlats, ylons, iret )
C************************************************************************
C* GETMAP								*
C* 									*
C* This subroutine returns the latitude and longitude data points for a	*
C* map.  An index that points to the start of each map element is also	*
C* returned.  The map elements may be returned as lines or as polygons.	*
C* The map file to be used may be specified in GSMFIL.			*
C*									*
C* GETMAP  ( MAPTYP, MXELTS, MXPTS, NELTS, NPTS, IELTS, XLATS, YLONS,	*
C*	     IRET )							*
C* 									*
C* Input parameters:							*
C*      MAPTYP          INTEGER		Map type                        *
C*					  0 = lines                     *
C*					  1 = polygons                  *
C*	MXELTS		INTEGER		Size of IELTS array		*
C*	MXPTS		INTEGER		Size of XLATS and YLONS arrays	*
C*									*
C* Output parameters:							*
C*	NELTS		INTEGER		Number of map elements          *
C*	NPTS		INTEGER		Number of lats/lons             *
C*	IELTS (NELTS)	INTEGER		Index to map elements           *
C*	XLATS (NPTS)	REAL		Map latitudes                   *
C*	YLONS (NPTS)	REAL		Map longitudes                  *
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Krueger/EAI	7/97						*
C* A. Hardy/GSC         6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER		ielts (*)
	REAL		xlats (*), ylons (*)
C*
	INTEGER 	isend (5), irecv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 5
	isend (2) = FGTMAP
	isend (3) = maptyp
	isend (4) = mxelts
	isend (5) = mxpts
C
	CALL GPUT ( isend, 5, iret )
	IF( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .eq. NORMAL ) CALL GGET ( irecv, 2, ier )
	nelts = irecv (1)
	npts  = irecv (2)
	IF ( ier .eq. NORMAL ) CALL GGET ( ielts, nelts, ier )
	IF ( ier .eq. NORMAL ) CALL GGETR ( xlats, npts, ier )
	IF ( ier .eq. NORMAL ) CALL GGETR ( ylons, npts, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
