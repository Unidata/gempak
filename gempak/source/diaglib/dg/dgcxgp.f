	SUBROUTINE DG_CXGP ( uipnts, ijskip, npmx, np, rgx, rgy, 
     +				       rlat, rlon, iret )
C************************************************************************
C* DG_CXGP                                                              *
C*                                                                      *
C* This subroutine translates the user input for a grid point or for	*
C* the end points of a cross-section line through a grid into actual	*
C* grid point(s), x and y coordinates, and latitude and longitude.  	*
C*                                                                      *
C* DG_CXGP  ( UIPNTS, IJSKIP, NPMX, NP, RGX, RGY, RLAT, RLON, IRET )    *
C*                                                                      *
C* Input parameters:                                                    *
C*      UIPNTS          CHAR*           User input for grid point       *
C*	IJSKIP		CHAR*		User input for grid skipping	*
C*	NPMX		INTEGER		Max allowed value for NP	*
C*                                                                      *
C* Output parameters:                                                   *
C*	NP		INTEGER		Number of output points		*
C*      RGX  (NPMX)     REAL            X grid point                    *
C*      RGY  (NPMX)     REAL            Y grid point                    *
C*      RLAT (NPMX)     REAL            Latitude                        *
C*      RLON (NPMX)     REAL            Longitude                       *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -45 = blank uipnts	        *
C*                                      -46 = invalid grid point	*
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC		 3/06	Fortran wrapper of DGC_CXGP		*	
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)   uipnts, ijskip
	REAL            rgx (*), rgy (*), rlat(*), rlon(*)
C*
	CHARACTER	tmpuip*(LLMXLN), tmpijs*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( uipnts, tmpuip, nt, ier )
	CALL ST_NULL ( ijskip, tmpijs, nt, ier )
	CALL DGC_CXGP ( tmpuip, tmpijs, npmx, np, rgx, rgy,
     +	                rlat, rlon, iret )
C*
	RETURN
	END
