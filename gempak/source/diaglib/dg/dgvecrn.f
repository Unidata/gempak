	SUBROUTINE DG_VECRN ( gdattm, glevel, gvcord, gvect, pfunc, 
     +			     ugrid, vgrid, igx, igy, time, level, 
     +			     ivcord, parmu, parmv, iret )
C************************************************************************
C* DG_VECRN								*
C*									*
C* This subroutine computes a grid diagnostic vector quantity.  The 	*
C* u and v components returned in UGRID and VGRID are in grid relative	*
C* coordinates.  GDATTM, GLEVEL, GVCORD and GVECT should have the	*
C* values entered by the user.						*
C*									*
C* DG_VECRN  ( GDATTM, GLEVEL, GVCORD, GVECT, PFUNC, UGRID,		*
C*            VGRID, IGX, IGY, TIME, LEVEL, IVCORD, PARMU, PARMV,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	GDATTM 		CHAR*		Input date/time			*
C*	GLEVEL		CHAR*		Input level			*
C*	GVCORD		CHAR*		Input vertical coordinate	*
C*	GVECT		CHAR*		Diagnostic function		*
C*									*
C* Output parameters:							*
C*	PFUNC		CHAR*		Diagnostic error string		*
C*	UGRID (IGX,IGY)	REAL		Output u component grid		*
C*	VGRID (IGX,IGY)	REAL		Output v component grid		*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	TIME  (2)	CHAR*		Output date/time		*
C*	LEVEL (2)	INTEGER		Output level 			*
C*	IVCORD		INTEGER		Output vertical coordinate	*
C*	PARMU		CHAR*		Parameter name for u component 	*
C*	PARMV		CHAR*		Parameter name for v component 	*
C*	IRET		INTEGER		Return code			*
C*				  	  3 = user typed EXIT		*
C*					  0 = normal return		*
C*					 -3 = parsing table is empty	*
C*					 -5 = output grid not a vector	*
C*					 -6 = wrong number of operands	*
C*					 -7 = grid cannot be found	*
C*					 -8 = grid is the wrong size	*
C*					 -9 = incorrect operands	*
C*					-10 = internal grid list full	*
C*					-11 = operand must be a vector	*
C*					-12 = operand must be a scalar	*
C*					-13 = operand must be from file	*
C*					-14 = DG_INIT not initialized	*
C*					-15 = polar grid cent. not valid*
C*					-16 = map proj is invalid	*
C*					-17 = LEVEL must be a layer	*
C*					-18 = TIME must be a range	*
C*					-19 = invalid operator		*
C*					-20 = stack is full		*
C*					-21 = stack is empty		*
C*					-22 = TIME is invalid		*
C*					-23 = LEVEL is invalid		*
C*					-24 = IVCORD is invalid		*
C*					-26 = layer of layers invalid	*
C*					-27 = time range layer invalid	*
C*					-47 = internal grid is too big	*
C*					-70 = cannot computer ensemble	*
C*					-71 = cannot computer layer	*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_VECR             *
C* S. Gilbert/NCEP       7/07   Renamed from DG_VECR			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdattm, glevel, gvcord, gvect, pfunc
	CHARACTER*(*)	time (2), parmu, parmv
	REAL		ugrid (*), vgrid (*)
	INTEGER		level (*)
C*
        CHARACTER       tmpgda*(LLMXLN), tmpgle*(LLMXLN),
     +                  tmpgvc*(LLMXLN), tmpgve*(LLMXLN)
C------------------------------------------------------------------------
        CALL ST_NULL ( gdattm, tmpgda, nt, ier )
        CALL ST_NULL ( glevel, tmpgle, nt, ier )
        CALL ST_NULL ( gvcord, tmpgvc, nt, ier )
        CALL ST_NULL ( gvect,  tmpgve, nt, ier )
	CALL DGC_VECR ( tmpgda, tmpgle, tmpgvc, tmpgve, pfunc,
     +			ugrid, vgrid, igx, igy, time (1), time (2),
     +                  level (1), level (2), ivcord, parmu, parmv,
     +                  iret )
        CALL ST_RNUL ( pfunc, pfunc, nt, ier )
        CALL ST_RNUL ( time (1), time (1), nt, ier )
        CALL ST_RNUL ( time (2), time (2), nt, ier )
        CALL ST_RNUL ( parmu, parmu, nt, ier )
        CALL ST_RNUL ( parmv, parmv, nt, ier )
C*
	RETURN
	END
