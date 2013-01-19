	SUBROUTINE GDASFC  ( gdatim, gfunc, gfun2, rlvl, nlev,
     +			     ngp, sfcp, havsp, fctr, gfsfc, iret )
C************************************************************************
C* GDASFC								*
C*									*
C* This subroutine computes lower bound GFUNC value from surface data 	*
C* or the data on the lowest level above the surface.  If GFUNC 	*
C* cannot be evaluated, GFUN2 is attempted.  If the function cannot	*
C* be evaluated at the surface, it is estimated from values on the	*
C* next level above the surface.					*
C*									*
C* GDASFC ( GDATIM, GFUNC, GFUN2, RLVL, NLEV, NGP, SFCP, HAVSP,	FCTR,	*
C*	    GFSFC, IRET )						*
C*									*
C* Input parameters:							*
C*	GDATIM		  CHAR*		User input date/time		*
C*	GFUNC		  CHAR*		Function to be evaluated	*
C*	GFUN2		  CHAR*		Alternate function		*
C*	RLVL (NLEV)	  REAL		Pressure levels			*
C*	NLEV		  INTEGER	Number of levels		*
C*	NGP		  INTEGER	Number of grid points		*
C*	SFCP(*)		  REAL		Surface pressure		*
C*	HAVSP		  LOGICAL	Flag for surface pressure	*
C*	FCTR		  REAL		Factor to multiply 1st level	*
C*					value when HAVSP is false	*
C*									*
C* Output parameters:							*
C*      GFSFC(*)	  REAL		Lower bound function values	*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      10/90						*
C* K. Brill/NMC      05/91	Generalized				*
C* K. Brill/NMC	     06/91	Added check for PALT in functions	*
C* M. desJardins/NMC  1/92	GDOSFC-->GDASFC				*
C* R. Tian/SAIC	      1/05	Removed iflno				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gfunc, gfun2
	REAL		sfcp (*), gfsfc (*), rlvl (*)
	LOGICAL		havsp
C*
	REAL		hold (LLMXGD)
	CHARACTER	time (2)*20, glevel*20, 
     +                  pfunc*80, parm*12, gvcord*12
C*
	INTEGER		lev (2)
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Set function values to missing everywhere.
C
	DO ij = 1, ngp
	    gfsfc (ij) = RMISSD
	END DO
C
C*	Compute the function.
C
	IF ( .not. havsp ) THEN
C
C*	    See if the first level data can be used to estimate the
C*	    function.
C
	    intlvl = INT ( rlvl (1) )
	    CALL ST_INCH  ( intlvl, glevel, ier )
	    gvcord = 'PRES'
	    CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +	 		    pfunc, gfsfc, igx, igy, time, lev, 
     +			    ivc, parm, iret )
            IF ( iret .ne. 0 ) THEN
		IF ( gfun2 .ne. ' ' ) 
     +	        CALL DG_GRID  ( gdatim, glevel, gvcord, gfun2, 
     +			        pfunc, gfsfc, igx, igy, time, lev, 
     +			        ivc, parm, iret )
	            IF ( iret .ne. 0 ) THEN
	                    DO ij = 1, ngp
                                gfsfc (ij) = 0.0
	                    END DO
		    ELSE
			    DO ij = 1, ngp
				IF ( .not. ERMISS ( gfsfc (ij) ) ) THEN
     			 	    gfsfc (ij) = fctr * gfsfc (ij)
			        ELSE
				    gfsfc (ij) = 0.0
				END IF
			    END DO			    
                    END IF
	    ELSE
	        DO ij = 1, ngp
	            IF ( .not. ERMISS ( gfsfc (ij) ) ) THEN
     	                gfsfc (ij) = fctr * gfsfc (ij)
		    ELSE
			gfsfc (ij) = 0.0
		    END IF
	        END DO			    
            END IF
C*
	ELSE
C
C*	    Surface pressure exists.
C
            glevel = '0'
            gvcord = 'NONE'
            CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +			    pfunc, gfsfc, igx, igy, time, lev, 
     +			    ivc, parm, iret )
	    IF ( iret .ne. 0 ) THEN
	        gvcord = 'PRES'
	        CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +		                pfunc, gfsfc, igx, igy, time, lev, 
     +			        ivc, parm, iret )
	        IF ( iret .ne. 0 ) THEN
		    IF ( gfun2 .ne. ' ' )
     +	            CALL DG_GRID  ( gdatim, glevel, gvcord, gfun2, 
     +		                    pfunc, gfsfc, igx, igy, time, lev, 
     +			            ivc, parm, iret )
		    IF ( iret .ne. 0 ) THEN
C
C*	                See if the first level data above the surface
C*			can be used to estimate the function. 
C
C*			First set function values to zero at all points
C*			where the surface pressure is missing.
C
			icnt = 0
			DO ij = 1, ngp
			    IF ( ERMISS ( sfcp (ij) ) ) THEN
     				gfsfc (ij) = 0.0
				icnt = icnt + 1
			    END IF
			END DO
C
C*			Check the functions and change all PALT's to
C*			PRES.
C
			ist01 = INDEX ( gfunc, 'PALT' )
			IF ( ist01 .ne. 0 ) THEN
			    iend = ist01 + 3
			    gfunc ( ist01:iend ) = 'PRES'
		   	    gfun2 ( ist01:iend ) = 'PRES'    
			END IF
C*				
			ilev = 1
			DO WHILE ( ilev .le. nlev .and. icnt .lt. ngp )
C*
			    pp = rlvl (ilev)
	                    intlvl = INT ( rlvl (ilev) )
	    		    CALL ST_INCH  ( intlvl, glevel, ier )
	    		    CALL DG_GRID  ( gdatim, glevel, gvcord,
     +					    gfunc, pfunc, hold, igx,
     +					    igy, time, lev, ivc, parm,
     +					    iret )
            		    IF ( iret .ne. 0 ) THEN
				IF ( gfun2 .ne. ' ' ) 
     +	        	        CALL DG_GRID  ( gdatim, glevel,	gvcord,
     +						gfun2, pfunc, hold, igx,
     +						igy, time, lev, ivc,
     +						parm, iret )
	            		IF ( iret .ne. 0 ) THEN
	                    	    DO ij = 1, ngp
				        IF ( ERMISS ( gfsfc (ij) )
     +					         .and. 
     + 				  	     sfcp (ij) .ge. pp ) THEN
					    icnt = icnt + 1
					    gfsfc (ij) = 0.0
					END IF	
				    END DO
				ELSE
			    	    DO ij = 1, ngp
				        IF ( ERMISS ( gfsfc (ij) )
     +					         .and. 
     + 				  	     sfcp (ij) .ge. pp ) THEN
					    IF ( .not.
     +				                 ERMISS ( hold (ij) )
     +					       ) THEN
					        icnt = icnt + 1
					        gfsfc (ij) = hold (ij)
     +								* .5
					    ELSE
						icnt = icnt + 1
						gfsfc (ij) = 0.0
					    END IF
					END IF	
				    END DO
				END IF
			    ELSE
				DO ij = 1, ngp
				    IF ( ERMISS ( gfsfc (ij) ) .and.
     +					 sfcp (ij) .ge. pp ) THEN	
					IF ( .not. ERMISS ( hold (ij) )
     +					   ) THEN
					    icnt = icnt + 1
					    gfsfc (ij) = hold (ij) * .5
					ELSE
					    icnt = icnt + 1
					    gfsfc (ij) = 0.0
				 	END IF
				    END IF
				END DO
			    END IF
			    ilev = ilev + 1
			END DO
		    END IF
		END IF
	    END IF
	END IF
C
C*	Set any missing function values to zero.
C
	DO ij = 1, ngp
	    IF (  ERMISS ( gfsfc (ij) ) ) gfsfc (ij) = 0.0
        END DO			 
C*
	RETURN
	END
