	SUBROUTINE DP_UNMC  ( idata, kxky, nbits, ref, scale,
     +			      grid, iret )
C************************************************************************
C* DP_UNMC								*
C*									*
C* This subroutine unpacks a grid in the NMC format.  The unpacking	*
C* equation is:								*
C*									*
C*	GRID   =  REF  +  IDATA * SCALE					*
C*									*
C* Each grid point must be packed into 16 bits which can be treated	*
C* as an INTEGER*2 word.  The scaling factor is a multiplier for the	*
C* data.  It must be set to  1 / 2 ** (15-N) where N is the exponent	*
C* with the original NMC grid.  This subroutine assumes there is no	*
C* missing data.							*
C*									*
C* DP_UNMC  ( IDATA, KXKY, NBITS, REF, SCALE, GRID, IRET )		*
C*									*
C* Input parameters:							*
C*	IDATA (KXKY)	INTEGER*2	Packed data			*
C*	KXKY		INTEGER		Number of grid points		*
C*	NBITS		INTEGER		Number of bits			*
C*	REF		REAL		Reference value of grid		*
C*	SCALE		REAL		Scaling factor			*
C*									*
C* Output parameters:							*
C*	GRID (KXKY)	REAL		Grid data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = NBITS invalid		*
C*					-12 = invalid scale		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* M. desJardins/GSFC	 2/91	Rewrite using INTEGER*2 words		*
C* K. Brill/NMC		 5/91   Added MV_SW42				*
C* S. Jacobs/EAI	 8/92	Added check for ULTRIX machine		*
C* S. Jacobs/EAI	10/93	Added check for ALPHA machine		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 2/01	Added check for LINUX machine		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (*)
	INTEGER*2	idata (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for valid input.
C
	IF  ( nbits .ne. 16 )  THEN
	    iret = -10
	    RETURN
	END IF
	IF  ( scale .eq. 0. )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	If the machine is a VAX, swap the 16-bit words.
C
	IF ( ( MTMACH .eq. MTVAX  ) .or. 
     +	     ( MTMACH .eq. MTIGPH ) .or.
     +	     ( MTMACH .eq. MTALPH ) .or.
     +	     ( MTMACH .eq. MTLNUX ) .or.
     +	     ( MTMACH .eq. MTULTX ) ) THEN
	    nwords = kxky / 2
	    IF ( MOD ( kxky, 2 ) .ne. 0 ) nwords = nwords + 1
	    ier = MV_SW42 ( nwords, idata, idata )
	END IF
C
C*	Loop through all the grid points.
C
	DO  i = 1, kxky
	    grid (i) = ref + idata (i) * scale
	END DO
C*
	RETURN
	END
