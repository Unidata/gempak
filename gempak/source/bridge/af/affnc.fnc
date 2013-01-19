C************************************************************************
C*									*
C*	This statement function returns the index of the last		*
C*	"like-type" group to scan based upon the index of the first     *
C*	"like-type" group to scan (i.e., isdx), the maximum number of	*
C*	"like-type" groups following isdx to scan (i.e., maxsc),	*
C*	and the total number of "like-type" groups (i.e., nflds).	*
C*									*
C************************************************************************
	IEDX ( isdx, maxsc, nflds ) =
     +		MIN0 ( ( isdx + maxsc ), nflds )
