/************************************************************************
 * sleep.h								*
 *									*
 * This header file determines whether to implement the nanosecond	*
 * sleep function wrapper (cnsleep) or the system nanosleep function.	*
 *									*
 **									*
 * Log:									*
 * R. McTaggart-Cowan/SUNY	01/05	Initial implementation		*
 ***********************************************************************/
#ifdef NO_NANOSLEEP

#define cnsleep		nanosleep

#endif
