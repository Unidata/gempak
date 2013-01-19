#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

void user_func ( void );

/************************************************************************
 * user.c								*
 * 									*
 * This module contains user_func() function which links user testing	*
 * items to guitst module. Other functions can be added in this module	*
 *									*
 * CONTENTS:								*
 *									*
 *	user_func()	interface module between programmer and GUITST	*
 *									*
 ************************************************************************/
 
/*=====================================================================*/

void user_func ( void )
/************************************************************************
 * user_func								*
 * 									*
 * This functions serves as the interface between GUITST's basic	*
 * template program and programmer's testing modules. Its contents 	*
 * are composed by programmers according to their testing needs. 	*
 * guitst_AddUserItem() must be called below to add a testing item	*
 *									*
 * user_func ()								*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*	
 *									*
 ** Log:								*
 *									*
 *	S. Wang/GSC	10/97						*
 ************************************************************************/
{

/*	
	programmer defined contents which must include
	guitst_AddUserItem() such as
	...
	
	guitst_AddUserItem("DUMMY",create_dummy_func,load_dummy_func);

	...

*/
	return;
}
