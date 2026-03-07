#include <stdio.h>
#include <stdlib.h>
#include "readline/readline.h"
#include "readline/history.h"

/* A static variable for holding the line. */
static char *line_read = (char *)NULL;

void rl_get (char* prompt, char** line)
/**************************************************************
 * rl_get                                                     *
 *                                                            *
 * This function reads a string using the GNU Readline        *
 * library, and returns a pointer to it.  Returns NULL on     *
 * EOF.                                                       *
 *                                                            *
 * void rl_get ( prompt, line )                               *
 *                                                            *
 * Input parameters:                                          *
 *       *prompt        char         Prompt text              *
 *                                                            *
 * Output parameters:                                         *
 *       **line         char         User input               *
 **                                                           *
 * S. Decker/Rutgers     9/19                                 *
 *************************************************************/
{
  /* If the buffer has already been allocated,
     return the memory to the free pool. */
  if (line_read)
    {
      free (line_read);
      line_read = (char *)NULL;
    }
  
  /* Get a line from the user. */
  line_read = readline (prompt);
  
  /* If the line has any text in it, save it to history. */
  if (line_read && *line_read)
    add_history (line_read);
  
  *line = line_read;
}
