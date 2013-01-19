#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

int main(int argc, char **argv)
{
  Radar *radar;

  if (argc != 2) {
	fprintf(stderr, "No filename specified.\n");
	exit(0);
  }

  RSL_radar_verbose_on();
  radar = RSL_dorade_to_radar(argv[1]);
  if (radar == NULL) 
	printf("radar == NULL\n");
  else
	printf("radar == %x\n", (unsigned int)radar);

  exit(0);
}
