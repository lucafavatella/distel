#include <stdio.h>

int main(int argc, char** argv)
{
	unsigned int a,b,c,d;
	if (argc != 5)
		exit(1);
	
	a = atoi(argv[1]);
	b = atoi(argv[2]);
	c = atoi(argv[3]);
	d = atoi(argv[4]);

	printf("%u", (a << 24) | (b << 16) | (c << 8) | d);
}

