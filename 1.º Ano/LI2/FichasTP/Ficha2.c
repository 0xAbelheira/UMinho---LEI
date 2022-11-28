#include <stdio.h>
#include <assert.h>
#include <math.h>

int main() 
{
	int x=0, y=0, z=0, perimetro=0, fst=0, snd=0, thrd=0;
    double area, s;
	assert(scanf("%d%d%d", &x, &y, &z) == 3);
    perimetro = x + y + z;
    s = (double) perimetro / 2;
    area = sqrt(s*(s-x)*(s-y)*(s-z));
    if (x >= y && y >= z)
    {
        fst = z; snd = y; thrd = x;
    }
    else if (y >= x && x >= z)
    {
        fst = z; snd = x; thrd = y;
    }
    else if (z >= y && y >= x)
    {
        fst = x; snd = y; thrd = z;
    }
    else if (z >= x && x >= y)
    {
        fst = y; snd = x; thrd = z;
    }
    else if (y >= z && z >= x)
    {
        fst = x; snd = z; thrd = y;
    }
    else if (x >= z && z >= y)
    {
        fst = y; snd = z; thrd = x;
    }
	if
		((x + y <= z) || (x + z <= y) || (y + z <= x))
		printf("INVALIDO\n");
    else if (x == y && y == z && x == z) 
    	printf("EQUILATERO %d %.2f\n", perimetro, area);
    else if ((x == y && x !=z) || (z == y && z != x) || (z == x && z != y))
    	printf("ISOSCELES %d %.2f\n", perimetro, area);
    else if (fst*fst + snd*snd == thrd*thrd)
        printf("RETANGULO %d %.2f\n", perimetro, area);
    else if (x != y && x != z && y != z)
    	printf("ESCALENO %d %.2f\n", perimetro, area);
    return 0;
}



