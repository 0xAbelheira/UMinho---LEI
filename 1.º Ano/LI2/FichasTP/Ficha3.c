#include <stdio.h>
#include <assert.h>

void replicate (int a, char c) 
{
  for (int i = 0; i < a; i++)
  	putchar(c);
}

int main()
{
	int n, num_asteriscos=1, num_pontos=1, p1, p2=1, p3;
	assert(scanf("%d",&n) == 1);
	p1 = n;
	p3 = n;
	for (int linha=1;linha<=n;linha++)
	{
		num_pontos = (p1-1)*2;
		replicate (num_asteriscos,'*');
		replicate (num_pontos,'.');
		replicate (num_asteriscos,'*');
		num_asteriscos = num_asteriscos+1;
		p1 -= 1;
		putchar('\n');
	}
	for (int linha=(n-1);linha>0;linha--)
	{
	    p3 -= 1;
	    num_pontos = p2 * 2;
	    num_asteriscos = p3;
		replicate (num_asteriscos, '*');
		replicate (num_pontos, '.');
		replicate (num_asteriscos, '*');
		p2 += 1;
		putchar('\n');
	}
	return 0;
}