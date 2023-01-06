#include <stdio.h>
#include <assert.h>

void replicate (int n, int c)
{
	for (int i = 0; i < n; i++)
		printf("%d", c);
}

int main()
{
	int x=0, m [10][10] = {0};
	while (x != (-1))
	{
		assert(scanf("%d", &x) == 1);
		if (x >= 0 && x < 100)
			{
				m[x/10] [x%10]++;
			}
	}
	for (int i = 0; i < 10; i++)
	{
		printf("%d|", i);
		for (int j = 0; j < 10; j++)
		{
			if (m[i][j] != 0) 
			{
				replicate (m[i] [j], j);
			}
		}
	putchar('\n');
	}
	return 0;
}








