#include <stdio.h>
#include <assert.h>

int main() 
{
    int x=0, y=0;

    assert(scanf("%d%d", &x, &y) == 2);
    printf("%d\n", x*x + y*y);

    return 0;
}
