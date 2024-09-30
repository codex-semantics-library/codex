//

struct pair {
    int x ;
    int y ;
};


void test1(struct pair * p)
{
    p->x = 3 ;
    p->y = 5 ;
    unsigned long * ptr = (unsigned long *) p ;
    __VERIFIER_assert(*ptr = 0x0000000500000003) ;
}

void test1_inverted(struct pair * p)
{
    p->x = 5 ;
    p->y = 3 ;
    unsigned long * ptr = (unsigned long *) p ;
    __VERIFIER_assert(*ptr = 0x0000000300000005) ;
}

void test2(struct pair * p)
{
    unsigned long * ptr = (unsigned long *) p ;
    __VERIFIER_assert(*ptr = 0x0000000500000003) ;
}

void test3(struct pair * p)
{
    p->x = 3 ;
    p->y = 5 ;
    unsigned int * ptr = (unsigned int *) p ;
    __VERIFIER_assert(*ptr = 0x00000003) ;
}

void test4(struct pair * p)
{
    p->x = 3 ;
    p->y = 5 ;
    unsigned int * ptr = (unsigned int *) p ;
    __VERIFIER_assert(*(ptr+1) = 0x00000005) ;
}

void test5()
{
    struct pair p ;
    p.x = 3 ;
    p.y = 5 ;
    unsigned long * ptr = (unsigned long *) &p ;
    __VERIFIER_assert(*ptr = 0x0000000500000003) ;
}

void main() {}
