#include <stdio.h> 
// A normal function with an int parameter 
// and void return type 
void fun(int a) 
{ 
    printf("Value of a is %d\n", a); 
} 

int fun2(int a) { return a * 2; }

int fun3(int a) { return a * 4; }
  
void test1() 
{
    void (*funptr)(int) = &fun;
    (*funptr)(10);
} 

void test2(void (*funptr)(int)) {
    printf("starting fun ptr test\n");
    (*funptr)(10) ;
    printf("ending fun ptr test\n");
}

void test3(void (*funptr)(int)) {
    printf("starting fun ptr test\n");
    funptr(10) ;
    printf("ending fun ptr test\n");
}

void test4() {
    void (*funptr)(int) = &fun; 
    funptr(10); 
} 

void test5() {
    void (*funptr)(int) = fun; 
    (*funptr)(10); 
} 

//int test6() { return 6 ; }

int test6() {
    int (*funptr)(int) = fun2; 
    return ((*funptr)(10) + 5) ; 
} 

int test7(int (*funptr)(int)) {
    return (*funptr)(10) + 5 ; 
} 

void test8() 
{
    void (*funptr)(int) = &fun;
    //(*funptr)(10); 
    test2(&fun) ;
}

void test9() 
{
    void (*funptr)(int) = &fun;
    test2(*funptr) ;
} 

void main() {}

