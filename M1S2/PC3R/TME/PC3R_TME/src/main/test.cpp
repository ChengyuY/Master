#nclude <iostream>

void f(int i){
    int a = 10;
    i = a;
}

void g(int* i){
    int a = 20;
    *i = a;
}

int main(){
    int x = 100;
    f(x);
    std::cout<<x<<std::endl;
    g(&x);
    std::cout<<x<<std::endl;
    return 0;
}