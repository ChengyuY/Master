**Exo 2.1**

#include<stdio.h>
#include<unistd.h>

int main(){
    int v = 42;
    int pid = fork();
    if(pid){
        v = 38;
        printf("Je suis le pere , v vaut %d.\n",v);
        sleep(4);
    }else{
        sleep(1);
        printf("Je suis le fils , v vaut %d.\n",v);
    }
}

Sortie standard :
Je suis le pere , v vaut 38.
Je suis le fils , v vaut 42.

===================================================

#include<stdio.h>
#include<pthread.h>

int v = 0;
pthread_t th1;

void *fun_fils(void *rien){
    sleep(10);
    printf("Je suis le fils , v vaut %d.\n",v);
}


int main(){
    v = 42;
    pthread_create(&th1,NULL,fun_fils,NULL);
    v = 38;
    printf("Je suis le fils , v vaut %d.\n",v);
    sleep(40);
    return 0;
}

Sortie standard :
Je suis le pere , v vaut 38.
Je suis le fils , v vaut 38.

===================================================

Thread -> Légers , Partagent une mémoire , Ordonnancés par l'application

Processus -> Lourds , Ont leur propre mémoire , Ordonnancés par le Système d'Exploitation

***Exercice 2.2*

#include<stdio.h>
#include<pthread.h>

pthread_t th1;
pthread_t th2;
pthread_t th3;
pthread_t th4;
pthread_t th5;


void *fun_fils1(void *rien){
    printf("Belle Marquise\n");
}

void *fun_fils2(void *rien){
    printf("Vos beaux yeux\n");
}

void *fun_fils3(void *rien){
    printf("me font\n");
}

void *fun_fils4(void *rien){
    printf("mourir\n");
}

void *fun_fils5(void *rien){
    printf("d'amour\n");
}

int main(){
    pthread_create(&th1,NULL,fun_fils1,NULL);
    pthread_create(&th2,NULL,fun_fils2,NULL);
    pthread_create(&th3,NULL,fun_fils3,NULL);
    pthread_create(&th4,NULL,fun_fils4,NULL);
    pthread_create(&th5,NULL,fun_fils5,NULL);
    sleep(10);
    return 0;
}

===================================================

class Jourdain implements Runnable{
    int id;
    Jourdain(i){ id = i; }
    run(){
        if (i == 1) System.out.println("Belle Marquise");
        if (i == 2) System.out.println("Vos beaux yeux");
        if (i == 3) System.out.println("me font");
        if (i == 4) System.out.println("mourir");
        if (i == 5) System.out.println("d'amour");
    }
}

Jourdain j1 = new Jourdain(1);
Jourdain j1 = new Jourdain(1);
Jourdain j1 = new Jourdain(1);
Jourdain j1 = new Jourdain(1);
Jourdain j1 = new Jourdain(1);
j1.start();
j2.start();
j3.start();
j4.start();
j5.start();

===================================================

fn poeme(num : i32) -> i32{
    match num {
    1 => println!("Belle Marquise")
    2 => println!("Vos beaux yeux")
    3 => println!("me font")
    4 => println!("mourir")
    5 => println!("d'amour")
    _ => println!("BUG")
    }
}

fn main(){
    let mut handles = Vec :: new();
    for i in 1 .. 6 {
        handles.push(std::thread::spawn(
            move || poeme(i);
        ))
    }
    for h in handles {
        h.join().unwrap;
    }
}

===================================================























