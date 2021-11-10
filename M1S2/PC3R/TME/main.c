#include "fthread.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int nb_prod = 2;
int nb_cons = 2;
int nb_mess = 2;
int capa = 5;
char *name[2] = {"apple","banana"};
int num = 5;
int compteur = 10;

struct paquets {
    char *paq;
};

void make_paquets(struct paquets *tp,char *name){
    char *res = malloc(sizeof(char) * strlen(name)+1);
    for (int i = 0; i <= strlen(name); i++) {
        res[i] = name[i];
    }
    tp -> paq = res;
};

void free_paquets(struct paquets *tp){
    if (tp != NULL) {
        free(tp -> paq);
        free(tp);
    }
}

struct tapis{
    struct paquets **fifo_paq;
    size_t capacite;
    size_t pos;
    size_t size;
    ft_event_t vide;
    ft_event_t pleine;
};

void * make_tapis(struct tapis *tp , ft_scheduler_t *sche){
    tp -> fifo_paq = malloc(sizeof(struct paquets)*capa);
    tp-> capacite = capa;
    tp -> pos = 0;
    tp -> size = 0;
    tp -> vide = ft_event_create(*sche);
    tp -> pleine = ft_event_create(*sche);
};

void free_tapis(struct tapis *tp){
    if (tp != NULL) {
        free(tp -> fifo_paq);
    }
}


void * enfiler(struct tapis *tp , struct paquets *paq){
    while (tp -> size == tp -> capacite) {
        ft_thread_await(tp -> pleine);
    }
    tp -> fifo_paq[(tp -> pos + tp -> size) % (tp -> capacite)] = paq;
    tp -> size++;
    ft_thread_generate(tp -> vide);
}

struct paquets * defiler(struct tapis *tp){
    while (tp -> size == 0) {
        //printf("Nothing to consume!\n");
        ft_thread_await(tp -> vide);
    }
    struct paquets *paq = tp -> fifo_paq[tp -> pos];
    tp -> pos = (tp -> pos + 1) % (tp -> capacite);
    tp -> size--;
    //printf("contenue de tapis %zu\n",tp -> size);
    compteur--;
    ft_thread_generate(tp -> pleine);
    return paq;
}

struct paquets * transport_prod(struct tapis * prod , struct tapis * cons) {
    while (prod -> size == 0) {
        printf("No paquets to transport!\n");
        ft_thread_await(prod -> vide);
        ft_thread_await(cons -> pleine);
    }
    struct paquets *paq = prod -> fifo_paq[prod -> pos];
    prod -> pos = (prod -> pos + 1) % (prod -> capacite);
    prod -> size--;
    ft_thread_generate(prod -> pleine);
    ft_thread_generate(cons -> vide);
    return paq;
}

void * transport_cons(struct tapis * prod , struct  tapis * cons , struct paquets * paq){
    while (cons -> size == cons -> capacite){
        printf("No place for the paquets!\n");
        ft_thread_await(prod -> vide);
        ft_thread_await(cons -> pleine);
    }
    cons -> fifo_paq[(cons -> pos + cons -> size) % (cons -> capacite)] = paq;
    cons -> size++;
    ft_thread_generate(prod -> pleine);
    ft_thread_generate(cons -> vide);
    return  paq ;
}

struct prod{
    int cible;
    char *name;
    int cpt;
    struct tapis *tapis;
};

void free_prod(struct prod *pro){
    pro = NULL;
    free(pro);
}

struct cons{
    int id;
    struct tapis *tapis;
};

void free_cons(struct cons *con){
    con = NULL;
    free(con);
}

struct mess{
    int id;
    ft_scheduler_t *sche_prod;
    ft_scheduler_t *sche_cons;
    struct tapis *tapis_prod;
    struct tapis *tapis_cons;
};

void  free_mess(struct  mess *mess){
    mess = NULL;
    free(mess);
}

void * product(void *prod){
    //printf("Begin production!\n");
    struct prod *p = (struct prod *) prod;
    while (p -> cpt < p -> cible) {
        struct paquets *paq = malloc(sizeof(struct paquets));
        char* str = malloc((sizeof(char)*strlen(p -> name)+2));
        strcat(str, p -> name);
        char s[sizeof(char)*2];
        sprintf(s, "%d", p -> cpt);
        strcat(str, s);
        make_paquets(paq, str);
        enfiler(p -> tapis, paq);
        p -> cpt++;
        ft_thread_cooperate();
        FILE *file = fopen("prod.txt","a");
        fprintf(file,"%s produce %d\n",p -> name , p -> cpt - 1);
        fclose(file);
        //printf("%s produce %d\n",p -> name , p -> cpt - 1);
    }
    //printf("End production!\n");
    free(p);
}

void * consume(void *cons){
    struct cons *c = (struct cons *) cons;
    while (compteur > 0) {
        //printf("----------Paquets in the market %d\n" , c -> tapis -> size);
        struct paquets *paq = defiler(c -> tapis);
        //printf("----------C%d mange %s\n", c -> id , paq -> paq);
        free_paquets(paq);
        ft_thread_cooperate();
        FILE *file = fopen("cons.txt","a");
        fprintf(file , "C%d mange %s\n",c -> id , paq -> paq);
        fclose(file);
    }
    free(c);
}

void * message(void * mess){
    struct mess  *m = (struct mess *) mess;
    ft_thread_unlink();
    while (compteur > 0){
        ft_thread_link(m -> sche_prod);
        //printf("Link to industry!\n");
        struct paquets * paq = transport_prod(m -> tapis_prod , m -> tapis_cons);
        //printf("Get %s from industry by %D, rest paquets in the industry : %d\n",paq -> paq , m -> id , m ->tapis_prod ->size );
        ft_thread_cooperate();
        ft_thread_unlink();
        FILE *file_1 = fopen("mess.txt","a");
        fprintf(file_1,"Get %s from industry by %D, rest paquets in the industry : %d\n",paq -> paq , m -> id , m ->tapis_prod ->size);
        fclose(file_1);
        ft_thread_link(m -> sche_cons);
        //printf("Link to market!\n");
        transport_cons(m -> tapis_prod , m -> tapis_cons ,  paq);
        //printf("Send %s to market by %d, rest paquets in the market : %d \n",paq -> paq , m -> id , m -> tapis_cons -> size);
        ft_thread_cooperate();
        ft_thread_unlink();
        FILE *file_2 = fopen("mess.txt","a");
        fprintf(file_2,"Send %s to market by %d, rest paquets in the market : %d \n",paq -> paq , m -> id , m -> tapis_cons -> size);
        fclose(file_2);
    }
}

int main(){
    ft_thread_t producteurs[nb_prod];
    ft_thread_t consommateurs[nb_cons];
    ft_thread_t messagers[nb_mess];
    ft_scheduler_t sche_prod = ft_scheduler_create();
    ft_scheduler_t sche_cons = ft_scheduler_create();
    struct tapis tp_prod;
    struct tapis tp_cons;
    make_tapis(&tp_prod,&sche_prod);
    make_tapis(&tp_cons,&sche_cons);
    int po , * cpt_prod = &po;

    int i = 0;
    while (i < nb_prod) {
        struct prod *p = malloc(sizeof(struct prod));
        p -> name = name[i];
        p -> cpt = 0;
        p -> cible = num;
        p -> tapis = &tp_prod;
        producteurs[i] = ft_thread_create(sche_prod , &product , NULL , p);
        //fprintf (stdout ,"Create prod success\n");
        i++;
    }
    while (i < nb_prod + nb_cons) {
        struct cons *c = malloc(sizeof(struct cons));
        c -> id = i - nb_prod;
        c -> tapis = &tp_cons;
        producteurs[i - nb_prod] = ft_thread_create(sche_cons , &consume , NULL , c);
        //fprintf (stdout ,"Create cons success\n");
        i++;
    }
    while (i < nb_prod + nb_cons + nb_mess) {
        struct mess *m = malloc(sizeof(struct mess));
        m -> id = i - nb_prod;
        m -> sche_prod = sche_prod;
        m -> sche_cons = sche_cons;
        m -> tapis_prod = & tp_prod;
        m -> tapis_cons = & tp_cons;
        messagers[i - nb_prod - nb_cons] = ft_thread_create(sche_cons , &message , NULL , m);
        //fprintf (stdout ,"Create mess success\n");
        i++;
    }
    ft_scheduler_start(sche_prod);
    ft_scheduler_start(sche_cons);
    int j = 0;
    while (j < nb_prod) {
    //    pthread_join(producteurs[j],NULL);
        pthread_join (ft_pthread (producteurs[j]),(void **)&cpt_prod );
        fprintf (stdout ,"Join prod success\n");
        j++;
    }
    while (j < nb_cons + nb_prod) {
    //    pthread_join(consommateurs[j - nb_prod],NULL);
        pthread_join (ft_pthread (consommateurs[j - nb_prod]),(void **)&cpt_prod );
        fprintf (stdout ,"Join cons success\n");
        j++;
    }
    while (j < nb_cons + nb_prod + nb_mess) {
        //    pthread_join(consommateurs[j - nb_prod],NULL);
        pthread_join (ft_pthread (messagers[j - nb_prod - nb_mess]),(void **)&cpt_prod );
        fprintf (stdout ,"Join mess success\n");
        j++;
    }
    fprintf (stdout ,"exit\n");
    free_tapis(&tp_prod);
    free_tapis(&tp_cons);

    return 0;
}




