#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <complex.h>
#include <stdlib.h>
#include <string.h>

int nb_prod = 2;
int nb_cons = 2;
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
	pthread_mutex_t mutex;
	pthread_cond_t cprod;
	pthread_cond_t ccons;
};

void * make_tapis(struct tapis *tp){
	tp -> fifo_paq = malloc(sizeof(struct paquets)*capa);
	tp-> capacite = capa;
	tp -> pos = 0;
	tp -> size = 0;
	if (pthread_mutex_init(&tp -> mutex , NULL) != 0) {
		perror("Init_error mutex!\n");
		exit(1);
	}
	if (pthread_cond_init(&tp -> cprod , NULL) != 0) {
		perror("Init_error cprod!\n");
		exit(1);
	}
	if (pthread_cond_init(&tp -> ccons , NULL) != 0) {
		perror("Init_error cprod!\n");
		exit(1);
	}
	
};

void free_tapis(struct tapis *tp){
	if (tp != NULL) {
		free(tp -> fifo_paq);
		pthread_cond_destroy(&(tp -> ccons));
		pthread_cond_destroy(&(tp -> cprod));
		pthread_mutex_destroy(&(tp -> mutex));
	}
}


void * enfiler(struct tapis *tp , struct paquets *paq){
	pthread_mutex_lock(&tp -> mutex);
	while (tp -> size == tp -> capacite) {
		pthread_cond_wait(&tp -> cprod, &tp -> mutex);
	}
	tp -> fifo_paq[(tp -> pos + tp -> size) % (tp -> capacite)] = paq;
	tp -> size++;
	if (pthread_mutex_unlock(&tp -> mutex) != 0) {
		perror("Fail_unlock");
	}
	pthread_cond_signal(&tp -> ccons);
}

struct paquets * defiler(struct tapis *tp){
	pthread_mutex_lock(&tp -> mutex);
		
	while (tp -> size == 0) {
		pthread_cond_wait(&tp -> ccons, &tp -> mutex);
	}
	struct paquets *paq = tp -> fifo_paq[tp -> pos];
	tp -> pos = (tp -> pos + 1) % (tp -> capacite);
	tp -> size--;
	//printf("cotenue de tapis %zu\n",tp -> size);
	compteur--;
	if (pthread_mutex_unlock(&tp -> mutex) != 0) {
		perror("Fail_unlock");
	}
	pthread_cond_signal(&tp -> cprod);
	return paq;
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

void * product(void *prod){
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
		//printf("%s produce %d\n",p -> name , p -> cpt - 1);
	}
	free(p);
}

void * consume(void *cons){
	struct cons *c = (struct cons *) cons;
	while (compteur > 0) {
		struct paquets *paq = defiler(c -> tapis);
		printf("C%d mange %s\n", c -> id , paq -> paq);
		free_paquets(paq);
	}
	free(c);
}

int main(){
	pthread_t producteurs[nb_prod];
	pthread_t consommateurs[nb_cons];
	struct tapis tp;
	make_tapis(&tp);
	
	int i = 0;
	while (i < nb_prod) {
		struct prod *p = malloc(sizeof(struct prod));
		p -> name = name[i];
		p -> cpt = 0;
		p -> cible = num;
		p -> tapis = &tp;
		if (pthread_create(&(producteurs[i]),NULL,&product,p) != 0) {
			perror("Fail_create prod\n");
		}
		i++;
	}
	while (i < nb_prod + nb_cons) {
		struct cons *c = malloc(sizeof(struct cons));
		c -> id = i - nb_prod;
		c -> tapis = &tp;
		if (pthread_create(&(consommateurs[i - nb_prod]),NULL,&consume,c) != 0) {
			perror("Fail_create cons\n");
		}
		i++;
	}
	int j = 0;
	while (j < nb_prod) {
		pthread_join(producteurs[j],NULL);
		j++;
	}
	while (j < nb_cons + nb_prod) {
		pthread_join(consommateurs[j - nb_prod],NULL);
		j++;
	}
	
	free_tapis(&tp);
	
	return 0;;
}




