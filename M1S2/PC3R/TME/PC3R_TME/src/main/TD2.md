*** Exercise ***

let compteur = ref 0
let compteur_lock = Mutex.create ()
let compteur_cond = Condition.create()
let fin_entrees = ref False


let rec entree nb =
    if nb > 0 then 
        begin
            Mutex.lock(compteur_lock);
            compteur := compteur + 1
            print_int nb;
            print_endline "=> Entree"
            if !compteur == 0 then Condition.signal
            compteur_cond;
            Mutex.unlock(compteur_lock)
            Thread.delay (Random.float 0.2)
            entree (nb - 1)
        end
    else pirnt_endline "Fin entree"

let rec sortie () = 
    while not !fin_entrees or !comptuer > 0 
    do 
        Mutex.lock comptuer_lock;
        while !compteur == 0 do
            condition.wait compteur_cond compteur_lock
        if !compteur > 0 then
            begin 
                compteur := !compteur - 1;
                print_int !compteur;
                print_endline "=> Sortie";
            end;
        Mutex.unlock compteur_lock;
        Thread.delay (Random.float 0.4);
    done;
    print_endline "Fin des sorties";

let main () =
    let nb_vis = int_of_string Sys.argv.(1) in
    let ts1 = Thread.create sortie () in
    let ts2 = Thread.create sortie () in
    let te1 = Thread.create entree () in
    let te2 = Thread.create entree () in
    Thread.join te1;
    Thread.join te2;
    fin_entrees = True;
    Thread.kill ts1;
    Thread.kill ts2;
    print_string "Fin"
    exit 0;;

let _ = main ();;

** Probleme 1 **

Course :
    Les quatre threads lisent et ecrivent en meme temps la variable compteur

Solution :
    Verrou => Un mutex pour proteger compteur 
    Les manipulation de compteur auront lieu en section critique

** Probleme 2 **

Attente Active :
    Les threads sortie et le thread principal peuvent coubler sans rien produire si le compteur est vide

Solution :
    Condition (primitieve attente et de reveil - wait / signal)

** Probleme 3 **

C'est bon (on ne fait pas )



*** Exo 2.1 ***

void run_p (void *phrase){
    while (1){
        printf("%s \n" , (char *) phrase);
        ft_thread_cooperate();
    }
}

int main(void){
    ft_scheduler sched = ft_schduler_create ();
    ft_thread_create(sched , run_p , NULL , (void *) "1");
    ft_thread_create(sched , run_p , NULL , (void *) "2");
    ft_thread_create(sched , run_p , NULL , (void *) "3");
    ft_thread_create(sched , run_p , NULL , (void *) "4");
    ft_thread_create(sched , run_p , NULL , (void *) "5");
    ft_scheduler_start (sched);
    return 0;
}























