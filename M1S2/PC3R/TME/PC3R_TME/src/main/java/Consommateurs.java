public class Consommateurs extends Thread{

    Thread_principal thread_principal;
    public Consommateurs(String name,Thread_principal thread_principal) {
        super(name);
        this.thread_principal = thread_principal;
    }

    public void run(){
        while (thread_principal.compteur > 0){
            try {
                //String tmp = thread_principal.tapis.products.get(0).getPaq();
                paquets p = thread_principal.tapis.defiler();
                thread_principal.compteur = thread_principal.compteur - 1;
                System.out.println(this.getName()+ " mange " + p.getPaq());
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
