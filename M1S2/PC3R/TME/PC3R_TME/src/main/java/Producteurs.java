public class Producteurs extends Thread{

    public int cible ;
    public Thread_principal thread_principal;
    public int com = 0;

    public Producteurs(String name,int cible ,Thread_principal thread_principal) {
        super(name);
        this.thread_principal = thread_principal;
        this.cible = cible;
    }

    public void run(){
        while (com < cible){
            try {
                thread_principal.tapis.enfiler(new paquets(this.getName()+com));
                com++;
                //System.out.println("Paquets producted ");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
