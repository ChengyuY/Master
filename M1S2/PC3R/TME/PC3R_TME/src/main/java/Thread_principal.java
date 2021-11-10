import java.util.ArrayList;

public class Thread_principal extends Thread{

    public int compteur , m , n ,cible;
    public tapis tapis;
    public ArrayList<String> prod;

    public Thread_principal(String name , int com , int m , int n,int capacite ,ArrayList<String> prod,int cible){
        super(name);
        this.compteur = com * m ;
        this.m = m ;
        this.n = n ;
        this.prod = prod;
        tapis = new tapis(capacite);
        this.cible = cible;
    }

    public void run(){
        ArrayList<Consommateurs> lis_cons = new ArrayList<Consommateurs>(m);
        ArrayList<Producteurs> lis_prod = new ArrayList<Producteurs>(m);
        for (int i = 0 ; i < m ; i++){
            lis_prod.add(i , new Producteurs(prod.get(i),cible,this));
            //System.out.println("Create producteur "+lis_prod.get(i).getName());
        }
        for (int i = 0 ; i < n ; i++){
            lis_cons.add(i , new Consommateurs("cons "+i,this));
            //System.out.println("Create consommateur "+i);
        }
        //System.out.println(compteur);
        for (Producteurs producteur : lis_prod){
            producteur.start();
            //System.out.println("Production starts "+producteur.getName());
        }
        for (Consommateurs consommateur : lis_cons) {
            consommateur.start();
        }
        return;
    }
}
