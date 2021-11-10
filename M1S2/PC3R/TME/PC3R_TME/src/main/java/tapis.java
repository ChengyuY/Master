import java.util.ArrayList;

public class tapis {
    public static ArrayList<paquets> products = new ArrayList<paquets>();
    public int idpro = 0;
    public static int quantite = 0;
    public static int capacite;

    public tapis(int capacite){
        this.capacite = capacite;
    }

    public synchronized paquets defiler() throws InterruptedException {
        //System.out.println("Cons " + quantite + "-----" + capacite);
        while (quantite == 0) {
            wait();
        }
        quantite = quantite - 1;
        paquets tmp = (paquets) products.get(0);
        products.remove(0);
        notifyAll();
        return tmp;
    }

    public synchronized void enfiler(paquets paquets) throws InterruptedException {
        //System.out.println("Prod " + quantite + "-----" + capacite);
        while (quantite == capacite) {
            wait();
        }
        quantite = quantite + 1;
        products.add(paquets);
        notifyAll();
    }

    }
