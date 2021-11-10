package algorithms;

import java.awt.Point;
import java.util.ArrayList;

import supportGUI.Circle;
import supportGUI.Line;

public class DefaultTeam {

    // calculDiametre: ArrayList<Point> --> Line
    //   renvoie une paire de points de la liste, de distance maximum.
    public Line calculDiametre(ArrayList<Point> points) {
        return tme5exercice7(points);
        //return tme5exercice6(points);
    }

    // calculCercleMin: ArrayList<Point> --> Circle
    //   renvoie un cercle couvrant tout point de la liste, de rayon minimum.
    public Circle calculCercleMin(ArrayList<Point> points) {
        return tme5exercice5(points);
        //return tme5exercice4(points);
    }
    private Circle tme5exercice4(ArrayList<Point> inputPoints){
        ArrayList<Point> points = (ArrayList<Point>) inputPoints.clone();
        if (points.size()<1) return null;
        double cX,cY,cRadius,cRadiusSquared;
        for (Point p: points){
            for (Point q: points){
                cX = .5*(p.x+q.x);
                cY = .5*(p.y+q.y);
                cRadiusSquared = 0.25*((p.x-q.x)*(p.x-q.x)+(p.y-q.y)*(p.y-q.y));
                boolean allHit = true;
                for (Point s: points)
                    if ((s.x-cX)*(s.x-cX)+(s.y-cY)*(s.y-cY)>cRadiusSquared){
                        allHit = false;
                        break;
                    }
                if (allHit) return new Circle(new Point((int)cX,(int)cY),(int)Math.sqrt(cRadiusSquared));
            }
        }
        double resX=0;
        double resY=0;
        double resRadiusSquared=Double.MAX_VALUE;
        for (int i=0;i<points.size();i++){
            for (int j=i+1;j<points.size();j++){
                for (int k=j+1;k<points.size();k++){
                    Point p=points.get(i);
                    Point q=points.get(j);
                    Point r=points.get(k);
                    //si les trois sont colineaires on passe
                    if ((q.x-p.x)*(r.y-p.y)-(q.y-p.y)*(r.x-p.x)==0) continue;
                    //si p et q sont sur la meme ligne, ou p et r sont sur la meme ligne, on les echange
                    if ((p.y==q.y)||(p.y==r.y)) {
                        if (p.y==q.y){
                            p=points.get(k); //ici on est certain que p n'est sur la meme ligne de ni q ni r
                            r=points.get(i); //parce que les trois points sont non-colineaires
                        } else {
                            p=points.get(j); //ici on est certain que p n'est sur la meme ligne de ni q ni r
                            q=points.get(i); //parce que les trois points sont non-colineaires
                        }
                    }
                    //on cherche les coordonnees du cercle circonscrit du triangle pqr
                    //soit m=(p+q)/2 et n=(p+r)/2
                    double mX=.5*(p.x+q.x);
                    double mY=.5*(p.y+q.y);
                    double nX=.5*(p.x+r.x);
                    double nY=.5*(p.y+r.y);
                    //soit y=alpha1*x+beta1 l'equation de la droite passant par m et perpendiculaire a la droite (pq)
                    //soit y=alpha2*x+beta2 l'equation de la droite passant par n et perpendiculaire a la droite (pr)
                    double alpha1=(q.x-p.x)/(double)(p.y-q.y);
                    double beta1=mY-alpha1*mX;
                    double alpha2=(r.x-p.x)/(double)(p.y-r.y);
                    double beta2=nY-alpha2*nX;
                    //le centre c du cercle est alors le point d'intersection des deux droites ci-dessus
                    cX=(beta2-beta1)/(double)(alpha1-alpha2);
                    cY=alpha1*cX+beta1;
                    cRadiusSquared=(p.x-cX)*(p.x-cX)+(p.y-cY)*(p.y-cY);
                    if (cRadiusSquared>=resRadiusSquared) continue;
                    boolean allHit = true;
                    for (Point s: points)
                        if ((s.x-cX)*(s.x-cX)+(s.y-cY)*(s.y-cY)>cRadiusSquared){
                            allHit = false;
                            break;
                        }
                    if (allHit) {System.out.println("Found r="+Math.sqrt(cRadiusSquared));resX=cX;resY=cY;resRadiusSquared=cRadiusSquared;}
                }
            }
        }
        return new Circle(new Point((int)resX,(int)resY),(int)Math.sqrt(resRadiusSquared));
    }
    private Circle tme5exercice5(ArrayList<Point> points){
        if (points.size()<1) return null;
        ArrayList<Point> rest = (ArrayList<Point>)points.clone();
        Point dummy=rest.get(0);
        Point p=dummy;
        for (Point s: rest) if (dummy.distance(s)>dummy.distance(p)) p=s;
        Point q=p;
        for (Point s: rest) if (p.distance(s)>p.distance(q)) q=s;
        double cX=.5*(p.x+q.x);
        double cY=.5*(p.y+q.y);
        double cRadius=.5*p.distance(q);
        rest.remove(p);
        rest.remove(q);
        while (!rest.isEmpty()){
            Point s=rest.remove(0);
            double distanceFromCToS=Math.sqrt((s.x-cX)*(s.x-cX)+(s.y-cY)*(s.y-cY));
            if (distanceFromCToS<=cRadius) continue;
            double cPrimeRadius=.5*(cRadius+distanceFromCToS);
            double alpha=cPrimeRadius/(double)(distanceFromCToS);
            double beta=(distanceFromCToS-cPrimeRadius)/(double)(distanceFromCToS);
            double cPrimeX=alpha*cX+beta*s.x;
            double cPrimeY=alpha*cY+beta*s.y;
            cRadius=cPrimeRadius;
            cX=cPrimeX;
            cY=cPrimeY;
        }
        return new Circle(new Point((int)cX,(int)cY),(int)cRadius);
    }
    private Line tme5exercice6(ArrayList<Point> points) {
        if (points.size()<2) return null;
        Point p=points.get(0);
        Point q=points.get(1);
        for (Point s: points) for (Point t: points) if (s.distance(t)>p.distance(q)) {p=s;q=t;}
        return new Line(p,q);
    }
    private ArrayList<Point> tme5exercice7(ArrayList<Point> points){
        //VOIR CORRECTION TME6EXERCICE3
        return null;
    }
    private ArrayList<Point> tme5exercice8(ArrayList<Point> points){
        //VOIR CORRECTION TME6EXERCICE6
        return null;
    }

}
