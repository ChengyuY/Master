package algorithms;

import java.awt.Point;
import java.util.ArrayList;

import supportGUI.Circle;
import supportGUI.Line;

public class DefaultTeam {

    // calculDiametre: ArrayList<Point> --> Line
    //   renvoie une pair de points de la liste, de distance maximum.
    public Line calculDiametre(ArrayList<Point> points) {
        if (points.size()<3) {
            return null;
        }

        Point p=points.get(0);
        Point q=points.get(1);

        /*******************
         * PARTIE A ECRIRE *
         *******************/
        return new Line(p,q);
    }

    // calculDiametreOptimise: ArrayList<Point> --> Line
    //   renvoie une pair de points de la liste, de distance maximum.
    public Line calculDiametreOptimise(ArrayList<Point> points) {
        if (points.size()<3) {
            return null;
        }

        Point p=points.get(1);
        Point q=points.get(2);

        /*******************
         * PARTIE A ECRIRE *
         *******************/
        return new Line(p,q);
    }

    // calculCercleMin: ArrayList<Point> --> Circle
    //   renvoie un cercle couvrant tout point de la liste, de rayon minimum.
    public Circle calculCercleMin(ArrayList<Point> points) {
        if (points.isEmpty()) {
            return null;
        }

        Point center=points.get(0);
        int radius=100;

        /*******************
         * PARTIE A ECRIRE *
         *******************/
        return new Circle(center,radius);
    }

    // enveloppeConvexe: ArrayList<Point> --> ArrayList<Point>
    //   renvoie l'enveloppe convexe de la liste.
    public ArrayList<Point> enveloppeConvexe(ArrayList<Point> points){
        //return exercice1(points);
        //return exercice2(points);
        //return exercice1(exercice2(points));
        //return exercice1(exercice3(points));
        //return exercice4(points);
        //return exercice5(points);
        return exercice6(points);
    }
    private ArrayList<Point> exercice1(ArrayList<Point> points){
        if (points.size()<4) return points;

        ArrayList<Point> enveloppe = new ArrayList<Point>();

        for (Point p: points) {
            for (Point q: points) {
                if (p.equals(q)) continue;
                Point ref=p;
                for (Point r: points) if (crossProduct(p,q,p,r)!=0) {ref=r;break;}
                if (ref.equals(p)) {enveloppe.add(p); enveloppe.add(q); continue;}
                double signeRef = crossProduct(p,q,p,ref);
                boolean estCote = true;
                for (Point r: points) if (signeRef * crossProduct(p,q,p,r)<0) {estCote = false;break;} //ici sans le break le temps de calcul devient horrible
                if (estCote) {enveloppe.add(p); enveloppe.add(q);}
            }
        }

        return enveloppe; //ici l'enveloppe n'est pas trie dans le sens trigonometrique, et contient des doublons, mais tant pis!
    }
    private double crossProduct(Point p, Point q, Point s, Point t){
        return ((q.x-p.x)*(t.y-s.y)-(q.y-p.y)*(t.x-s.x));
    }
    private ArrayList<Point> exercice2(ArrayList<Point> points){
        if (points.size()<4) return points;
        int maxX=points.get(0).x;
        for (Point p: points) if (p.x>maxX) maxX=p.x;
        Point[] maxY = new Point[maxX+1];
        Point[] minY = new Point[maxX+1];
        for (Point p: points) {
            if (maxY[p.x]==null||p.y>maxY[p.x].y) maxY[p.x]=p;
            if (minY[p.x]==null||p.y<minY[p.x].y) minY[p.x]=p;
        }
        ArrayList<Point> result = new ArrayList<Point>();
        for (int i=0;i<maxX+1;i++) if (maxY[i]!=null) result.add(maxY[i]);
        for (int i=maxX;i>=0;i--) if (minY[i]!=null && !result.get(result.size()-1).equals(minY[i])) result.add(minY[i]);

        if (result.get(result.size()-1).equals(result.get(0))) result.remove(result.size()-1);

        return result;
    }
    private ArrayList<Point> exercice3(ArrayList<Point> points){
        if (points.size()<4) return points;

        Point ouest = points.get(0);
        Point sud = points.get(0);
        Point est = points.get(0);
        Point nord = points.get(0);
        for (Point p: points){
            if (p.x<ouest.x) ouest=p;
            if (p.y>sud.y) sud=p;
            if (p.x>est.x) est=p;
            if (p.y<nord.y) nord=p;
        }
        ArrayList<Point> result = (ArrayList<Point>)points.clone();
        for (int i=0;i<result.size();i++) {
            if (triangleContientPoint(ouest,sud,est,result.get(i)) ||
                    triangleContientPoint(ouest,est,nord,result.get(i))) {
                result.remove(i);
                i--;
            }
        }
        return result;
    }
    private boolean triangleContientPoint(Point a, Point b, Point c, Point x) {
        double l1 = ((b.y-c.y)*(x.x-c.x)+(c.x-b.x)*(x.y-c.y))/(double)((b.y-c.y)*(a.x-c.x)+(c.x-b.x)*(a.y-c.y));
        double l2 = ((c.y-a.y)*(x.x-c.x)+(a.x-c.x)*(x.y-c.y))/(double)((b.y-c.y)*(a.x-c.x)+(c.x-b.x)*(a.y-c.y));
        double l3 = 1-l1-l2;
        return (0<l1 && l1<1 && 0<l2 && l2<1 && 0<l3 && l3<1);
    }
    private ArrayList<Point> exercice4(ArrayList<Point> points){
        if (points.size()<4) return points;

        Point ouest = points.get(0);
        for (Point p: points) if (p.x<ouest.x||(p.x==ouest.x && p.y>ouest.x)) ouest=p;
        ArrayList<Point> enveloppe = new ArrayList<Point>();
        enveloppe.add(ouest);
        for (Point q: points) {
            if (q.equals(ouest)) continue;
            Point ref=q;
            for (Point r: points) if (crossProduct(ouest,q,ouest,r)!=0) {ref=r;break;}
            if (ref.equals(q)) { enveloppe.add(q); continue;}
            double signeRef = crossProduct(ouest,q,ouest,ref);
            boolean estCote = true;
            for (Point r: points) if (signeRef * crossProduct(ouest,q,ouest,r)<0) {estCote = false;break;}
            if (estCote) {enveloppe.add(q);break;}
        }

        do {
            Point p = enveloppe.get(enveloppe.size()-2);
            Point q = enveloppe.get(enveloppe.size()-1);
            Point r = points.get(0);
            for (Point s: points) if (!s.equals(p) && !s.equals(q)) {r=s;break;}
            for (Point s: points) {
                if (s.equals(p)||s.equals(q)) continue;
                if (angle(p,q,q,s)<angle(p,q,q,r)) r=s;
            }
            enveloppe.add(r);
        } while (!enveloppe.get(enveloppe.size()-1).equals(enveloppe.get(0)));
        enveloppe.remove(0);
        return enveloppe;
    }
    private double angle(Point p, Point q, Point s, Point t) {
        if (p.equals(q)||s.equals(t)) return Double.MAX_VALUE;
        double cosTheta = dotProduct(p,q,s,t)/(double)(p.distance(q)*s.distance(t));
        return Math.acos(cosTheta);
    }
    private double dotProduct(Point p, Point q, Point s, Point t) {
        return ((q.x-p.x)*(t.x-s.x)+(q.y-p.y)*(t.y-s.y));
    }
    private ArrayList<Point> exercice5(ArrayList<Point> points){
        if (points.size()<4) return points;

        ArrayList<Point> result = exercice2(points);
        for (int i=1;i<result.size()+2;i++) {
            Point p = result.get((i-1)%result.size());
            Point q = result.get(i%result.size());
            Point r = result.get((i+1)%(result.size()));
            if (crossProduct(p,q,p,r)>0) {
                result.remove(i%result.size());
                if (i==2) i=1;
                if (i>2) i-=2;
            }
        }
        return result;
    }
    private ArrayList<Point> exercice6(ArrayList<Point> points){
        if (points.size()<4) return points;

        Point ouest = points.get(0);
        Point sud = points.get(0);
        Point est = points.get(0);
        Point nord = points.get(0);
        for (Point p: points){
            if (p.x<ouest.x) ouest=p;
            if (p.y>sud.y) sud=p;
            if (p.x>est.x) est=p;
            if (p.y<nord.y) nord=p;
        }
        ArrayList<Point> result = new ArrayList<Point>();
        result.add(ouest);
        result.add(sud);
        result.add(est);
        result.add(nord);

        ArrayList<Point> rest = (ArrayList<Point>)points.clone();
        for (int i=0;i<rest.size();i++) {
            if (triangleContientPoint(ouest,sud,est,rest.get(i)) ||
                    triangleContientPoint(ouest,est,nord,rest.get(i))) {
                rest.remove(i);
                i--;
                    }
        }

        for (int i=0;i<result.size();i++) {
            Point a = result.get(i);
            Point b = result.get((i+1)%result.size());
            Point ref = result.get((i+2)%result.size());

            double signeRef = crossProduct(a,b,a,ref);
            double maxValue = 0;
            Point maxPoint = a;

            for (Point p: points) {
                double piki = crossProduct(a,b,a,p);
                if (signeRef*piki<0 && Math.abs(piki)>maxValue) {
                    maxValue = Math.abs(piki);
                    maxPoint = p;
                }
            }
            if (maxValue!=0){
                for (int j=0;j<rest.size();j++) {
                    if (triangleContientPoint(a,b,maxPoint,rest.get(j))){
                        rest.remove(j);
                        j--;
                    }
                }
                result.add(i+1,maxPoint);
                i--;
            }
        }
        return result;
    }
}
