package algorithms;

import java.awt.Point;
import java.util.ArrayList;


public class DefaultTeam {
    public Tree2D calculSteiner(ArrayList<Point> points, int edgeThreshold, ArrayList<Point> hitPoints) {
        // calcul the shortest paths between all points
        int[][] paths = calculShortestPaths(points, edgeThreshold);
        /*
        use the first version of algorithme heuristique
        ArrayList<Edge> res = heuristique_v1(points,hitPoints,paths);
        */
        // use the second version of algorithme heuristique
        ArrayList<Edge> res = heuristique_v1(points,hitPoints,paths);
        // we transform the edges to a tree
        return edgesToTree(res,res.get(1).getP());
    }

    public Tree2D calculSteinerBudget(ArrayList<Point> points, int edgeThreshold, ArrayList<Point> hitPoints) {
        int[][] paths= calculShortestPaths(points, edgeThreshold);
        /*
        get the first version of tree steiner
        ArrayList<Edge> res = heuristique_v1(points,hitPoints,paths);
        */
        // get the second version of tree steiner
        ArrayList<Edge> res = heuristique_v2(points,hitPoints,paths);
        res = sort(res);
        // we start with the first point of 'hitPoint'
        Point start = hitPoints.get(0);
        // we use the method 'filtre' to get the tree steiner with the budget 1664
        ArrayList<Edge> budgetEdges = filtre(res, start, 1664);
        // we transform the edges to a tree
        return edgesToTree(budgetEdges,start);
    }

    // justify a edge contains the point p and q.
    private boolean contains(ArrayList<Edge> edges,Point p,Point q){
        for (Edge e:edges){
            if (e.p.equals(p) && e.q.equals(q) ||
                    e.p.equals(q) && e.q.equals(p) ) return true;
        }
        return false;
    }

    private Tree2D edgesToTree(ArrayList<Edge> edges, Point root) {
        ArrayList<Edge> remainder = new ArrayList<Edge>();
        ArrayList<Point> subTreeRoots = new ArrayList<Point>();
        Edge current;
        while (edges.size()!=0) {
            current = edges.remove(0);
            if (current.p.equals(root)) {
                subTreeRoots.add(current.q);
            } else {
                if (current.q.equals(root)) {
                    subTreeRoots.add(current.p);
                } else {
                    remainder.add(current);
                }
            }
        }
        ArrayList<Tree2D> subTrees = new ArrayList<Tree2D>();
        for (Point subTreeRoot: subTreeRoots) subTrees.add(edgesToTree((ArrayList<Edge>)remainder.clone(),subTreeRoot));
        return new Tree2D(root, subTrees);
    }

    // get the rank of all edges
    private ArrayList<Edge> sort(ArrayList<Edge> edges) {
        if (edges.size()==1) return edges;
        ArrayList<Edge> left = new ArrayList<Edge>();
        ArrayList<Edge> right = new ArrayList<Edge>();
        int n=edges.size();
        for (int i=0;i<n/2;i++) { left.add(edges.remove(0)); }
        while (edges.size()!=0) { right.add(edges.remove(0)); }
        left = sort(left);
        right = sort(right);
        ArrayList<Edge> result = new ArrayList<Edge>();
        while (left.size()!=0 || right.size()!=0) {
            if (left.size()==0) { result.add(right.remove(0)); continue; }
            if (right.size()==0) { result.add(left.remove(0)); continue; }
            if (left.get(0).distance() < right.get(0).distance()) result.add(left.remove(0));
            else result.add(right.remove(0));
        }
        return result;
    }

    // Algorithme Kruskal
    private ArrayList<Edge> kruskal(ArrayList<Point> points){
        ArrayList<Edge> edges = new ArrayList<Edge>();
        // get the list 'edges' which contains all possible edges
        for (Point p: points) {
            for (Point q: points) {
                if (p.equals(q) || contains(edges,p,q)) continue;
                edges.add(new Edge(p,q));
            }
        }
        // compare the distance of edges from short to long
        edges = sort(edges);
        ArrayList<Edge> kruskal = new ArrayList<Edge>();
        Edge current;
        NameTag forest = new NameTag(points);
        while (edges.size()!=0) {
            current = edges.remove(0);
            if (forest.tag(current.p)!=forest.tag(current.q)) {
                kruskal.add(current);
                forest.reTag(forest.tag(current.p),forest.tag(current.q));
            }
        }
        return kruskal;
    }

    // Algorithme Floyd-Warshall
    public static int[][] calculShortestPaths(ArrayList<Point> points, int edgeThreshold) {
        // initialize the matrice 'pahts'
        int[][] paths = new int[points.size()][points.size()];
        for (int i = 0; i < paths.length; i++)
            for (int j = 0; j < paths.length; j++)
                paths[i][j] = i;
        double[][] dist = new double[points.size()][points.size()];
        // find shortest paths and verify the distance is smaller than 'edgeThreshold'
        for (int i = 0; i < paths.length; i++) {
            for (int j = 0; j < paths.length; j++) {
                if (i == j) {
                    dist[i][j] = 0;
                    continue;
                }
                if (points.get(i).distance(points.get(j)) <= edgeThreshold)
                    dist[i][j] = points.get(i).distance(points.get(j));
                else dist[i][j] = Double.POSITIVE_INFINITY;
                paths[i][j] = j;
            }
        }
        for (int k = 0; k < paths.length; k++) {
            for (int i = 0; i < paths.length; i++) {
                for (int j = 0; j < paths.length; j++) {
                    // replace the number in path[i][j] when we find a shorter route
                    if (dist[i][j] > dist[i][k] + dist[k][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                        paths[i][j] = paths[i][k];
                    }
                }
            }
        }
        return paths;
    }

    // get points on paths
    public  ArrayList<Integer> getPointsOnPaths(int depart, int arrive, int[][] paths){
        ArrayList<Integer> path = new ArrayList<>();
        int i = depart , j = arrive;
        // add the first point
        path.add(i);
        int pos;
        while (true){
            pos = paths[i][j];
            // it will continue until it's value is j
            if (pos != j){
                i = pos;
                path.add(pos);
            }else{
                break;
            }
        }
        // add the last point
        path.add(j);
        return path;
    }

    // heuristique 1-st version
    private ArrayList<Edge> heuristique_v1(ArrayList<Point> points, ArrayList<Point> hitPoints, int[][] paths){
        /*
        get the list edges which has the smallest total distance of hitPoints by Kruskal
        in this way , these edges may not exist in the real graph
        we should try to get the shortest paths of all edges from one side of the edge to another side
        and we add those points in these shortest paths to the list pointOnPaths
        after using Kruskal again , we can get the smallest total distance of hitPoints
        */
        // find the list 'edges' with smallest total distance
        ArrayList<Edge> edges = kruskal(hitPoints);
        ArrayList<Point> pointOnPaths = new ArrayList<>();
        for (Edge e : edges){
            int i = points.indexOf(e.getP());
            int j = points.indexOf(e.getQ());
            // find every point on the shortest path
            ArrayList<Integer> pointEntreij = getPointsOnPaths(i,j,paths);
            for (Integer k : pointEntreij){
                pointOnPaths.add(points.get(k));
            }
            //System.out.println("real distance : " + real_distance(e,points,paths));
        }
        // use kruskal to get the smallest total distance
        edges = kruskal(pointOnPaths);
        //System.out.println(distance(edges));
        return edges;
    }

    // heuristique 2-nd version
    private ArrayList<Edge> heuristique_v2(ArrayList<Point> points, ArrayList<Point> hitPoints, int[][] paths){
        ArrayList<Edge> edges;
        while (true) {
            // get the result of first version to do the comparaison with the barycentre point
            edges = heuristique_v1(points,hitPoints,paths);
            Point A = new Point(0,0);
            Point B = new Point(0,0);
            Point C = new Point(0,0);
            Double distance_v1 = distance(edges);
            for (Edge e1 : edges) {
                for (Edge e2 : edges) {
                    if (e1.equals(e2)) continue;
                    // find edges 'e1' and 'e2' that can form a triangle
                    if (e1.getP().equals(e2.getP())) {
                        A = e1.getP();
                        B = e1.getQ();
                        C = e2.getQ();
                    }
                    if (e1.getP().equals(e2.getQ())) {
                        A = e1.getP();
                        B = e1.getQ();
                        C = e2.getP();
                    }
                    if (e1.getQ().equals(e2.getP())) {
                        A = e1.getQ();
                        B = e1.getP();
                        C = e2.getQ();
                    }
                    if (e1.getQ().equals(e2.getQ())) {
                        A = e1.getQ();
                        B = e1.getP();
                        C = e2.getP();
                    }
                    // get the gravity of the triangle
                    Point barycentre = pointBarycentre(A, B, C);
                    // end if we don't find the triangle
                    if (barycentre.equals(new Point(0, 0))) continue;
                    // get the nearest point of barycentre (because it may bot exist in the graph)
                    barycentre = pointPlusProche(barycentre,points);
                    double distance_bary = barycentre.distance(A) + barycentre.distance(B) + barycentre.distance(C);
                    // compare the new distance with the older
                    if (distance_bary < A.distance(B) + A.distance(C)) {
                        hitPoints.add(barycentre);
                    }
                }
            }
            /*
            we get the new total distance and compare with the older
            if it's smaller then before , we continue this algorithme
            otherwise we shut it down
             */
            double distance_v2 = distance(heuristique_v1(points,hitPoints,paths));
            //System.out.println(distance_v2);
            if (distance_v2 < distance_v1){
                continue;
            }else{
                return edges;
            }
        }
    }

    // find the nearest point of a point given (which may not exist in the graphe)
    private Point pointPlusProche(Point pointBarycentre, ArrayList<Point> points) {
        double d1 = pointBarycentre.distance(points.get(0));
        Point ppp = new Point();
        for (Point p : points){
            double d2 = pointBarycentre.distance(p);
            if (d2 < d1){
                d1 = d2;
                ppp = p;
            }
        }
        return ppp;
    }

    // method barycentre which can find the gravity of a triangle
    public Point pointBarycentre(Point p1, Point p2, Point p3){
        return new Point((int)((p1.getX()+p2.getX()+p3.getX())/3),(int)((p1.getY()+p2.getY()+p3.getY())/3));
    }

    // calcul the total distance of all edges in the Arraylist.
    public double distance(ArrayList<Edge> edges){
        double distance = 0;
        for(Edge e : edges){
            distance += e.distance();
        }
        return distance;
    }

    // get the tree which has the most hitPoints with the start point 'root' and its total distance is under 'maxBudget'
    private ArrayList<Edge> filtre(ArrayList<Edge> edges, Point root, double maxBudget) {
        /*
        in this method , we will find the shortest path of the point 'root'
        then we will apply the same method to its successors and stop when budget is over 'maxBudget'
        'successor' stocks the points which are successors of the root point
        'rest' stocks the edges which haven't treated
        'res' allows add edges which has the same points in the 'successor'
        on start with the point correspond to 'root'
        */
        ArrayList<Edge> res = new ArrayList<Edge>();
        ArrayList<Edge> rest = new ArrayList<Edge>();
        ArrayList<Point> successor = new ArrayList<Point>();
        Edge current;
        //System.out.println("root : " + root.getX() + ", " + root.getY());
        double budget = 0;
        while (edges.size() != 0) {
            current = edges.remove(0);
            // justify each side of the edge chosen and verify the budget
            if (current.p.equals(root)) {
                if(budget + current.distance() < maxBudget) {
                    successor.add(current.q);
                    if(!res.contains(current)) {
                        res.add(current);
                    }
                    budget = budget + current.distance();
                }
            } else {
                if (current.q.equals(root)) {
                    if(budget + current.distance() < maxBudget) {
                        successor.add(current.p);
                        if(!res.contains(current)) {
                            res.add(current);
                        }
                        budget = budget + current.distance();
                    }
                } else {
                    // add the edges which haven't treated in 'rest'
                    rest.add(current);
                }
            }
        }

        // do the same to successors
        ArrayList<Edge> subres = new ArrayList<Edge>();
        for (Point suc : successor) {
            ArrayList<Edge> subEdges= filtre((ArrayList<Edge>) rest.clone(), suc, maxBudget - budget);
            for(Edge e : subEdges) {
                subres.add(e);
            }
        }
        // add 'subres' to 'res' while verify the budget
        for(Edge e : subres) {
            if(budget + e.distance() < maxBudget) {
                res.add(e);
                budget = budget + e.distance();
            }else {
                break;
            }
        }
        return res;
    }

}


class Edge {
    protected Point p,q;
    protected Edge(Point p,Point q){ this.p=p; this.q=q;}
    protected double distance(){ return p.distance(q); }
    protected Point getP(){return this.p;}
    protected Point getQ(){return this.q;}
}


class NameTag {
    private ArrayList<Point> points;
    private int[] tag;
    protected NameTag(ArrayList<Point> points){
        this.points=(ArrayList<Point>)points.clone();
        tag=new int[points.size()];
        for (int i=0;i<points.size();i++) tag[i]=i;
    }
    protected void reTag(int j, int k){
        for (int i=0;i<tag.length;i++) if (tag[i]==j) tag[i]=k;
    }
    protected int tag(Point p){
        for (int i=0;i<points.size();i++) if (p.equals(points.get(i))) return tag[i];
        return 0xBADC0DE;
    }
}
