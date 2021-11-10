import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Triangle {

private Map<Integer, Set<Integer>> graph;
private Map<Integer, Set<Integer>> pregraph;


public Triangle (Map<Integer, Set<Integer>> graph) {
	this.graph = graph;
	
	Map<Integer, Set<Integer>> res = new HashMap<Integer, Set<Integer>>();
	//traitement
	for(int i : graph.keySet()) {
		Set<Integer> to = new HashSet<>();
		for(int j : graph.get(i)) {
			if(j > i) {
				to.add(j);
			}
		}
		res.put(i, to);
	}
	this.pregraph = res;
}

public Map<Integer, Set<Integer>> list_triangle(){
	Map<Integer, Set<Integer>> res = new HashMap<Integer, Set<Integer>>();
	int nb = 1;
	
	for(int i : pregraph.keySet()) {
		if(pregraph.get(i).size()>=2) {
			for(int j = 0; j< pregraph.get(i).size()-1;j++) {
				for (int k = 1; k < pregraph.get(i).size();k++) {
					int n1 = (int) pregraph.get(i).toArray()[j];
					int n2 = (int) pregraph.get(i).toArray()[k];
					if(pregraph.get(n1).contains(n2)) {
						Set<Integer> to = new HashSet<>();
						to.add(i);
						to.add(n1);
						to.add(n2);
						res.put(nb, to);
						nb++;
					}
				}
				
			}
		}
		
	}
	return res;
	
}

public Map<Integer, Set<Integer>> getpre(){
	return this.pregraph;
}
	
	
}
