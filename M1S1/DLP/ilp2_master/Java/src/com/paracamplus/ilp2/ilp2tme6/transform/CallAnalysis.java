package com.paracamplus.ilp2.ilp2tme6.transform;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.interfaces.IASTalternative;
import com.paracamplus.ilp1.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp1.interfaces.IASTblock;
import com.paracamplus.ilp1.interfaces.IASTboolean;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTfloat;
import com.paracamplus.ilp1.interfaces.IASTinteger;
import com.paracamplus.ilp1.interfaces.IASTinvocation;
import com.paracamplus.ilp1.interfaces.IASTsequence;
import com.paracamplus.ilp1.interfaces.IASTstring;
import com.paracamplus.ilp1.interfaces.IASTunaryOperation;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp2.interfaces.IASTassignment;
import com.paracamplus.ilp2.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp2.interfaces.IASTloop;
import com.paracamplus.ilp2.interfaces.IASTprogram;
import com.paracamplus.ilp2.interfaces.IASTvisitor;

public class CallAnalysis implements IASTvisitor<Void, Set<String>, CompilationException>{
	protected Map<String, Set<String>> recur;
	
	public boolean isRecursive(IASTvariable f){
		return recur.containsKey(f.getName());
	}
	
	
	public Void visit(IASTprogram iast) throws CompilationException {
	
		Map<String, Set<String>> recur = new HashMap<>();

		for(IASTfunctionDefinition f: iast.getFunctionDefinitions()){
			Set<String> set = new HashSet<>();
			visit(f, set);
			recur.put(f.getFunctionVariable().getName(), set);
			System.out.print(isRecursive(f.getFunctionVariable()));
		}
		return null;
	}
	
	private Void visit(IASTfunctionDefinition iast, Set<String> data) throws CompilationException {
		// TODO Auto-generated method stub
		for (IASTexpression expr : iast.getVariables()) {
			expr.accept(this, data);
		}
		return null;
	}

	@Override
	public Void visit(IASTalternative iast, Set<String> data)
			throws CompilationException {
		iast.getCondition().accept(this, data);
		iast.getConsequence().accept(this, data);
		iast.getAlternant().accept(this, data);
		return null;
	}

	@Override
	public Void visit(IASTbinaryOperation iast, Set<String> data)
			throws CompilationException {
		iast.getLeftOperand().accept(this, data);
        iast.getRightOperand().accept(this, data);
		return null;
	}

	@Override
	public Void visit(IASTblock iast, Set<String> data)
			throws CompilationException {
		for ( IASTbinding binding : iast.getBindings() ) {
			binding.getInitialisation().accept(this, data);
        }
		iast.getBody().accept(this, data);
		return null;
	}

	@Override
	public Void visit(IASTboolean iast, Set<String> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(IASTfloat iast, Set<String> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(IASTinteger iast, Set<String> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(IASTinvocation iast, Set<String> data)
			throws CompilationException {
		
		for(IASTexpression e: iast.getArguments()){
			e.accept(this, data);
		}
		
		IASTexpression var = iast.getFunction();
		if(var instanceof IASTvariable){
			data.add(((IASTvariable)var).getName());
		}
		return null;
	}

	@Override
	public Void visit(IASTsequence iast, Set<String> data)
			throws CompilationException {
		IASTexpression[] expressions = iast.getExpressions();
        for ( IASTexpression e : expressions ) {
            e.accept(this, data);
        }
		return null;
	}

	@Override
	public Void visit(IASTstring iast, Set<String> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public Void visit(IASTunaryOperation iast, Set<String> data)
			throws CompilationException {
		 iast.getOperand().accept(this, data);
		 return null;
	}

	@Override
	public Void visit(IASTvariable iast, Set<String> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(IASTassignment iast, Set<String> data)
			throws CompilationException {
		iast.getExpression().accept(this, data);
		return null;
	}

	@Override
	public Void visit(IASTloop iast, Set<String> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		iast.getCondition().accept(this, data);
		iast.getBody().accept(this, data);
		return null;
	}
	
}