package com.paracamplus.ilp2.ilp2tme5n.compiler.normalizer;


import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.normalizer.INormalizationEnvironment;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTloop;

public class Normalizer 
extends com.paracamplus.ilp2.compiler.normalizer.Normalizer 
implements 
 IASTvisitor<IASTexpression, INormalizationEnvironment, CompilationException> {

    public Normalizer (INormalizationFactory factory) {
    	super(factory);
    }

	@Override
	public IASTexpression visit(IASTbreak iast, INormalizationEnvironment data) throws CompilationException {
		// TODO Auto-generated method stub
		return ((INormalizationFactory)factory).newBreak(iast.getLabel());
	}

	@Override
	public IASTexpression visit(IASTcontinue iast, INormalizationEnvironment data) throws CompilationException {
		// TODO Auto-generated method stub
		return ((INormalizationFactory)factory).newContinue(iast.getLabel());
	}

	@Override
	public IASTexpression visit(IASTloop iast, INormalizationEnvironment data) throws CompilationException {
		// TODO Auto-generated method stub
		String label = iast.getLabel();
		IASTexpression condition = iast.getCondition();
		IASTexpression body = iast.getBody();
		return ((INormalizationFactory)factory).newLoop(label,condition,body);
	}
    
    
}