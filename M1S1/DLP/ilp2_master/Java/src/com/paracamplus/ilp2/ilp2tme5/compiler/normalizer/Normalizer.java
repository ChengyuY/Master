package com.paracamplus.ilp2.ilp2tme5.compiler.normalizer;


import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.normalizer.INormalizationEnvironment;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTvisitor;

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
		return ((INormalizationFactory)factory).newBreak();
	}

	@Override
	public IASTexpression visit(IASTcontinue iast, INormalizationEnvironment data) throws CompilationException {
		// TODO Auto-generated method stub
		return ((INormalizationFactory)factory).newContinue();
	}
    
    
}