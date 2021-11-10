package com.paracamplus.ilp2.ilp2tme5n.compiler.normalizer;

import com.paracamplus.ilp2.ilp2tme5n.ast.ASTbreak;
import com.paracamplus.ilp2.ilp2tme5n.ast.ASTcontinue;
import com.paracamplus.ilp2.ilp2tme5n.ast.ASTloop;
import com.paracamplus.ilp1.interfaces.IASTexpression;

public class NormalizationFactory
extends com.paracamplus.ilp2.compiler.normalizer.NormalizationFactory
implements INormalizationFactory {

	@Override
	public IASTexpression newContinue(String label) {
		// TODO Auto-generated method stub
		return new ASTcontinue(label);
	}

	@Override
	public IASTexpression newBreak(String label) {
		// TODO Auto-generated method stub
		return new ASTbreak(label);
	}

	@Override
	public IASTexpression newLoop(String label, IASTexpression condition, IASTexpression body) {
		// TODO Auto-generated method stub
		return new ASTloop(label,condition,body);
	}


}
