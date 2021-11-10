package com.paracamplus.ilp2.ilp2tme5.compiler.normalizer;

import com.paracamplus.ilp2.ilp2tme5.ast.ASTbreak;
import com.paracamplus.ilp2.ilp2tme5.ast.ASTcontinue;
import com.paracamplus.ilp1.interfaces.IASTexpression;

public class NormalizationFactory
extends com.paracamplus.ilp2.compiler.normalizer.NormalizationFactory
implements INormalizationFactory {

	@Override
	public IASTexpression newContinue() {
		// TODO Auto-generated method stub
		return new ASTcontinue();
	}

	@Override
	public IASTexpression newBreak() {
		// TODO Auto-generated method stub
		return new ASTbreak();
	}


}
