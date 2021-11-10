package com.paracamplus.ilp4.exam1920.compiler.normalizer;

import com.paracamplus.ilp1.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp4.exam1920.compiler.ast.ASTCforeach;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCforeach;

public class NormalizationFactory extends com.paracamplus.ilp4.compiler.normalizer.NormalizationFactory
		implements INormalizationFactory {

	@Override
	public IASTCforeach newForeach(IASTCvariable variable, IASTexpression iterator, IASTexpression body) {
		return new ASTCforeach(variable, iterator, body);
	}

}
