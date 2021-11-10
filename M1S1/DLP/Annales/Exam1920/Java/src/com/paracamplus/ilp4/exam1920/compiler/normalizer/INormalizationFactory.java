package com.paracamplus.ilp4.exam1920.compiler.normalizer;

import com.paracamplus.ilp1.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCforeach;

public interface INormalizationFactory extends com.paracamplus.ilp4.compiler.normalizer.INormalizationFactory {
	IASTCforeach newForeach(IASTCvariable variable,  IASTexpression iterator, IASTexpression body);
}
