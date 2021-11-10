package com.paracamplus.ilp2.ilp2tme5.interpreter;

import com.paracamplus.ilp2.interfaces.IASTloop;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTvisitor;
import com.paracamplus.ilp1.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp1.interpreter.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp1.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp1.interpreter.interfaces.IOperatorEnvironment;


public class Interpreter extends com.paracamplus.ilp2.interpreter.Interpreter
implements IASTvisitor<Object, ILexicalEnvironment, EvaluationException> {

	public Interpreter(IGlobalVariableEnvironment globalVariableEnvironment, IOperatorEnvironment operatorEnvironment) {
		super(globalVariableEnvironment, operatorEnvironment);
		// TODO Auto-generated constructor stub
	}

    @Override
	public Object visit(IASTloop iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        while ( true ) {
            Object condition = iast.getCondition().accept(this, lexenv);
            if ( condition instanceof Boolean ) {
                Boolean c = (Boolean) condition;
                if ( ! c ) {
                    break;
                }
            }
            try {
            	iast.getBody().accept(this, lexenv);
            }catch (BreakException br) {
            	break;
            }catch (ContinueException co) {
            	System.out.println("Continue.");
            	continue;
            }
        }
        return Boolean.FALSE;
    }
	
	@Override
	public Object visit(IASTbreak iast, ILexicalEnvironment data) throws EvaluationException {
		// TODO Auto-generated method stub
		throw new BreakException("break");
	}

	@Override
	public Object visit(IASTcontinue iast, ILexicalEnvironment data) throws EvaluationException {
		// TODO Auto-generated method stub
		throw new ContinueException("continue");
	}
	
}
