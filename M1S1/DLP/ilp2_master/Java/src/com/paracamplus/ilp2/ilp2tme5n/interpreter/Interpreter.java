package com.paracamplus.ilp2.ilp2tme5n.interpreter;

import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTloop;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor;

import java.util.ArrayList;

import com.paracamplus.ilp1.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp1.interpreter.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp1.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp1.interpreter.interfaces.IOperatorEnvironment;


public class Interpreter extends com.paracamplus.ilp2.interpreter.Interpreter
implements IASTvisitor<Object, ILexicalEnvironment, EvaluationException> {
	
	private ArrayList<String> labels;

	public Interpreter(IGlobalVariableEnvironment globalVariableEnvironment, IOperatorEnvironment operatorEnvironment) {
		super(globalVariableEnvironment, operatorEnvironment);
		// TODO Auto-generated constructor stub
		labels = new ArrayList<>();
	}

    @Override
	public Object visit(IASTloop iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
    	if (!iast.getLabel().isBlank()) {
    		labels.add(iast.getLabel());
    	}
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
            	String msg = br.getMessage();
            	if (msg.equals(iast.getLabel())||msg.equals("break")) {
            		labels.remove(labels.lastIndexOf(iast.getLabel()));
            		break;
            	}
            	int i = labels.lastIndexOf(msg);
            	if (i == -1) throw new EvaluationException("Label Undefined!");
            	labels.remove(labels.lastIndexOf(iast.getLabel()));
            	throw new BreakException(msg);
            }catch (ContinueException co) {
            	String msg = co.getMessage();
            	if (msg.equals(iast.getLabel())||msg.equals("continue")) {
            		continue;
            	}
            	int i = labels.lastIndexOf(msg);
            	if (i == -1) throw new EvaluationException("Label Undefined!");
            	labels.remove(labels.lastIndexOf(iast.getLabel()));
            	throw new ContinueException(msg);
            }
        }
        return Boolean.FALSE;
    }
	
	@Override
	public Object visit(IASTbreak iast, ILexicalEnvironment data) throws EvaluationException {
		// TODO Auto-generated method stub
		if (iast.getLabel().isBlank()) {
			throw new BreakException("break");
		}else {
			throw new BreakException(iast.getLabel());
		}
	}

	@Override
	public Object visit(IASTcontinue iast, ILexicalEnvironment data) throws EvaluationException {
		// TODO Auto-generated method stub
		if (iast.getLabel().isBlank()) {
			throw new ContinueException("continue");
		}else {
			throw new ContinueException(iast.getLabel());
		}
	}
	
}