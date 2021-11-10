package com.paracamplus.ilp2.ilp2tme5n.compiler;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Set;

import com.paracamplus.ilp1.compiler.AssignDestination;
import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.NoDestination;
import com.paracamplus.ilp1.compiler.VoidDestination;
import com.paracamplus.ilp1.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp1.compiler.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp1.compiler.interfaces.IOperatorEnvironment;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp2.ilp2tme5n.compiler.normalizer.INormalizationFactory;
import com.paracamplus.ilp2.ilp2tme5n.compiler.normalizer.NormalizationFactory;
import com.paracamplus.ilp2.ilp2tme5n.compiler.normalizer.Normalizer;
import com.paracamplus.ilp2.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp2.interfaces.IASTprogram;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTloop;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor;


public class Compiler extends com.paracamplus.ilp2.compiler.Compiler 
implements IASTvisitor<Void, Compiler.Context, CompilationException>{
    
 
    public Compiler(IOperatorEnvironment ioe, IGlobalVariableEnvironment igve) {
		super(ioe, igve);
	}
    
    //

    @Override
    public IASTCprogram normalize(IASTprogram program) 
            throws CompilationException {
        INormalizationFactory nf = new NormalizationFactory();
        Normalizer normalizer = new Normalizer(nf);
        IASTCprogram newprogram = normalizer.transform(program);
        return newprogram;
    }


    @Override
    public String compile(IASTprogram program) 
            throws CompilationException {
        
        IASTCprogram newprogram = normalize(program);
        newprogram = ((IASTCprogram) optimizer.transform(newprogram));

        GlobalVariableCollector gvc = new GlobalVariableCollector();
        Set<IASTCglobalVariable> gvs = gvc.analyze(newprogram);
        newprogram.setGlobalVariables(gvs);
        
        FreeVariableCollector fvc = new FreeVariableCollector(newprogram);
        newprogram = (fvc.analyze());
      
        Context context = new Context(NoDestination.NO_DESTINATION);
        StringWriter sw = new StringWriter();
        try {
            out = new BufferedWriter(sw);
            visit(newprogram, context);
            out.flush();
        } catch (IOException exc) {
            throw new CompilationException(exc);
        }
        return sw.toString();
    }

    //
    
   
	@Override
	public Void visit(IASTbreak iast, Context data) throws CompilationException {
		if (iast.getLabel().isBlank()) {
			emit("break;\n");
		}else {
			emit("goto" + iast.getLabel() + ";");
		}
		return null;
	}

	@Override
	public Void visit(IASTcontinue iast, Context data)
			throws CompilationException {
		if (iast.getLabel().isBlank()) {
			emit("continue;\n");
		}else {
			emit("goto" + iast.getLabel() + ";");
		}
		return null;
	}

	@Override
	public Void visit(IASTloop iast, Context context)
            throws CompilationException {
        emit("while ( 1 ) { \n");
        IASTvariable tmp = context.newTemporaryVariable();
        emit("  ILP_Object " + tmp.getMangledName() + "; \n");
        
        if(!iast.getLabel().isBlank()) {
        	emit(iast.getLabel() + ":\n");
        }
        
        Context c = context.redirect(new AssignDestination(tmp));
        iast.getCondition().accept(this, c);
        emit("  if ( ILP_isEquivalentToTrue(");
        emit(tmp.getMangledName());
        emit(") ) {\n");
        Context cb = context.redirect(VoidDestination.VOID_DESTINATION);
        iast.getBody().accept(this, cb);
        emit("\n} else { \n");
        emit("    break; \n");
        emit("\n}\n}\n");
        
        if(!iast.getLabel().isBlank()) {
        	emit(iast.getLabel()+"break : \n");
        }
        
        whatever.accept(this, context);
        return null;
    }
    
  


}