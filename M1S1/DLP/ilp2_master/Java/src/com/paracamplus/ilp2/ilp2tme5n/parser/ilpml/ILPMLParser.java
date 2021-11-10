package com.paracamplus.ilp2.ilp2tme5n.parser.ilpml;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import antlr4.ILPMLgrammar2tme5nLexer;
import antlr4.ILPMLgrammar2tme5nParser;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTfactory;
import com.paracamplus.ilp2.interfaces.IASTprogram;
import com.paracamplus.ilp1.parser.ParseException;

public class ILPMLParser
extends com.paracamplus.ilp1.parser.ilpml.ILPMLParser {
	
	public ILPMLParser(IASTfactory factory) {
		super(factory);
	}
		
	@Override
    public IASTprogram getProgram() throws ParseException {
		try {
			ANTLRInputStream in = new ANTLRInputStream(input.getText());
			// flux de caractères -> analyseur lexical
			ILPMLgrammar2tme5nLexer lexer = new ILPMLgrammar2tme5nLexer(in);
			// analyseur lexical -> flux de tokens
			CommonTokenStream tokens =	new CommonTokenStream(lexer);
			// flux tokens -> analyseur syntaxique
			ILPMLgrammar2tme5nParser parser = new ILPMLgrammar2tme5nParser(tokens);
			// démarage de l'analyse syntaxique
			ILPMLgrammar2tme5nParser.ProgContext tree = parser.prog();		
			// parcours de l'arbre syntaxique et appels du Listener
			ParseTreeWalker walker = new ParseTreeWalker();
			ILPMLListener extractor1 = new ILPMLListener((IASTfactory)factory);
			//ILPMLListener extractor3 = new ILPMLListener((IASTfactory)factory);
			walker.walk(extractor1, tree);
			//walker.walk(extractor3, tree);
			return tree.node;
		} catch (Exception e) {
			throw new ParseException(e);
		}
    }

}