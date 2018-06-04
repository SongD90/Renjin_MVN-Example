package renjinExamaple_mvn.mvnTest_1;

import javax.script.*;
import org.renjin.script.*;
import org.renjin.eval.*;
import java.io.*;

public class RenjinExample1 {
	public static void main(String[] args) throws Exception{

	    Session session = new SessionBuilder()
                .withDefaultPackages()
                .build();

		// script engin manager
		RenjinScriptEngineFactory factory = new RenjinScriptEngineFactory();
		// Renjin engine
		ScriptEngine engine = factory.getScriptEngine();
		// Store pirnt() output text to a java string
//        StringWriter outptuWriter = new StringWriter();
//		engine.getContext().setWriter(outptuWriter);
		// Reset output to console
         engine.getContext().setWriter(new PrintWriter(System.out));
		
		engine.eval(new java.io.FileReader("C:\\Users\\hsong\\eclipse-workspace\\mvnTest_1_git\\Renjin_MVN-Example\\mvnTest_1\\R script\\MTGMacro.r"));


	//2. MTG_LoanData.r example ( data.table package is not working.)
		engine.eval(new java.io.FileReader("C:\\Users\\hsong\\eclipse-workspace\\mvnTest_1_git\\Renjin_MVN-Example\\mvnTest_1\\R script\\exampleRCode.r"));
	//3. mortgage_v1.0 r example. ( don't have library() )
		engine.eval(new java.io.FileReader("C:\\Users\\hsong\\eclipse-workspace\\mvnTest_1_git\\Renjin_MVN-Example\\mvnTest_1\\R script\\MTG01.r"));

	//4. package loading test. 5/31 failed
        engine.eval(new java.io.FileReader("C:\\Users\\hsong\\eclipse-workspace\\mvnTest_1_git\\Renjin_MVN-Example\\mvnTest_1\\R script\\Tutorial_dplyr.r"));
	}
}
