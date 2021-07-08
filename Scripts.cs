using System;
using System.IO;

using BeeDevelopment.Brass3;

public class BbcBasicScripts {
	
	private static void CreateConstant(Compiler compiler, string name, double value) {
		compiler.Labels.Add(new Label(compiler.Labels, new TokenisedSource.Token(compiler.Labels.ModuleGetFullLabelPath(name)), true, value, compiler.Labels.ProgramCounter.Page, null));
	}
	
	public static void IncSym(Compiler compiler, string filename, string labelPrefix) {
		string[] SymComponents = File.ReadAllText(compiler.ResolveFilename(filename)).Split((char)0x1A)[0].Split(new char[] { ' ', '\r', '\n', '\t' }, StringSplitOptions.RemoveEmptyEntries);
		for (int i = 0; i < SymComponents.Length; i += 2) {
			CreateConstant(compiler, labelPrefix + SymComponents[i + 1], int.Parse(SymComponents[i + 0], System.Globalization.NumberStyles.HexNumber));
		}
	}
	
	
}