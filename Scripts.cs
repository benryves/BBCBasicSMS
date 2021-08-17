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
	
	public static void BCall(Compiler compiler, string label) {
		compiler.WriteDynamicOutput(3, delegate(Compiler.DynamicOutputData d) {
			d.Data = new byte[3];
			Label Target = compiler.Labels[label];
			if (Target.Page == compiler.Labels.ProgramCounter.Page) {
				d.Data[0] = 0xCD; // CALL NN
			} else if (Target.Page == 2) {
				d.Data[0] = 0xCF; // RST $08
			}
			d.Data[1] = (byte)(Target.NumericValue);
			d.Data[2] = (byte)((int)(Target.NumericValue) >> 8);
		});
	}
	
	
}