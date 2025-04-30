module LatexCreator

open System
open System.IO
open System.Diagnostics

let runPdfLatex (texSource: string) (outputDir: string) (baseName: string) : string =
    // Write .tex file
    let texFile = Path.Combine(outputDir, baseName + ".tex")
    File.WriteAllText(texFile, texSource)

    // Setup pdflatex process
    let psi = new ProcessStartInfo()
    psi.FileName <- "pdflatex"
    psi.Arguments <- $"-interaction=nonstopmode -output-directory={outputDir} {texFile}"
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    let proc = Process.Start(psi)
    let stdout = proc.StandardOutput.ReadToEnd()
    let stderr = proc.StandardError.ReadToEnd()
    proc.WaitForExit()

    if proc.ExitCode = 0 then
        Path.Combine(outputDir, baseName + ".pdf")
    else
        failwithf "pdflatex failed:\n%s\n%s" stdout stderr
