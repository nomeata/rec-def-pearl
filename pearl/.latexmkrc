# latexmk support for lhs2TeX
$pdflatex = 'lhs2TeX %S -o %R.lhs_out; lualatex %O %R.lhs_out';
$pdf_mode = 1;
$postscript_mode = $dvi_mode = 0;

