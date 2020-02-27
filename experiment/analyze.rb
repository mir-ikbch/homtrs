#!ruby

CmpDir   = 'complete'
RawDir    = 'raw'
ResDir = 'result'

RawURL = 'https://raw.githubusercontent.com/mir-ikbch/homtrs/master/experiment/raw'
CompURL = 'https://raw.githubusercontent.com/mir-ikbch/homtrs/master/experiment/complete'

OutHtml  = 'result.html'
OutLaTeX = 'result.tex'

# Find all TRS
names = []
Dir.open(RawDir).each do |fname|
  names << $` if fname =~ /\.trs$/
end

# Count the number of rules
def count_rules(text)
  if text =~ /->/ then 
    return text.scan('->').size
  else
    return text.count('.')
  end
end
num_raw = {}
#num_cmp = {}
results = {}
names.each do |name|
  data = {}
  STDERR.puts'Processing %s ...'%name

  # to count for raw/
  text = open('%s/%s.trs'%[RawDir,name]).read
  if text =~ /\(RULES\s*(.+)\)/im then
    text = $&
  end
  data[:rule_raw] = count_rules(text)

  # to count for complete/
  text = open('%s/%s.dat'%[CmpDir,name]).read
  num_cmp = count_rules(text)
  # num_cmp[name] = count_rules(text)

  # to analyze the result files
  text = open('%s/%s.dat'%[ResDir,name]).read
  text.split(/\s*[,\n]\s*/).each do |line|
    # p line
    case line
    when /^degree\s*=\s*(\d+)$/
      data[:degree] = $1.to_i
    when /^#symbol\s*=\s*(\d+)$/
      data[:symbol] = $1.to_i
    when /^#rule\s*=\s*(\d+)$/
      data[:rule_cmp] = $1.to_i
    when /^#cp\s*=\s*(\d+)$/
      data[:cp] = $1.to_i
    when /^(dim|s)\(H2\)\s*=\s*(\d+)$/
      data[:s] = $2.to_i
    when /^#rule-e\(R\)\s*=\s*(\d+)$/
      data[:rule_eR] = $1.to_i
    end
  end

 # to check the validity of data
  abort'degree not found' unless data[:degree]
  abort'counting rules before completion failed.' unless data[:rule_raw]
  if data[:symbol] then
    abort'#rule not found' unless data[:rule_cmp]
    abort'counting rules after completion failed.' if num_cmp != data[:rule_cmp]
    abort'#cp not found' unless data[:cp]
    abort'neither s(H2) nor dim(H2) found' unless data[:s]
    abort'#rule-e(R) not found' unless data[:rule_eR]
  end
  data[:rule_cmp] = num_cmp
  results[name] = data
end

# Output HTML 
def puts_html_table_line(out,data,name)
  out.puts <<-HTML
  <td>#{data[:degree]}</td>
  <td>#{data[:rule_raw]}</td>
  <td><a href="#{CompURL}/#{name}.dat">#{data[:rule_cmp]}</a></td>
  HTML
  if data[:symbol] then
    out.puts <<-HTML
  <!-- td>#{data[:symbol]}</td -->
  <!-- td>#{data[:cp]}</td -->
  <td>#{data[:s]}</td>
  <td>#{data[:rule_eR]}</td>
    HTML
  else
    out.puts <<-HTML
  <td colspan="2">N/A</td>
    HTML
  end
end

def output_html(results)
  out = open(OutHtml,'w')
  out.puts <<-HTML
<html>
<head><title>Homtrs - Experimental Results</title></head>
<body>
<table border="1">
<tr>
  <td>name</td>
  <td>degree</td>
  <!-- td>#symbol</td -->
  <td>#rule_before</td>
  <td>#rule_after</td>
  <!-- td>#cp</td -->
  <td>s(H<sub>2</sub>)</td>
  <td>#rule_after - e(R)</td>
</tr>
  HTML
  results.sort.each do |name,data|
    out.puts <<-HTML
<tr>
  <td><a href="#{RawURL}/#{name}.trs">#{name}</a></td>
    HTML
    puts_html_table_line(out,data,name)
    out.puts <<-HTML
</tr>
    HTML
  end
  out.puts <<-HTML
</table>
</body>
</html>
  HTML
  out.close
end

output_html(results)

# Output LaTeX
def puts_latex_table_line(out,data)
  out.puts <<-LaTeX
  #{data[:degree]} & #{data[:rule_raw]} &
  % #{data[:symbol]||'--'} & 
  #{data[:rule_cmp]||'--'} &
  % #{data[:cp]||'--'} &
  #{data[:s]||'--'} & #{data[:rule_eR]||'--'}
  LaTeX
end

def output_latex(results)
  out = open(OutLaTeX,'w')
  out.puts <<-LaTeX
\\begin{tabular}{lrrrrr}
name & degree & % \\(\\#\mathit{symbol}\\) &
\\(\\#\\mathit{rule}_{\\mathit{before}}\\) &
\\(\\#\\mathit{rule}_{\\mathit{after}}\\) &
\\(s(H_2)\\) &
\\(\\#\\mathit{rule}_{\\mathit{after}}-e(R)\\)
  LaTeX
  first = true          
  results.sort.each do |name,data|
    out.puts '\\\\'
    if first then out.puts '\\hline'; first = false end
    out.puts <<-LaTeX
\\texttt{#{name.gsub('_','\\_')}} &
    LaTeX
    puts_latex_table_line(out,data)
  end            
  out.puts <<-LaTeX
\\end{tabular}
  LaTeX
  out.close
end

output_latex(results)
