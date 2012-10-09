#!/usr/bin/ruby

require 'time'
require 'optparse'

helpdoc = <<END_HELP_DOC

yasobjc is a tool for yasnippet snippet generation for cocoa standard library.

Basic Usage: yasobjc.rb -o DIR file1 file 2 ...

-o DIR: The output directory for the snippets generated

Example: find /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS4.0.sdk/System/Library/Frameworks -name "*.h" | xargs yasobjc.rb -o ~/.emacs.d/yasnippet/text-mode/cc-mode/objc-mode

which will generate all snippets into standard yasnippet directory.

One snippet is generated per objective-c function, and the snippets are categoried by header file name. For example, all functions inside "NSString.h" will be in "NSString" category.

It's recommended to use ETAGS plus auto-complete library for better completion experience.

END_HELP_DOC


$spaces = /[\t ]*/
$word = /[_a-zA-Z0-9]+/
$type = /\((#{$spaces}#{$word}){1,3}#{$spaces}\**#{$spaces}\)/
$return_type = /#{$type}?/
$function_name = /#{$word}/
$param_type = /#{$type}/
$param_name = /#{$word}/
$param = /(#{$function_name})#{$spaces}:#{$spaces}(#{$param_type})#{$spaces}(#{$param_name})/
$function = /#{$spaces}[+-]#{$spaces}#{$return_type}#{$spaces}((#{$param}#{$spaces})+)/ 


def transform_function(definition, header_file_basename, output_dir)
  full_function_name = ""
  result = ""
  index = 1
  
  definition.scan($param).each do |param_match|
    full_function_name << ":" if not full_function_name.empty?
    full_function_name << param_match[0]
    result << " " if not result.empty?
    result << param_match[0] << ':'  << "${#{index}:#{param_match[1]}#{param_match[3]}}"
    index += 1
  end

  output_filename = full_function_name.gsub(/:/, '-')
  output_full_filename = File.expand_path(File.join(output_dir, output_filename))

  open(output_full_filename, 'w') do |f|
    f << '#name : ' << full_function_name << "\n"
    f << '#group : ' << header_file_basename << "\n"
    f << '# --' << "\n"
    f << result
  end	

end

options = {}

optparse = OptionParser.new do|opts|
  opts.banner = "Usage: yasobjc -o DIR file1 file2 ..."
  
  options[:verbose] = false
  opts.on( '-v', '--verbose', 'Output more information' ) do
    options[:verbose] = true
  end
  
  options[:outputdir] = nil
  opts.on( '-o', '--output DIR', 'Output directory' ) do|dir|
    options[:outputdir] = dir
  end
  
  opts.on( '-h', '--help', 'Display help document' ) do
    puts helpdoc
    exit
  end
end

optparse.parse!
 
puts "Start generation..." if options[:verbose]
start_time = Time.now
snippets_count = 0

# now ARGV contains header files only

ARGV.each do|f|  
  basename = File.basename(File.expand_path(f), ".h")
  puts "Processing #{basename}.h" if options[:verbose]
  
  open(File.expand_path(f), 'r').each do |line|
    function_match = $function.match(line)

    if function_match
      transform_function(function_match[2], basename, options[:outputdir])
      snippets_count += 1
    end    
  end
end

end_time = Time.now
secs = (end_time - start_time)
puts "#{snippets_count} snippets generated in #{secs} seconds.\n" if options[:verbose]

