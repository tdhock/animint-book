require 'tempfile'

module Jekyll
  class RMarkdownConverter < Converter
    safe :false
    priority :high

    def matches(ext)
      ext =~ /^\.Rmd$/
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      f = File.new("temp.Rmd", "w")
      f.write(content)
      f.write("\n")
      f.flush
      STDOUT.puts File.expand_path(__FILE__)
      STDOUT.puts Dir.pwd
      # http://rubyquicktips.com/post/5862861056/execute-shell-commands
      #system("~/bin/Rscript _plugins/knit.R temp.Rmd")
      content = `~/bin/Rscript _plugins/knit.R temp.Rmd`
      
      if $?.exitstatus != 0
        raise "Knitting failed" 
      end
      
      content
      # File.unlink f.path
    end
  end
end
