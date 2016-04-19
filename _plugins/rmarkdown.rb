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
      #content = `~/bin/R --no-save --args temp.Rmd < _plugins/knit.R`
      
      if $?.exitstatus != 0
        raise "Knitting failed" 
      end

      # Use sub here to get rid of any jquery, so we don't load it
      # twice (it is already included in _layouts/default.html). Some
      # pages have animints so there will be a jquery script tag in
      # the content, and some pages do not (for example Ch00-preface
      # which does not have any animints).
      content.sub %r{<script type="text/javascript" src="[^/]+/vendor/jquery-1.11.3.min.js"></script>}, ""
      # File.unlink f.path
    end
  end
end
