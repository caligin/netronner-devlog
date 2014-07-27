base_path = "vars.properties"
overrides_path = ENV["HOME"] + "/.vars.properties"
source = File.file? overrides_path ? overrides_path : base_path 

File.open(source, "r") do |file|
    file.each_line do |line|
         variable, value = line.strip.split(/\s*=\s*/)
        `sed -i 's|@@#{variable}@@|#{value}|g' target/site/index.html`
    end
end