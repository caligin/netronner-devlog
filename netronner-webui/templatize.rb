File.open("vars.properties", "r") do |file|
    file.each_line do |line|
         variable, value = line.strip.split(/\s*=\s*/)
        `sed -i 's|@@#{variable}@@|#{value}|g' target/site/index.html`
    end
end