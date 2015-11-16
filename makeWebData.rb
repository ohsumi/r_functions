
ARGF.each do |line|
  id, name, url, other = line.split("\t")
  # an irregular processing.
  if /(.*)(http.*)/.match(name) then
    # get the company name and remove useless white spaces.
    name = $1.strip
  end
  # main
  if name.include?("_") && name.include?("/") then
    # include both "_" and "/".
    print id + ','
    splited = name.split("/")
    # print all splited string.
    splited.each {|value|
      if value.include?("_") then
        i =  value.index("_")
        print value[0, i] + "," + value[i+1, value.length]
      else
        print value + ','
      end
    }
    print ",\n"
  elsif name.include?("_") && (name.include?("/") == FALSE) then
    # include only "_"
    print id + ","
    i = name.index("_")
    print name[0, i] + "," + name[i + 1, name.length] + ",\n"
  elsif (name.include?("_") == FALSE) && name.include?("/") then
    # include only "/"
    print id + ","
    i = name.index("/")
    print name[0, i] + "," + name[i+1, name.length] + ",\n"
  else
    # include neither "_" nor "/"
    print id + "," + name.chomp + ",\n"
  end
end
