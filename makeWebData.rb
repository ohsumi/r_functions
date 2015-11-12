
ARGF.each do |line|
  id, name, url, other = line.split("\t")
  if name.include?("_") && name.include?("/") then
    # include both "_" and "/"
    print id 
    print ","
    splited = name.split("/")
    # print all splited string.
    splited.each {|value|
      if value.include?("_") then
        i =  value.index("_")
        print value[0, i] + "," + value[i+1, value.length]
      else
        print value
        print ","
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
    print id + "," + name + ",\n"
  end
end
