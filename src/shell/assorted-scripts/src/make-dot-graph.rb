#!/usr/bin/env ruby

puts "digraph {"

ARGF.each do |line|

  if (line =~ /(.*?)\s*=\s*(.*)$/)
    puts("#{$1}[label=\"#{$2}\"];")
  elsif (line =~ /(.*?)\s*:\s*(.*)$/)
    puts("#{$1}[shape=box,label=\"#{$2}\"];")
  else
    puts(line)
  end
end

puts "}"
