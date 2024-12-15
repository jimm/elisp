#!/usr/bin/env ruby
#
# usage: hexlify.rb [-de]
#
# Hexlify or dehexlify stdin.

def hexlify
  offset = 0
  while (str = $stdin.read(16))
    out = "#{'%08x' % offset} "
    out << hex_digits(str)
    out << '  '
    out << ascii(str)
    puts out
    offset += 16
  end
end

def hex_digits(str)
  offset = 0
  out = ''
  str.each_byte { | c |
    out << ' ' if offset == 0
    out << '%02x' % c
    offset = (offset == 0 ? 1 : 0)
  }
  '%-40s' % out
end

def ascii(str)
  out = ''
  str.each_byte { | c | out << (printable?(c) ? c.chr : '.') }
  out
end

def printable?(c)
  c >= 32 && c < 127
end

def dehexlify
  while (str = $stdin.gets)
    str = str[/  .*  /].gsub(/ /, '')
    print str.gsub(/(..)/) { '%c' % $1.to_i(16) }
  end
end

if ARGV[0] == '-de'
  dehexlify
else
  hexlify
end
