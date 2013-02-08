EOF = nil

$buf = []
def getchar
  $buf += ($stdin.gets || '').split('') if $buf.empty?
  $buf.shift
end
def isspace c; c =~ /^\s$/ end
def isalpha c; c =~ /^[a-zA-Z]$/ end
def isalnum c; c =~ /^[a-zA-Z0-9]$/ end
def isdigit c; c =~ /^[0-9]$/ end
def isascii c
  if c.is_a? String
    code = c.bytes.to_a.first
    0 <= code and code <= 0x7f
  else
    false
  end
end
