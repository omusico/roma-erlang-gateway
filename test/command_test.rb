#!/usr/bin/env ruby
require 'socket'

if ARGV.length < 1
  puts "usage:sample address:port"
  exit
end

addr = ARGV[0].split(/:/)
puts "#{addr}"

def version(socket)
  puts "[version]"
  socket.write "version\r\n" 
  puts socket.gets
  puts ""
end
def add(socket)
  puts "[add]"
  socket.write "add key1 0 0 4\r\nroma\r\n" 
  puts socket.gets
  puts ""
end
def set(socket)
  puts "[set]"
  socket.write "set key1 0 0 5\r\nitaly\r\n" 
  puts socket.gets
  puts ""
end
def get(socket)
  puts "[get]"
  socket.write "get key1\r\n" 
  puts socket.gets
  puts socket.gets
  puts socket.gets
  puts ""
end
def delete(socket)
  puts "[delete]"
  socket.write "delete key1\r\n" 
  puts socket.gets
  puts ""
end


def alist_insert(socket)
  puts "[alist_insert]"
  socket.write "alist_insert list1 0 5\r\nwhite\r\n" 
  puts socket.gets

  socket.write "alist_insert list1 1 6\r\nyellow\r\n" 
  puts socket.gets

  socket.write "alist_insert list1 2 4\r\nblue\r\n" 
  puts socket.gets
  puts ""
end
def alist_first(socket)
  puts "[alist_first]"
  socket.write "alist_first list1\r\n" 
  puts socket.gets
  puts socket.gets
  puts socket.gets
  puts ""
end
def alist_last(socket)
  puts "[alist_last]"
  socket.write "alist_last list1\r\n" 
  puts socket.gets
  puts socket.gets
  puts socket.gets
  puts ""
end
def alist_index(socket)
  puts "[alist_index]"
  socket.write "alist_index list1 4\r\nblue\r\n"
  puts socket.gets
  puts ""
end
def alist_delete_at(socket)
  puts "[alist_delete_at]"
  socket.write "alist_delete_at list1 2\r\n" 
  puts socket.gets
  puts ""
end
def alist_join(socket)
  puts "[alist_join]"
  socket.write "alist_join list1 1\r\n,\r\n"
  puts socket.gets
  puts socket.gets
  puts socket.gets
  puts socket.gets
  puts ""
end

socket = TCPSocket.open(addr[0], addr[1].to_i)

## basic commands #
add(socket)
get(socket)
set(socket)
get(socket)
delete(socket)

## alist commands #
alist_insert(socket)
alist_first(socket)
alist_last(socket)
alist_index(socket)
alist_delete_at(socket)
alist_join(socket)

socket.close

