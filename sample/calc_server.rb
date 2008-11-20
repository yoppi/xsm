#!/usr/bin/env ruby

require 'xmlrpc/server'

PORT = 8080
HOST = '127.0.0.1'

server = XMLRPC::Server.new(PORT, HOST)
#server = XMLRPC::CGIServer.new  # For CGI

interface = XMLRPC::Service::Interface.new("calc") do
  add_method(["double add(double, double)"], "A + B")
  add_method(["double sub(double, double)"], "A - B")
  add_method(["double multi(double, double)"], "A * B")
  add_method(["double div(double, double)"], "A / B")
end

class Calc

  def add(a, b)
    a + b
  end

  def sub(a, b)
    a - b
  end

  def multi(a, b)
    a * b
  end

  def div(a, b)
    a / b
  end

end

server.add_handler(interface, Calc.new)
server.add_multicall

server.serve
