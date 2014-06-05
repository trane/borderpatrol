require 'rubygems'
require 'rack'

Rack::Server.start :app => Proc.new {|env|
    # pick http headers
    http_headers = env.select { |key, value| !!(key.to_s =~ /(HTTP).+/i) }
    response_code = 200
    response_code = 401 if http_headers['HTTP_MASTER_TOKEN'].nil?
    req_body = env['rack.input']
    resp_body = "Headers: "
    http_headers.each { |k,v|
        resp_body << k.gsub("HTTP_", "")
        resp_body << ":#{v}"
        resp_body << ","
    }
    resp_body << "\nRequest Body= #{req_body}"
    p "request received = #{resp_body}"
    [response_code, {"Content-Type" => "text/html"}, [resp_body]]
}, :Port => 9292
