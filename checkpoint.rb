require 'sinatra'
require 'json'

set :bind, '0.0.0.0'

get '/auth' do
  "this is a login page"
end

post '/auth' do
  headers "master_token" => "super_secret"
  "login success!"
end
