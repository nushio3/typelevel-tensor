11.times{|n|

  args = (0...n).map{|i| "x#{i}"}
  code = (['Vec']+ args).join(' :~ ')

  puts <<HS
vec#{n} :: #{'a -> '*n}Vec#{n} a
vec#{n} #{args.join(' ')} = #{code}
HS

}
