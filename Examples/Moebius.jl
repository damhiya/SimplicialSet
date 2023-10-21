moebius(u,v) = [ (1 + u * cos(v/2)) * cos(v)
               , (1 + u * cos(v/2)) * sin(v)
               , u * sin(v/2)
               ]

area(n) = sum([sqrt((1 + u * cos(v/2))^2 + (u/2)^2) * (2π/n) * (1/n) for u = range(-0.5,0.5,n+1)[1:end-1], v = range(0,2π,n+1)[1:end-1]])
# 6.35327

function main()
    n = parse(Int, readline())
    println("($(n*(n+1)), $(n*n*2))")
    
    u(i) = -0.5 + 1 / n * i
    v(j) = 2π / n * j
    vertices(i,j) = moebius(u(i),v(j))
    ι(i,j) = j == n ? n*(n-i) : (n*i + j)

    for i = 0:n
        for j = 0:n-1
            v0 = vertices(i,j)
            println("($(v0[1]), $(v0[2]), $(v0[3]))")
        end
    end

    for i = 0:n-1
        for j = 0:n-1
            i′ = i+1
            j′ = j+1
            println("($(ι(i,j )), $(ι(i′,j)), $(ι(i ,j′)))")
            println("($(ι(i,j′)), $(ι(i′,j)), $(ι(i′,j′)))")
        end
    end
end

main()