
torus(a,b,ϕ,θ) = [ a * cos(ϕ) + b * cos(ϕ) * cos(θ)
                 , a * sin(ϕ) + b * sin(ϕ) * cos(θ)
                 , b * sin(θ)
                 ]

function main()
    a = parse(Float64, readline())
    b = parse(Float64, readline())
    m = parse(Int, readline())
    n = parse(Int, readline())
    println("($(m*n), $(m*n*2))")
    
    ϕ(i) = 2π / m * i
    θ(j) = 2π / n * j
    vertices(i,j) = torus(a,b,ϕ(i),θ(j))
    ι(i,j) = n*i + j

    for i = 0:m-1
        for j = 0:n-1
            v = vertices(i,j)
            println("($(v[1]), $(v[2]), $(v[3]))")
        end
    end

    for i = 0:m-1
        for j = 0:n-1
            i′ = (i+1) % m
            j′ = (j+1) % n
            println("($(ι(i,j )), $(ι(i′,j)), $(ι(i ,j′)))")
            println("($(ι(i,j′)), $(ι(i′,j)), $(ι(i′,j′)))")
        end
    end
end

main()
