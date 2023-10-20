hemisphere(r,θ,ϕ) = [ r * sin(θ) * cos(ϕ)
                    , r * sin(θ) * sin(ϕ)
                    , r * cos(θ)
                    ]

function main()
    r = parse(Float64, readline())
    n = parse(Int, readline())
    println("($(1+n*n), $(2n*(n-1)))")
    
    θ(i) = 0.5π / n * i
    ϕ(j) = 2π / n * j
    vertices(i,j) = hemisphere(r,θ(i),ϕ(j))
    ι(i,j) = (i == 0) ? 0 : (1 + n*(i-1) + j)

    for i = 0:n
        if i == 0
            v = vertices(0,0)
            println("($(v[1]), $(v[2]), $(v[3]))")
        else
            for j = 0:n-1
                v = vertices(i,j)
                println("($(v[1]), $(v[2]), $(v[3]))")
            end
        end
    end

    for i = 0:n-1
        if i == 0
            for j = 0:n-1
                j′ = (j+1) % n
                println("($(ι(0,0)), $(ι(1,j)), $(ι(1,j′)))")
            end
        else
            for j = 0:n-1
                i′ = i+1
                j′ = (j+1) % n
                println("($(ι(i,j)), $(ι(i′,j )), $(ι(i′,j′)))")
                println("($(ι(i,j)), $(ι(i′,j′)), $(ι(i ,j′)))")
            end
        end
    end
end

main()
