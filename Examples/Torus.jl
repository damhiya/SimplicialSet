# volume = 2π²ab²
# area = 4π²ab

torus(a,b,ϕ,θ) = [ a * cos(ϕ) + b * cos(ϕ) * cos(θ)
                 , a * sin(ϕ) + b * sin(ϕ) * cos(θ)
                 , b * sin(θ)
                 ]

torusMesh(a,b,m,n) =
    let ϕ(i) = 2π * (i/m),
        θ(j) = 2π * (j/n),
        ι(i,j) = n * (i%m) + (j%n),
        vmap = hcat((torus(a,b,ϕ(i),θ(j)) for i=0:m-1 for j=0:n-1)...),
        fmap = hcat(([ι(i,j+p), ι(i+1,j), ι(i+p,j+1)] for i=0:m-1 for j=0:n-1 for p=0:1)...)
        (vmap, fmap)
    end
