# area = 2πr²
# perimeter = 2πr

hemisphere(r,θ,ϕ) = [ r * sin(θ) * cos(ϕ)
                    , r * sin(θ) * sin(ϕ)
                    , r * cos(θ)
                    ]

hemisphereMesh(r,n) =
    let θ(i) = 0.5π * (i/n),
        ϕ(j) = 2π * (j/n),
        ι(i,j) = (i == 0) ? 0 : (1 + n*(i-1) + j%n),
        vmap = hcat(hemisphere(r,θ(0),ϕ(0)), (hemisphere(r,θ(i),ϕ(j)) for i=1:n for j=0:n-1)...)
        fmap = hcat( ([ι(0,j+1), ι(1  ,j), ι(1  ,j+1)] for j=0:n-1)...
                   , ([ι(i,j+p), ι(i+1,j), ι(i+p,j+1)] for i=1:n-1 for j=0:n-1 for p=0:1)...
                   )
        (vmap, fmap)
    end
