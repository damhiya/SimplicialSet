# area = 6.35327
# perimeter = 13.0067544662

moebius(u,v) = [ (1 + u * cos(v/2)) * cos(v)
               , (1 + u * cos(v/2)) * sin(v)
               , u * sin(v/2)
               ]

moebiusMesh(n) =
    let u(i) = -0.5 + 1 / n * i,
        v(j) = 2π / n * j,
        ι(i,j) = j == n ? n*(n-i) : (n*i + j),
        vmap = hcat((moebius(u(i), v(j)) for i=0:n for j=0:n-1)...),
        fmap = hcat(([ι(i,j+p), ι(i+1,j), ι(i+p,j+1)] for i=0:n-1 for j=0:n-1 for p=0:1)...)
        (vmap, fmap)
    end
