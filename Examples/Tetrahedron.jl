# volume = 1/6
# area = (3 + sqrt 3)/2

tetrahedronMesh =
    let vmap = [ 0. 1. 0. 0.
               ; 0. 0. 1. 0.
               ; 0. 0. 0. 1.
               ]
        fmap = [ 0 0 0 1
               ; 2 1 3 2
               ; 1 3 2 3
               ]
        (vmap, fmap)
    end
