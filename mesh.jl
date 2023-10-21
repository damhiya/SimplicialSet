include("Examples/Tetrahedron.jl")
include("Examples/Torus.jl")
include("Examples/Hemisphere.jl")
include("Examples/Moebius.jl")

function examples(id)
    if id == "tetrahedron"
        return tetrahedronMesh
    elseif id == "torus"
        return torusMesh(10.0,1.0,100,100)
    elseif id == "hemisphere"
        return hemisphereMesh(1.0,100)
    elseif id == "moebius"
        return moebiusMesh(100)
    else
        println("No such example : $id")
        exit(-1)
    end
end

function main()
    id = ARGS[1]
    (vmap, fmap) = examples(id)
    vs = eachcol(vmap)
    fs = eachcol(fmap)

    println("($(length(vs)), $(length(fs)))")
    for v in vs
        println("($(v[1]), $(v[2]), $(v[3]))")
    end
    for f in fs
        println("($(f[1]), $(f[2]), $(f[3]))")
    end
end

main()
