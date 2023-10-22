import Plots
import PlotlyJS

include("Examples/Tetrahedron.jl")
include("Examples/Torus.jl")
include("Examples/Hemisphere.jl")
include("Examples/Moebius.jl")

function examples(id)
    if id == "tetrahedron"
        return tetrahedronMesh
    elseif id == "torus"
        return torusMesh(3.0,1.0,100,100)
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
    cmd = ARGS[1]
    id = ARGS[2]
    if ! (cmd == "print" || cmd == "plot")
        return("No such command : $cmd")
    end

    (vmap, fmap) = examples(id)
    if cmd == "print"
        vs = eachcol(vmap)
        fs = eachcol(fmap)

        println("$(length(vs))")
        for v in vs
            println("($(v[1]), $(v[2]), $(v[3]))")
        end
        println("$(length(fs))")
        for f in fs
            println("($(f[1]), $(f[2]), $(f[3]))")
        end
    elseif cmd == "plot"
        Plots.display(Plots.plot(
            vmap[1,:], vmap[2,:], vmap[3,:],
            seriestype=:mesh3d,
            connections=(fmap[1,:], fmap[2,:], fmap[3,:])
           ))
    end
end

Plots.plotly()
main()
