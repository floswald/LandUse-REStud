

function flux_data()
    x = Array(CSV.read("out/par_grid.txt", DataFrame))
    return (x[:, 1:7], x[:, 8:13])
end

function build_model(nin,nout)
    return Chain(
            Dense(nin,nout+2, relu),
            Dense(nout+2,nout))
end