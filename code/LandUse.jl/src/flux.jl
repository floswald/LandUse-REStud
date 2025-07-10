module fl
    
    using Flux
    using Statistics
    using Flux.Data: DataLoader
    using DelimitedFiles
    using Base: @kwdef
    using MLDataUtils: splitobs, shuffleobs
    using CUDA
    using Flux.Losses
    using BSON
    using Statistics: mean


    @kwdef mutable struct Args
        η::Float64 = 3e-4       # learning rate
        batchsize::Int = 256    # batch size
        epochs::Int = 1000        # number of epochs
        use_cuda::Bool = true   # use gpu (if cuda available)
        train_prop::Float64 = 0.7     # proportion of data to use for trainging
        nin::Int = 7
        nout::Int = 6
    end

    sqnorm(x) = sum(abs2, x)

    function getdata(args)
        # loading Data
        # (params, observations )
        # (7 , 16000)
        input = readdlm(joinpath(@__DIR__,"..","out","input.txt"))'
        output = readdlm(joinpath(@__DIR__,"..","out","output.txt"))'

        # Normalise the data
        x = (input .- mean(input, dims = 2)) ./ std(input, dims = 2)
        y = (output .- mean(output, dims = 2)) ./ std(output, dims = 2)

        n = size(input)[2]

        # split into training and testing
        itrain, itest = splitobs(shuffleobs(1:n), at = args.train_prop)

        # Split into train and test sets
        # split_index = floor(Int,size(x,2)*split_ratio)
        # x_train = x[:,1:split_index]
        # y_train = y[:,1:split_index]
        # x_test = x[:,split_index+1:size(x,2)]
        # y_test = y[:,split_index+1:size(x,2)]

        # train_data = (x_train, y_train)
        # test_data = (x_test, y_test)

        xtrain = input[:,itrain]
        ytrain = output[:, itrain]

        xtest = input[:, itest ]
        ytest = output[:, itest ]

        #mini batch iterators
        train_loader = DataLoader((xtrain, ytrain), batchsize = args.batchsize, shuffle = true)
        test_loader = DataLoader((xtest, ytest), batchsize = args.batchsize)

        return train_loader, test_loader
        # return train_data,test_data

    end

    function build_model(din,dout)
        return Chain(
                Dense(din,8, relu),
                Dense(8,8, relu),
                Dense(8,7, relu),
                Dense(7,dout))
    end

    function loss_function(data_loader, model, device)
        ls = 0.0f0
        num = 0
        for (x, y) in data_loader
            x, y = device(x), device(y)
            ŷ = model(x)
            ls += mse(model(x), y)
            num +=  size(x, 2)
        end
        return ls / num  
    end

    function train(; kws...)
        args = Args(; kws...) # collect options in a struct for convenience

        if CUDA.functional() && args.use_cuda
            @info "Training on CUDA GPU"
            CUDA.allowscalar(false)
            device = gpu
        else
            @info "Training on CPU"
            device = cpu
        end

        # Create test and train dataloaders
        train_loader, test_loader = getdata(args)
        dsizes = size.(train_loader.data)

        # Construct model
        model = build_model(dsizes[1][1],dsizes[2][1]) |> device
        ps = Flux.params(model) # model's trainable parameters
        
        ## Optimizer
        opt = ADAM(args.η)
        
        ## Training
        for epoch in 1:args.epochs
            for (x, y) in train_loader
                x, y = device(x), device(y) # transfer data to device
                gs = gradient(() -> mse(model(x), y), ps) # compute gradient
                Flux.Optimise.update!(opt, ps, gs) # update parameters
            end
            
            # Report on train and test
            train_loss = loss_function(train_loader, model, device)
            test_loss = loss_function(test_loader, model, device)
            if epoch % 10 == 0
                println("Epoch=$epoch")
                println("  train_loss = $train_loss")
                println("  test_loss = $test_loss")
            end
        end
        BSON.@save joinpath(@__DIR__,"..","out","mymodel.bson") model
        return model
    end

end