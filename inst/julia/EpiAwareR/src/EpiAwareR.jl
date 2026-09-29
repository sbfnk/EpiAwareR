# Julia side of EpiAwareR. R renders model components to Julia source; the
# functions here build, simulate from and fit those models, and return plain
# arrays that JuliaConnectoR translates into R objects. Fitted chains stay in
# Julia behind integer handles so that predictions can reuse them.
module EpiAwareR

using ComposableTuringIDModels: ComposableTuringIDModels, as_turing_model
using ADTypes: AutoForwardDiff, AutoMooncake
using DynamicPPL: DynamicPPL, @varname, returned
using FlexiChains: FlexiChains
using Logging: ConsoleLogger, Warn, with_logger
using Mooncake: Mooncake
using Random: Random
using Turing: Turing, NUTS, MCMCSerial, MCMCThreads, sample, predict

const HANDLES = Dict{Int, Any}()
const NEXT_HANDLE = Ref(0)

# Handles count from the same base in every session, so a handle from an
# earlier one would name a different fit here. The token tells the two apart.
const SESSION = Ref("")

function __init__()
    SESSION[] = string(rand(Random.RandomDevice(), UInt128); base = 16)
    return nothing
end

function keep!(x)
    NEXT_HANDLE[] += 1
    HANDLES[NEXT_HANDLE[]] = x
    return NEXT_HANDLE[]
end

# A handle from a dead session names whichever fit now holds that number, so
# releasing it would destroy a live fit.
function release!(handle::Integer, session::AbstractString)
    session == SESSION[] && delete!(HANDLES, Int(handle))
    return nothing
end

# Julia errors from deep inside a model print type signatures that run to
# pages; only the message is useful in R.
function concise_errors(f)
    try
        return f()
    catch e
        error(sprint(showerror, e))
    end
end

# Components reference constructors exported into Main by `using`.
build(code::AbstractString) = Core.eval(Main, Meta.parse(code))

# JuliaConnectoR passes length-one R vectors as scalars.
as_vector(x::AbstractVector) = x
as_vector(x) = [x]

# Integer-valued data become `Int` so count likelihoods receive counts.
function as_data(values)
    values = Float64.(as_vector(values))
    return all(isinteger, values) ? Int.(values) : values
end

seed!(seed) = seed === nothing || Random.seed!(Int(seed))

# A generated trajectory as a Float64 vector of length `n`. Observation
# modifiers such as delays may return shorter series, which are aligned to the
# end of the time axis.
function as_trajectory(x, n)
    x isa AbstractVector{<:Union{Missing, Real}} || return nothing
    out = fill(NaN, n)
    offset = n - length(x)
    for (i, v) in enumerate(x)
        out[offset + i] = ismissing(v) ? NaN : Float64(v)
    end
    return out
end

# Stack per-draw generated quantities into a draws x time x quantities array,
# dropping quantities that are not single time series (e.g. stratified
# output). A single array translates to R directly, where a vector of
# matrices would arrive as a Julia proxy.
function stack_generated(gens::AbstractVector, n; extra = Pair{String, Matrix{Float64}}[])
    names = String[]
    mats = Matrix{Float64}[]
    for field in propertynames(first(gens))
        rows = [as_trajectory(getproperty(g, field), n) for g in gens]
        any(isnothing, rows) && continue
        push!(names, String(field))
        push!(mats, permutedims(reduce(hcat, rows)))
    end
    for (name, mat) in extra
        push!(names, name)
        push!(mats, mat)
    end
    return (names = names, values = cat(mats...; dims = 3))
end

function simulate(code::AbstractString, n::Integer, nsim::Integer, seed)
    return concise_errors(() -> _simulate(code, n, nsim, seed))
end

function _simulate(code, n, nsim, seed)
    seed!(seed)
    mdl = as_turing_model(build(code), missing, Int(n))
    return stack_generated([mdl() for _ in 1:nsim], Int(n))
end

# Draws are ordered chain by chain, matching posterior's draws_df.
draw_order(A::AbstractMatrix) = vec(A)

function flatten_parameters(chain)
    names = String[]
    cols = Vector{Float64}[]
    for key in FlexiChains.parameters(chain)
        values = draw_order(chain[key])
        name = string(key)
        first_value = first(values)
        if first_value isa Real
            push!(names, name)
            push!(cols, Float64.(values))
        elseif first_value isa AbstractArray{<:Real}
            for idx in CartesianIndices(first_value)
                push!(names, string(name, "[", join(Tuple(idx), ","), "]"))
                push!(cols, [Float64(v[idx]) for v in values])
            end
        end
    end
    return names, cols
end

function flatten_stats(chain)
    names = String[]
    cols = Vector{Float64}[]
    for key in FlexiChains.extras(chain)
        values = draw_order(chain[key])
        first(values) isa Real || continue
        push!(names, string(key.name))
        push!(cols, Float64.(values))
    end
    return names, cols
end

# Sampler info messages (e.g. the initial step size) are noise in R; warnings
# such as divergent transitions are kept.
function fit(code::AbstractString, values; kwargs...)
    return concise_errors() do
        with_logger(ConsoleLogger(stderr, Warn)) do
            _fit(code, values; kwargs...)
        end
    end
end

function _fit(
        code, values;
        draws::Integer, warmup::Integer, chains::Integer,
        target_acceptance::Real, max_depth::Integer, ad::AbstractString, seed
    )
    model = build(code)
    y = as_data(values)
    n = length(y)
    posterior = as_turing_model(model, y, n)
    adtype = ad == "mooncake" ? AutoMooncake(; config = nothing) : AutoForwardDiff()
    ensemble = chains > 1 && Threads.nthreads() > 1 ? MCMCThreads() : MCMCSerial()
    seed!(seed)
    chain = sample(
        posterior,
        NUTS(Int(warmup), Float64(target_acceptance);
            adtype = adtype, max_depth = Int(max_depth)),
        ensemble, Int(draws), Int(chains); progress = false
    )
    ni, nc = size(chain)
    par_names, par_values = flatten_parameters(chain)
    stat_names, stat_values = flatten_stats(chain)
    generated = stack_generated(
        draw_order(returned(posterior, chain)), n;
        extra = ["predicted_y_t" => predicted_observations(model, chain, n)]
    )
    handle = keep!((; model, y, chain))
    return (
        handle = handle,
        session = SESSION[],
        chain = repeat(1:nc; inner = ni),
        iteration = repeat(1:ni; outer = nc),
        parameter_names = par_names,
        parameters = reduce(hcat, par_values),
        stat_names = stat_names,
        stats = reduce(hcat, stat_values),
        generated = generated,
    )
end

# Posterior predictive draws of `y_t[from:to]` from a chain returned by
# `predict`, as a draws x time matrix. The chain stores `y_t` whole or per
# element, and indexing reads both; a time point a delay leaves unmodelled is
# absent either way and becomes NaN.
function observation_draws(pred, from, to)
    ndraws = prod(size(pred))
    cols = map(from:to) do i
        values = try
            draw_order(pred[@varname(y_t[i])])
        catch e
            e isa KeyError || rethrow()
            fill(missing, ndraws)
        end
        Float64[ismissing(v) ? NaN : Float64(v) for v in values]
    end
    all(col -> all(isnan, col), cols) && error(
        "No observations found in the chain for t = $from:$to. The model's " *
            "observations may be named something other than `y_t`."
    )
    return reduce(hcat, cols)
end

function predicted_observations(model, chain, n)
    pred = predict(as_turing_model(model, fill(missing, n), n), chain)
    return observation_draws(pred, 1, n)
end

# Forecast observations over `horizon` time points after the fitted period.
function forecast_observations(
        handle::Integer, horizon::Integer, seed, session::AbstractString
    )
    return concise_errors(
        () -> _forecast_observations(handle, horizon, seed, session)
    )
end

function _forecast_observations(handle, horizon, seed, session)
    session == SESSION[] && haskey(HANDLES, Int(handle)) ||
        error("This fit is no longer available in the Julia session.")
    (; model, y, chain) = HANDLES[Int(handle)]
    n = length(y)
    seed!(seed)
    fc = ComposableTuringIDModels.forecast(model, y, chain, Int(horizon))
    return observation_draws(fc, n + 1, n + horizon)
end

end
