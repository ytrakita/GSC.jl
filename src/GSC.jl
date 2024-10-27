module GSC

include("game.jl")
include("solution.jl")
include("bruteforce.jl")
include("ibr.jl")

export
    # basic types
    Strategy, StrategySpace, StrategyProfile, Player, Game,

    # game functions
    isbr, br, iseqm,

    # soltion
    GSCAlgorithm, solve,

    # brute_force algorithm
    BruteForce,

    # iterating best-replies
    IBR, IminBR, ImaxBR, ItrIBR

end # module GSC
