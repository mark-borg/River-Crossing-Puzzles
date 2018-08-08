# Mark Borg
# see: https://mark-borg.github.io/blog/2016/river-crossing-puzzles/
#

require(igraph)
require(combinat)
require(vecsets)


# checks whether the objects on the specified side of the river bank are valid according to the specified rules
is.bank.valid <- function(gr, state, side)
{
    b <- state[[side]]
    if (! is.element(farmer.symbol,b) & length(b) > 1)
    {
        pairs <- combn(b, 2)
        if (! is.matrix(pairs))
        {
            pairs <- as.matrix(pairs, ncol=1)
        }
        pairs <- cbind(pairs, apply(pairs, 2, rev))               # we have a directed rule graph, so order matters (we must check both G -> C and C -> G)
        res <- apply(pairs, 2, function(x){ are.connected(gr, which(V(gr)$name == x[1]), which(V(gr)$name == x[2])) })
        pairs <- pairs[,res]               # the pairs which are invalid
        valid.state <- (max(res) == FALSE)
    } else {
        valid.state <- TRUE
    }
    return(valid.state)
}


# checks whether the current state of the river crossing puzzle is valid according to the specified rules.
# we check whether both sides of the river bank are valid or not.
is.state.valid <- function(gr, state)
{
    return(is.bank.valid(gr, state, 1) & is.bank.valid(gr, state, 2))
}


# checks whether the specified transition is valid or not
# here we consider a transition as valid as long as the boat has the farmer in it
is.transition.valid <- function(transition)
{
    return(is.element(farmer.symbol, transition))
}


# has the given state been visited already?
is.state.visited <- function(gss, state)
{
    return(is.element(state$name , V(gss)$name))
}


# is the given state a goal state? 
# here we consider a goal state as that for which no objects remain on the left-hand river bank
is.final.state <- function(state)
{
    return(length(state$bank.l) == 0)
}


# create a string representation of the current state
make.state.name <- function(state)
{
    bl <- ifelse(length(state$bank.l) > 0, paste(sort(unlist(strsplit(state$bank.l,''))),collapse=''), "")
    br <- ifelse(length(state$bank.r) > 0, paste(sort(unlist(strsplit(state$bank.r,''))),collapse=''), "")
    if (state$boat.pos == 1)
    {
        bl <- paste(bl, 'b', sep='')
    } else {
        br <- paste(br, 'b', sep='')
    }
    state$name <- paste(bl, br, sep = '|')
    return(state)
}



# this method creates the state space for this river crossing puzzle, including also invalid states (but not invalid transitions).
# state0 is the starting state.
solve_all_states <- function(gss, state0)
{
    if (!is.state.valid(gr, state0))
        return(gss);
    
    cat('solving', state0$name, '\n')
    
    # generate all possible state transitions
    bank <- state0[[state0$boat.pos]]
    ops <- combn(c(bank, rep(NA, boat.capacity-1)), boat.capacity)                    # we add the NA to generate state transitions where the boat is not at full capacity
    
    # remove invalid state transitions
    ops <- ops[,apply(ops, 2, is.transition.valid)]
    ops <- unique(ops, MARGIN = 2)
    
    
    # for each valid state transition...
    if (length(ops) > 0)
    {
        for (n in 1:ncol(ops))
        {
            op <- ops[,n]
            cat('  transition', op, '\n')
            
            boat <- op
            
            if (state0$boat.pos == 1)
            {
                state <- list(bank.l = vsetdiff(state0$bank.l, boat), bank.r = c(state0$bank.r, boat), boat.pos = 2)
            } else
            {
                state <- list(bank.l = c(state0$bank.l, boat), bank.r = vsetdiff(state0$bank.r, boat), boat.pos = 1)
            }
            state <- make.state.name(state)
            

            not.visited <- ! is.state.visited(gss, state)
            
            if (not.visited)
            {
                gss <- add.vertices(gss, 1, name=state$name, valid=is.state.valid(gr, state))
            }
            
            gss <- add.edges(gss,c(which(V(gss)$name == state0$name), which(V(gss)$name == state$name)))
            
            if (! is.final.state(state) & not.visited & is.state.valid(gr, state))
            {
                gss <- solve_all_states(gss, state)
            }
        }
    }
    
    return(gss)
}


# this method creates the state space for this river crossing puzzle, including only valid states and valid transitions
# state0 is the starting state.
solve <- function(gss, state0)
{
    cat('solving', state0$name, '\n')
    
    # generate all possible state transitions
    bank <- state0[[state0$boat.pos]]
    ops <- combn(c(bank, rep(NA, boat.capacity-1)), boat.capacity)                    # we add the NA to generate state transitions where the boat is not at full capacity
    
    # remove invalid state transitions
    ops <- ops[,apply(ops, 2, is.transition.valid)]
    ops <- unique(ops, MARGIN = 2)
    
    
    # for each valid state transition...
    if (length(ops) > 0)
    {
        for (n in 1:ncol(ops))
        {
            op <- ops[,n]
            cat('  transition', op, '\n')
            
            boat <- op
            
            if (state0$boat.pos == 1)
            {
                state <- list(bank.l = vsetdiff(state0$bank.l, boat), bank.r = c(state0$bank.r, boat), boat.pos = 2)
            } else
            {
                state <- list(bank.l = c(state0$bank.l, boat), bank.r = vsetdiff(state0$bank.r, boat), boat.pos = 1)
            }
            state <- make.state.name(state)
            
            if (is.state.valid(gr, state))
            {
                cat(state$name, 'is valid', '\n')
                
                not.visited <- ! is.state.visited(gss, state)
                
                if (not.visited)
                {
                    gss <- add.vertices(gss, 1, name=state$name, valid=is.state.valid(gr, state))
                }
                
                gss <- add.edges(gss,c(which(V(gss)$name == state0$name), which(V(gss)$name == state$name)))
                
                if (! is.final.state(state) & not.visited & is.state.valid(gr, state))
                {
                    gss <- solve(gss, state)
                }
            }
        }
    }
    
    return(gss)
}

