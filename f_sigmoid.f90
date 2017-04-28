real function sigmoid(x)
    implicit none
    real x

    sigmoid = 1/(1+exp(-1*(x)))
end function
