subroutine netCheck
    use variables
    implicit none

!asking user for value with which the network will be checked
write(*,*) "Checking the net with value "
read(*,*) testValue(1,1)

!normalizing the input given by user with the parameters (min,max,average) of training data
testValue(1,1) = (testValue(1,1)-inputValuesParameters(1))/(inputValuesParameters(3)-inputValuesParameters(2))

write(*,*) "Normalized test value:", testValue(1,1)

!feeding the trained net with the checking input data
hiddenValuesChecking = matmul(testValue,inputToHiddenWeights)

write(*,*) "hidden values checking"
write(*,*) hiddenValuesChecking

call matrixSigmoid(hiddenValuesChecking,hiddenValuesCheckingSigmoid,1,hiddenValuesColumns)

write(*,*) "hidden values checking sigmoid"
write(*,*) hiddenValuesCheckingSigmoid

additionalValuesChecking = matmul(hiddenValuesCheckingSigmoid,additionalLayerWeights)

write(*,*) "additional values checking"
write(*,*) additionalValuesChecking

call matrixSigmoid(additionalValuesChecking,additionalValuesCheckingSigmoid,1,additionalLayerValuesColumns)

write(*,*) "additional values checking sigmoid"
write(*,*) additionalValuesCheckingSigmoid

outputValuesChecking = matmul(additionalValuesCheckingSigmoid,hiddenToOutputWeights)

write(*,*) "Output value: ", outputValuesChecking(1,1)

end subroutine
