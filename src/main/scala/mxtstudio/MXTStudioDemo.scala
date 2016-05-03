package mxtstudio
  
object MXTStudioTest extends MXTStudio {
  def main(args:Array[String]) = {
    HAI
    VISIBLE("Welcome to the guessing game!")
    VISIBLE("Each turn you can guess a number between 1 and 20.")
    VISIBLE("We will give you hints regarding your guess.")
    VISIBLE("--------------")
    I_HAZ_A('number) ITZ RAND(1, 20)
    I_HAZ_A('attempts) ITZ 1
    IM_IN_YR_LOOP
    VISIBLE("Guess a number between 1 and 20 (inclusive)")
    GIMMEH('guess)
    IZ('guess SMALLR_THAN 'number)
    	VISIBLE("Higher!")
    	NOWAI
    KTHX
    IZ('guess BIGR_THAN 'number)
    	VISIBLE("Lower!")
    	NOWAI
    KTHX
    IZ('guess LIEK 'number)
    	VISIBLE("You got it!")
    	GTFO
    	NOWAI
    KTHX
    LOL('attempts) ITZ ('attempts UP 1)
    IM_OUTTA_YR_LOOP
    VISIBLE("It took you", 'attempts, "turns!")
    KTHXBYE
  }
}
