blend2 <- function(c,D,a,b){
 returnvalx <- (a*D-b*D+4*c-4)/(8*(c-1))
 returnvals <- list(returnvalx,1-returnvalx)
 return(returnvals)
 }

blend3 <- function(A,B,C,D,g,h,k){
a <- 1-A
b <- 1-B
c <- 1-C
D <- -1*D
returnvalx <- (0-a*c-D/4*a*h+D/4*a*k-b*c+D/4*b*h-D/4*b*k+c*c-D/4*2*c*g+D/4*c*h+D/4*c*k)/(a*a-2*a*b-2*a*c+b*b-2*b*c+c*c)
returnvaly <- (0-a*b-D/4*a*g+D/4*a*k+b*b-b*c+D/4*b*g-D/4*2*b*h+D/4*b*k+D/4*c*g-D/4*c*k)/(a*a-2*a*b-2*a*c+b*b-2*b*c+c*c)
returnvals <- list(returnvalx,returnvaly,1-returnvalx-returnvaly)
return(returnvals)
}

blend <- function(firstinput,secondinput,thirdinput=NULL,firstscore,secondscore,thirdscore=NULL){
	if(is.vector(firstinput))
	{
		if(missing(secondinput))
		{
			stop("missing second input")
		}
		else if (is.null(thirdinput))
		{
			blend2(cor(firstinput,secondinput),20,firstscore,secondscore)
		}
        else if (!is.null(thirdinput))
        {
            blend3(cor(firstinput,secondinput),cor(firstinput,thirdinput),cor(secondinput,thirdinput),20,firstscore,secondscore,thirdscore)
        }
	}
}
