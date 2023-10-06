#Probability assignment 2023
#Name: Daniel Keane
#Programme: Applied/Forensics

studentID = 20098745

# Question 1 
q1 = function()
{
	exactP = choose(9, 3)*choose(18, 3)/choose(27, 6)
	estimateP = -1

totalSims = 200
threeCrockery = 0

for(i in 1:totalSims)
{


	selection = sample(1:27, 6, replace = FALSE)
	crockVal = length(selection[selection <= 9])

	if(crockVal == 3)
		threeCrockery = threeCrockery + 1
}

estimateP = threeCrockery / totalSims

	c(round(exactP, 4), round(estimateP, 4))
}


# Question 2
q2 = function()
{
	exactP = pbinom(1, 108, 1/choose(8, 3), lower.tail = FALSE)
	estimateP = -1

totalSims = 200
sampleOfThree = 0
twoOrMore = 0

for(i in 1:totalSims)
{
	for(i in 1:108){
	selection = sample(1:8, 3, replace = FALSE)
	teamAOnly = length(selection[selection <=3])

	if(teamAOnly == 3)
	sampleOfThree = sampleOfThree + 1
	}
	if(sampleOfThree >= 2){
		twoOrMore = twoOrMore + 1
	}
sampleOfThree = 0
}
estimateP = twoOrMore / totalSims

	c(round(exactP, 4), round(estimateP, 4))
}


# Question 3
q3 = function()
{
	exactP = 1 - (choose(6,4)*factorial(4))/(6^4)
	extimateP = -1

totalSims = 200
duplicateInBatch = 0

for(i in 1:totalSims) {
	batch = sample(1:6, 4, replace = TRUE)
	if((batch[1]==batch[2]) | (batch[1]==batch[3]) | (batch[1]==batch[4]) | 
	(batch[2]==batch[3]) | (batch[2]==batch[4]) | (batch[3]==batch[4])) {
	duplicateInBatch = duplicateInBatch + 1
	}
}
estimateP = duplicateInBatch /totalSims

	c(round(exactP, 4), round(estimateP, 4))
}


# Question 4
q4 = function()
{
	exactP = ppois(6, 217/22, lower.tail=FALSE)
	estimateP = -1

totalSims = 200
blemishPot = 0
sampleOfSeven = 0

for(i in 1:totalSims)
{
	for(i in 1:31){
	selection = sample(1:22, 1, replace = FALSE)
	}
	if(selection <= 7){
	blemishPot = blemishPot + 1
	}
	if(blemishPot >= 7){
	sampleOfSeven = sampleOfSeven + 1
	}
}
estimateP = sampleOfSeven / totalSims

	c(round(exactP, 4), round(estimateP, 4))
}

# Question 5
q5 = function()
{
	exactP = pnorm(480, 443, 37) - pnorm(391, 443, 37)
	estimateP = -1

totalSims = 200
inBounds = 0

for(i in 1:totalSims)
{
	firing = rnorm(1, 443, 37)
	if((firing >= 391) && (firing <= 480)) {
	inBounds = inBounds + 1
	}
}
estimateP = inBounds/totalSims

	c(round(exactP, 4), round(estimateP, 4))
}