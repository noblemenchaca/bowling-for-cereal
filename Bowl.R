View(Cereals)

# Seperate the cereal brands in question into their own subsets
kell <- subset(Cereals, mfr == "K")
gen <- subset(Cereals, mfr == "G")

# Bootstrapping to find the avarage (mean) reting of a sample of cereals from each brand

kellsamp <- sample(kell$rating, 10, replace=T)
kellboot <- replicate(1000, 
{
  samprep=sample(kellsamp, 10, replace=T);
  kelldist=mean(samprep)
})
mean(kellboot)
#[1] 48.64062

gensamp <- sample(gen$rating, 10, replace=T)
genboot <- replicate(1000, 
{
  samprep=sample(gensamp, 10, replace=T);
  gendist=mean(samprep)
})
mean(genboot)
#[1] 35.65092

#Boxes
boxplot(kell$rating, gen$rating, 
        names=c("Kellogg's", "General Mills"), 
        xlab="Brands", ylab="Rating", 
        col=c("red",'cyan'), main="Cereal Ratings by Manufacturer")

#Histos
betterorworse=replicate(10000, {kbootsamp=sample(kellsamp, replace=T);  
                             gbootsamp=sample(gensamp, replace=T);
                             diffs= mean(kbootsamp)-mean(gbootsamp)})

hist(betterorworse, 
     main= "Difference between Cereal Ratings", 
     freq=F, xlab= "Kellogg's Ratings - General Mills' Ratings", col=c("orange","brown4","brown2")) #inspired by Reecess Puffs

(CI= quantile(betterorworse, c(0.05, 1)))
#       5%      100% 
#-3.131512 24.417675 
abline(v=CI, lwd=4, col=c("red","black"))