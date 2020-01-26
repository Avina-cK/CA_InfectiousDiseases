library(caTools)
library(imager)

N = 20                  #sqrt root of population
pop_i =rep(0,(N)^2)

pop = matrix(pop_i, nrow = N, ncol = N)

'Partitions of population: 
Dead              = 0 
Susceptible/Alive = 1 
Infected          = 2
Recovered         = 3
'
i=1
range_pop <- NULL
#c((N+2):(2*N-1), (2*N+2):(3*N-1),... )
while(((i+1)*N)<(N^2)){
  range_pop<-c(range_pop, (i*N+2):((i+1)*N-1))
  i=i+1
}

inf_0 = (N^2)*15/100
pop_i[sample(range_pop, inf_0)] = 2

for(i in range_pop){
  if(pop_i[i]==0){
    a=sample(0:10, 1)
    if(a<5){pop_i[i]=1}
  }
}

t=0
T=60
Time=NULL
Pop_list=as.list(NULL)

pop = matrix(pop_i[range_pop], nrow = (N-2), ncol = (N-2))
pop0=pop

t_dead=NULL
t_alive=NULL
t_infected=NULL
t_recovered=NULL

br = 0.1    #birth rate
rr = 0.05   #recovery rate
dr = 0.1    #death rate
ir = 0.75    #infection rate
mr = 0.25    #mutation rate
vr = 0.02   #vaccination rate

while(t<T){
  dead = NULL
  alive=NULL
  infected=NULL
  recovered=NULL
  
  p_dead=rep(0,(N)^2)
  p_alive=p_dead
  p_infected=p_alive
  p_recovered=p_infected
  
  i=1
  while (i<=(N^2)) {
    if(!(i%in%range_pop)){
      pop_i[1]=0
      
    }
    if(i%in%range_pop){
      if(pop_i[i]==0){
        dead = c(dead,i)
      }
      if(pop_i[i]==1){
        alive= c(alive,i)
      }
      if(pop_i[i]==2){
        infected= c(infected,i)
        
      }
      if(pop_i[i]==3){
        recovered= c(recovered,i)
      } 
    }
    i=i+1
  }
  
  if(t>0){
    Time=c(Time, t)
  }
  
  if(is.null(dead)){
    t_dead<- c(t_dead, 0)
  }
  if(is.null(alive)){
    t_alive<- c(t_alive, 0)
  }
  if(is.null(infected)){
    t_infected<- c(t_infected, 0)
  }
  if(is.null(recovered)){
    t_recovered<-c(t_recovered, 0)
  }
  
  if(!(is.null(dead))){
    t_dead<- c(t_dead, length(dead))
    born=sample((1/N^2)*(0:(N^2)),length(dead))
    pop_i[dead[which(born<br)]]=1
  }
  if(!(is.null(infected))){
    t_infected<-c(t_infected, length(infected))
    recover=sample((1/N^2)*(0:(N^2)),length(infected))
    pop_i[infected[which(recover<rr)]]=3
  }
  if(!(is.null(alive))){
    t_alive<-c(t_alive, length(alive))
    die=sample((1/N^2)*(0:(N^2)),length(alive))
    pop_i[alive[which(die<dr)]]=0
    alive<-alive[-which(die<dr)]
    for(i in alive){
      if((pop_i[i-1]==2)|(pop_i[i+1]==2)|(pop_i[i+N]==2)|(pop_i[i+N+1]==2)|(pop_i[i+N-1]==2)|(pop_i[i-N]==2)|(pop_i[i-N-1]==2)|(pop_i[i-N+1]==2)){
        sick=sample((1/N^2)*(0:(N^2)),1)
        pop_i[alive[which(sick<ir)]]=1
      }
    }
    alive<-alive[-which(sick<ir)]
    vac<-sample((1/N^2)*(0:(N^2)),length(alive))
    pop_i[alive[which(vac<vr)]]=3
  }
  if(!(is.null(recovered))){
    t_recovered<-c(t_recovered, length(recovered))
    die=sample((1/N^2)*(0:(N^2)),length(recovered))
    pop_i[recovered[which(die<dr)]]=0
    recovered<-recovered[-which(die<dr)]
    mut<-sample((1/N^2)*(0:(N^2)),length(recovered))
    pop_i[recovered[which(mut<mr)]]=1
  }
  Pop_list[[t+1]]<-matrix(pop_i[range_pop], nrow = (N-2), ncol = (N-2))
  
  t=t+1
  
}

pop = matrix(pop_i[range_pop], nrow = (N-2), ncol = (N-2))
img=image(rbind(pop,-1, c(rep(-1,N-6), 3:0)), axes=FALSE, col=c("white", "red", "blue", "orange", "green"))

plot(Time, t_dead[-1], type = "l", ylim = c(0,N^2 /2), col="red", ylab = "No. of people")
lines(Time, t_alive[-1], col="blue", type="l")
lines(Time, t_infected[-1], col="orange", type="l")
lines(Time, t_recovered[-1], col="green", type = "l")
legend("topleft", c("Dead", "Alive", "Infected", "Recovered"), col=c("red", "blue", "orange", "green"), horiz = TRUE, cex=0.8, lty=1)
