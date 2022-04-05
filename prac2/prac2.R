"Indepentent T test"
data = read.csv(file.choose(), header = T)
data
view(data)
t.test(data$united.kingdom,data$germany,alternative="two.sided",var.equal=TRUE)


"Paired T test"
data2 = read.csv(file.choose(), header = T)
data2
t.test(data2$Weight.before,data2$Weights.after,alternative="greater",paired=TRUE)


"One Sample T test"
data3 = read.csv(file.choose(), header = T)
data3
t.test(data3$IQ,alternative="two.sided",mu=100)
