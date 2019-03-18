#########################
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02", "03")
subtitle = "2018.7 - 2019.3" 

udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
input_data$title_prep <- prep_fun(input_data$title)
input_data$title_prep <- prep_fun2(input_data$title_prep) 
input_data$title_prep[1:3]

### title - entire period
cooc_data <- coocurrence_data(input_data$title_prep)
row.weird.character.finder <- grepl("^[[:alnum:]]+$", cooc_data[,1])&grepl("^[[:alnum:]]+$", cooc_data[,2])
cooc_data <- cooc_data[row.weird.character.finder, ]

g1 <- network_graph(cooc_data, cut.point = cut.point, 
                    title = paste0("Cooccurrence Network in ", input), 
                    subtitle = subtitle,
                    edge.col = "#FF0000",
                    text.col = "#35274A",
                    default.text.size = 3,
                    default.node.size= 4)

pdf(file=paste0(file.name, "_network_total.pdf"),
    family="sans", width=8, height=8)
g1
dev.off()
png(file=paste0(file.name, "_network_total.png"),
    width = 205, height = 205, units='mm', res = 150)
print(g1)
dev.off()


### title - monthly
for(i in 1:length(month.name)){
    year <- ifelse(i > 6, "2019", "2018")
    cooc_data <- coocurrence_data(input_data$title_prep[input_data$month==month.name[i]])
    row.weird.character.finder <- grepl("^[[:alnum:]]+$", cooc_data[,1])&grepl("^[[:alnum:]]+$", cooc_data[,2])
    cooc_data <- cooc_data[row.weird.character.finder, ]
    gg <- network_graph(cooc_data, cut.point = cut.point, 
                        title = paste0("Cooccurrence Network in ", input, " at ", year, "-", month.name[i]), 
                        default.text.size = 3,
                        default.node.size= 5)
    pdf(file=paste0(file.name, "_network_at", year,"-",month.name[i], ".pdf"),
        family="sans", width=8, height=8)
    print(gg)
    dev.off()
    png(file=paste0(file.name, "_network_at", year,"-",month.name[i], ".png"),
        width = 205, height = 205, units='mm', res = 150)
    print(gg)
    dev.off()
    
}



### before
cooc_data <- coocurrence_data(input_data$title_prep[is.element(input_data$month, c("07" ,"08" ,"09" ,"10"))])
row.weird.character.finder <- grepl("^[[:alnum:]]+$", cooc_data[,1])&grepl("^[[:alnum:]]+$", cooc_data[,2])
cooc_data <- cooc_data[row.weird.character.finder, ]
gg <- network_graph(cooc_data, cut.point = cut.point,  layout = "nicely", 
                    title = paste0("Cooccurrence network in ", input, " before the mid-term election"), 
                    default.text.size = 3,
                    default.node.size= 5)
pdf(file=paste0(file.name, "_network_preelection.pdf"),
    family="sans", width=8, height=8)
print(gg)
dev.off()
png(file=paste0(file.name, "_network_preelection.png"),
    width = 205, height = 205, units='mm', res = 300)
print(gg)
dev.off()

### after
cooc_data <- coocurrence_data(input_data$title_prep[is.element(input_data$month, c("10" ,"11" ,"12", "01", "02", "03"))])
row.weird.character.finder <- grepl("^[[:alnum:]]+$", cooc_data[,1])&grepl("^[[:alnum:]]+$", cooc_data[,2])
cooc_data <- cooc_data[row.weird.character.finder, ]
gg <- network_graph(cooc_data, cut.point = cut.point, layout = "nicely", 
                    title = paste0("Cooccurrence network in ", input, " after the mid-term election"), 
                    default.text.size = 3,
                    default.node.size= 5)
pdf(file=paste0(file.name, "_network_postelection.pdf"),
    family="sans", width=8, height=8)
print(gg)
dev.off()
png(file=paste0(file.name, "_network_postelection.png"),
    width = 205, height = 205, units='mm', res = 300)
print(gg)
dev.off()





