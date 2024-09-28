library("magrittr")
library("janitor")
library("ggplot2")
library("forcats")
library("dplyr")
library("phyloseq")
library("ggpubr")
library("tidyr")
library("Rmisc")
library("microViz")
library("readxl")
library("tidyverse")
library("conflicted")
library("ggExtra")
library("cowplot")
library("gridExtra")
library("ggforce")
library("vegan")
library('rstatix')
library("ggpmisc")
library("data.table")
library("tibble")
library("pheatmap")
library("grid")

My_Theme = theme(axis.title.x = element_text(face="bold",size=24),
                 axis.text.x = element_text(colour = "black", size=22), 
                 axis.text.y = element_text(colour = "black", size=22),
                 axis.title.y = element_text(face="bold",size=24),
                 plot.title = element_text(size = 24),
                 legend.title =element_text(face="bold",size = 14),
                 legend.text = element_text(size = 14),
                 legend.key.size = unit(1, "cm"),
                 strip.text.x = element_text(size=22, face="bold"),
                 strip.text.y = element_text(size=22, face="bold"),
                 panel.border = element_rect(fill = NA, colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank())

library(microViz)
brewerPlus <- distinct_palette()

###Figure 1.
library(viridis)
library(ggExtra)
#import data from Table S1 with all bins 
All_bins <- read.csv("/Users/yugibeast/Library/CloudStorage/OneDrive-UniversityofOtago/ARF/Manuscripts/Soil_Global_MAG/raw_data/all_soil_bins.csv",fill = TRUE, header = TRUE, sep = ",")

All_bins$Treatment = factor(All_bins$Treatment, levels = c("AB", "AF", "AB+AF", "PC", "NC"))

#making baseplot
Baseplot <- ggplot(All_bins, aes(x=Completeness, y=Contamination, color = Treatment)) + 
  geom_point(aes(size=Bin_Size.Mbp.)) + 
  scale_color_viridis(discrete=TRUE)+
  theme_light()+
  My_Theme+
  xlab("Completeness") + 
  ylab("Contamination")+
  labs(colour = "Treatment", size = "Bin Size (Mbp)")+
  guides(color = guide_legend(override.aes = list(size = 5) ) )
Baseplot

#add marginal density plots
density_plot <- ggMarginal(Baseplot, type = "histogram", binwidth=1)

#pdf("/Volumes/micro-shared$/MoralesLab/Manuscripts/MOTS_global_paper/temp_figs/comp_cont_binsize.pdf",width=7,height=5) # Open a new pdf file

#view
density_plot

dev.off()
#Save as PDF

mean(All_bins$Contamination)
range(All_bins$Contamination)
mean(All_bins$Completeness)
range(All_bins$Completeness)

#facetted
baseplot_facet<-Baseplot +facet_grid(.~Treatment)
baseplot_facet

#load data
library(ggforce)

dodge <- position_dodge(width=0.5)  # move dots .01 to the left and right to avoid overlap


qual_plot<-ggplot(All_bins, aes(x=Completeness, y=Contamination, colour=Treatment, group=Treatment)) +
  geom_point(aes(size=Bin_Size.Mbp.),alpha = 0.5)+
  scale_color_viridis(discrete=TRUE)+
  labs(colour = "Treatment", size = "Bin Size (Mbp)", tag = "A")+
  theme_light()+
  My_Theme+
  theme(legend.position="none")+ 
  guides(colour = guide_legend(override.aes = list(size=5, alpha = 0.5))) +
  xlim(40, 110) +
  scale_y_continuous(breaks=c(0, 4, 8, 12))
qual_plot 


library(cowplot)
#Save the legend 
conflicts_prefer(cowplot::get_legend)
legend <- get_legend(qual_plot)

#Remove the legend from qual_plot
qual_plot <- qual_plot + theme(legend.position="none")      



library(ggridges)
library(ggpubr)

binsize_ridge<-ggplot(All_bins, aes(x=Bin_Size.Mbp., y=Treatment, fill=Treatment))+
  geom_density_ridges(jittered_points = TRUE,alpha = 0.5) +
  scale_y_discrete(drop=FALSE)+
  scale_color_viridis(discrete=TRUE)+
  theme_light()+
  My_Theme+
  xlab("Bin Size (Mbp)") + ylab("Treatment")+ 
  expand_limits(y= c(1, length(levels(All_bins$Treatment)) + 1.5))+
  theme(legend.position="none")      

binsize_ridge

bincont_ridge<-ggplot(All_bins, aes(x=Contamination, y=Treatment, fill=Treatment))+
  geom_density_ridges(jittered_points = TRUE,alpha = 0.5) +
  scale_y_discrete(drop=FALSE)+
  scale_color_viridis(discrete=TRUE)+
  theme_light()+
  My_Theme+
  labs(tag = "B") +
  xlab("Contamination") + ylab("Treatment")+ 
  expand_limits(y= c(1, length(levels(All_bins$Treatment)) + 1.5))+
  rotate()+theme(legend.position="none")


bincont_ridge

bincomp_ridge<-ggplot(All_bins, aes(x=Completeness, y=Treatment, fill=Treatment))+
  geom_density_ridges(jittered_points = TRUE,alpha = 0.5) +
  scale_y_discrete(drop=FALSE)+
  scale_color_viridis(discrete=TRUE)+
  theme_light()+
  theme(axis.title.x = element_text(face="bold",size=20),
        axis.text.x = element_text(colour = "black", size=22), 
        axis.text.y = element_text(colour = "black", size=18),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=22, face="bold"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("Completeness") + ylab("Treatment")+ 
  labs(tags = "C") +
  expand_limits(y= c(1.5, length(levels(All_bins$Treatment)) + 1.5))+
  theme(legend.position="none") +
  xlim(40,110)

bincomp_ridge

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )
library(gridExtra)
pdf("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Yugen_Figuresbin_quality.pdf", width = 12, height = 10) # Open a new pdf file
grid.arrange(bincomp_ridge, legend, qual_plot, bincont_ridge, 
             ncol=2, nrow=2, widths=c(4, 2.5), heights=c(3, 4))



dev.off()



###MAG by taxa
#Melt phyloseq object

soils.taxa.csv <- read.csv(file="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv", header = TRUE, na.strings = "unclassified", fileEncoding="UTF-8-BOM", check.names = FALSE, row.names = 1)

rank_df <- soils.taxa.csv[ -c(1:11) ]

#rank_df[is.na(rank_df)] <- Unclassified

#calculate unique levels by rank
rank_summary <-as.data.frame(apply(rank_df, 2, function(x) length(unique(x))))
rank_summary

library(tibble)
rank_summary<-rank_df %>% 
  #group_by(Watermass) %>%
  summarise_all(n_distinct)%>% 
  t()%>%
  data.frame()%>%
  rownames_to_column(var = "Rank")%>%
  rename_with(.cols = 2, ~"Count")

rank_summary

#calculate the number of BINS classified at each rank
rank_summary$Count <-as.numeric(rank_summary$Count)
rank_summary$Unclassified_count <-colSums(is.na(rank_df) | rank_df == "")
rank_summary$Classified_count<-969-rank_summary$Unclassified_count
rank_summary$percent_bins_classified<- ((rank_summary$Classified_count)/nrow(rank_df))*100

#reorder Ranks
rank_summary$Rank <- factor(rank_summary$Rank, levels=c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Watermass"))

rank_summary <- rank_summary[-8,]

library(viridis)
percent_classified_MAGs <- ggplot(rank_summary,aes(x= Rank, y= Count, size=percent_bins_classified, label=Count)) + 
  geom_point()+
  labs(y="Number of Distinct Taxa", x="Taxonomic Rank", size = "Percent of classified MAGS", tag = "E")+
  #My_Theme +
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(angle=45, colour = "black", vjust=1, hjust = 1, size=22), 
        axis.text.y = element_text(colour = "black", size=25),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        panel.background = element_blank(),
        strip.text.x = element_text(size=22, color="black"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black")) +
  theme(legend.position=c(0.3, 0.7)) +
  scale_y_continuous(limits = c(0, 60),breaks=c(0,10,20,30, 40, 50, 60))+geom_text(aes(label = Count),size = 6, color="black", vjust = -1.5)


#SOIL
# Separate SOIL data from Metagenome_Read_counts-2 and rename it to Soil_metagenome_read_counts, remove % mapped column because it messes with R, calculate this later in R at mutate step
Soil_metagenome_read_counts=read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig3_and_4/Soil_metagenome_read_count_corrected.csv")

#Rename column #of Hits to Number_of_Hits 
Soil_ORF_CLASS_edited=read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig3_and_4/Soil_ORF_CLASS_edited.csv")

Soil_metadata=read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig3_and_4/Soil_metadata.csv")

#Join metagenome reads with ORF by sample ID
Soil_Joined_metagenome_ORF=merge(Soil_metagenome_read_counts,Soil_ORF_CLASS_edited, by="Sample", all=TRUE)

#Join merged metagenome reads +ORF above with metadata
Soil_Joined_metagenome_ORF_metadata=merge(Soil_Joined_metagenome_ORF,Soil_metadata, by="Sample", all=TRUE)


#Mutate and create new column ((# of ORF hits)/(# of Sample Reads))*1,000,000 to get ORF hits per million reads for each class present in each sample
conflicts_prefer(rstatix::mutate)
Soil_ORF_Class_hits_per_mil_reads=Soil_Joined_metagenome_ORF_metadata %>% 
  select (total_Reads,Class, Number_of_Hits, Samples,Sample, Treatment, Day,mapped_Reads,unmapped_Reads) %>%
  mutate(ORF_hits_per_million_reads=(Number_of_Hits/total_Reads)*1000000) %>%
  mutate(Percent_mapped=(mapped_Reads/total_Reads)*100)%>%
  mutate(Percent_unmapped=(unmapped_Reads/total_Reads)*100)



#Set day as factor
Soil_ORF_Class_hits_per_mil_reads$Day<-as.factor(Soil_ORF_Class_hits_per_mil_reads$Day)
Soil_ORF_Class_hits_per_mil_reads$Samples<-factor(Soil_ORF_Class_hits_per_mil_reads$Samples, levels=c("205R1","205R2","205R3","P205","210R1","210R2","210R3","P210","215R1","215R2","215R3","P215","225R1","225R2","225R3","P225","230R1", "230R2","230R3","P230", "P305","P310","P315","P325","P330","P405","P410","P415","P425","P430","P505","P510","P515","P525","P530","P605","P610","P615","P625","P630","P705","P710","P715","P725","P730","P805","P810","P815","P825","P830"))

#Calculate summary SE
library(Rmisc)
Summary_Soil_ORF_Class_hits_per_mil_reads<-summarySE(Soil_ORF_Class_hits_per_mil_reads,measurevar="ORF_hits_per_million_reads",groupvars=c("Treatment","Day","Class"))



#Remove NA's
Summary_Soil_ORF_Class_hits_per_mil_reads=Summary_Soil_ORF_Class_hits_per_mil_reads[!is.na(Summary_Soil_ORF_Class_hits_per_mil_reads$Class), ]


#write.csv
#write.csv(Summary_Soil_ORF_Class_hits_per_mil_reads,"/Users/syalinyganasamurthy/Dropbox/Ganasamurthy_Global_MAG_paper/Code_files/Fig3/Summary_Soil_ORF_Class_hits_per_mil_reads.csv")


#chnage to factor
Summary_Soil_ORF_Class_hits_per_mil_reads$Day<-as.factor(Summary_Soil_ORF_Class_hits_per_mil_reads$Day)

#order treatment
Summary_Soil_ORF_Class_hits_per_mil_reads$Treatment<-factor(Summary_Soil_ORF_Class_hits_per_mil_reads$Treatment, levels=c("NC","PC","AB","AF","AB+AF"))

abundance <- subset(Summary_Soil_ORF_Class_hits_per_mil_reads, select = -c(Day, N, sd, se, ci))

library(data.table)
setDT(abundance)
abundance1  = abundance [ , .(counted = sum(ORF_hits_per_million_reads)), by = .(Class, Treatment)]

for_phylum <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv")

for_phylum <- subset(for_phylum, select = c(Phylum, Class))

abundance_phylum <- merge(abundance, for_phylum, by = c("Class"), allow.cartesian=TRUE)

abundance_phylum <- subset(abundance_phylum, select = -c(Class))

library(data.table)
setDT(abundance_phylum)
abundance_phylum1  = abundance_phylum [ , .(counted = sum(ORF_hits_per_million_reads)), by = .(Phylum, Treatment)]

#reorder Phyla by highest number of bins and replot
abundance1$Class<-factor(abundance1$Class,levels =c( "Alphaproteobacteria", "Gammaproteobacteria", "Gemmatimonadetes", "Bacteroidia", "Actinobacteria", "Bacilli_A", "Myxococcia", "Nitrososphaeria", "Nitrospiria", "Polyangia", "Saccharimonadia", "Thermoanaerobaculia", "Thermoleophilia","Verrucomicrobiae", "Vicinamibacteria"))

for_absolute_abundance <- read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv")

for_Phylum_absolute_abundance <- subset(for_absolute_abundance, select = c(Bin_ID, Treatment, Phylum))
for_Class_absolute_abundance <- subset(for_absolute_abundance, select = c(Bin_ID, Treatment, Class))

conflicts_prefer(dplyr::count)
Phylum_absolute_abundance <- dplyr::rename(count(for_Phylum_absolute_abundance, Phylum, Treatment), Freq = n)
Class_absolute_abundance <- dplyr::rename(count(for_Class_absolute_abundance, Class, Treatment), Freq = n)

Class_absolute_abundance$Class<-factor(Class_absolute_abundance$Class,levels =c("Alphaproteobacteria", "Gammaproteobacteria", "Gemmatimonadetes", "Bacteroidia", "Actinobacteria", "Bacilli_A", "Myxococcia", "Nitrososphaeria", "Nitrospiria", "Polyangia", "Saccharimonadia", "Thermoanaerobaculia", "Thermoleophilia","Verrucomicrobiae", "Vicinamibacteria"))

Class_absolute_abundance$Treatment<-factor(Class_absolute_abundance$Treatment,levels =c("AB","AF","AB+AF","PC","NC"))

Class_taxanomic_breakdown <- ggplot(Class_absolute_abundance, aes(x=Treatment, y = Freq, fill=Class)) +
  geom_bar(stat="identity", color = "black")  +
  scale_fill_manual(values = c("Actinobacteria" = "#C2DD9B", "Alphaproteobacteria" = "#B4CCDF", "Bacilli_A" = "#4A75AA", "Bacteroidia" = "#E3A29D", "Gammaproteobacteria" = "#C04335", "Gemmatimonadetes" = "#ECC485", "Myxococcia" = "#E18B46", "Nitrososphaeria" = "#4B9B7A", "Nitrospiria" = "#5F4190", "Polyangia" = "#CA6728", "Saccharimonadia" = "#9C623C", "Thermoanaerobaculia" = "#666666", "Thermoleophilia" = "#D43F88","Verrucomicrobiae" = "#FFFFB0", "Vicinamibacteria" = "#7470AF")) +
  labs(y = "Absolute abundance", x = "Treatment", tag = "F") +
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(colour = "black", size=22, angle = 45, hjust = 1), 
        axis.text.y = element_text(colour = "black", size=22),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=22, face="bold"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  guides(fill=guide_legend(ncol=2))+
  theme(legend.position = "none")

Class_taxanomic_breakdown


blank <- grid.rect(gp=gpar(col="white"))

pdf("/Users/yugibeast/Library/CloudStorage/Dropbox/Ganasamurthy_Global_MAG_paper/Main_Figures/Figure_1.pdf", width = 16, height = 18) # Open a new pdf file
grid.arrange(qual_plot, bincont_ridge, bincomp_ridge, blank, percent_classified_MAGs, Class_taxanomic_breakdown,
             ncol=2, nrow=3, widths=c(3, 3), heights=c(4, 4, 4))

dev.off()

###Figure 2

mots.data <- read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/MOTS_global_paper/raw_data/mots_mag_fnct.csv")


#Rename column lablled n to Counts
names(mots.data)[names(mots.data) == "n"] <- "Counts"
names(mots.data)

#Remove count data that are showing up as NA
mots.data2=na.omit(mots.data)

#Order phylum ascending
mots.data2$Phylum=factor(mots.data2$Phylum, levels=c("Dadabacteria","Binatota","Fibrobacterota","Marinisomatota","Myxococcota","Nitrospinota","Acidobacteriota","Gemmatimonadota","Planctomycetota","SAR324",                                            "Crenarchaeota","Chloroflexota","Cyanobacteria","Verrucomicrobiota","Actinobacteriota","Bacteroidota",
                                                     "Thermoplasmatota","Proteobacteria"))


library(tidyverse)
library(grid)
library(cowplot)



p2 <- ggplot(data = mots.data2, aes(x = Phylum,y = Function, size=Counts))+
  geom_point(aes(fill=Completeness),pch=21)+
  scale_fill_viridis_c(option="plasma", limits = c(75, 90), oob = scales::squish)+ scale_size_continuous(limits=c(0,400), range=c(0.5,30), breaks=c(1, 50, 100, 200 ,300))+
  facet_grid("Category", scales = "free_y", space = "free",labeller=labeller(Category=label_wrap_gen(10)))+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey35"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title =element_text(face="bold",size = 24),
    legend.text = element_text(size = 22),
    axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, face="bold",size=22),
    axis.text.y = element_text(size=22),
    axis.title.x = element_text(color = "black",size=22),
    axis.title.y = element_text(color = "black",size=22),
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(colour = "white",size=5,lineheight=0.01),
    legend.position="none"
  )+
  labs(y="Gene", x="Phylum")



p2=p2+theme(panel.spacing =unit(.05, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = .5), 
            strip.background = element_rect(color = "white", size = .5))

p2


g2 <- ggplot_gtable(ggplot_build(p2))
stripr <- which(grepl('strip-r', g2$layout$name))
fills <- c("cadetblue3","aquamarine2","darkgoldenrod1","lightcoral","thistle3","rosybrown4","lightsteelblue3","yellow")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g2$grobs[[i]]$grobs[[1]]$childrenOrder))
  g2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

grid.draw(g2)



func_col <- c("Alt e acceptor"= "cadetblue3",
              "Alt e donor"="aquamarine2",
              "Carbon fixation"="darkgoldenrod1",
              "Nitrogen cycle"="lightcoral",
              "Phototrophy"="thistle3",
              "Respiration"="rosybrown4",
              "Sulfur cycle"="lightsteelblue3",
              "Trace gas metabolism
"="yellow")


legend_func<-ggplot(mots.data2, aes(x=Category, fill=Phylum)) +
  geom_bar(stat="count")+
  scale_fill_manual(values = func_col)+
  My_Theme+
  theme(legend.position="bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"))+
  guides(fill=guide_legend(nrow=2))


#Save the legend  
func_legend <- get_legend(legend_func)
grid.newpage()
grid.draw(func_legend)

pdf("/Volumes/micro-shared$/MoralesLab/Manuscripts/MOTS_global_paper/temp_figs/diamond_plot.pdf", width = 8, height = 10) # Open a new pdf file
grid.arrange(g2, func_legend, ncol=1, nrow=2, widths=c(8), heights=c(10, 1))
dev.off()

###Figure 3 DRAM important Heatmap
###PER BIN
#load in data for summary heatmap
product1 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product2 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product3 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product4 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product5 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)

product_combined <- dplyr::bind_rows(product1, product2, product3, product4, product5)


mdata <- melt(best_bins, id=c("Bin_ID","Day", "Treatment", "Class"))
Row_clusters<-mdata[ -c(5:6)]
Row_clusters<-distinct(Row_clusters,Bin_ID,.keep_all= TRUE)
rownames(Row_clusters) <- Row_clusters$Bin_ID
Row_clusters <- Row_clusters[ -c(1) ]
Row_clusters$Treatment<-as.character(Row_clusters$Treatment)

#keep only bins being analysed
conflicts_prefer(rstatix::filter)
product_combined<-filter(product_combined, genome %in% best_bins$Bin_ID)


#split into presence/absence vs pathway completeness results
all_bin_pcompleteness <- product_combined[ -c(34:99) ]

all_bin_cazy <- product_combined[ -c(2:33) ]

#rename 'genome' to 'Bin_ID' for consistency
conflicts_prefer(plyr::rename)
all_bin_pcompleteness<-rename(all_bin_pcompleteness, c("genome" = "Bin_ID" ))
all_bin_cazy<-rename(all_bin_cazy, c("genome" = "Bin_ID"  ))

#remove duplicate rows/bins
all_bin_pcompleteness<-unique(all_bin_pcompleteness, by = "Bin_ID")
all_bin_cazy<-unique(all_bin_cazy, by = "Bin_ID")

#replace True/False for 1/0
all_bin_cazy[all_bin_cazy == "False"] <- "0"
all_bin_cazy[all_bin_cazy == "True"] <- "1"

#make functional data into matrix

rownames(all_bin_pcompleteness) <- all_bin_pcompleteness$Bin_ID

#identify and delete genes with no hits
all_bin_pcompleteness<-all_bin_pcompleteness[, colSums(all_bin_pcompleteness != 0) > 0]
pcompleteness_matrix <- as.matrix(all_bin_pcompleteness[,-1])

rownames(pcompleteness_matrix) <- all_bin_pcompleteness[,1]


completeness_matrix <- mapply(pcompleteness_matrix, FUN=as.numeric)
completeness_matrix <- matrix(data=completeness_matrix, ncol=length(colnames(pcompleteness_matrix)), nrow=length(row.names(pcompleteness_matrix)))
row.names(completeness_matrix) <- row.names(pcompleteness_matrix)
colnames(completeness_matrix) <- colnames(pcompleteness_matrix)

pheatmap(completeness_matrix)


#import modified annotations
functional_categories <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Gemm/raw_data/DRAM/genome_summaries/functional_catgories_rev.csv",fill = TRUE, header = TRUE, sep = ",")

#subset by those detected
func_list <- colnames(pcompleteness_matrix)
functional_categories<-filter(functional_categories, Function %in% func_list)

#Keep only two columns used for plots
rownames(functional_categories) <- functional_categories$Function
functional_categories <- functional_categories[ -c(1) ]
functional_categories<-rename(functional_categories, c("Renamed_function" = "Function"))
functional_categories_annotation <- functional_categories[ -c(2) ]



#test heatmap
pheatmap(completeness_matrix, annotation_row = Row_clusters, annotation_col = functional_categories,show_colnames=FALSE, labels_row = Row_clusters$Treatment)

func_category_colour = list(
  Class = c("Actinobacteria" = "#C2DD9B", "Alphaproteobacteria" = "#B4CCDF", "Bacilli_A" = "#4A75AA", "Bacteroidia" = "#E3A29D", "Gammaproteobacteria" = "#C04335", "Gemmatimonadetes" = "#ECC485", "Myxococcia" = "#E18B46", "Nitrososphaeria" = "#4B9B7A", "Nitrospiria" = "#5F4190", "Polyangia" = "#CA6728", "Saccharimonadia" = "#9C623C", "Thermoanaerobaculia" = "#666666", "Thermoleophilia" = "#D43F88","Verrucomicrobiae" = "#FFFFB0", "Vicinamibacteria" = "#7470AF"), "Category" = c("Central C metabolism"="#000000", "ETC complex I"="#990F0F", "ETC complex II"="#99540F", "ETC complex III"="#6B990F", "ETC complex IV High affinity"="#0F6B99", "ETC complex IV Low affinity"="#260F99", "ETC complex V"="#CC79A7"),
  Treatment = c("AB" = "#0B087C", "AF" = "#691F9C", "AB+AF" = "#B05776", "PC"="#DF9D60", "NC"="#F3F973"))
#set color palette

Row_clusters <- subset(Row_clusters, select = c(Bin))

#library(viridis)
#pheatmap(completeness_matrix,
 ##       annotation_colors = func_category_colour,
   #      annotation_row = Row_clusters, 
    #     annotation_col = functional_categories_annotation,
     #    labels_col = functional_categories$Function,
      #   show_colnames=TRUE,
       #  drop_levels = TRUE,
        # annotation_legend= TRUE,
         #annotation_names_row = TRUE,
         #fontsize = 10, 
         #fontsize_row = 6, 
         #fontsize_col = 6,
         #cellwidth = 8,
         #angle_col = 45,
         #cellheight = 5,
         #cutree_rows = 6)
         #filename ="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Yugen_Figures/DRAM_summary_heatmap.pdf")

#dev.off()

rownames(all_bin_cazy) <- all_bin_cazy$Bin_ID

#identify and delete genes with no hits
all_bin_cazy<-all_bin_cazy[, colSums(all_bin_cazy != 0) > 0]
cazy_matrix <- as.matrix(all_bin_cazy[,-1])

rownames(cazy_matrix) <- all_bin_cazy[,1]


bincazy_matrix <- mapply(cazy_matrix, FUN=as.numeric)
bincazy_matrix <- matrix(data=bincazy_matrix, ncol=length(colnames(cazy_matrix)), nrow=length(row.names(cazy_matrix)))
row.names(bincazy_matrix) <- row.names(cazy_matrix)
colnames(bincazy_matrix) <- colnames(cazy_matrix)

pheatmap(bincazy_matrix)

#create a document with functional annotations
cazy_categories<-as.data.frame(t(cazy_matrix))
cazy_categories <- tibble::rownames_to_column(cazy_categories, "Cazy_Function") # Apply rownames_to_column
#save as csv and modify file to create category annotations
#write.csv(cazy_categories, "/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/cazy_categories.csv", row.names = T)

#re-import modified annotations
cazy_categories <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/cazy_categories_rev.csv",fill = TRUE, header = TRUE, sep = ",")

#remove leading space
cazy_categories$Renamed_function <- trimws(cazy_categories$Renamed_function, which = c("left"))

#subset by those detected
cazy_list <- colnames(cazy_matrix)
cazy_categories<-filter(cazy_categories, Cazy_Function %in% cazy_list)

#Keep only two columns used for plots
rownames(cazy_categories) <- cazy_categories$Cazy_Function
cazy_categories <- cazy_categories[ -c(1) ]
cazy_categories<-rename(cazy_categories, c("Renamed_function" = "CAZy_Function"))
cazy_categories_annotation <- cazy_categories[ -c(2) ]





#test heatmap
pheatmap(bincazy_matrix, 
         annotation_row = Row_clusters, 
         annotation_col = cazy_categories,
         show_colnames=FALSE, 
         labels_row = Row_clusters$mOTU)


#set color palette
cazy_category_colour = list(
  Class = c("Actinobacteria" = "#C2DD9B", "Alphaproteobacteria" = "#B4CCDF", "Bacilli_A" = "#4A75AA", "Bacteroidia" = "#E3A29D", "Gammaproteobacteria" = "#C04335", "Gemmatimonadetes" = "#ECC485", "Myxococcia" = "#E18B46", "Nitrososphaeria" = "#4B9B7A", "Nitrospiria" = "#5F4190", "Polyangia" = "#CA6728", "Saccharimonadia" = "#9C623C", "Thermoanaerobaculia" = "#666666", "Thermoleophilia" = "#D43F88","Verrucomicrobiae" = "#FFFFB0", "Vicinamibacteria" = "#7470AF"),
  "Category" = c("CAZy"="#CC79A7", "Methanogenesis and methanotrophy"="#003C30", "Nitrogen metabolism"="#7F0000", "Other Reductases"="black", "SCFA and alcohol conversions"="#6A51A3", "Sulfur metabolism"="#FDAE61"),
  Treatment = c("AB" = "#0B087C", "AF" = "#691F9C", "AB+AF" = "#B05776", "PC"="#DF9D60", "NC"="#F3F973"))

#pheatmap(bincazy_matrix,
         color=c("red", "blue"),
         annotation_colors = cazy_category_colour,
         annotation_row = Row_clusters, 
         annotation_col = cazy_categories_annotation,
         labels_col = cazy_categories$CAZy_Function,
         show_colnames=TRUE,
         drop_levels = TRUE,
         annotation_legend= TRUE,
         annotation_names_row = TRUE,
         fontsize = 10, 
         fontsize_row = 6, 
         fontsize_col = 6,
         cellwidth = 9,
         angle_col = 45,
         cellheight = 5,
         cutree_rows = 6)
         filename ="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Yugen_Figures/DRAM_cazy_heatmap.pdf")

dev.off()

###PER GENUS
product1 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product2 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product3 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product4 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)
product5 <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/product.tsv",fill = TRUE, header = TRUE, sep = "\t",check.names=FALSE)

remove(product_combined)
remove(product_combined_genus)
product_combined <- dplyr::bind_rows(product1, product2, product3, product4, product5)

best_bins <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/Soils_bin_base_data.csv",fill = TRUE, header = TRUE, sep = ",")



best_bins_taxa <- as.matrix(best_bins[12:18])
  
best_bins_taxa <- tax_table(best_bins_taxa)


best_bins_taxa <- best_bins_taxa %>%
  tax_fix()

best_bins_taxa <- data.frame(best_bins_taxa)
  
best_bins <- best_bins %>%
  .[-c(12:18)]

best_bins <- column_to_rownames(best_bins, var = "Bin_ID")

best_bins_combined <- cbind(best_bins, best_bins_taxa)

best_bins <- rownames_to_column(best_bins_combined, var = "Bin_ID")

mdata <- melt(best_bins, id=c("Bin_ID","Day", "Class", "Treatment", "Genus"))

##combining for just by genome
product_combined_genus <- merge(product_combined, mdata, by.x = ("genome"), by.y = ("Bin_ID"))
product_combined_genus <- subset(product_combined_genus, select = -c(genome, Day, Treatment, Class, variable, value))

product_combined_genus <- product_combined_genus %>%
  select(Genus, everything())


Row_clusters<-mdata[ -c(6:7)]
Row_clusters<-distinct(Row_clusters,Genus,.keep_all= TRUE)
rownames(Row_clusters) <- Row_clusters$Genus
Row_clusters <- Row_clusters[ -c(1) ]
#Row_clusters$Treatment<-as.character(Row_clusters$Treatment)

Row_clusters <- Row_clusters[ -c(4)]


#keep only bins being analysed
library(conflicted)
conflicts_prefer(rstatix::filter)
product_combined<-filter(product_combined_genus, Genus %in% best_bins$Genus)




#split into presence/absence vs pathway completeness results
all_bin_pcompleteness <- product_combined[ -c(34:99) ]

all_bin_cazy <- product_combined[ -c(2:33) ]

#rename 'genome' to 'Bin_ID' for consistency
conflicts_prefer(plyr::rename)
all_bin_pcompleteness<-rename(all_bin_pcompleteness, c("Genus" = "Bin_ID" ))
all_bin_cazy<-rename(all_bin_cazy, c("Genus" = "Bin_ID"  ))



#remove duplicate rows/bins
all_bin_pcompleteness<-unique(all_bin_pcompleteness, by = "Bin_ID")
all_bin_cazy<-unique(all_bin_cazy, by = "Bin_ID")

#replace True/False for 1/0
all_bin_cazy[all_bin_cazy == "False"] <- "0"
all_bin_cazy[all_bin_cazy == "True"] <- "1"

#make functional data into matrix

all_bin_pcompleteness <- aggregate(. ~ Bin_ID, data = all_bin_pcompleteness, FUN = mean)

rownames(all_bin_pcompleteness) <- all_bin_pcompleteness$Bin_ID

#identify and delete genes with no hits
all_bin_pcompleteness<-all_bin_pcompleteness[, colSums(all_bin_pcompleteness != 0) > 0]
pcompleteness_matrix <- as.matrix(all_bin_pcompleteness[,-1])

rownames(pcompleteness_matrix) <- all_bin_pcompleteness[,1]


completeness_matrix <- mapply(pcompleteness_matrix, FUN=as.numeric)
completeness_matrix <- matrix(data=completeness_matrix, ncol=length(colnames(pcompleteness_matrix)), nrow=length(row.names(pcompleteness_matrix)))
row.names(completeness_matrix) <- row.names(pcompleteness_matrix)
colnames(completeness_matrix) <- colnames(pcompleteness_matrix)

library(pheatmap)
library(viridis)

pheatmap(completeness_matrix)


#import modified annotations
functional_categories <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Gemm/raw_data/DRAM/genome_summaries/functional_catgories_rev.csv",fill = TRUE, header = TRUE, sep = ",")

#subset by those detected
func_list <- colnames(pcompleteness_matrix)
functional_categories<-filter(functional_categories, Function %in% func_list)

#Keep only two columns used for plots
rownames(functional_categories) <- functional_categories$Function
functional_categories <- functional_categories[ -c(1) ]
functional_categories<-rename(functional_categories, c("Renamed_function" = "Function"))
functional_categories_annotation <- functional_categories[ -c(2) ]



#test heatmap
pheatmap(completeness_matrix, 
         annotation_row = Row_clusters, 
         annotation_col = functional_categories,
         show_colnames=TRUE, labels_row = Row_clusters$Genus)

func_category_colour = list(
  Class = c("Actinobacteria" = "#C2DD9B", "Alphaproteobacteria" = "#B4CCDF", "Bacilli_A" = "#4A75AA", "Bacteroidia" = "#E3A29D", "Gammaproteobacteria" = "#C04335", "Gemmatimonadetes" = "#ECC485", "Myxococcia" = "#E18B46", "Nitrososphaeria" = "#4B9B7A", "Nitrospiria" = "#5F4190", "Polyangia" = "#CA6728", "Saccharimonadia" = "#9C623C", "Thermoanaerobaculia" = "#666666", "Thermoleophilia" = "#D43F88","Verrucomicrobiae" = "#FFFFB0", "Vicinamibacteria" = "#7470AF"), "Category" = c("Central C metabolism"="#000000", "ETC complex I"="#990F0F", "ETC complex II"="#99540F", "ETC complex III"="#6B990F", "ETC complex IV High affinity"="#0F6B99", "ETC complex IV Low affinity"="#260F99", "ETC complex V"="#CC79A7"),
  Treatment = c("AB" = "#0B087C", "AF" = "#691F9C", "AB+AF" = "#B05776", "PC"="#DF9D60", "NC"="#F3F973"))
#set color palette

pheatmap(completeness_matrix,
         annotation_col = functional_categories,
         color=turbo(10),
         annotation_colors = func_category_colour,
         labels_col = functional_categories$Function)

library(viridis)
pheatmap(completeness_matrix,
         color=turbo(10),
         annotation_colors = func_category_colour,
         annotation_row = Row_clusters, 
         annotation_col = functional_categories_annotation,
         labels_col = functional_categories$Function,
         show_colnames=TRUE,
         drop_levels = TRUE,
         annotation_legend= TRUE,
         annotation_names_row = TRUE,
         fontsize = 10, 
         fontsize_row = 6, 
         fontsize_col = 6,
         cellwidth = 8,
         angle_col = 45,
         cellheight = 5,
         cutree_rows = 6)
         filename ="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Yugen_Figures/DRAM_summary_heatmap_genus.pdf")

dev.off()

###Getting rid of double up of Genus



write.csv(all_bin_cazy, "/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/all_bin_cazy.csv")

all_bin_cazy <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/all_bin_cazy.csv", header = TRUE)

all_bin_cazy <- all_bin_cazy[-1]

all_bin_cazy <- aggregate(. ~ Bin_ID, all_bin_cazy, sum)

rownames(all_bin_cazy) <- all_bin_cazy$Bin_ID

all_bin_cazy_list <- all_bin_cazy$Bin_ID

#replace True/False for 1/0
all_bin_cazy[all_bin_cazy > 0] <- "1"
all_bin_cazy[all_bin_cazy == "0"] <- "0"



all_bin_cazy <- all_bin_cazy[-1]
all_bin_cazy$Bin_ID <- all_bin_cazy_list

all_bin_cazy = all_bin_cazy %>% dplyr::select("Bin_ID",  
                                              everything())

rownames(all_bin_cazy) <- all_bin_cazy$Bin_ID

#identify and delete genes with no hits
all_bin_cazy<-all_bin_cazy[, colSums(all_bin_cazy != 0) > 0]
cazy_matrix <- as.matrix(all_bin_cazy[,-1])

rownames(cazy_matrix) <- all_bin_cazy[,1]


bincazy_matrix <- mapply(cazy_matrix, FUN=as.numeric)
bincazy_matrix <- matrix(data=bincazy_matrix, ncol=length(colnames(cazy_matrix)), nrow=length(row.names(cazy_matrix)))
row.names(bincazy_matrix) <- row.names(cazy_matrix)
colnames(bincazy_matrix) <- colnames(cazy_matrix)

pheatmap(bincazy_matrix)

#create a document with functional annotations
cazy_categories<-as.data.frame(t(cazy_matrix))
cazy_categories <- tibble::rownames_to_column(cazy_categories, "Cazy_Function") # Apply rownames_to_column
#save as csv and modify file to create category annotations
write.csv(cazy_categories, "/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/cazy_categories.csv", row.names = T)



#re-import modified annotations
cazy_categories <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/cazy_categories_rev_rev.csv",fill = TRUE, header = TRUE, sep = ",")
#cazy_categories <- cazy_categories[-1]
#remove leading space
cazy_categories$Renamed_function <- trimws(cazy_categories$Renamed_function, which = c("left"))

#subset by those detected


cazy_list <- colnames(all_bin_cazy)
cazy_list <- cazy_list[-1]
cazy_categories<-filter(cazy_categories, Cazy_Function %in% cazy_list)

#Keep only two columns used for plots
rownames(cazy_categories) <- cazy_categories$Cazy_Function
cazy_categories <- cazy_categories[ -c(1) ]
cazy_categories<-rename(cazy_categories, c("Renamed_function" = "CAZy_Function"))
cazy_categories_annotation <- cazy_categories[ -c(2) ]





#test heatmap
pheatmap(bincazy_matrix,
         annotation_col = cazy_categories,
         show_colnames=FALSE, 
         labels_row = Row_clusters$Genus)


#set color palette
cazy_category_colour = list(
  Class = c("Actinobacteria" = "#C2DD9B", "Alphaproteobacteria" = "#B4CCDF", "Bacilli_A" = "#4A75AA", "Bacteroidia" = "#E3A29D", "Gammaproteobacteria" = "#C04335", "Gemmatimonadetes" = "#ECC485", "Myxococcia" = "#E18B46", "Nitrososphaeria" = "#4B9B7A", "Nitrospiria" = "#5F4190", "Polyangia" = "#CA6728", "Saccharimonadia" = "#9C623C", "Thermoanaerobaculia" = "#666666", "Thermoleophilia" = "#D43F88","Verrucomicrobiae" = "#FFFFB0", "Vicinamibacteria" = "#7470AF"),
  "Category" = c("CAZy"="#CC79A7", "Methanogenesis and methanotrophy"="#003C30", "Nitrogen metabolism"="#7F0000", "Other Reductases"="black", "SCFA and alcohol conversions"="#6A51A3", "Sulfur metabolism"="#FDAE61"),
  Treatment = c("AB" = "#0B087C", "AF" = "#691F9C", "AB+AF" = "#B05776", "PC"="#DF9D60", "NC"="#F3F973"))

pheatmap(bincazy_matrix,
         color=c("red", "blue"),
         annotation_colors = cazy_category_colour,
         annotation_row = Row_clusters, 
         annotation_col = cazy_categories_annotation,
         labels_col = cazy_categories$CAZy_Function,
         show_colnames=TRUE,
         drop_levels = TRUE,
         annotation_legend= TRUE,
         annotation_names_row = TRUE,
         fontsize = 10, 
         fontsize_row = 6, 
         fontsize_col = 6,
         cellwidth = 9,
         angle_col = 45,
         cellheight = 5,
         cutree_rows = 6)
         filename ="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Yugen_Figures/DRAM_cazy_heatmap_genus.pdf")

dev.off()

###Figure 4
conflicts_prefer(rstatix::mutate)
MISC1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "MISC")

MISC1 <-MISC1 %>%
  mutate(MISC1,"function_description" = "Misc", .after = gene_description)

carbon_utilization1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization")

carbon_utilization1 <-carbon_utilization1 %>%
  mutate(carbon_utilization1,"function_description" = "carbon utilization", .after = gene_description)

Transporters1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "Transporters")

Transporters1 <-Transporters1 %>%
  mutate(Transporters1,"function_description" = "Transporters", .after = gene_description)

Energy1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "Energy")

Energy1 <-Energy1 %>%
  mutate(Energy1,"function_description" = "Energy", .after = gene_description)

organic_nitrogen1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "Organic Nitrogen")

organic_nitrogen1 <-organic_nitrogen1 %>%
  mutate(organic_nitrogen1,"function_description" = "Organic Nitrogen", .after = gene_description)

Woodcroft1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization (Woodcroft)")

Woodcroft1 <-Woodcroft1 %>%
  mutate(Woodcroft1,"function_description" = "carbon utilization (Woodcroft)", .after = gene_description)

rRNA1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "rRNA")

rRNA1 <-rRNA1 %>%
  mutate(rRNA1,"function_description" = "rRNA", .after = gene_description)

tRNA1 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_1/genome_summaries/metabolism_summary.xlsx", sheet = "tRNA")

tRNA1 <-tRNA1 %>%
  mutate(tRNA1,"function_description" = "tRNA", .after = gene_description)



##2_genome_summaries
MISC2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "MISC")

MISC2 <-MISC2 %>%
  mutate(MISC2,"function_description" = "Misc", .after = gene_description)

MISC2 <- subset(MISC2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

carbon_utilization2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization")

carbon_utilization2 <-carbon_utilization2 %>%
  mutate(carbon_utilization2,"function_description" = "carbon utilization", .after = gene_description)

carbon_utilization2 <- subset(carbon_utilization2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Transporters2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "Transporters")

Transporters2 <-Transporters2 %>%
  mutate(Transporters2,"function_description" = "Transporters", .after = gene_description)

Transporters2 <- subset(Transporters2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Energy2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "Energy")

Energy2 <-Energy2 %>%
  mutate(Energy2,"function_description" = "Energy", .after = gene_description)

Energy2 <- subset(Energy2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

organic_nitrogen2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "Organic Nitrogen")

organic_nitrogen2 <-organic_nitrogen2 %>%
  mutate(organic_nitrogen2,"function_description" = "Organic Nitrogen", .after = gene_description)

organic_nitrogen2 <- subset(organic_nitrogen2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Woodcroft2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization (Woodcroft)")

Woodcroft2 <-Woodcroft2 %>%
  mutate(Woodcroft2,"function_description" = "carbon utilization (Woodcroft)", .after = gene_description)

Woodcroft2 <- subset(Woodcroft2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

rRNA2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "rRNA")

rRNA2 <-rRNA2 %>%
  mutate(rRNA2,"function_description" = "rRNA", .after = gene_description)

rRNA2 <- subset(rRNA2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

tRNA2 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_2/genome_summaries/metabolism_summary.xlsx", sheet = "tRNA")

tRNA2 <-tRNA2 %>%
  mutate(tRNA2,"function_description" = "tRNA", .after = gene_description)

tRNA2 <- subset(tRNA2, select = -c(gene_id, gene_description, function_description, module, header, subheader))

##3_genome_summaries
MISC3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "MISC")

MISC3 <-MISC3 %>%
  mutate(MISC3,"function_description" = "Misc", .after = gene_description)

MISC3 <- subset(MISC3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

carbon_utilization3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization")

carbon_utilization3 <-carbon_utilization3 %>%
  mutate(carbon_utilization3,"function_description" = "carbon utilization", .after = gene_description)

carbon_utilization3 <- subset(carbon_utilization3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Transporters3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "Transporters")

Transporters3 <-Transporters3 %>%
  mutate(Transporters3,"function_description" = "Transporters", .after = gene_description)

Transporters3 <- subset(Transporters3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Energy3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "Energy")

Energy3 <-Energy3 %>%
  mutate(Energy3,"function_description" = "Energy", .after = gene_description)

Energy3 <- subset(Energy3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

organic_nitrogen3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "Organic Nitrogen")

organic_nitrogen3 <-organic_nitrogen3 %>%
  mutate(organic_nitrogen3,"function_description" = "Organic Nitrogen", .after = gene_description)

organic_nitrogen3 <- subset(organic_nitrogen3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Woodcroft3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization (Woodcroft)")

Woodcroft3 <-Woodcroft3 %>%
  mutate(Woodcroft3,"function_description" = "carbon utilization (Woodcroft)", .after = gene_description)

Woodcroft3 <- subset(Woodcroft3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

rRNA3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "rRNA")

rRNA3 <-rRNA3 %>%
  mutate(rRNA3,"function_description" = "rRNA", .after = gene_description)

rRNA3 <- subset(rRNA3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

tRNA3 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_3/genome_summaries/metabolism_summary.xlsx", sheet = "tRNA")

tRNA3 <-tRNA3 %>%
  mutate(tRNA3,"function_description" = "tRNA", .after = gene_description)

tRNA3 <- subset(tRNA3, select = -c(gene_id, gene_description, function_description, module, header, subheader))

##4_genome_summaries
MISC4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "MISC")

MISC4 <-MISC4 %>%
  mutate(MISC4,"function_description" = "Misc", .after = gene_description)

MISC4 <- subset(MISC4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

carbon_utilization4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization")

carbon_utilization4 <-carbon_utilization4 %>%
  mutate(carbon_utilization4,"function_description" = "carbon utilization", .after = gene_description)

carbon_utilization4 <- subset(carbon_utilization4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Transporters4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "Transporters")

Transporters4 <-Transporters4 %>%
  mutate(Transporters4,"function_description" = "Transporters", .after = gene_description)

Transporters4 <- subset(Transporters4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Energy4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "Energy")

Energy4 <-Energy4 %>%
  mutate(Energy4,"function_description" = "Energy", .after = gene_description)

Energy4 <- subset(Energy4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

organic_nitrogen4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "Organic Nitrogen")

organic_nitrogen4 <-organic_nitrogen4 %>%
  mutate(organic_nitrogen4,"function_description" = "Organic Nitrogen", .after = gene_description)

organic_nitrogen4 <- subset(organic_nitrogen4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Woodcroft4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization (Woodcroft)")

Woodcroft4 <-Woodcroft4 %>%
  mutate(Woodcroft4,"function_description" = "carbon utilization (Woodcroft)", .after = gene_description)

Woodcroft4 <- subset(Woodcroft4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

rRNA4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "rRNA")

rRNA4 <-rRNA4 %>%
  mutate(rRNA4,"function_description" = "rRNA", .after = gene_description)

rRNA4 <- subset(rRNA4, select = -c(gene_id, gene_description, function_description, module, header, subheader))

tRNA4 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_4/genome_summaries/metabolism_summary.xlsx", sheet = "tRNA")

tRNA4 <-tRNA4 %>%
  mutate(tRNA4,"function_description" = "tRNA", .after = gene_description)

tRNA4 <- subset(tRNA4, select = -c(gene_id, gene_description, function_description, module, header, subheader))


##5_genome_summaries
MISC5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "MISC")

MISC5 <-MISC5 %>%
  mutate(MISC5,"function_description" = "Misc", .after = gene_description)

MISC5 <- subset(MISC5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

carbon_utilization5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization")

carbon_utilization5 <-carbon_utilization5 %>%
  mutate(carbon_utilization5,"function_description" = "carbon utilization", .after = gene_description)

carbon_utilization5 <- subset(carbon_utilization5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Transporters5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "Transporters")

Transporters5 <-Transporters5 %>%
  mutate(Transporters5,"function_description" = "Transporters", .after = gene_description)

Transporters5 <- subset(Transporters5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Energy5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "Energy")

Energy5 <-Energy5 %>%
  mutate(Energy5,"function_description" = "Energy", .after = gene_description)

Energy5 <- subset(Energy5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

organic_nitrogen5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "Organic Nitrogen")

organic_nitrogen5 <-organic_nitrogen5 %>%
  mutate(organic_nitrogen5,"function_description" = "Organic Nitrogen", .after = gene_description)

organic_nitrogen5 <- subset(organic_nitrogen5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

Woodcroft5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "carbon utilization (Woodcroft)")

Woodcroft5 <-Woodcroft5 %>%
  mutate(Woodcroft5,"function_description" = "carbon utilization (Woodcroft)", .after = gene_description)

Woodcroft5 <- subset(Woodcroft5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

rRNA5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "rRNA")

rRNA5 <-rRNA5 %>%
  mutate(rRNA5,"function_description" = "rRNA", .after = gene_description)

rRNA5 <- subset(rRNA5, select = -c(gene_id, gene_description, function_description, module, header, subheader))

tRNA5 <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/DRAM/Soil_DRAM_5/genome_summaries/metabolism_summary.xlsx", sheet = "tRNA")

tRNA5 <-tRNA5 %>%
  mutate(tRNA5,"function_description" = "tRNA", .after = gene_description)

tRNA5 <- subset(tRNA5, select = -c(gene_id, gene_description, function_description, module, header, subheader))


MISC_combined <- dplyr::bind_cols(MISC1,MISC2,MISC3,MISC4,MISC5)

carbon_utilization_combined <- dplyr::bind_cols(carbon_utilization1,carbon_utilization2,carbon_utilization3,carbon_utilization4,carbon_utilization5)

Transporters_combined <- dplyr::bind_cols(Transporters1,Transporters2,Transporters3,Transporters4,Transporters5)

Energy_combined <- dplyr::bind_cols(Energy1,Energy2,Energy3,Energy4,Energy5)

organic_nitrogen_combined <- dplyr::bind_cols(organic_nitrogen1,organic_nitrogen2,organic_nitrogen3,organic_nitrogen4,organic_nitrogen5)

Woodcroft_combined <- dplyr::bind_cols(Woodcroft1,Woodcroft2,Woodcroft3,Woodcroft4,Woodcroft5)

rRNA_combined <- dplyr::bind_cols(rRNA1,rRNA2,rRNA3,rRNA4,rRNA5)

#tRNA_combined <- dplyr::bind_cols(tRNA1,tRNA2,tRNA3,tRNA4,tRNA5,tRNA6,tRNA7,tRNA8,tRNA9)

library(vegan)

remove(metabolism_summary)

metabolism_summary<-dplyr::bind_rows(MISC_combined,carbon_utilization_combined,Transporters_combined,Energy_combined,organic_nitrogen_combined,Woodcroft_combined,rRNA_combined)

names(metabolism_summary)[1] <- "gene_id"
names(metabolism_summary)[2] <- "gene_description"
names(metabolism_summary)[3] <- "function_description"
names(metabolism_summary)[4] <- "module"
names(metabolism_summary)[5] <- "header"
names(metabolism_summary)[6] <- "subheader"

metabolism_summary<-dplyr::bind_rows(MISC_combined,carbon_utilization_combined,Transporters_combined,Energy_combined,organic_nitrogen_combined,Woodcroft_combined)

t_metabolism_summary <- t(metabolism_summary)

com = metabolism_summary[,8:ncol(metabolism_summary)]

com <- t(com)

m_com <- as.matrix(com)

tot <- rowSums(m_com)
m_com <- m_com[tot > 0, ]

m_com <- na.omit(m_com)

set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds

data.scores = as.data.frame(scores(nmds)$sites)

data.scores$bin_ID <- row.names(data.scores)

for_class_merge <- read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv")

data.scores$Bin_ID <- row.names(data.scores)

data.scores <- merge(data.scores, for_class_merge, by = c("Bin_ID"))

functional_NMDS_Class <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(color = Class)) +
  #geom_mark_ellipse(aes(group = cluster, label = cluster)) +
  scale_color_manual(values = c("Actinobacteria" = "#C2DD9B", "Alphaproteobacteria" = "#B4CCDF", "Bacilli_A" = "#4A75AA", "Bacteroidia" = "#E3A29D", "Gammaproteobacteria" = "#C04335", "Gemmatimonadetes" = "#ECC485", "Myxococcia" = "#E18B46", "Nitrososphaeria" = "#4B9B7A", "Nitrospiria" = "#5F4190", "Polyangia" = "#CA6728", "Saccharimonadia" = "#9C623C", "Thermoanaerobaculia" = "#666666", "Thermoleophilia" = "#D43F88","Verrucomicrobiae" = "#FFFFB0", "Vicinamibacteria" = "#7470AF")) +
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(colour = "black", size=22), 
        axis.text.y = element_text(colour = "black", size=22),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=22, face="bold"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "NMDS1", colour = "Class", y = "NMDS2", shape = "Type") +
  ylim(-1.5, 1.1)
#Cluster 1 vs rest
metabolism_summary_for_dotplot <- gather(metabolism_summary, bin, count, "10_bin.1":"5_bin.9")
metabolism_summary_for_dotplot_class <- subset(metabolism_summary_for_dotplot, select = -c(gene_description, gene_id, function_description, module, subheader))

conflicts_prefer(dplyr::filter)
metabolism_summary_for_dotplot_class <- filter(metabolism_summary_for_dotplot_class, count > 0)

soils.taxa.csv <- read.csv(file="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv", header = TRUE, na.strings = "unclassified", fileEncoding="UTF-8-BOM", check.names = FALSE, row.names = 1)

soils_taxa <- tibble::rownames_to_column(soils.taxa.csv, "bin")

metabolism_summary_for_dotplot_class <- merge(metabolism_summary_for_dotplot_class, soils_taxa, by ="bin")

metabolism_summary_for_dotplot_Nitrososphaeria <- metabolism_summary_for_dotplot_class %>% filter(grepl('Nitrososphaeria', Class))

metabolism_summary_for_dotplot_Nitrososphaeria <- subset(metabolism_summary_for_dotplot_Nitrososphaeria, select = -c(Treatment))

library(data.table)
setDT(metabolism_summary_for_dotplot_Nitrososphaeria)
metabolism_summary_for_dotplot_Nitrososphaeria1  = metabolism_summary_for_dotplot_Nitrososphaeria [ , .(counted = sum(count)), by = .(header, bin, Class)]

metabolism_summary_for_dotplot_rest <- metabolism_summary_for_dotplot_class %>% filter(!grepl('Nitrososphaeria', Class))

metabolism_summary_for_dotplot_rest <- subset(metabolism_summary_for_dotplot_rest, select = -c(Treatment))

library(data.table)
setDT(metabolism_summary_for_dotplot_rest)
metabolism_summary_for_dotplot_rest1  = metabolism_summary_for_dotplot_rest [ , .(counted = sum(count)), by = .(header, bin, Class)]


to_merge_gene_counts_Nitro <- metabolism_summary_for_dotplot_Nitrososphaeria1 %>%
  add_column(add_column = "Archaea")
to_merge_gene_counts_rest <- metabolism_summary_for_dotplot_rest1 %>%
  add_column(add_column = "Bacteria")

Cluster1_Nitro_rest_gene <- rbind(to_merge_gene_counts_Nitro, to_merge_gene_counts_rest)

Cluster1_Nitro_rest_gene$num <- Cluster1_Nitro_rest_gene$add_column

Cluster1_Nitro_rest_gene$num <- as.numeric(str_replace_all(Cluster1_Nitro_rest_gene$num, c("Archaea" = "26", "Bacteria" ="886")))

Cluster1_Nitro_rest_gene$norm <- Cluster1_Nitro_rest_gene$counted/Cluster1_Nitro_rest_gene$num

Cluster1_Nitro_rest_gene$header <- factor(Cluster1_Nitro_rest_gene$header, levels=c("Information systems", "Peptidase", "Amino Acid", "central carbon", "CAZY", "C1", "Electron transport Chain", "MISC", "Flagella Structure", "Oxygen", "Antibiotic Resistance", "pyruvate metabolism", "ADO-CBL synthesis", "Photosynthesis", "Sulfur", "hydrocarbon degradation", "sugar utilization (woodcroft)", "C1-methane", "aerobic corrin ring synthesis", "Flagellar cytoplasmic chaperone", "Metal Reduction", "Nitrogen", "SCFA and alcohol conversions", "CRISPR", "Vitamin B12 transport system", "Hydrogenases", "NA"))

#Cluster1_Nitro_rest_gene_count$add_column <- str_replace(Cluster1_Nitro_rest_gene_count$add_column, "Cluster 2", "Rest")



#ggboxplot(Cluster1_Nitro_rest_gene, x = "header", y = "counted",
   #       facet.by = "add_column", width = .5) +stat_compare_means(label = "p.signif", label.y = c(45, 55, 55)) +
  #labs(x = "Gene category", y = "Total gene count (log)") +
  #scale_y_continuous(trans='log10', breaks = c(1,10,100, 1000), limits = c(1,1000)) +
  #My_Theme 


##rename headers
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"Amino Acid", "Amino acid")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"central carbon", "Central carbon")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"Electron transport Chain", "Electron transport chain")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"Antibiotic Resistance", "Antibiotic resistance")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"pyruvate metabolism", "Pyruvate metabolism")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"Flagella Structure", "Flagella structure")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"hydrocarbon degradation", "Hydrocarbon degradation")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"sugar utilization (woodcroft)", "Sugar utilization (Woodcroft)")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"aerobic corrin ring synthesis", "Aerobic corrin ring synthesis")
Cluster1_Nitro_rest_gene$header <- str_replace(Cluster1_Nitro_rest_gene$header,"Metal Reduction", "Metal reduction")



kruskal.test(counted ~ add_column, data = Cluster1_Nitro_rest_gene)
pairwise.wilcox.test(Cluster1_Nitro_rest_gene$counted, Cluster1_Nitro_rest_gene$add_column)

cluster_metabolism_compared <- compare_means(norm ~ add_column,  data = Cluster1_Nitro_rest_gene,
                                             group.by = "header")

cluster_metabolism_compared

metabolism_summary_for_dotplot <- gather(metabolism_summary, bin, count, "10_bin.1":"5_bin.9")

for_treatment <- read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv")

for_treatment <- subset(for_treatment, select = c(Bin_ID, Treatment))

DRAM_by_treatment <- merge(metabolism_summary_for_dotplot, for_treatment, by.x = "bin", by.y = "Bin_ID")

DRAM_by_treatment_for_dotplot <- subset(DRAM_by_treatment, select = c(header, Treatment, count))

#DRAM_by_treatment_treatment_for_dotplot <- subset(DRAM_by_treatment_for_dotplot, select = c(header, Treatment, count))

conflicts_prefer(dplyr::count)
Treatment_freq <- subset(for_treatment, select = c(Treatment))
Treatment_freq <- dplyr::rename(count(Treatment_freq, Treatment), Freq = n)



conflicts_prefer(dplyr::filter)
DRAM_by_treatment_for_dotplot <- filter(DRAM_by_treatment_for_dotplot, count > 0)

DRAM_by_treatment_for_dotplot$count <- as.numeric(DRAM_by_treatment_for_dotplot$count)

library(data.table)
setDT(DRAM_by_treatment_for_dotplot)
DRAM_by_treatment_for_dotplot1  = DRAM_by_treatment_for_dotplot [ , .(counted = sum(count)), by = .(header, Treatment)]

DRAM_by_treatment_for_dotplot_norm <- merge(DRAM_by_treatment_for_dotplot1, Treatment_freq, by = c("Treatment"))
DRAM_by_treatment_for_dotplot_norm$normalized <- DRAM_by_treatment_for_dotplot_norm$counted/DRAM_by_treatment_for_dotplot_norm$Freq




Treatment_metabolism_compared <- compare_means(counted ~ Treatment,  data = DRAM_by_treatment_for_dotplot1,
                                               group.by = "header")

Treatment_metabolism_compared

metabolism_summary_for_dotplot <- gather(metabolism_summary, bin, count, "10_bin.1":"5_bin.9")
metabolism_summary_for_dotplot_treatment <- merge(metabolism_summary_for_dotplot, for_treatment, by.x = "bin", by.y = "Bin_ID")
metabolism_summary_for_dotplot_treatment <- subset(metabolism_summary_for_dotplot_treatment, select = -c(gene_description, gene_id, function_description, module, subheader, unique_id))

conflicts_prefer(dplyr::filter)
metabolism_summary_for_dotplot_treatment <- filter(metabolism_summary_for_dotplot_treatment, count > 0)

library(data.table)
setDT(metabolism_summary_for_dotplot_treatment)
metabolism_summary_for_dotplot_treatment1  = metabolism_summary_for_dotplot_treatment [ , .(counted = sum(count)), by = .(header, Treatment, bin)]



DRAM_overview_per_treatment <- ggplot(metabolism_summary_for_dotplot_treatment1, aes(x = reorder(header, -counted), y = counted, fill = header)) +
  scale_y_continuous(trans='log10') +
  geom_boxplot() +
  facet_wrap(~Treatment, ncol = 1)+
  scale_fill_manual(values = c("#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C","white", "#E31A1C", "white", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C","white", "white", "#E31A1C", "white", "white", "white", "#E31A1C", "#E31A1C")) +
  labs(x = "Gene category", y = "Average gene counts / bin / water mass (Log)") +
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(angle=90, colour = "black", vjust=1, hjust = 1, size=22), 
        axis.text.y = element_text(colour = "black", size=22),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 22),
        legend.title =element_text(face="bold",size = 24),
        legend.text = element_text(size = 22),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=18, face="bold"),
        strip.text.y = element_text(size=18, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")


DRAM_overview_per_treatment


Cluster1_Nitro_rest_gene$add_column <- str_replace(Cluster1_Nitro_rest_gene$add_column,"Cluster 1", "Archaea")
Cluster1_Nitro_rest_gene$add_column <- str_replace(Cluster1_Nitro_rest_gene$add_column,"Cluster 2", "Bacteria")

Cluster1_Nitro_rest_boxplot <- ggplot(Cluster1_Nitro_rest_gene, aes(x = reorder(header, -norm), y = norm, fill = header)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C","white", "#E31A1C", "white", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C","white", "white", "#E31A1C", "white", "white", "white", "#E31A1C")) +
  geom_hline(yintercept=0.1, linetype='dotted', col = 'red') +
  facet_wrap(~add_column, ncol = 1) +
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(angle=90, colour = "black", size=22, vjust=0.5, hjust = 1), 
        axis.text.y = element_text(colour = "black", size=22),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=22, face="bold"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  labs(x = "Functional category", y = "Normalized gene count/cluster (log10 scaling)") +
  scale_y_continuous(trans='log10')
#scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#1ff8ff", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#4b6a53", "#b249d5", "#7edc45", "#5c47b8", "#cfd251"))
Cluster1_Nitro_rest_boxplot

pdf("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Yugen_Figures/Archaea_Nitro_Bacteria_rest_boxplot.pdf", width = 12, height = 15)
Cluster1_Nitro_rest_boxplot
dev.off()

pdf("/Users/yugibeast/Library/CloudStorage/Dropbox/Ganasamurthy_Global_MAG_paper/Figures/Main_Figures/Figure_5.pdf", width = 18, height = 12) # Open a new pdf file
grid.arrange(functional_NMDS_Class, Cluster1_Nitro_rest_boxplot, ncol = 2)
dev.off()


###Figure 5. ordinatino and abundance overtime
#NMDS
#OTU table
soils.otu.csv <- read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig3_and_4/Soil_ORF_COMPILE_edited.csv")


for_sample_name <- read_excel("/Users/yugibeast/Library/CloudStorage/OneDrive-UniversityofOtago/ARF/Manuscripts/Soil_Global_MAG/SOIL_BINS-5.xlsx", sheet = "Bin_Overview")

for_sample_name <- subset(for_sample_name, select = c(Bin_ID, Sample))

colnames(for_sample_name) <- c("Bin", "Sample_Name")




soils.otu <- merge(soils.otu.csv, for_sample_name, by = "Bin")

soils.otu <- subset(soils.otu, select = c(Bin, X..of.Hits, Sample))

#soils.otu$X..of.Hits <- as.numeric(soils.otu$X..of.Hits)

#soils.otu.trix <- pivot_wider(soils.otu, names_from = Sample_Name, values_from = X..of.Hits)

#soils.otu.trix  <- spread(soils.otu, Sample_Name, X..of.Hits)
#soils.otu.trix <- dcast(soils.otu, Bin~Sample, value.var="X..of.Hits")

#daply(soils.otu, .(Bin, Sample), function(x) x$X..of.Hits)
soils.otu.trix <- reshape(soils.otu, idvar="Bin", timevar="Sample", direction="wide")

names(soils.otu.trix) = gsub(pattern = "X..of.Hits.", replacement = "", x = names(soils.otu.trix))

library(tidyverse)
soils.otu.trix <- soils.otu.trix %>% remove_rownames %>% column_to_rownames(var="Bin")

#soils.otu.trix <- soils.otu.trix[ , order(names(soils.otu.trix))]

soils.otu.matrix <- as.matrix(soils.otu.trix)

soils.otu <- otu_table(soils.otu.trix, taxa_are_rows = TRUE)





#Taxa table
soils.taxa.csv <- read.csv(file="/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig1/Bin_taxonomy_Table_MAGS.csv", header = TRUE, na.strings = "unclassified", fileEncoding="UTF-8-BOM", check.names = FALSE, row.names = 1)



# #####
# #To allow all ranks to be shown I recoded the taxa_table file. This is how it was done. 
# #update taxonomy
# temp_meta_file <- read.csv(file="/Volumes/micro-shared$/MoralesLab/Projects/MOTS/MOTS_MAGs/bin_taxonomy.csv", header = TRUE, na.strings = "unclassified", fileEncoding="UTF-8-BOM", check.names = FALSE, row.names = 1)
# 
# library(stringr)
# #split 'player' column using '_' as the separator
# temp_meta_file[c('Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species')] <- str_split_fixed(temp_meta_file$Classification, ';', 7)
# 
# temp_meta_file[, c(2:8)] <- as.data.frame(apply(temp_meta_file[,c(c(2:8))], 2,
#                                          function(temp_meta_file) {
#                                            gsub(".__", "", temp_meta_file)}
# ))
# 
# temp_meta_file<-temp_meta_file[ -c(1) ]
# 
# #remove taxonomy columns
# mots.taxa.csv <- mots.taxa.csv[ -c(1:5) ]
# 
# 
# #merge taxa_table
# new_taxa_table<-merge(mots.taxa.csv,temp_meta_file,by=0)
# library(tibble)
# new_taxa_table<-column_to_rownames(new_taxa_table, var = "Row.names")
# 
# #reorder
# new_taxa_table<-new_taxa_table %>% 
#    # dplyr::relocate(disp) %>% ## simply make disp the first column
#    relocate(c("Domain":"Species"))
# 
# 
# #save as csv and check file for errors
# write.csv(new_taxa_table, "/Volumes/micro-shared$/MoralesLab/Projects/MOTS/MOTS_MAGs/read_recruitment_to_bins/import/taxa_table.csv", row.names = T)

soils.taxa.matrix <- as.matrix(soils.taxa.csv)

soils.taxa <- tax_table(soils.taxa.matrix)


#Meta-data
soils.sample.meta <- read.csv(file="/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig3_and_4/Soil_metadata.csv", header = TRUE, fileEncoding="UTF-8-BOM", check.names = FALSE)
soils.meta.df <- as.data.frame(soils.sample.meta)

rownames(soils.sample.meta) <- soils.sample.meta$Sample

soils.meta <- sample_data(soils.meta.df)


#Fix samples names to link w/ OTU

sample_names(soils.meta) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50") # don't do this manually 
# sample_names(mots.meta) <-as.character(seq(1:83)) 


#Create phyloseq object
soils.phylo <- phyloseq(soils.otu, soils.taxa, soils.meta)

Soil_metagenome_read_counts=read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig3_and_4/Soil_metagenome_read_count_corrected.csv")

#Rename column #of Hits to Number_of_Hits 
Soil_ORF_CLASS_edited=read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig3_and_4/Soil_ORF_CLASS_edited.csv")

Soil_metadata=read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/Code_files/Fig3_and_4/Soil_metadata.csv")

#Join metagenome reads with ORF by sample ID
Soil_Joined_metagenome_ORF=merge(Soil_metagenome_read_counts,Soil_ORF_CLASS_edited, by="Sample", all=TRUE)

#Join merged metagenome reads +ORF above with metadata
Soil_Joined_metagenome_ORF_metadata=merge(Soil_Joined_metagenome_ORF,Soil_metadata, by="Sample", all=TRUE)


#Mutate and create new column ((# of ORF hits)/(# of Sample Reads))*1,000,000 to get ORF hits per million reads for each class present in each sample
conflicts_prefer(rstatix::mutate)

Soil_ORF_Class_hits_per_mil_reads=Soil_Joined_metagenome_ORF_metadata %>% 
  select (total_Reads,Class, Number_of_Hits, Samples,Sample, Treatment, Day,mapped_Reads,unmapped_Reads) %>%
  mutate(ORF_hits_per_million_reads=(Number_of_Hits/total_Reads)*1000000) %>%
  mutate(Percent_mapped=(mapped_Reads/total_Reads)*100)%>%
  mutate(Percent_unmapped=(unmapped_Reads/total_Reads)*100)

#Set day as factor
Soil_ORF_Class_hits_per_mil_reads$Day<-as.factor(Soil_ORF_Class_hits_per_mil_reads$Day)
Soil_ORF_Class_hits_per_mil_reads$Samples<-factor(Soil_ORF_Class_hits_per_mil_reads$Samples, levels=c("205R1","205R2","205R3","P205","210R1","210R2","210R3","P210","215R1","215R2","215R3","P215","225R1","225R2","225R3","P225","230R1", "230R2","230R3","P230", "P305","P310","P315","P325","P330","P405","P410","P415","P425","P430","P505","P510","P515","P525","P530","P605","P610","P615","P625","P630","P705","P710","P715","P725","P730","P805","P810","P815","P825","P830"))

#Calculate summary SE
library(Rmisc)
Summary_Soil_ORF_Class_hits_per_mil_reads<-summarySE(Soil_ORF_Class_hits_per_mil_reads,measurevar="ORF_hits_per_million_reads",groupvars=c("Treatment","Day","Class"))



#Remove NA's
Summary_Soil_ORF_Class_hits_per_mil_reads=Summary_Soil_ORF_Class_hits_per_mil_reads[!is.na(Summary_Soil_ORF_Class_hits_per_mil_reads$Class), ]


#write.csv
#write.csv(Summary_Soil_ORF_Class_hits_per_mil_reads,"/Users/syalinyganasamurthy/Dropbox/Ganasamurthy_Global_MAG_paper/Code_files/Fig3/Summary_Soil_ORF_Class_hits_per_mil_reads.csv")


#chnage to factor
Summary_Soil_ORF_Class_hits_per_mil_reads$Day<-as.factor(Summary_Soil_ORF_Class_hits_per_mil_reads$Day)

#order treatment
Summary_Soil_ORF_Class_hits_per_mil_reads$Treatment<-factor(Summary_Soil_ORF_Class_hits_per_mil_reads$Treatment, levels=c("NC","PC","AB","AF","AB+AF"))


#Lg by class
Lg_Soil_by_class=ggplot(Summary_Soil_ORF_Class_hits_per_mil_reads,aes(x=Day,y=ORF_hits_per_million_reads,group=Treatment,colour=Treatment))+geom_line(lwd=1)+
  geom_point(size=2)+#+geom_errorbar(aes(ymin=ORF_hits_per_million_reads-se,ymax=ORF_hits_per_million_reads+se),colour="black",width=0.25)+
  facet_wrap(~Class, scales = "free_y",ncol=3)+theme_bw()+theme(axis.title.x = element_text(face="bold",size=14),
                                                                axis.text.x = element_text(colour = "black", vjust=1, hjust = 1, size=10, angle=45),
                                                                axis.text.y = element_text(colour = "black", size=10),
                                                                axis.title.y = element_text(face="bold", size=14),
                                                                legend.title = element_text(size=12,face="bold"),
                                                                legend.text = element_text(size = 12),
                                                                legend.position="none",
                                                                #Manipulating the facet features
                                                                strip.text.x = element_text(size=10,face="bold"),
                                                                strip.background = element_rect(colour="black"))+scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958"))+xlab("Day")+ylab("Relative per million reads (RPM)")


Lg_Soil_by_class

library(vegan)

ord <- ordinate(soils.phylo, "NMDS", "bray")
p1 = plot_ordination(soils.phylo, ord, type="taxa", color="Treatment")
p1 <- p1 +
  geom_point(size = 3) +
  My_Theme +
  scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958"))

p1

pdf("/Users/yugibeast/Library/CloudStorage/Dropbox/Ganasamurthy_Global_MAG_paper/Main_Figures/NMDS_taxa_Class_RPM.pdf", width = 14, height = 12)
grid.arrange(p1, Lg_Soil_by_class,
             ncol = 2)
dev.off()

p1_samples = plot_ordination(soils.phylo, ord, type="samples", color="Treatment")
p1_samples <- p1_samples +
  geom_point(size = 3) +
  My_Theme +
  scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958"))

p1_samples

pdf("/Users/yugibeast/Library/CloudStorage/Dropbox/Ganasamurthy_Global_MAG_paper/Figures/NMDS_samples_Class_RPM.pdf", width = 14, height = 12)
grid.arrange(p1_samples, Lg_Soil_by_class,
             ncol = 2)
dev.off()

p1_samples = plot_ordination(soils.phylo, ord, type="samples", color="Treatment")
p1_samples <- p1_samples +
  geom_point(size = 3) +
  My_Theme +
  scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958"))

p1_samples



###Figure 6
#This data was pulled out from soil_gobal_MASTER (bin specifc tab), with mOTU info added according to class with VLOOKUP in excel
A = read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig5/bin_specific.csv",fill = TRUE, header = TRUE, sep = ",", check.names=FALSE)

library(dplyr)


library(tidyr)
#Change data to long format, this makes aggregating by a specific variable a lot easier
A_long= A %>% 
  pivot_longer(
    cols = c(2:35), 
    names_to = "Sample", 
    values_to = "Count")

#A <- t(A)
#A <- row_to_colnames(A, row = 1)

#A <- data.frame(A, check.names = FALSE)
#A <- tibble::rownames_to_column(A, "Sample")

#Aggregate by mOTU, Sample and Count (Hits), added class info
A_agg=aggregate(Count~Sample+Class, data=A_long, FUN=sum)  #added Class for poltting later

#Aggregate now by total of mOTU hits per sample, this now represent number of mapped reads
A_agg2=aggregate(Count~Sample, data=A_long, FUN=sum)

#Rename count to percent mapped
colnames(A_agg2)[2]= "Mapped_reads"

#Join the two df by sample
Agg_df<-merge(A_agg, A_agg2, by=c("Sample"))


#Total reads, this data was pulled out from soil_gobal_MASTER (read count tab)
B = read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig5/Total_reads.csv",fill = TRUE, header = TRUE, sep = ",", check.names=FALSE)


#Metagenome
C = read.csv("/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig5/soil_global_meta_edited.csv",fill = TRUE, header = TRUE, sep = ",", check.names=FALSE)


#Change samples to factors
Agg_df$Sample=as.factor(Agg_df$Sample)
B$Sample=as.factor(B$Sample)
C$Sample=as.factor(C$Sample)

#Left join
AB_df2<-merge(Agg_df, B, by=c("Sample"))

ABC_df2<-merge(AB_df2,C, by=c("Sample"))



#Calculate relative to per million reads and percent mapped
conflicts_prefer(rstatix::mutate)
ABC_df_hits_per_mil_reads_percent_mappped=ABC_df2 %>% 
  select (Total_reads,Sample,Count,Biomes,Habitat,Mapped_reads,Class) %>%
  mutate(Relative_per_million_reads=(Count/Total_reads)*1000000) %>%
  mutate(Percent_mapped_reads=(Mapped_reads/Total_reads)*100)
# mutate(Percent_unmapped=(unmapped_Reads/total_Reads)*100)


#write.csv(ABC_df_hits_per_mil_reads_percent_mappped,"/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig5/ABC_df_hits_per_mil_reads_percent_mappped.csv")



#Calculate summary SE
library(Rmisc)
Summary_mg_motus_hits_per_mil_reads<-summarySE(ABC_df_hits_per_mil_reads_percent_mappped,measurevar="Relative_per_million_reads",groupvars=c("Habitat","Biomes","Class")) #Class included here







#Boxplot RPM coloured by Class facet by moTU

#Agrregate data
S_agg=aggregate(Relative_per_million_reads~Habitat+Class+Biomes, data=ABC_df_hits_per_mil_reads_percent_mappped, FUN=sum)
S2_agg=aggregate(Relative_per_million_reads~Habitat+Class+Biomes, data=ABC_df_hits_per_mil_reads_percent_mappped, FUN=sum)

#Colour palette
Phyla_colour_list= c(Actinobacteria
                     ="#ff9500",Alphaproteobacteria="#8dfca2",Bacilli_A="#cc0000",Bacteroidia="#00a2ff",Gammaproteobacteria
                     ="#ff7575",Gemmatimonadetes
                     ="#ffd780",Myxococcia="#21cf3b",Nitrososphaeria="#d894ff",Nitrospiria="#939da3",Saccharimonadia="#0023a3",Thermoanaerobaculia="#00990f",Thermoleophilia="#ff1cd5",Vicinamibacteria="#820073", Verrucomicrobiae="#0affde",Polyangia="#000000")



#Calculate relative to per million reads and percent mapped
ABC_df_hits_per_mil_reads_percent_mappped=ABC_df2 %>% 
  select (Total_reads,Sample,Count,Biomes,Mapped_reads,Class) %>%
  mutate(Relative_per_million_reads=(Count/Total_reads)*1000000) %>%
  mutate(Percent_mapped_reads=(Mapped_reads/Total_reads)*100)
# mutate(Percent_unmapped=(unmapped_Reads/total_Reads)*100)


#write.csv(ABC_df_hits_per_mil_reads_percent_mappped,"/Volumes/micro-shared$/MoralesLab/Manuscripts/Global_soil_MAG_paper/Code_files/Fig5/ABC_df_hits_per_mil_reads_percent_mappped.csv")



#Calculate summary SE
library(Rmisc)
Summary_mg_motus_hits_per_mil_reads<-summarySE(ABC_df_hits_per_mil_reads_percent_mappped,measurevar="Relative_per_million_reads",groupvars=c("Biomes","Class")) #Class included here







#Boxplot RPM coloured by Class facet by moTU

#Agrregate data
S_agg=aggregate(Relative_per_million_reads~Class+Biomes, data=ABC_df_hits_per_mil_reads_percent_mappped, FUN=sum)
S2_agg=aggregate(Relative_per_million_reads~+Class+Biomes, data=ABC_df_hits_per_mil_reads_percent_mappped, FUN=sum)



library(base)


library(ggplot2)




#Boxplot RPM by Class 


Phyla_colour_list= c(Actinobacteria
                     ="#ff9500",Alphaproteobacteria="#8dfca2",Bacilli_A="#cc0000",Bacteroidia="#00a2ff",Gammaproteobacteria
                     ="#ff7575",Gemmatimonadetes
                     ="#ffd780",Myxococcia="#21cf3b",Nitrososphaeria="#d894ff",Nitrospiria="#939da3",Saccharimonadia="#0023a3",Thermoanaerobaculia="#00990f",Thermoleophilia="#ff1cd5",Vicinamibacteria="#820073", Verrucomicrobiae="#0affde",Polyangia="#000000")

Boxplot_RPM_Class=ggplot(ABC_df_hits_per_mil_reads_percent_mappped, aes(x= Biomes, y=Relative_per_million_reads, group = Biomes, fill=Class, shape=Biomes),alpha=0.1) + 
  facet_wrap(~Class , scales="free",
             labeller = labeller(Class = label_wrap_gen(width = 20, multi_line=TRUE)))+ 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2)+
  ylab("Relative per million reads (RPM)") + 
  xlab("Biomes") + 
  scale_fill_manual(values=Phyla_colour_list)+
  theme_light()+
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(angle=45, colour = "black", vjust=0.75, hjust = 0.5, size=22), 
        axis.text.y = element_text(colour = "black", size=22),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 14),
        legend.position="right",
        legend.key.size = unit(1, "cm"),
        panel.background = element_blank(),
        strip.text.x = element_text(size=22, color="black"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black")) #+
#scale_y_continuous(trans="log2",labels = trans_format("log2", math_format(2^.x)))
Boxplot_RPM_Class=Boxplot_RPM_Class+scale_x_discrete(labels=c("Boreal forest" = " Boreal \n forest", "Grassland" = "Grass \n land","Tropical rainforest" = "Tropical \n rainforest"))

Boxplot_RPM_Class

ggsave("/Users/yugibeast/Library/CloudStorage/Dropbox/Ganasamurthy_Global_MAG_paper/Figures/Main_Figures/Figure_7.pdf",width=30,height=45,units ="in", device="pdf")  #30, 45


pdf("/Users/yugibeast/Library/CloudStorage/Dropbox/Ganasamurthy_Global_MAG_paper/Figures/Main_Figures/Figure_7s.pdf", width=25,height=30)
Boxplot_RPM_Class
dev.off()





###NMDS for 16S
Bact_16S_matrix <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/dismat_Bact.csv")
Bact_16S_matrix <- column_to_rownames(Bact_16S_matrix, var = "X")
Bact_16S_matrix <- t(Bact_16S_matrix)
Bact_16S_matrix <- as.matrix(Bact_16S_matrix)

#Bact_16S_matrix <- Bact_16S_matrix[sort(rownames(Bact_16S_matrix)), sort(colnames(Bact_16S_matrix))]
#write.csv(Bact_16S_matrix, "/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/dismat_alphabetical_Bact.csv")

Bact_16S_sample <- read_excel("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/16S_sample_data.xlsx")
Bact_16S.mds <- metaMDS(Bact_16S_matrix, distance = "bray", autotransform = FALSE)
Bact_16S.mds

Bact_16S_data.scores <- as.data.frame(scores(Bact_16S.mds))

Bact_16S_data.scores <- rownames_to_column(Bact_16S_data.scores, var = "Sample")
  
Bact_16S_data.scores <- merge(Bact_16S_data.scores, Bact_16S_sample, by = "Sample")

Bact_16S_data.scores$Treatment <- str_replace(Bact_16S_data.scores$Treatment,"Antibacterial", "AB")
Bact_16S_data.scores$Treatment <- str_replace(Bact_16S_data.scores$Treatment,"Antifungal", "AF")
Bact_16S_data.scores$Treatment <- str_replace(Bact_16S_data.scores$Treatment,"Antibacterial+Antifungal", "AB+AF")
Bact_16S_data.scores$Treatment <- str_replace(Bact_16S_data.scores$Treatment,"Positive Control", "PC")
Bact_16S_data.scores$Treatment <- str_replace(Bact_16S_data.scores$Treatment,"Negative Control", "NC")

Bact_16S_NMDS <- ggplot(Bact_16S_data.scores, aes(x = NMDS1, y = NMDS2)) +
  scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958")) +
  geom_point(size = 4, aes(color = Treatment)) +
  theme(axis.title.x = element_text(face="bold",size=24),
        axis.text.x = element_text(colour = "black", size=22), 
        axis.text.y = element_text(colour = "black", size=22),
        axis.title.y = element_text(face="bold",size=24),
        plot.title = element_text(size = 24),
        legend.title =element_text(face="bold",size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=22, face="bold"),
        strip.text.y = element_text(size=22, face="bold"),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "16S NMDS1", colour = "Treatment", y = "16S NMDS2", shape = "Type")

grid.arrange(p1_samples, Bact_16S_NMDS)

##Mantel test

library(phyloseq)  
library(ggplot2)
library(vegan)
library(plyr)
library(dplyr)
library(grid)
library(Rmisc)
#library(scales)
library(reshape)
#library(reshape2)
library(RColorBrewer)
#library(microbiome)
library(MultNonParam)
library(PMCMR)
library(DescTools)
library(FSA)
library(ggpubr)


#Importing biom file and setting working directory for prokaryotes and fungi


setwd("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/")
#Bacteria
# Now import the .biom-formatted otu_table-tax_table file.
Bact_biom_otu_tax <- import_biom("10000_merged_otu_table_json.biom")

# Import sample data
Bact_bmsd <- import_qiime_sample_data("Rex_antibiotic_16S_Sainur.txt")



#Merging otu table with metadata and creating a phyloseq object


#Merge into phyloseq format for prokaryotes
Bacteria_phyloseq <- merge_phyloseq(Bact_biom_otu_tax, Bact_bmsd)

#check your new merged file and all your datasets should be accounted for
Bacteria_phyloseq



#Attach OTU ID


#Prokaryotes
tax_table(Bacteria_phyloseq) <- cbind(tax_table(Bacteria_phyloseq), OTU=taxa_names(Bacteria_phyloseq))



#Renaming ranks


#Prokaryotes
colnames(tax_table(Bacteria_phyloseq)) = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU")
#This step deletes extra characters that indicate the taxonomic level. If you wish to plot class, family, genus, etc, you will need to repeat this step replacing replacing the number with the level you wish to remove. 
tax_table(Bacteria_phyloseq) =gsub("D_0__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_1__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_2__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_3__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_4__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_5__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_6__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_7__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_8__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_9__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_10__", "", tax_table(Bacteria_phyloseq))
tax_table(Bacteria_phyloseq) =gsub("D_11__", "", tax_table(Bacteria_phyloseq))




#Avergae results for multiple rarefractions

#Prokaryotes
Bacteria_phyloseq = transform_sample_counts(Bacteria_phyloseq, function(x) x/10)
sample_sums(Bacteria_phyloseq)



#Round off and confirm count number

#Prokaryotes
Bacteria_phyloseq = transform_sample_counts(Bacteria_phyloseq, round)
sample_sums(Bacteria_phyloseq)
Bacteria_phyloseq = prune_samples(sample_sums(Bacteria_phyloseq)>=1, Bacteria_phyloseq)
sample_sums(Bacteria_phyloseq)




#Identify if there are taxa which don't have a count 


#Prokaryotes
sum(taxa_sums(Bacteria_phyloseq) > 0)
any(taxa_sums(Bacteria_phyloseq)== 0)
sum(taxa_sums(Bacteria_phyloseq) == 0)
any(taxa_sums(Bacteria_phyloseq) > 1)
sum(taxa_sums(Bacteria_phyloseq) > 1)
any(taxa_sums(Bacteria_phyloseq) < 1)
sum(taxa_sums(Bacteria_phyloseq) < 1)


#Create new file with taxa that have count data


#Create new file with only present (no zeroes) taxa
#Prokaryotes
Bacteria_phyloseq = prune_taxa(taxa_sums(Bacteria_phyloseq) > 1, Bacteria_phyloseq)
any(sample_sums(Bacteria_phyloseq) == 0)
any(sample_sums(Bacteria_phyloseq) > 0)
sum(taxa_sums(Bacteria_phyloseq) > 0)
any(sample_sums(Bacteria_phyloseq) < 1)
sum(taxa_sums(Bacteria_phyloseq) < 1)


#Bacteria_df<-psmelt(Bacteria_phyloseq)
#write.csv(Bacteria_df, file ="/Volumes/S_ GANASAMU/Paper 3/Section 1/Bacteria_df.csv")


#Compare sequences per sample or OTU


#Prokaryotes
Bactreadsumsdf = data.frame(nreads = sort(taxa_sums(Bacteria_phyloseq),TRUE), sorted = 1:ntaxa(Bacteria_phyloseq), type = "OTU")

Bactreadsumsdf = rbind(Bactreadsumsdf,data.frame(nreads = sort(sample_sums(Bacteria_phyloseq),TRUE),sorted = 1:nsamples(Bacteria_phyloseq), type = "Samples"))

title = "Total number of reads"

Bact_seq_persample = ggplot(Bactreadsumsdf, aes(x = sorted, y = nreads)) +
  geom_bar(stat = "identity")

Bact_seq_persample + ggtitle(title) +
  scale_y_log10() +
  facet_wrap(~type, 1, scales = "free")





###MERGE SAMPLES
sample_16S_data <- read.csv("/Volumes/micro-shared$/MoralesLab/Projects/Syaliny_Soils/MAGs/data_analysis_by_paper/Global_MAG_paper/raw_data_for_figs/sample_16S_data.csv")
sample_16S_data <- column_to_rownames(sample_16S_data, var = "X")

sample_data(Bacteria_phyloseq) <- sample_16S_data

BP = Bacteria_phyloseq
BP = prune_taxa(taxa_sums(Bacteria_phyloseq) > 0, Bacteria_phyloseq)
MAG_samples = c("205R1", "205R2", "205R3", "210R1", "210R2", "210R3", "215R1", "215R2", "215R3", "225R1", "225R2", "225R3", "230R1", "230R2", "230R3", "P205", "P210", "P215", "P225", "P230", "P305", "P310", "P315", "P325", "P330", "P405", "P410", "P415", "P425", "P430", "P505", "P510", "P515", "P525", "P530", "P605", "P610", "P615", "P625", "P630", "P705", "P710", "P715", "P725", "P730", "P805", "P810", "P815", "P825", "P830")
sample_data(BP)$MAG <- get_variable(BP, "Pooled")

mergedBP = merge_samples(BP, "Pooled")
SD = merge_samples(sample_data(BP), "Pooled")
print(SD[, "Pooled"])

print(mergedBP)

sample_names(BP)

sample_names(mergedBP)

identical(SD, sample_data(mergedGP))


#unpooled adding
BP2 = Bacteria_phyloseq
BP2 = prune_taxa(taxa_sums(Bacteria_phyloseq) > 0, Bacteria_phyloseq)
MAG_samples = c("205R1", "205R2", "205R3", "210R1", "210R2", "210R3", "215R1", "215R2", "215R3", "225R1", "225R2", "225R3", "230R1", "230R2", "230R3", "P205", "P210", "P215", "P225", "P230", "P305", "P310", "P315", "P325", "P330", "P405", "P410", "P415", "P425", "P430", "P505", "P510", "P515", "P525", "P530", "P605", "P610", "P615", "P625", "P630", "P705", "P710", "P715", "P725", "P730", "P805", "P810", "P815", "P825", "P830")
sample_data(BP2)$MAG <- get_variable(BP2, "not_pooled")

mergedBP2 = merge_samples(BP2, "not_pooled")
SD = merge_samples(sample_data(BP2), "not_pooled")
print(SD[, "not_pooled"])

print(mergedBP2)

sample_names(BP2)

sample_names(mergedBP2)


new_mergedBP2 <- prune_samples(!(sample_names(mergedBP2) %in% "pooled"), mergedBP2)
sample_names(new_mergedBP2)

MAG_16S_phyloseq <- merge_phyloseq(mergedBP, new_mergedBP2)

new_16S_metadata <- sample_names(MAG_16S_phyloseq)

new_16S_metadata <- data.frame(new_16S_metadata)

#new_16S_metadata <- new_16S_metadata[order(new_16S_metadata$new_16S_metadata),]
new_16S_metadata <- data.frame(new_16S_metadata)

new_16S_metadata$SampleID <- new_16S_metadata$new_16S_metadata
new_16S_metadata$new_16S_metadata <- NULL
new_16S_metadata$Treatment <- c("AB", "AF", "AB+AF", "PC", "NC", "AB", "AF", "AB+AF", "PC", "NC", "AB", "AF", "AB+AF", "PC", "NC", "AB", "AF", "AB+AF", "PC", "AB", "AF", "AB+AF", "PC", "NC", "AB", "AF", "AB+AF", "PC", "NC", "AB", "AF", "AB+AF", "PC", "NC", "AB", "AB", "AB", "AF", "AF", "AF", "AB+AF", "AB+AF", "AB+AF", "PC", "PC", "PC", "NC", "NC", "NC")

new_16S_metadata <- new_16S_metadata[, c(2,1)]


sample_data(MAG_16S_phyloseq) <- sample_data(new_16S_metadata)

ord16S <- ordinate(MAG_16S_phyloseq, "NMDS", "bray")
ordination_MAG_16S_phyloseq = plot_ordination(MAG_16S_phyloseq, ord16S, type = "samples")

ordination_MAG_16S_phyloseq <- ordination_MAG_16S_phyloseq +
  geom_point(size = 3) +
  My_Theme #+
scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958"))

ordination_MAG_16S_phyloseq

grid.arrange(p1_samples, ordination_MAG_16S_phyloseq)

library(tidyverse)
new_16S_metadata <- new_16S_metadata %>% column_to_rownames(var="SampleID")

new_16S_metadata %>%
  row_to_names(row_number = "Treatment")

sample_data(MAG_16S_phyloseq) <- new_16S_metadata

  
sample_data(MAG_16S_phyloseq)

ord16S <- ordinate(MAG_16S_phyloseq, "NMDS", "bray", color = "Treatment")



ordination_MAG_16S_phyloseq = plot_ordination(MAG_16S_phyloseq, ord16S, type = "samples", color = "Treament")

ordination_MAG_16S_phyloseq <- ordination_MAG_16S_phyloseq +
  geom_point(size = 3) +
  My_Theme +
scale_color_manual(values = c("AB" = "#0C0881", "AF" = "#7316A2", "AB+AF" = "#BD5077", "PC" = "#EA9953", "NC" = "#F2F958"))

ordination_MAG_16S_phyloseq

dist_matrix_16S <- distance(MAG_16S_phyloseq, method = "bray")
dist_matrix_16S <- as.matrix(dist_matrix_16S)

dist_matrix_16S <- dist_matrix_16S[sort(rownames(dist_matrix_16S)), sort(colnames(dist_matrix_16S))]

dist_matrix_MAGs <- distance(soils.phylo, method = "bray", check.names = FALSE)
dist_matrix_MAGs <- as.matrix(dist_matrix_MAGs)


dist_matrix_MAGs <- dist_matrix_MAGs[ order(as.numeric(row.names(dist_matrix_MAGs))), ]
dist_matrix_MAGs <- dist_matrix_MAGs[, rownames(dist_matrix_MAGs)]

mantel(dist_matrix_16S, dist_matrix_MAGs, method = "spearman", permutations = 9999, na.rm = TRUE)
